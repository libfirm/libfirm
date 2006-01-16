/**
 * Author:      Daniel Grund, Sebastian Hack
 * Date:		29.09.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "pset.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "debug.h"
#include "irgwalk.h"

#include "besched.h"
#include "bespill.h"
#include "benode_t.h"
#include "bechordal_t.h"

typedef struct _reloader_t reloader_t;
typedef struct _spill_info_t spill_info_t;

struct _reloader_t {
	reloader_t *next;
	ir_node *reloader;
};

struct _spill_info_t {
	ir_node *spilled_node;
	reloader_t *reloaders;
};

typedef struct _spill_ctx_t {
	ir_node *spilled;  /**< The spilled node. */
	ir_node *user;     /**< The node this spill is for. */
	ir_node *spill;    /**< The spill itself. */
} spill_ctx_t;

struct _spill_env_t {
	firm_dbg_module_t *dbg;
	const arch_register_class_t *cls;
	const be_chordal_env_t *chordal_env;
	struct obstack obst;
	set *spill_ctxs;
	set *spills;				/**< all spill_info_t's, which must be placed */
	pset *mem_phis;				/**< set of all special spilled phis. allocated and freed seperately */
	decide_irn_t is_mem_phi;	/**< callback func to decide if a phi needs special spilling */
	void *data;					/**< data passed to all callbacks */
};

static int cmp_spillctx(const void *a, const void *b, size_t n) {
	const spill_ctx_t *p = a;
	const spill_ctx_t *q = b;
	return !(p->user == q->user && p->spilled == q->spilled);
}

static int cmp_spillinfo(const void *x, const void *y, size_t size) {
	const spill_info_t *xx = x;
	const spill_info_t *yy = y;
	return ! (xx->spilled_node == yy->spilled_node);
}

spill_env_t *be_new_spill_env(firm_dbg_module_t *dbg,
							  const be_chordal_env_t *chordal_env,
							  decide_irn_t is_mem_phi, void *data) {

	spill_env_t *env = malloc(sizeof(env[0]));
	env->spill_ctxs  = new_set(cmp_spillctx, 1024);
	env->spills      = new_set(cmp_spillinfo, 1024);
	env->cls         = chordal_env->cls;
	env->dbg         = dbg;
	env->is_mem_phi  = is_mem_phi;
	env->data        = data;
	env->chordal_env = chordal_env;
	obstack_init(&env->obst);
	return env;
}

void be_delete_spill_env(spill_env_t *senv) {
	del_set(senv->spill_ctxs);
	del_set(senv->spills);
	obstack_free(&senv->obst, NULL);
	free(senv);
}

static spill_ctx_t *be_get_spill_ctx(set *sc, ir_node *to_spill, ir_node *ctx_irn) {
	spill_ctx_t templ;

	templ.spilled = to_spill;
	templ.user    = ctx_irn;
	templ.spill   = NULL;

	return set_insert(sc, &templ, sizeof(templ), HASH_COMBINE(HASH_PTR(to_spill), HASH_PTR(ctx_irn)));
}

static ir_node *be_spill_irn(spill_env_t *senv, ir_node *irn, ir_node *ctx_irn) {
	spill_ctx_t *ctx;
	DBG((senv->dbg, LEVEL_1, "%+F in ctx %+F\n", irn, ctx_irn));

	ctx = be_get_spill_ctx(senv->spill_ctxs, irn, ctx_irn);
	if(!ctx->spill) {
		const be_main_env_t *env = senv->chordal_env->main_env;
		ctx->spill = be_spill(env->node_factory, env->arch_env, irn, ctx_irn);
	}

	return ctx->spill;
}

/**
 * If the first usage of a phi result would be out of memory
 * there is no sense in allocating a register for it.
 * Thus we spill it and all its operands to the same spill slot.
 * Therefore the phi/dataB becomes a phi/Memory
 */
static ir_node *be_spill_phi(spill_env_t *senv, ir_node *phi, ir_node *ctx_irn) {
	int i, n = get_irn_arity(phi);
	ir_node **ins, *bl = get_nodes_block(phi);
	ir_graph *irg = senv->chordal_env->irg;
	spill_ctx_t *ctx;

	assert(is_Phi(phi));
	DBG((senv->dbg, LEVEL_1, "%+F in ctx %+F\n", phi, ctx_irn));

	/* search an existing spill for this context */
	ctx = be_get_spill_ctx(senv->spill_ctxs, phi, ctx_irn);

	/* if not found spill the phi */
	if(!ctx->spill) {
		/* build a new PhiM with dummy in-array */
		ins  = malloc(n * sizeof(ins[0]));
		for(i=0; i<n; ++i)
			ins[i] = new_r_Unknown(irg, mode_M);
		ctx->spill = new_r_Phi(senv->chordal_env->irg, bl, n, ins, mode_M);
		free(ins);

		/* re-wire the phiM */
		for(i=0; i<n; ++i) {
			ir_node *arg = get_irn_n(phi, i);
			ir_node *sub_res;

			if(is_Phi(arg) && pset_find_ptr(senv->mem_phis, arg))
				sub_res = be_spill_phi(senv, arg, ctx_irn);
			else
				sub_res = be_spill_irn(senv, arg, ctx_irn);

			set_irn_n(ctx->spill, i, sub_res);
		}
	}
	return ctx->spill;
}

static ir_node *be_spill_node(spill_env_t *senv, ir_node *to_spill) {
	ir_node *res;
	if (pset_find_ptr(senv->mem_phis, to_spill))
		res = be_spill_phi(senv, to_spill, to_spill);
	else
		res = be_spill_irn(senv, to_spill, to_spill);

	return res;
}

static void phi_walker(ir_node *irn, void *env) {
	spill_env_t *senv = env;
	const arch_env_t *arch = senv->chordal_env->main_env->arch_env;

	if (is_Phi(irn) && arch_irn_has_reg_class(arch, irn, 0, senv->cls)
			&& senv->is_mem_phi(irn, senv->data)) {
		DBG((senv->dbg, LEVEL_1, "  %+F\n", irn));
		pset_insert_ptr(senv->mem_phis, irn);
	}
}

void be_insert_spills_reloads(spill_env_t *senv, pset *reload_set) {
	ir_graph *irg = senv->chordal_env->irg;
	ir_node *irn;
	spill_info_t *si;
	struct obstack ob;

	obstack_init(&ob);

	/* get all special spilled phis */
	DBG((senv->dbg, LEVEL_1, "Mem-phis:\n"));
	senv->mem_phis = pset_new_ptr_default();
	irg_walk_graph(senv->chordal_env->irg, phi_walker, NULL, senv);

	/* Add reloads for mem_phis */
	/* BETTER: These reloads (1) should only be inserted, if they are really needed */
	DBG((senv->dbg, LEVEL_1, "Reloads for mem-phis:\n"));
	for(irn = pset_first(senv->mem_phis); irn; irn = pset_next(senv->mem_phis)) {
		const ir_edge_t *e;
		DBG((senv->dbg, LEVEL_1, " Mem-phi %+F\n", irn));
		foreach_out_edge(irn, e) {
			ir_node *user = e->src;
			if (is_Phi(user) && !pset_find_ptr(senv->mem_phis, user)) {
					ir_node *use_bl = get_nodes_block(user);
					DBG((senv->dbg, LEVEL_1, " non-mem-phi user %+F\n", user));
					be_add_reload_on_edge(senv, irn, use_bl, e->pos); /* (1) */
			}
		}
	}

	/* process each spilled node */
	DBG((senv->dbg, LEVEL_1, "Insert spills and reloads:\n"));
	for(si = set_first(senv->spills); si; si = set_next(senv->spills)) {
		reloader_t *rld;
		ir_node **reloads;
		int n_reloads = 0;
		ir_mode *mode = get_irn_mode(si->spilled_node);

		/* go through all reloads for this spill */
		for(rld = si->reloaders; rld; rld = rld->next) {
			/* the spill for this reloader */
			ir_node *spill   = be_spill_node(senv, si->spilled_node);

			/* the reload */
			ir_node *bl      = is_Block(rld->reloader) ? rld->reloader : get_nodes_block(rld->reloader);
			ir_node *reload  = new_Reload(senv->chordal_env->main_env->node_factory,
				senv->cls, irg, bl, mode, spill);

			DBG((senv->dbg, LEVEL_1, " %+F of %+F before %+F\n", reload, si->spilled_node, rld->reloader));
			if(reload_set)
				pset_insert_ptr(reload_set, reload);

			/* remember the reaload */
			obstack_ptr_grow(&ob, reload);
			sched_add_before(rld->reloader, reload);
			n_reloads++;
		}

		assert(n_reloads > 0);
		reloads = obstack_finish(&ob);
		be_introduce_copies_ignore(senv->chordal_env->dom_front, si->spilled_node,
				n_reloads, reloads, senv->mem_phis);
		obstack_free(&ob, reloads);
	}

	obstack_free(&ob, NULL);

	for(irn = pset_first(senv->mem_phis); irn; irn = pset_next(senv->mem_phis)) {
		int i, n;
		for(i = 0, n = get_irn_arity(irn); i < n; ++i)
			set_irn_n(irn, i, new_r_Bad(senv->chordal_env->irg));
		sched_remove(irn);
	}

	del_pset(senv->mem_phis);
}

void be_add_reload(spill_env_t *senv, ir_node *to_spill, ir_node *before) {
	spill_info_t templ, *res;
	reloader_t *rel;

	templ.spilled_node = to_spill;
	templ.reloaders    = NULL;
	res = set_insert(senv->spills, &templ, sizeof(templ), HASH_PTR(to_spill));

	rel           = obstack_alloc(&senv->obst, sizeof(rel[0]));
	rel->reloader = before;
	rel->next     = res->reloaders;
	res->reloaders = rel;
}

void be_add_reload_on_edge(spill_env_t *senv, ir_node *to_spill, ir_node *bl, int pos) {
	ir_node *insert_bl = get_irn_arity(bl) == 1 ? sched_first(bl) : get_Block_cfgpred_block(bl, pos);
	be_add_reload(senv, to_spill, insert_bl);
}



/*

		SPILL SLOT MANAGEMENT AND OPTS

*/

typedef struct _spill_slot_t {
	unsigned size;
	unsigned offset;
	pset *members;
} spill_slot_t;

typedef struct _ss_env_t {
	firm_dbg_module_t *dbg;
	struct obstack ob;
	be_chordal_env_t *cenv;
	pmap *slots;		/* maps spill_contexts to spill_slots */
} ss_env_t;


static void compute_spill_slots_walker(ir_node *spill, void *env) {
	ss_env_t *ssenv = env;
	ir_node *ctx;
	pmap_entry *entry;
	spill_slot_t *ss;

	if (!be_is_Spill(spill))
		return;

	/* check, if this spill is for a context already known */
	ctx = get_Spill_context(spill);
	entry = pmap_find(ssenv->slots, ctx);

	if (!entry) {
		/* this is a new spill context */
		ss = obstack_alloc(&ssenv->ob, sizeof(*ss));
		ss->members = pset_new_ptr(8);
		ss->size = get_mode_size_bytes(get_irn_mode(get_irn_n(spill, 0)));
		pmap_insert(ssenv->slots, ctx, ss);
	} else {
		ir_node *irn;
		/* values with the same spill_ctx must go into the same spill slot */
		ss = entry->value;
		assert(ss->size == (unsigned)get_mode_size_bytes(get_irn_mode(get_irn_n(spill, 0))) && "Different sizes for the same spill slot");
		for (irn = pset_first(ss->members); irn; irn = pset_next(ss->members)) {
			/* use values_interfere here, because it uses the dominance check,
			   which does work for values in memory */
			assert(!values_interfere(spill, irn) && "Spills for the same spill slot must not interfere!");
		}
	}

	pset_insert_ptr(ss->members, spill);
}

static int ss_sorter(const void *v1, const void *v2) {
	const spill_slot_t *ss1 = v1;
	const spill_slot_t *ss2 = v2;
	return ((int) ss2->size) - ((int) ss1->size);
}


/* NOTE/TODO: This function assumes, that all spill slot sizes are a power of 2.
   Further it assumes, that the alignment is equal to the size and the
   baseaddr of the spill area is aligned sufficiently for all possible aligments.
*/
static void coalesce_slots(ss_env_t *ssenv) {
	int i, o, used_slots;
	unsigned curr_offset;
	pmap_entry *entr;
	spill_slot_t **ass;

	/* Build an array of all spill slots */
	int count = pmap_count(ssenv->slots);
	ass = obstack_alloc(&ssenv->ob, count * sizeof(*ass));

	i=0;
	pmap_foreach(ssenv->slots, entr)
		ass[i++] = entr->value;

	/* Sort the array to minimize fragmentation and cache footprint.
	   Large slots come first */
	qsort(ass, count, sizeof(ass[0]), ss_sorter);

	/* For each spill slot:
		- assign a new offset to this slot
	    - xor find another slot to coalesce with */
	curr_offset = 0;
	used_slots = 0;
	for (i=0; i<count; ++i) { /* for each spill slot */
		ir_node *n1;
		int tgt_slot = -1;

		DBG((ssenv->dbg, LEVEL_1, "Spill slot %d members:\n", i));
		for(n1 = pset_first(ass[i]->members); n1; n1 = pset_next(ass[i]->members))
			DBG((ssenv->dbg, LEVEL_1, "  %+F\n", n1));


		for (o=0; o < used_slots && tgt_slot == -1; ++o) { /* for each offset-assigned spill slot */
			/* check inter-slot-pairs for interference */
			ir_node *n2;
			for(n1 = pset_first(ass[i]->members); n1; n1 = pset_next(ass[i]->members))
				for(n2 = pset_first(ass[o]->members); n2; n2 = pset_next(ass[o]->members))
					if(values_interfere(n1, n2)) {
						pset_break(ass[i]->members);
						pset_break(ass[o]->members);
						DBG((ssenv->dbg, LEVEL_1, "    Interf %+F -- %+F\n", n1, n2));
						goto interf_detected;
					}

			/* if we are here, there is no interference between ass[i] and ass[o] */
			tgt_slot = o;

interf_detected: /*nothing*/ ;
		}

		/* now the members of ass[i] join the members of ass[tgt_slot] */

		/* do we need a new slot? */
		if (tgt_slot == -1) {
			tgt_slot = used_slots;
			used_slots++;

			ass[tgt_slot]->offset = curr_offset;
			curr_offset += ass[i]->size;

			/* init slot */
			if (tgt_slot != i) {
				ass[tgt_slot]->size = ass[i]->size;
				del_pset(ass[tgt_slot]->members);
				ass[tgt_slot]->members = pset_new_ptr(8);
			}
		}

		/* copy the members to the target pset */
		for(n1 = pset_first(ass[i]->members); n1; n1 = pset_next(ass[i]->members)) {
			/* NOTE: If src and tgt pset are the same, inserting while iterating is not allowed */
			if (tgt_slot != i)
				pset_insert_ptr(ass[tgt_slot]->members, n1);

			set_Spill_offset(n1, ass[tgt_slot]->offset);
			DBG((ssenv->dbg, LEVEL_1, "    Offset %+F  %d\n", n1, ass[tgt_slot]->offset));
		}
	}

	/* free all used psets, all other stuff is on the ssenv-obstack */
	for (i=0; i<count; ++i)
		del_pset(ass[i]->members);

}

void be_compute_spill_offsets(be_chordal_env_t *cenv) {
	ss_env_t ssenv;

	obstack_init(&ssenv.ob);
	ssenv.cenv  = cenv;
	ssenv.slots = pmap_create();
	ssenv.dbg   = firm_dbg_register("ir.be.spillslots");

	irg_walk_graph(cenv->irg, NULL, compute_spill_slots_walker, &ssenv);
	coalesce_slots(&ssenv);

	pmap_destroy(ssenv.slots);
	obstack_free(&ssenv.ob, NULL);
}
