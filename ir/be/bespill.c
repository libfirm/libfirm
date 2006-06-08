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
#include "ident_t.h"
#include "type_t.h"
#include "entity_t.h"
#include "debug.h"
#include "irgwalk.h"
#include "array.h"
#include "pdeq.h"

#include "belive_t.h"
#include "besched_t.h"
#include "bespill.h"
#include "benode_t.h"
#include "bechordal_t.h"

#define REMAT
/* This enables re-computation of values. Current state: Unfinished and buggy. */
#undef BUGGY_REMAT

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
	const arch_register_class_t *cls;
	const be_chordal_env_t *chordal_env;
	struct obstack obst;
	set *spill_ctxs;
	set *spills;				/**< all spill_info_t's, which must be placed */
	pset *mem_phis;				/**< set of all special spilled phis. allocated and freed separately */
	ir_node **copies;			/**< set of copies placed because of phi spills */
	DEBUG_ONLY(firm_dbg_module_t *dbg;)
};

/* associated Phi -> Spill*/
typedef struct _phi_spill_assoc_t {
	ir_node *phi;
	ir_node *spill;
} phi_spill_assoc_t;

/**
 * Compare two Phi->Spill associations.
 */
static int cmp_phi_spill_assoc(const void *a, const void *b, size_t n) {
	const phi_spill_assoc_t *p1 = a;
	const phi_spill_assoc_t *p2 = b;
	return p1->phi != p2->phi;
}

/**
 * compare two spill contexts.
 */
static int cmp_spillctx(const void *a, const void *b, size_t n) {
	const spill_ctx_t *p = a;
	const spill_ctx_t *q = b;
	return p->user != q->user || p->spilled != q->spilled;
}

/**
 * Compare two spill infos.
 */
static int cmp_spillinfo(const void *x, const void *y, size_t size) {
	const spill_info_t *xx = x;
	const spill_info_t *yy = y;
	return xx->spilled_node != yy->spilled_node;
}

DEBUG_ONLY(
/* Sets the debug module of a spill environment. */
void be_set_spill_env_dbg_module(spill_env_t *env, firm_dbg_module_t *dbg) {
	env->dbg = dbg;
}
)

/* Creates a new spill environment. */
spill_env_t *be_new_spill_env(const be_chordal_env_t *chordal_env) {
	spill_env_t *env	= xmalloc(sizeof(env[0]));
	env->spill_ctxs		= new_set(cmp_spillctx, 1024);
	env->spills			= new_set(cmp_spillinfo, 1024);
	env->cls			= chordal_env->cls;
	env->chordal_env	= chordal_env;
	env->mem_phis		= pset_new_ptr_default();
	env->copies			= NEW_ARR_F(ir_node*, 0);
	obstack_init(&env->obst);
	return env;
}

/* Deletes a spill environment. */
void be_delete_spill_env(spill_env_t *env) {
	del_set(env->spill_ctxs);
	del_set(env->spills);
	del_pset(env->mem_phis);
	DEL_ARR_F(env->copies);
	obstack_free(&env->obst, NULL);
	free(env);
}

/**
 * Returns a spill context. If the context did not exists, create one.
 *
 * @param sc        the set containing all spill contexts
 * @param to_spill  the node that should be spilled
 * @param ctx_irn   an user of the spilled node
 *
 * @return a spill context.
 */
static spill_ctx_t *be_get_spill_ctx(set *sc, ir_node *to_spill, ir_node *ctx_irn) {
	spill_ctx_t templ;

	templ.spilled = to_spill;
	templ.user    = ctx_irn;
	templ.spill   = NULL;

	return set_insert(sc, &templ, sizeof(templ), HASH_COMBINE(HASH_PTR(to_spill), HASH_PTR(ctx_irn)));
}

/**
 * Creates a spill.
 *
 * @param senv      the spill environment
 * @param irn       the node that should be spilled
 * @param ctx_irn   an user of the spilled node
 *
 * @return a be_Spill node
 */
static ir_node *be_spill_irn(spill_env_t *senv, ir_node *irn, ir_node *ctx_irn) {
	spill_ctx_t *ctx;
	const be_main_env_t *env = senv->chordal_env->birg->main_env;
	DBG((senv->dbg, LEVEL_1, "%+F in ctx %+F\n", irn, ctx_irn));

	// Has the value already been spilled?
	ctx = be_get_spill_ctx(senv->spill_ctxs, irn, ctx_irn);
	if(ctx->spill)
		return ctx->spill;

	/* Trying to spill an already spilled value, no need for a new spill
	 * node then, we can simply connect to the same one for this reload
	 */
	if(be_is_Reload(irn)) {
		return get_irn_n(irn, be_pos_Reload_mem);
	}

	ctx->spill = be_spill(env->arch_env, irn, ctx_irn);

	return ctx->spill;
}

/**
 * Removes all copies introduced for phi-spills
 */
static void remove_copies(spill_env_t *env) {
	int i;

	for(i = 0; i < ARR_LEN(env->copies); ++i) {
		ir_node *node = env->copies[i];
		ir_node *src;
		const ir_edge_t *edge, *ne;

		assert(be_is_Copy(node));

		src = be_get_Copy_op(node);
		foreach_out_edge_safe(node, edge, ne) {
			ir_node *user = get_edge_src_irn(edge);
			int user_pos  = get_edge_src_pos(edge);

			set_irn_n(user, user_pos, src);
		}
	}

	ARR_SETLEN(ir_node*, env->copies, 0);
}

/**
 * Inserts a copy (needed for spilled phi handling) of a value at the earliest
 * possible location in a block. That is after the last use/def of the value or at
 * the beginning of the block if there is no use/def.
 */
static ir_node *insert_copy(spill_env_t *env, ir_node *block, ir_node *value) {
	ir_node* node;
	ir_graph *irg = get_irn_irg(block);
	ir_node *copy = be_new_Copy(env->cls, irg, block, value);

	ARR_APP1(ir_node*, env->copies, copy);

	// walk schedule backwards until we find a use/def, or until we have reached the first phi
	// TODO we could also do this by iterating over all uses and checking the
	// sched_get_time_step value. Need benchmarks to decide this...
	sched_foreach_reverse(block, node) {
		int i, arity;

		if(is_Phi(node)) {
			sched_add_after(node, copy);
			goto placed;
		}
		if(value == node) {
			sched_add_after(node, copy);
			goto placed;
		}
		for(i = 0, arity = get_irn_arity(node); i < arity; ++i) {
			ir_node *arg = get_irn_n(node, i);
			if(arg == value) {
				sched_add_after(node, copy);
				goto placed;
			}
		}
	}
	// we didn't find a use or a phi yet, so place the copy at the beginning of the block
	sched_add_before(sched_first(block), copy);

placed:

	return copy;
}

/**
 * If the first usage of a Phi result would be out of memory
 * there is no sense in allocating a register for it.
 * Thus we spill it and all its operands to the same spill slot.
 * Therefore the phi/dataB becomes a phi/Memory
 *
 * @param senv      the spill environment
 * @param phi       the Phi node that should be spilled
 * @param ctx_irn   an user of the spilled node
 *
 * @return a be_Spill node
 */
static ir_node *spill_phi(spill_env_t *senv, ir_node *phi, ir_node *ctx_irn, set *already_visited_phis, bitset_t *bs) {
	int         i;
	int arity = get_irn_arity(phi);
	ir_graph    *irg      = senv->chordal_env->irg;
	ir_node     *bl       = get_nodes_block(phi);
	ir_node     **ins, *phi_spill;
	phi_spill_assoc_t key;
	spill_ctx_t *ctx;

	assert(is_Phi(phi));
	DBG((senv->dbg, LEVEL_1, "%+F in ctx %+F\n", phi, ctx_irn));

	/* build a new PhiM */
	NEW_ARR_A(ir_node *, ins, arity);
	for (i = 0; i < arity; ++i) {
		ins[i] = new_r_Bad(irg);
	}
	phi_spill = new_r_Phi(senv->chordal_env->irg, bl, arity, ins, mode_M);
	key.phi   = phi;
	key.spill = phi_spill;
	set_insert(already_visited_phis, &key, sizeof(key), HASH_PTR(phi));
	bitset_set(bs, get_irn_idx(phi));

	/* search an existing spill for this context */
	ctx = be_get_spill_ctx(senv->spill_ctxs, phi, ctx_irn);

	/* if not found spill the phi */
	if (! ctx->spill) {
		/* collect all arguments of the phi */
		for (i = 0; i < arity; ++i) {
			ir_node *arg = get_irn_n(phi, i);
			ir_node *sub_res;
			phi_spill_assoc_t *entry;

			if(is_Phi(arg) && pset_find_ptr(senv->mem_phis, arg)) {
				// looping edge?
				if(arg == phi) {
					sub_res = phi_spill;
				} else if (! bitset_is_set(bs, get_irn_idx(arg))) {
					sub_res = spill_phi(senv, arg, ctx_irn, already_visited_phis, bs);
				} else {
					/* we already visited the argument phi: get it's spill */
					key.phi   = arg;
					key.spill = NULL;
					entry     = set_find(already_visited_phis, &key, sizeof(key), HASH_PTR(arg));
					assert(entry && "argument phi already visited, but no spill found?!?");
					sub_res   = entry->spill;
					assert(sub_res && "spill missing?!?");
				}
			}
			else
				sub_res = be_spill_irn(senv, arg, ctx_irn);

			set_irn_n(phi_spill, i, sub_res);
		}

		ctx->spill = phi_spill;
	}
	return ctx->spill;
}

/**
 * Spill a node.
 *
 * @param senv      the spill environment
 * @param to_spill  the node that should be spilled
 *
 * @return a be_Spill node
 */
static ir_node *be_spill_node(spill_env_t *senv, ir_node *to_spill) {
	ir_graph *irg = get_irn_irg(to_spill);
	ir_node  *res;

	if (pset_find_ptr(senv->mem_phis, to_spill)) {
		set *already_visited_phis = new_set(cmp_phi_spill_assoc, 10);
		bitset_t *bs = bitset_alloca(get_irg_last_idx(irg));
		res = spill_phi(senv, to_spill, to_spill, already_visited_phis, bs);
		del_set(already_visited_phis);
	} else {
		res = be_spill_irn(senv, to_spill, to_spill);
	}

	return res;
}

#ifdef REMAT

#ifdef BUGGY_REMAT

/**
 * Check if a spilled node could be rematerialized.
 *
 * @param senv      the spill environment
 * @param spill     the Spill node
 * @param spilled   the node that was spilled
 * @param reloader  a irn that requires a reload
 */
static int check_remat_conditions(spill_env_t *senv, ir_node *spill, ir_node *spilled, ir_node *reloader) {
	int pos, max;

	/* check for 'normal' spill and general remat condition */
	if (!be_is_Spill(spill) || !arch_irn_is(senv->chordal_env->birg->main_env->arch_env, spilled, rematerializable))
		return 0;

	/* check availability of original arguments */
	if (is_Block(reloader)) {

		/* we want to remat at the end of a block.
		 * thus all arguments must be alive at the end of the block
		 */
		for (pos=0, max=get_irn_arity(spilled); pos<max; ++pos) {
			ir_node *arg = get_irn_n(spilled, pos);
			if (!is_live_end(reloader, arg))
				return 0;
		}

	} else {

		/* we want to remat before the insn reloader
		 * thus an arguments is alive if
		 *   - it interferes with the reloaders result
		 * or
		 *   - or it is (last-) used by reloader itself
		 */
		for (pos=0, max=get_irn_arity(spilled); pos<max; ++pos) {
			ir_node *arg = get_irn_n(spilled, pos);
			int i, m;

			if (values_interfere(reloader, arg))
				goto is_alive;

			for (i=0, m=get_irn_arity(reloader); i<m; ++i) {
				ir_node *rel_arg = get_irn_n(reloader, i);
				if (rel_arg == arg)
					goto is_alive;
			}

			/* arg is not alive before reloader */
			return 0;

is_alive:	;

		}

	}

	return 1;
}

#else /* BUGGY_REMAT */

/**
 * A very simple rematerialization checker.
 *
 * @param senv      the spill environment
 * @param spill     the Spill node
 * @param spilled   the node that was spilled
 * @param reloader  a irn that requires a reload
 */
static int check_remat_conditions(spill_env_t *senv, ir_node *spill, ir_node *spilled, ir_node *reloader) {
	const arch_env_t *aenv = senv->chordal_env->birg->main_env->arch_env;

	return get_irn_arity(spilled) == 0 &&
		   be_is_Spill(spill) &&
		   arch_irn_is(aenv, spilled, rematerializable);
}

#endif /* BUGGY_REMAT */

#endif /* REMAT */

/**
 * Re-materialize a node.
 *
 * @param senv      the spill environment
 * @param spilled   the node that was spilled
 * @param reloader  a irn that requires a reload
 */
static ir_node *do_remat(spill_env_t *senv, ir_node *spilled, ir_node *reloader) {
	ir_node *res;
	ir_node *bl = (is_Block(reloader)) ? reloader : get_nodes_block(reloader);

	/* recompute the value */
	res = new_ir_node(get_irn_dbg_info(spilled), senv->chordal_env->irg, bl,
		get_irn_op(spilled),
		get_irn_mode(spilled),
		get_irn_arity(spilled),
		get_irn_in(spilled) + 1);
	copy_node_attr(spilled, res);

	DBG((senv->dbg, LEVEL_1, "Insert remat %+F before reloader %+F\n", res, reloader));

	/* insert in schedule */
	if (is_Block(reloader)) {
		ir_node *insert = sched_skip(reloader, 0, sched_skip_cf_predicator, (void *) senv->chordal_env->birg->main_env->arch_env);
		sched_add_after(insert, res);
	} else {
		sched_add_before(reloader, res);
	}

	return res;
}

void be_spill_phi(spill_env_t *env, ir_node *node) {
	assert(is_Phi(node));

	pset_insert_ptr(env->mem_phis, node);
}

void be_insert_spills_reloads(spill_env_t *env) {
	const arch_env_t *arch_env = env->chordal_env->birg->main_env->arch_env;
	ir_node *node;
	spill_info_t *si;

	DBG((env->dbg, LEVEL_1, "Reloads for mem-phis:\n"));
	foreach_pset(env->mem_phis, node) {
		const ir_edge_t *e;
		int i, arity;

		assert(is_Phi(node));

		/* We have to place copy nodes in the predecessor blocks to temporarily
		 * produce new values that get separate spill slots
		 */
		for(i = 0, arity = get_irn_arity(node); i < arity; ++i) {
			ir_node *pred_block, *arg, *copy;

			/* Don't do anything for looping edges (there's no need
			 * and placing copies here breaks stuff as it suddenly
			 * generates new living values through the whole loop)
			 */
			arg = get_irn_n(node, i);
			if(arg == node)
				continue;

			pred_block = get_Block_cfgpred_block(get_nodes_block(node), i);
			copy = insert_copy(env, pred_block, arg);

			set_irn_n(node, i, copy);
		}

		/* Add reloads for mem_phis */
		/* BETTER: These reloads (1) should only be inserted, if they are really needed */
		DBG((env->dbg, LEVEL_1, " Mem-phi %+F\n", node));
		foreach_out_edge(node, e) {
			ir_node *user = e->src;
			if (is_Phi(user) && !pset_find_ptr(env->mem_phis, user)) {
				ir_node *use_bl = get_nodes_block(user);
				DBG((env->dbg, LEVEL_1, " non-mem-phi user %+F\n", user));
				be_add_reload_on_edge(env, node, use_bl, e->pos); /* (1) */
			}
		}
	}

	/* process each spilled node */
	DBG((env->dbg, LEVEL_1, "Insert spills and reloads:\n"));
	for(si = set_first(env->spills); si; si = set_next(env->spills)) {
		reloader_t *rld;
		ir_mode *mode = get_irn_mode(si->spilled_node);
		//ir_node *value;
		pset *values = pset_new_ptr(16);

		/* go through all reloads for this spill */
		for(rld = si->reloaders; rld; rld = rld->next) {
			ir_node *new_val;

			/* the spill for this reloader */
			ir_node *spill   = be_spill_node(env, si->spilled_node);

#ifdef REMAT
			if (check_remat_conditions(env, spill, si->spilled_node, rld->reloader)) {
				new_val = do_remat(env, si->spilled_node, rld->reloader);
				//pdeq_putl(possibly_dead, spill);
			}
			else
#endif
				/* do a reload */
				new_val = be_reload(arch_env, env->cls, rld->reloader, mode, spill);

			DBG((env->dbg, LEVEL_1, " %+F of %+F before %+F\n", new_val, si->spilled_node, rld->reloader));
			pset_insert_ptr(values, new_val);
		}

		/* introduce copies, rewire the uses */
		assert(pset_count(values) > 0 && "???");
		pset_insert_ptr(values, si->spilled_node);
		be_ssa_constr_set_ignore(env->chordal_env->dom_front, values, env->mem_phis);

		del_pset(values);
	}

	remove_copies(env);

	// reloads are placed now, but we might reuse the spill environment for further spilling decisions
	del_set(env->spills);
	env->spills = new_set(cmp_spillinfo, 1024);
}

void be_add_reload(spill_env_t *env, ir_node *to_spill, ir_node *before) {
	spill_info_t templ, *res;
	reloader_t *rel;

	assert(arch_irn_consider_in_reg_alloc(env->chordal_env->birg->main_env->arch_env, env->cls, to_spill));

	if(is_Phi(to_spill)) {
		be_spill_phi(env, to_spill);
	}

	templ.spilled_node = to_spill;
	templ.reloaders    = NULL;
	res = set_insert(env->spills, &templ, sizeof(templ), HASH_PTR(to_spill));

	rel           = obstack_alloc(&env->obst, sizeof(rel[0]));
	rel->reloader = before;
	rel->next     = res->reloaders;
	res->reloaders = rel;
}

void be_add_reload_on_edge(spill_env_t *env, ir_node *to_spill, ir_node *bl, int pos) {
	ir_node *insert_bl = get_irn_arity(bl) == 1 ? sched_first(bl) : get_Block_cfgpred_block(bl, pos);
	be_add_reload(env, to_spill, insert_bl);
}



/****************************************

	SPILL SLOT MANAGEMENT AND OPTS

****************************************/

typedef struct _spill_slot_t {
	unsigned size;
	unsigned align;
	pset *members;
	ir_mode *largest_mode;	/* the mode of all members with largest size */
} spill_slot_t;

typedef struct _ss_env_t {
	struct obstack ob;
	be_chordal_env_t *cenv;
	pmap *slots;		/* maps spill_contexts to spill_slots */
	pmap *types;    /* maps modes to types */
	DEBUG_ONLY(firm_dbg_module_t *dbg;)
} ss_env_t;


/**
 * Walker: compute the spill slots
 */
static void compute_spill_slots_walker(ir_node *spill, void *env) {
	ss_env_t *ssenv = env;
	ir_node *ctx;
	pmap_entry *entry;
	spill_slot_t *ss;

	if (!be_is_Spill(spill))
		return;

	/* check, if this spill is for a context already known */
	ctx = be_get_Spill_context(spill);
	entry = pmap_find(ssenv->slots, ctx);

	if (!entry) {
		struct _arch_env_t *arch_env     = ssenv->cenv->birg->main_env->arch_env;
		const arch_register_class_t *cls = arch_get_irn_reg_class(arch_env, spill, be_pos_Spill_val);
		ir_mode *largest_mode            = arch_register_class_mode(cls);

		/* this is a new spill context */
		ss = obstack_alloc(&ssenv->ob, sizeof(*ss));
		ss->members      = pset_new_ptr(8);
		ss->largest_mode = largest_mode;
		ss->size         = get_mode_size_bytes(ss->largest_mode);
		ss->align        = arch_isa_get_reg_class_alignment(arch_env->isa, cls);
		pmap_insert(ssenv->slots, ctx, ss);
	} else {
		/* values with the same spill_ctx must go into the same spill slot */
		ss = entry->value;

#ifndef NDEBUG
		/* ugly mega assert :-) */
		{
			ir_node *irn;
			struct _arch_env_t *arch_env     = ssenv->cenv->birg->main_env->arch_env;
			const arch_register_class_t *cls = arch_get_irn_reg_class(arch_env, spill, be_pos_Spill_val);
			int size = get_mode_size_bytes(arch_register_class_mode(cls));
			assert((int) ss->size == size && "Different sizes for the same spill slot are not allowed.");
			for (irn = pset_first(ss->members); irn; irn = pset_next(ss->members)) {
				/* use values_interfere here, because it uses the dominance check,
					 which does work for values in memory */
				assert(!values_interfere(spill, irn) && "Spills for the same spill slot must not interfere!");
			}
		}
#endif /* NDEBUG */
	}

	pset_insert_ptr(ss->members, spill);
}

/**
 * qsort compare function, sort spill slots by size.
 */
static int ss_sorter(const void *v1, const void *v2) {
	const spill_slot_t **ss1 = (const spill_slot_t **)v1;
	const spill_slot_t **ss2 = (const spill_slot_t **)v2;
	return ((int) (*ss2)->size) - ((int) (*ss1)->size);
}


/**
 * This function should optimize the spill slots.
 *  - Coalescing of multiple slots
 *  - Ordering the slots
 *
 * Input slots are in @p ssenv->slots
 * @p size The count of initial spill slots in @p ssenv->slots
 *         This also is the size of the preallocated array @p ass
 *
 * @return An array of spill slots @p ass in specific order
 **/
static void optimize_slots(ss_env_t *ssenv, int size, spill_slot_t *ass[]) {
	int i, o, used_slots;
	pmap_entry *entr;

	i=0;
	pmap_foreach(ssenv->slots, entr)
		ass[i++] = entr->value;

	/* Sort the array to minimize fragmentation and cache footprint.
	   Large slots come first */
	qsort(ass, size, sizeof(ass[0]), ss_sorter);

	/* For each spill slot:
		- assign a new offset to this slot
	    - xor find another slot to coalesce with */
	used_slots = 0;
	for (i=0; i<size; ++i) { /* for each spill slot */
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

			/* init slot */
			if (tgt_slot != i) {
				ass[tgt_slot]->size = ass[i]->size;
				del_pset(ass[tgt_slot]->members);
				ass[tgt_slot]->members = pset_new_ptr(8);
			}
		}

		/* copy the members to the target pset */
		/* NOTE: If src and tgt pset are the same, inserting while iterating is not allowed */
		if (tgt_slot != i)
			for(n1 = pset_first(ass[i]->members); n1; n1 = pset_next(ass[i]->members))
					pset_insert_ptr(ass[tgt_slot]->members, n1);
	}
}

#define ALIGN_SPILL_AREA 16
#define pset_foreach(pset, elm)  for(elm=pset_first(pset); elm; elm=pset_next(pset))

/**
 * Returns a spill type for a mode. Keep them in a map to reduce
 * the number of types.
 *
 * @param types  a map containing all created types
 * @param ss     the spill slot
 *
 * Note that type types should are identical for every mode.
 * This rule might break if two different register classes return the same
 * mode but different alignments.
 */
static ir_type *get_spill_type(pmap *types, spill_slot_t *ss) {
  pmap_entry *e = pmap_find(types, ss->largest_mode);
  ir_type *res;

  if (! e) {
		char buf[64];
    snprintf(buf, sizeof(buf), "spill_slot_type_%s", get_mode_name(ss->largest_mode));
    res = new_type_primitive(new_id_from_str(buf), ss->largest_mode);
		set_type_alignment_bytes(res, ss->align);
    pmap_insert(types, ss->largest_mode, res);
  }
  else {
    res = e->value;
		assert(get_type_alignment_bytes(res) == (int)ss->align);
	}
  return res;
}

/**
 * Create spill slot entities on the frame type.
 *
 * @param ssenv   the spill environment
 * @param n       number of spill slots
 * @param ss      array of spill slots
 */
static void assign_entities(ss_env_t *ssenv, int n_slots, spill_slot_t *ss[]) {
	int i, offset, frame_align;
	ir_type *frame = get_irg_frame_type(ssenv->cenv->irg);

	/* aligning by increasing frame size */
	offset = get_type_size_bits(frame) / 8;
	offset = round_up2(offset, ALIGN_SPILL_AREA);
	set_type_size_bytes(frame, -1);

	/* create entities and assign offsets according to size and alignment*/
	for (i = 0; i < n_slots; ++i) {
		char buf[64];
		ident *name;
		entity *spill_ent;
		ir_node *irn;

		/* build entity */
		snprintf(buf, sizeof(buf), "spill_slot_%d", i);
		name = new_id_from_str(buf);

		spill_ent = new_entity(frame, name, get_spill_type(ssenv->types, ss[i]));

		/* align */
		offset = round_up2(offset, ss[i]->align);
		/* set */
		set_entity_offset_bytes(spill_ent, offset);
		/* next possible offset */
		offset += round_up2(ss[i]->size, ss[i]->align);

		pset_foreach(ss[i]->members, irn)
			be_set_Spill_entity(irn, spill_ent);
	}


	/* set final size of stack frame */
	frame_align = get_type_alignment_bytes(frame);
	set_type_size_bytes(frame, round_up2(offset, frame_align));
}

void be_compute_spill_offsets(be_chordal_env_t *cenv) {
	ss_env_t ssenv;
	spill_slot_t **ss;
	int ss_size;
	pmap_entry *pme;

	obstack_init(&ssenv.ob);
	ssenv.cenv  = cenv;
	ssenv.slots = pmap_create();
	ssenv.types = pmap_create();
	FIRM_DBG_REGISTER(ssenv.dbg, "ir.be.spillslots");

	/* Get initial spill slots */
	irg_walk_graph(cenv->irg, NULL, compute_spill_slots_walker, &ssenv);

	/* Build an empty array for optimized spill slots */
	ss_size = pmap_count(ssenv.slots);
	ss = obstack_alloc(&ssenv.ob, ss_size * sizeof(*ss));
	optimize_slots(&ssenv, ss_size, ss);

	/* Integrate slots into the stack frame entity */
	assign_entities(&ssenv, ss_size, ss);

	/* Clean up */
	pmap_foreach(ssenv.slots, pme)
	del_pset(((spill_slot_t *)pme->value)->members);
	pmap_destroy(ssenv.slots);
	pmap_destroy(ssenv.types);
	obstack_free(&ssenv.ob, NULL);

	be_copy_entities_to_reloads(cenv->irg);
}
