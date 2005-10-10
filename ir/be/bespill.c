/**
 * Author:      Daniel Grund, Sebastian Hack
 * Date:		29.09.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include "pset.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "debug.h"

#include "besched.h"
#include "bespill.h"
#include "benode_t.h"

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
	const be_main_session_env_t *session;
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

spill_env_t *be_new_spill_env(firm_dbg_module_t *dbg, const be_main_session_env_t *session,
		const arch_register_class_t *cls, decide_irn_t is_mem_phi, void *data) {
	spill_env_t *env = malloc(sizeof(env[0]));
	env->spill_ctxs = new_set(cmp_spillctx, 1024);
	env->spills     = new_set(cmp_spillinfo, 1024);
	env->session    = session;
	env->cls        = cls;
	env->dbg        = dbg;
	env->is_mem_phi = is_mem_phi;
	env->data       = data;
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
		const be_main_env_t *env = senv->session->main_env;
		ctx->spill = be_spill(env->node_factory, env->arch_env, irn);
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
	ir_graph *irg = senv->session->irg;
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
		ctx->spill = new_r_Phi(senv->session->irg, bl, n, ins, mode_M);
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
	const arch_env_t *arch = senv->session->main_env->arch_env;

	if (is_Phi(irn) && arch_irn_has_reg_class(arch, irn, 0, senv->cls)
			&& senv->is_mem_phi(irn, senv->data)) {
		DBG((senv->dbg, LEVEL_1, "  %+F\n", irn));
		pset_insert_ptr(senv->mem_phis, irn);
	}
}

void be_insert_spills_reloads(spill_env_t *senv, pset *reload_set) {
	ir_graph *irg = senv->session->irg;
	ir_node *irn;
	spill_info_t *si;
	struct obstack ob;

	obstack_init(&ob);

	/* get all special spilled phis */
	DBG((senv->dbg, LEVEL_1, "Mem-phis:\n"));
	senv->mem_phis = pset_new_ptr_default();
	irg_walk_graph(senv->session->irg, phi_walker, NULL, senv);

	/* Add reloads for mem_phis */
	/* BETTER: These reloads (1) should only be inserted, if they are really needed */
	DBG((senv->dbg, LEVEL_1, "Reloads for mem-phis:\n"));
	for(irn = pset_first(senv->mem_phis); irn; irn = pset_next(senv->mem_phis)) {
		const ir_edge_t *e;
		DBG((senv->dbg, LEVEL_1, " Mem-phi %+F\n", irn));
		foreach_out_edge(irn, e) {
			ir_node *user = e->src;
			if (is_Phi(user) && !pset_find_ptr(senv->mem_phis, user)) {
					DBG((senv->dbg, LEVEL_1, " non-mem-phi user %+F\n", user));
					ir_node *use_bl = get_nodes_block(user);
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
			ir_node *reload  = new_Reload(senv->session->main_env->node_factory,
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
		be_introduce_copies_ignore(senv->session->dom_front, si->spilled_node,
				n_reloads, reloads, senv->mem_phis);
		obstack_free(&ob, reloads);
	}

	obstack_free(&ob, NULL);

	for(irn = pset_first(senv->mem_phis); irn; irn = pset_next(senv->mem_phis)) {
		int i, n;
		for(i = 0, n = get_irn_arity(irn); i < n; ++i)
			set_irn_n(irn, i, new_r_Bad(senv->session->irg));
		sched_remove(irn);
	}

	del_pset(senv->mem_phis);
}

void be_add_reload(spill_env_t *senv, ir_node *to_spill, ir_node *before) {
	spill_info_t templ, *res;
	reloader_t *rel;

//	assert(get_irn_opcode(to_spill) != iro_Unknown);

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
