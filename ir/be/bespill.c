/**
 * Author:      Daniel Grund, Sebastian Hack
 * Date:		29.09.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include "pset.h"
#include "irnode_t.h"
#include "ircons_t.h"
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
	set *spills;		/**< all spill_info_t's, which must be placed */
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
		const be_main_session_env_t *session, const arch_register_class_t *cls) {
	spill_env_t *env = malloc(sizeof(env[0]));
	env->spill_ctxs = new_set(cmp_spillctx, 1024);
	env->spills     = new_set(cmp_spillinfo, 1024);
	env->session    = session;
	env->cls        = cls;
	env->dbg        = dbg;
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
	DBG((senv->dbg, LEVEL_1, "spill_irn %+F\n", irn));

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
static ir_node *be_spill_phi(spill_env_t *senv, ir_node *phi, ir_node *ctx_irn,
		pset *mem_phis) {
	int i, n = get_irn_arity(phi);
	ir_node **ins, *bl = get_nodes_block(phi);
	ir_graph *irg = senv->session->irg;
	spill_ctx_t *ctx;

	assert(is_Phi(phi));
	DBG((senv->dbg, LEVEL_1, "spill_phi %+F\n", phi));

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

			if(is_Phi(arg) && pset_find_ptr(mem_phis, arg))
				sub_res = be_spill_phi(senv, arg, ctx_irn, mem_phis);
			else
				sub_res = be_spill_irn(senv, arg, ctx_irn);

			set_irn_n(ctx->spill, i, sub_res);
		}
	}
	return ctx->spill;
}

static ir_node *be_spill_node(spill_env_t *senv, ir_node *to_spill, pset *mem_phis) {
	ir_node *res;
	if (pset_find_ptr(mem_phis, to_spill))
		res = be_spill_phi(senv, to_spill, to_spill, mem_phis);
	else
		res = be_spill_irn(senv, to_spill, to_spill);

	return res;
}

void be_insert_spills_reloads(spill_env_t *senv, pset *reload_set, decide_irn_t is_mem_phi, void *data) {
	ir_graph *irg = senv->session->irg;
	ir_node *irn;
	spill_info_t *si;
	struct obstack ob;
	pset *mem_phis = pset_new_ptr_default();

	obstack_init(&ob);

	/* get all special spilled phis */
	for(si = set_first(senv->spills); si; si = set_next(senv->spills)) {
		irn = si->spilled_node;
		if (is_Phi(irn) && is_mem_phi(irn, data))
			pset_insert_ptr(mem_phis, irn);
	}

	/* process each spilled node */
	for(si = set_first(senv->spills); si; si = set_next(senv->spills)) {
		reloader_t *rld;
		ir_node **reloads;
		int n_reloads = 0;
		ir_mode *mode = get_irn_mode(si->spilled_node);

		/* go through all reloads for this spill */
		for(rld = si->reloaders; rld; rld = rld->next) {
			/* the spill for this reloader */
			ir_node *spill   = be_spill_node(senv, si->spilled_node, mem_phis);

			/* the reload */
			ir_node *bl      = is_Block(rld->reloader) ? rld->reloader : get_nodes_block(rld->reloader);
			ir_node *reload  = new_Reload(senv->session->main_env->node_factory,
														senv->cls, irg, bl, mode, spill);

			DBG((senv->dbg, LEVEL_2, " RELOADER %+F   Reload %+F of %+F\n",
						rld->reloader, reload, si->spilled_node));
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
				n_reloads, reloads, mem_phis);
		obstack_free(&ob, reloads);
	}

	obstack_free(&ob, NULL);

	for(irn = pset_first(mem_phis); irn; irn = pset_next(mem_phis)) {
		int i, n;
		for(i = 0, n = get_irn_arity(irn); i < n; ++i)
			set_irn_n(irn, i, new_r_Bad(senv->session->irg));
		sched_remove(irn);
	}

	del_pset(mem_phis);
}

void be_add_spill(spill_env_t *senv, ir_node *to_spill, ir_node *before) {
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

void be_add_spill_on_edge(spill_env_t *senv, ir_node *to_spill, ir_node *bl, int pos) {
	ir_node *insert_bl = get_irn_arity(bl) == 1
		? sched_first(bl) : get_Block_cfgpred_block(bl, pos);
	be_add_spill(senv, to_spill, insert_bl);
}
