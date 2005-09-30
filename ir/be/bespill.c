/**
 * Author:      Daniel Grund
 * Date:		29.09.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include "pset.h"
#include "irnode_t.h"
#include "ircons_t.h"

#include "besched.h"
#include "bespill.h"
#include "benode_t.h"

typedef struct _spill_ctx_t {
	ir_node *spilled;  /**< The spilled node. */
	ir_node *user;     /**< The node this spill is for. */
	ir_node *spill;    /**< The spill itself. */
} spill_ctx_t;

int be_set_cmp_spillctx(const void *a, const void *b, size_t n) {
	const spill_ctx_t *p = a;
	const spill_ctx_t *q = b;
	return !(p->user == q->user && p->spilled == q->spilled);
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
//	DBG((dbg, DBG_SPILL, "spill_irn %+F\n", irn));

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
	ir_graph *irg = get_irn_irg(bl);
	spill_ctx_t *ctx;

	assert(is_Phi(phi));
//	DBG((dbg, DBG_SPILL, "spill_phi %+F\n", phi));

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

ir_node *be_spill_node(spill_env_t *senv, ir_node *to_spill) {
	ir_node *res;
	if (pset_find_ptr(senv->mem_phis, to_spill))
		res = be_spill_phi(senv, to_spill, to_spill);
	else
		res = be_spill_irn(senv, to_spill, to_spill);

	return res;
}

void be_remove_spilled_phis(spill_env_t *senv) {
	ir_node *irn;
	/* remove all special phis form the irg and the schedule */
	for(irn = pset_first(senv->mem_phis); irn; irn = pset_next(senv->mem_phis)) {
		int i, n;
		for(i = 0, n = get_irn_arity(irn); i < n; ++i)
			set_irn_n(irn, i, new_r_Bad(senv->session->irg));
		sched_remove(irn);
	}
}
