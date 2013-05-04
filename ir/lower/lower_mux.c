/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Replaces Mux nodes with control-flow
 * @author  Olaf Liebe
 */
#include <assert.h>

#include "lowering.h"
#include "array.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irgmod.h"
#include "ircons.h"
#include "irpass_t.h"

typedef struct walk_env {
	lower_mux_callback *cb_func;
	ir_node            **muxes;
} walk_env_t;

static void find_mux_nodes(ir_node *mux, void *ctx)
{
	walk_env_t *env = (walk_env_t*)ctx;

	/* Skip non-mux nodes. */
	if (!is_Mux(mux))
		return;

	/* Skip nodes, depending on the callback function. */
	if (env->cb_func != NULL && !env->cb_func(mux)) {
		return;
	}

	/* Store the node. */
	ARR_APP1(ir_node*, env->muxes, mux);
}

static void lower_mux_node(ir_node* mux)
{
	ir_node  *upper_block;
	ir_node  *lower_block;
	ir_node  *cond;
	ir_node  *trueProj;
	ir_node  *falseProj;
	ir_node  *falseBlock;
	ir_node  *mux_jmps[2];
	ir_node  *mux_values[2];
	ir_node  *phi;
	ir_graph *irg;

	irg = get_irn_irg(mux);

	/* Split the block in two halfs, with the mux in the upper block. */
	lower_block = get_nodes_block(mux);
	assert(lower_block != 0);
	part_block(mux);
	upper_block = get_nodes_block(mux);

	/* Create a cond node with two projs and a phi as mux replacement. The
	 * true proj jumps directly to the lower block, the false proj uses a
	 * block in-between, so that the phi can be used to select the result
	 * value from the old mux node in the lower block. */
	cond        = new_r_Cond(upper_block, get_Mux_sel(mux));
	trueProj    = new_r_Proj(cond, mode_X, pn_Cond_true);
	falseProj   = new_r_Proj(cond, mode_X, pn_Cond_false);
	falseBlock  = new_r_Block(irg, 1, &falseProj);
	mux_jmps[0] = trueProj;
	mux_jmps[1] = new_r_Jmp(falseBlock);

	/* Kill the jump from upper to lower block and replace the in array. */
	assert(get_Block_n_cfgpreds(lower_block) == 1);
	kill_node(get_Block_cfgpred(lower_block, 0));
	set_irn_in(lower_block, 2, mux_jmps);

	/* Combine the two control flows with a phi to select the correct value
	 * and use it to replace the mux. */
	mux_values[0] = get_Mux_true(mux);
	mux_values[1] = get_Mux_false(mux);
	phi = new_r_Phi(lower_block, 2, mux_values, get_irn_mode(mux));
	exchange(mux, phi);

	/* Add links and update phi node lists, for the next part_block() call.
	 * lower_block and upper_block have been updated by part_block(). Link
	 * the projs with the cond. */
	set_irn_link(trueProj,   get_irn_link(cond));
	set_irn_link(falseProj,  trueProj);
	set_irn_link(cond,       falseProj);

	add_Block_phi(lower_block, phi);
}

void lower_mux(ir_graph *irg, lower_mux_callback *cb_func)
{
	size_t     i, n_muxes;
	walk_env_t env;

	/* Scan the graph for mux nodes to lower. */
	env.cb_func = cb_func;
	env.muxes   = NEW_ARR_F(ir_node*, 0);
	irg_walk_graph(irg, find_mux_nodes, 0, &env);

	n_muxes = ARR_LEN(env.muxes);
	if (n_muxes > 0) {
		ir_resources_t resources = IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST;

		/* This is required by part_block() later. */
		ir_reserve_resources(irg, resources);
		collect_phiprojs(irg);

		for (i = 0; i < n_muxes; ++i) {
			lower_mux_node(env.muxes[i]);
		}

		/* Cleanup, verify the graph. */
		ir_free_resources(irg, resources);

		clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);
	}
	DEL_ARR_F(env.muxes);
}

typedef struct pass_t {
	ir_graph_pass_t    pass;
	lower_mux_callback *cb_func;
} pass_t;

/**
 * Wrapper to run ir_lower_mux() as an ir_graph pass
 */
static int pass_wrapper(ir_graph *irg, void *context)
{
	pass_t *pass = (pass_t*)context;

	lower_mux(irg, pass->cb_func);
	return 0;
}

ir_graph_pass_t *lower_mux_pass(const char *name, lower_mux_callback *cb_func)
{
	pass_t *pass = XMALLOCZ(pass_t);

	pass->cb_func = cb_func;
	return def_graph_pass_constructor(
		&pass->pass, name ? name : "lower_mux", pass_wrapper);
}
