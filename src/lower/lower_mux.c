/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Replaces Mux nodes with control-flow
 * @author  Olaf Liebe
 */
#include "array.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "lowering.h"
#include "util.h"
#include <assert.h>

typedef struct walk_env {
	lower_mux_callback *cb_func;
	ir_node            **muxes;
} walk_env_t;

static void find_mux_nodes(ir_node *mux, void *ctx)
{
	/* Skip non-mux nodes. */
	if (!is_Mux(mux))
		return;

	/* Skip nodes, depending on the callback function. */
	walk_env_t *env = (walk_env_t*)ctx;
	if (env->cb_func != NULL && !env->cb_func(mux)) {
		return;
	}

	/* Store the node. */
	ARR_APP1(ir_node*, env->muxes, mux);
}

static void lower_mux_node(ir_node* mux)
{
	/* Split the block in two halfs, with the mux in the upper block. */
	ir_node *lower_block = get_nodes_block(mux);
	assert(lower_block != 0);
	part_block(mux);
	ir_node *upper_block = get_nodes_block(mux);

	/* Create a cond node with two projs and a phi as mux replacement. */
	ir_node  *cond        = new_r_Cond(upper_block, get_Mux_sel(mux));
	ir_node  *trueProj    = new_r_Proj(cond, mode_X, pn_Cond_true);
	ir_node  *falseProj   = new_r_Proj(cond, mode_X, pn_Cond_false);
	ir_node  *mux_jmps[]  = { trueProj, falseProj };
	/* Kill the jump from upper to lower block and replace the in array. */
	assert(get_Block_n_cfgpreds(lower_block) == 1);
	kill_node(get_Block_cfgpred(lower_block, 0));
	set_irn_in(lower_block, ARRAY_SIZE(mux_jmps), mux_jmps);

	/* Combine the two control flows with a phi to select the correct value
	 * and use it to replace the mux. */
	ir_node *mux_values[] = { get_Mux_true(mux), get_Mux_false(mux) };
	ir_mode *mode = get_irn_mode(mux);
	ir_node *phi  = new_r_Phi(lower_block, ARRAY_SIZE(mux_values), mux_values,
	                          mode);
	collect_new_phi_node(phi);
	exchange(mux, phi);

	/* Add links and update phi node lists, for the next part_block() call.
	 * lower_block and upper_block have been updated by part_block(). Link
	 * the projs with the cond. */
	set_irn_link(trueProj,  get_irn_link(cond));
	set_irn_link(falseProj, trueProj);
	set_irn_link(cond,      falseProj);
}

void lower_mux(ir_graph *irg, lower_mux_callback *cb_func)
{
	/* Scan the graph for mux nodes to lower. */
	walk_env_t env;
	env.cb_func = cb_func;
	env.muxes   = NEW_ARR_F(ir_node*, 0);
	irg_walk_graph(irg, find_mux_nodes, 0, &env);

	size_t n_muxes = ARR_LEN(env.muxes);
	if (n_muxes > 0) {
		ir_resources_t resources = IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST;

		/* This is required by part_block() later. */
		ir_reserve_resources(irg, resources);
		collect_phiprojs_and_start_block_nodes(irg);

		for (size_t i = 0; i < n_muxes; ++i) {
			lower_mux_node(env.muxes[i]);
		}

		/* Cleanup, verify the graph. */
		ir_free_resources(irg, resources);

		clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE | IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES);
	}
	DEL_ARR_F(env.muxes);
}
