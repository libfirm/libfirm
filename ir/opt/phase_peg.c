/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   Convert firm graphs to program expression graphs.
 * @author  Olaf Liebe
 * @version $Id: $
 */

#include "config.h"

#include "iroptimize.h"
#include "irgraph_t.h"
#include "irflag_t.h"
#include "irdump.h"
#include "ircons.h"
#include "irouts.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "array_t.h"

/**
 * Check if control flow can reach end from start. Note that the CFG is
 * guaranteed to be a tree at this point.
 *
 * TODO: optimize this later.
 */
static int cfg_reachable(ir_node *start, ir_node *end)
{
	int i;

	/* Every node can reach itself. */
	if (start == end) {
		return 1;
	}

	assert(is_Block(start));
	assert(is_Block(end));

	/* Iterate all predecessor blocks. */
	for (i = 0; i < get_Block_n_cfgpreds(end); i++) {

		ir_node *pred = get_Block_cfgpred(end, i);
		if (!is_Block(pred)) pred = get_nodes_block(pred);

		/* Unwind if the block has been found. */
		if (cfg_reachable(start, pred)) return 1;
	}

	return 0;
}

/**
 * Checks if any predecessor of the end block can be reached from the start
 * block. Returns -1 is no predecessor is reachable or the index of the only
 * predecessor that can be reached or the count of predecessors if more than
 * one is reachable.
 */
static int cfg_rechable_pred(ir_node *start, ir_node *end)
{
	int num_found = 0;
	int num_preds = get_Block_n_cfgpreds(end);
	int result;
	int i;

	/* Iterate the predecessor blocks of end. */
	for (i = 0; i < num_preds; i++) {

		ir_node *pred = get_Block_cfgpred(end, i);
		if (!is_Block(pred)) pred = get_nodes_block(pred);

		if (cfg_reachable(start, pred)) {

			num_found++;
			result = i;

			/* We can reach multiple predecessors. */
			if (num_found > 1) {
				result = num_preds;
				break;
			}
		}
	}

	if (num_found == 0) return -1;
	return result;
}

/**
 * Get the predecessor node of the given block that is inside the given
 * predecessor block. Used to select conditions and mode_X projs from the CFG.
 */
static ir_node *get_pred_in_block(ir_node *block, ir_node *pred_block)
{
	int i;

	/* Scan all predecessors and examine their block. */
	for (i = 0; i < get_Block_n_cfgpreds(block); i++){

		ir_node *pred_node = get_Block_cfgpred(block, i);

		if (get_nodes_block(pred_node) == pred_block) {
			return pred_node;
		}
	}

	assert(0 && "No predecessor in the given block.");
}

static ir_node *get_branch_cond(ir_node *block)
{
	ir_node *succ;
	ir_node *proj_node;
	ir_node *cond_node;
	int      num_outs;

	assert(is_Block(block));

	num_outs = get_Block_n_cfg_outs(block);
	assert(num_outs == 2);

	/* Get one of the two successors. */
	succ = get_Block_cfg_out(block, 0);

	proj_node = get_pred_in_block(succ, block);
	assert(is_Proj(proj_node));

	cond_node = get_Proj_pred(proj_node);
	assert(is_Cond(cond_node));

	/* Return the boolean value. */
	return get_Cond_selector(cond_node);
}

/**
 * Analyze all branches between block and end and construct gamma nodes which
 * select a value for each predecessor of end. The number of values has to match
 * the number of predecessors of the end block.
 */
static ir_node *select_value(ir_node *block, ir_node *end, ir_node **values)
{
	ir_node *block_a, *block_b;
	ir_node *value_a, *value_b;
	int      pred_a,   pred_b;
	int      num_outs, num_preds;

	ir_node *proj_a;
	int      proj_a_pn;

	ir_node *cond, *gamma;

	assert(is_Block(block));
	assert(is_Block(end));

	/* Walk along the CFG tree in execution order. */
	num_outs = get_Block_n_cfg_outs(block);
	assert(num_outs > 0);

	/* If the path to take is unambiguous, simply recurse. */
	if (num_outs == 1) {
		ir_node *next = get_Block_cfg_out(block, 0);
		return select_value(next, end, values);
	}

	assert((num_outs == 2) && "Branch with more than two alternatives.");

	/**
	 * Get the two successor blocks and find out whether they can reach
	 * the given end node.
	 */
	block_a = get_Block_cfg_out(block, 0);
	block_b = get_Block_cfg_out(block, 1);

	/**
	 * Analyze which branch can reach which predecessors of end.
	 */
	pred_a = cfg_rechable_pred(block_a, end);
	pred_b = cfg_rechable_pred(block_b, end);
	assert(((pred_a >= 0) || (pred_b >= 0)) && "Path leads to nowhere.");

	if ((pred_a < 0) || (pred_b < 0)) {
		/* If only one branch can reach predecessors of end, skip the other. */
		return select_value((pred_a < 0) ? block_b : block_a, end, values);
	}

	num_preds = get_Block_n_cfgpreds(end);

	/**
	 * Now select a value for both branches. Either by recursing or by taking
	 * the value corresponding to the predecessor block from the value array.
	 */
	if (pred_a < num_preds) value_a = values[pred_a];
	else value_a = select_value(block_a, end, values);

	if (pred_b < num_preds) value_b = values[pred_b];
	else value_b = select_value(block_b, end, values);

	/* Find the proj node in block that leads to block_a. */
	proj_a = get_pred_in_block(block_a, block);
	assert(is_Proj(proj_a));

	proj_a_pn = get_Proj_proj(proj_a);
	assert((proj_a_pn == pn_Cond_true) || (proj_a_pn == pn_Cond_false));

	/* From the projection number determine what is true and false. */
	if (proj_a_pn != pn_Cond_false) {
		/* Swap both values if necessary. */
		ir_node *temp = value_a;
		value_a = value_b;
		value_b = temp;
	}

	/* Create the gamma node to select the value. */
	cond  = get_branch_cond(block);
	gamma = new_r_Gamma(end, cond, value_a, value_b, mode_T);

	return gamma;
}

static void replace_phis_walk(ir_node *block, void *ctx)
{
	int i, j;
	int num_preds;

	ir_node **tuples;
	ir_node **phis;
	ir_node  *current;
	ir_node  *idom, *tuple;

	(void)ctx;

	assert(is_Block(block));

	/* Skip blocks without phis. */
	if (get_Block_phis(block) == NULL) {
		return;
	}

	num_preds = get_Block_n_cfgpreds(block);

	/* Create an array of phi nodes for this block. */
	phis = NEW_ARR_F(ir_node*, 0);

	current = get_Block_phis(block);
	while (current != NULL) {
		ARR_APP1(ir_node*, phis, current);
		current = get_Phi_next(current);
	}

	/* For each predecessor create a tuple of values that are selected. */
	tuples = NEW_ARR_F(ir_node*, num_preds);

	for (i = 0; i < num_preds; i++) {

		/* Collect the values of all phis on the given predecessor. */
		ir_node **values = NEW_ARR_F(ir_node*, ARR_LEN(phis));

		for (j = 0; j < ARR_LEN(phis); j++)
		{
			values[j] = get_Phi_pred(phis[j], i);
		}

		/* Create a tuple from them. */
		tuples[i] = new_r_Tuple(block, ARR_LEN(phis), values);
		DEL_ARR_F(values);
	}

	/**
	 * Get the immediate dominator of this block and analyze the CFG between
	 * both blocks, to create a PEG tree that selects the appropriate tuple
	 * using the branch conditions along the way.
	 */
	idom  = get_Block_idom(block);
	tuple = select_value(idom, block, tuples);

	/* Now from the resulting tuple construct projs to replace the phis. */

	for (i = 0; i < ARR_LEN(phis); i++) {
		ir_node *phi  = phis[i];
		ir_mode *mode = get_irn_mode(phi);
		ir_node *proj = new_r_Proj(tuple, mode, i);
		exchange(phi, proj);
	}

	DEL_ARR_F(tuples);
	DEL_ARR_F(phis);
}

static void replace_phis(ir_graph *irg)
{
	/* We need to walk the CFG in reverse order and access dominators. */
	assure_doms(irg);
	assure_irg_outs(irg);

	/* Create lists of phi nodes in each block. */
	ir_resources_t resources = IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST;
	ir_reserve_resources(irg, resources);
	collect_phiprojs(irg);

	/* Walk along the graph and replace phi nodes by gammas. */
	irg_block_walk_graph(irg, NULL, replace_phis_walk, NULL);

	ir_free_resources(irg, resources);
}

/**
 * Moves all nodes except those in the start- and end-block to the given block.
 */
static void move_to_block(ir_node *irn, void *ctx)
{
	ir_graph *irg;
	ir_node  *block, *target;

	if (is_Block(irn)) return;

	irg   = get_irn_irg(irn);
	block = get_nodes_block(irn);

	if ((block == get_irg_start_block(irg)) ||
	    (block == get_irg_end_block(irg))) {

		return;
	}

	target = (ir_node*)ctx;
	set_nodes_block(irn, target);
}

static void remove_blocks(ir_graph *irg)
{
	/* Create a single block to stuff everything else in. */
	ir_node *start = get_irg_start(irg);
	ir_node *exec  = new_r_Proj(start, mode_X, pn_Start_X_initial_exec);
	ir_node *block = new_r_Block(irg, 1, &exec);

	/**
	 * Move all nodes into that block. The old block structure will vanish,
	 * since it can't be reached from the end block, once the return has been
	 * moved away into the new block.
	 */
	irg_walk_graph(irg, NULL, move_to_block, block);
}

/**
 * Unfolds a value in a tree of tupelized gammas. This replicates the tree
 * along the way with the values mode.
 */
static ir_node *extract_gammas(ir_node *irn, int idx, ir_mode *mode)
{
	if (is_Gamma(irn))
	{
		/* Construct a new gamma for the value. */
		ir_node *block    = get_nodes_block(irn);
		ir_node *cond     = get_Gamma_cond(irn);
		ir_node *ir_true  = extract_gammas(get_Gamma_true(irn),  idx, mode);
		ir_node *ir_false = extract_gammas(get_Gamma_false(irn), idx, mode);
		ir_node *gamma    = new_r_Gamma(block, cond, ir_false, ir_true, mode);

		return gamma;
	}
	else if (is_Tuple(irn))
	{
		/* Extract the appropriate value from the tuple. */
		ir_node *res = get_Tuple_pred(irn, idx);
		assert((mode == get_irn_mode(res)) && "Unexpected mode.");

		return res;
	}

	assert(0 && "Invalid tupelized gamma tree.");
}

static void unfold_tuples_walk(ir_node *irn, void *ctx)
{
	ir_node *gamma, *value;
	ir_mode *value_mode;
	int      value_pn;

	(void)ctx;

	/* Search for proj nodes. */
	if (!is_Proj(irn)) return;

	/* That point to gammas. */
	gamma = get_Proj_pred(irn);
	if (!is_Gamma(gamma)) return;

	/* Unfold the value from the tupelized gamma. */
	value_pn   = get_Proj_proj(irn);
	value_mode = get_irn_mode(irn);
	value      = extract_gammas(gamma, value_pn, value_mode);

	exchange(irn, value);
}

static void unfold_tuples(ir_graph *irg)
{
	irg_walk_graph(irg, NULL, unfold_tuples_walk, NULL);
}

void convert_to_peg(ir_graph *irg)
{
	int opt_level = get_optimize();
	set_optimize(0);

	dump_ir_graph(irg, "cfg");

	/* Eliminate multiple returns. The program is exactly one value in PEG. */
	normalize_one_return(irg);
	dump_ir_graph(irg, "return");

	/* Replace phi nodes by gamma trees selecting tuples. */
	replace_phis(irg);
	dump_ir_graph(irg, "gamma");

	/* Remove the existing block structure. */
	remove_blocks(irg);
	dump_ir_graph(irg, "nocfg");

	/* Unfold tuples on gammas and thetas. */
	unfold_tuples(irg);
	dump_ir_graph(irg, "unfold");

	/* Most data is probably inconsistent now. */
	set_irg_outs_inconsistent(irg);
	set_irg_doms_inconsistent(irg);
	set_irg_extblk_inconsistent(irg);
	set_irg_loopinfo_inconsistent(irg);

	optimize_cf(irg);
	set_optimize(opt_level);
}
