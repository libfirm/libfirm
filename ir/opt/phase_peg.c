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
#include "irgmod.h"
#include "irgwalk.h"
#include "lowering.h"
#include "array_t.h"
#include "pmap.h"
#include "cdep.h"
#include "irouts.h"

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

typedef struct rev_deps {

	ir_node *idom;
	ir_node *end;
	pmap    *dep_map;

} rev_deps;

static rev_deps *rev_deps_create(ir_node *end) {

	rev_deps *deps = XMALLOC(rev_deps);
	deps->idom    = get_Block_idom(end);
	deps->end     = end;
	deps->dep_map = pmap_create();
	return deps;
}

static void rev_deps_destroy(rev_deps *deps) {

	/* Delete the edge arrays for each node. */
	pmap_entry *entry = pmap_first(deps->dep_map);
	while (entry != NULL) {
		DEL_ARR_F(entry->value);
		entry = pmap_next(deps->dep_map);
	}

	pmap_destroy(deps->dep_map);
}

static void rev_deps_add_dep(rev_deps *deps, ir_node *src, ir_node *dst) {

	/* Get or create an array for the outgoing deps of src. */
	ir_node **list = pmap_get(deps->dep_map, src);

	if (list == NULL) {
		list = NEW_ARR_F(ir_node*, 0);
	}

	/* Add dst to the array. Update the map (resize changes list). */
	ARR_APP1(ir_node*, list, dst);
	pmap_insert(deps->dep_map, src, list);
}

static void collect_rev_deps(ir_node *block, rev_deps *deps) {

	/* Start at the end. */
	if (block == NULL) {
		int i;

		/* Recurse on all predecessors of the end block. */
		for (i = 0; i < get_Block_n_cfgpreds(deps->end); i++) {

			ir_node *pred = get_Block_cfgpred(deps->end, i);
			if (!is_Block(pred)) pred = get_nodes_block(pred);

			collect_rev_deps(pred, deps);
		}

		return;
	}

	/* Stop at the idom. */
	if (block == deps->idom) return;

	assert(is_Block(block));

	/* Collect all reachable dep edges. */
	ir_cdep *dep = find_cdep(block);
	while (dep != NULL) {

		/* Recurse only on non-discovered paths. */
		if (!pmap_contains(deps->dep_map, dep->node)) {
			collect_rev_deps(dep->node, deps);
		}

		rev_deps_add_dep(deps, dep->node, block);
		dep = dep->next;
	}
}

static ir_node *select_values(ir_node *block, rev_deps *deps, ir_node **values) {

	int     i, succ_req, succ_cur, num_deps;
	ir_node *ir_true, *ir_false, *cond, *succ, *succ_proj;
	ir_mode *mode;

	/* Start at the idom. */
	if (block == NULL) {
		block = deps->idom;
	}

	ir_node **list = pmap_get(deps->dep_map, block);

	if (list == NULL) {

		/* One of the predecessors of end. Find the index. */
		for (i = 0; i < get_Block_n_cfgpreds(deps->end); i++) {

			ir_node *pred = get_Block_cfgpred(deps->end, i);
			if (!is_Block(pred)) pred = get_nodes_block(pred);

			if (pred == block) return values[i];
		}

		assert(0);
		return NULL;
	}

	num_deps = ARR_LEN(list);

	switch (num_deps) {
	case 1:
		/* Simply follow the edge. */
		return select_values(list[0], deps, values);

	case 2:
		/* Get the values for the gamma. True/false may still be flipped. */
		ir_true  = select_values(list[0], deps, values);
		ir_false = select_values(list[1], deps, values);

		/* Get the first successor and find out which branch it belongs to. */
		succ      = get_Block_cfg_out(block, 0);
		succ_proj = get_pred_in_block(succ, block);

		/* Determine where the successor should lead to and where it really
		 * leads. Note that list[0] will postdominate the successor that
		 * depends on block but not block itself (and the other successor). */
		succ_req = (get_Proj_proj(succ_proj) == pn_Cond_true);
		succ_cur = block_postdominates(list[0], succ);

		/* If succ_cur is true, succ leads to list[0] and therefore to the
		 * currently assigned ir_true. This has to match succ_req which is
		 * true, when succ belongs to the true proj of the branch. */
		if (succ_req != succ_cur) {
			/* Flip true and false if that is not the case. */
			ir_node *tmp = ir_true;
			ir_true  = ir_false;
			ir_false = tmp;
		}

		/* Construct the gamma node. */
		mode = get_irn_mode(ir_true);
		cond = get_Cond_selector(get_Proj_pred(succ_proj));
		return new_r_Gamma(deps->end, cond, ir_false, ir_true, mode);
	}

	assert(0);
	return NULL;
}

static void replace_phis_walk(ir_node *block, void *ctx)
{
	int i, j;
	int num_preds;

	ir_node  **phis, **tuples;
	ir_node  *current;
	ir_node  *res;
	rev_deps *deps;

	(void)ctx;

	assert(is_Block(block));

	/* Skip blocks without phis. */
	if (get_Block_phis(block) == NULL) {
		return;
	}

	num_preds = get_Block_n_cfgpreds(block);
	phis      = NEW_ARR_F(ir_node*, 0);
	tuples    = NEW_ARR_F(ir_node*, num_preds);

	/* Create an array of phi nodes for this block. */
	current = get_Block_phis(block);
	while (current != NULL) {
		ARR_APP1(ir_node*, phis, current);
		current = get_Phi_next(current);
	}

	/* For each predecessor create a tuple of values that are selected. */
	for (i = 0; i < num_preds; i++) {

		/* Collect the values of all phis on the given predecessor. */
		ir_node **values = NEW_ARR_F(ir_node*, ARR_LEN(phis));
		ir_node  *pred   = get_Block_cfgpred(block, i);

		if (!is_Block(pred)) pred = get_nodes_block(pred);

		for (j = 0; j < ARR_LEN(phis); j++)
		{
			values[j] = get_Phi_pred(phis[j], i);
		}

		/* Create a tuple from them. */
		tuples[i] = new_r_Tuple(block, ARR_LEN(phis), values);
		DEL_ARR_F(values);
	}

	/* Collect all reachable reverse control dependencies. */
	deps = rev_deps_create(block);
	collect_rev_deps(NULL, deps);

	/* Create the gamma nodes from there. */
	res = select_values(NULL, deps, tuples);
	rev_deps_destroy(deps);

	/* Now from the resulting tuple construct projs to replace the phis. */
	for (i = 0; i < ARR_LEN(phis); i++) {
		ir_node *phi  = phis[i];
		ir_mode *mode = get_irn_mode(phi);
		ir_node *proj = new_r_Proj(res, mode, i);
		exchange(phi, proj);
	}

	DEL_ARR_F(tuples);
	DEL_ARR_F(phis);
}

static void replace_phis(ir_graph *irg)
{
	ir_resources_t resources =
		IR_RESOURCE_IRN_LINK |
		IR_RESOURCE_PHI_LIST;

	/* We need to walk the CFG in reverse order and access dominators. */
	assure_doms(irg);
	assure_irg_outs(irg);

	/* Create lists of phi nodes in each block. */
	ir_reserve_resources(irg, resources);
	collect_phiprojs(irg);

	/* Create the control dependence graph. */
	compute_cdep(irg);

	/* Walk along the graph and replace phi nodes by gammas. */
	irg_block_walk_graph(irg, NULL, replace_phis_walk, NULL);

	free_cdep(irg);
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
	/* Use automatic out edges. Makes things easier later. */
	int opt_level = get_optimize();
	set_optimize(0);

	dump_ir_graph(irg, "cfg");

	/* Eliminate all switch nodes, we can't represent them in a PEG. */
	lower_switch(irg, 0);
	dump_ir_graph(irg, "switch");

	/* Eliminate multiple returns. The program is exactly one value in PEG. */
	normalize_one_return(irg);
	dump_ir_graph(irg, "return");

	/* Replace phi nodes by gamma trees selecting tuples. */
	replace_phis(irg);
	dump_ir_graph(irg, "gamma");

	/* Remove the existing block structure. */
	remove_blocks(irg);

	dump_ir_graph(irg, "nocfg-cf");
	ir_add_dump_flags(ir_dump_flag_hide_control_flow);
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
