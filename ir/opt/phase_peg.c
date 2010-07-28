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
#include "pset_new.h"
#include "cdep.h"
#include "iredges.h"

static int get_pred_index(ir_node *block, ir_node *pred_block)
{
	int i;

	/* Get the predecessors of block until the given block is found. */
	for (i = 0; i < get_Block_n_cfgpreds(block); i++) {

		ir_node *pred = get_Block_cfgpred(block, i);
		if (get_nodes_block(pred) == pred_block) return i;
	}

	return -1;
}

static ir_node *get_pred_in_block(ir_node *block, ir_node *pred_block)
{
	int pred_index = get_pred_index(block, pred_block);
	assert((pred_index >= 0) && "Predecessor not found.");

	return get_Block_cfgpred(block, pred_index);
}

static int is_in_loop(ir_node *node, ir_loop *outer_loop)
{
	ir_loop *last;
	ir_loop *loop;

	if (!is_Block(node)) node = get_nodes_block(node);
	loop = get_irn_loop(node);
	if (loop == NULL) return 0;

	do {
		if (loop == outer_loop) return 1;

		/* Stop when get_loop_outer_loop returns loop again. */
		last = loop;
		loop = get_loop_outer_loop(loop);
	}
	while(loop != last);

	return 0;
}

/* Collect all reachable nodes, stopping at the given end node. */
static void find_cdep_reach(pset_new_t *reach, ir_node *block, ir_node *idom)
{
	ir_cdep *dep;

	assert(is_Block(block));
	if (block == idom) return;

	pset_new_insert(reach, block);
	dep = find_cdep(block);

	/* Recurse all unvisited blocks. */
	while (dep != NULL) {
		if (!pset_new_contains(reach, dep->node)) {
			find_cdep_reach(reach, dep->node, idom);
		}

		dep = dep->next;
	}
}

static ir_node *select_values_walk(
	pset_new_t *reach,   /* Nodes that can reach the preds via cdeps. */
	ir_node    *block,   /* The current block to construct a selector for. */
	ir_node    *end,     /* The end node where selection takes place. */
	ir_node    **values) /* A value for each of the preds of end. */
{
	ir_cdep *dep;
	ir_node *used_deps[2];
	int      num_used_deps;
	int      pred_index;

	/* Get the reverse deps for the block. */
	dep = find_rev_cdep(block);

	/* Collect the deps that can reach the predecessors. */
	num_used_deps = 0;
	while (dep != NULL) {

		if (pset_new_contains(reach, dep->node)) {
			used_deps[num_used_deps] = dep->node;
			num_used_deps++;

			/* This won't happen I have proven it! */
			assert(num_used_deps <= 2);
		}

		dep = dep->next;
	}

	/* Check if this is a predecessor of end and get the index. There should
	 * always be somewhere to recurse except on a predecessor block. */
	pred_index = get_pred_index(end, block);
	assert(((pred_index >= 0) || (num_used_deps > 0)) && "Nowhere to walk.");

	/* There are four situations that may happen. (1) This is a predecessor
	 * we have a value for. (2) This is no predecessor but there is one dependee
	 * to recurse to. (3) This is a branch with two dependees to get the values
	 * from. (4) This is a branch with one dependee and the block is a
	 * predecessor itself to get the other value from. */

	/* Case one. */
	if ((num_used_deps == 0) && (pred_index >= 0)) {
		return values[pred_index];
	}

	/* Case two. */
	if ((num_used_deps == 1) && (pred_index < 0)) {
		return select_values_walk(reach, used_deps[0], end, values);
	}

	/* Case three. */
	if ((num_used_deps == 2) && (pred_index < 0)) {

		ir_loop *loop;
		ir_mode *mode;
		ir_node *cond, *ir_true, *ir_false, *succ, *succ_proj;
		int      succ_req, succ_cur;

		/* Get the values for the gamma. True/false may still be flipped. */
		ir_true  = select_values_walk(reach, used_deps[0], end, values);
		ir_false = select_values_walk(reach, used_deps[1], end, values);

		/* Get the first successor and find out which branch it belongs to. */
		succ      = get_edge_src_irn(get_block_succ_first(block));
		succ_proj = get_pred_in_block(succ, block);

		/* Determine where the successor should lead to and where it really
		 * leads. Note that list[0] will postdominate the successor that
		 * depends on block but not block itself (and the other successor). */
		succ_req = (get_Proj_proj(succ_proj) == pn_Cond_true);
		succ_cur = block_postdominates(used_deps[0], succ);

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

		/* Find out if cond is inside some loop that end (where the value is
		 * actually selected) is not in. */
		loop = get_irn_loop(cond);
		if (loop && !is_in_loop(end, loop)) {
			/* TODO: construct an extract node for the cond. */
			assert(0);
		}

		return new_r_Gamma(end, cond, ir_false, ir_true, mode);
	}

	/* Case four is not handled yet. */

	assert(0 && "Unexpected control dependence graph.");
	return NULL;
}

static ir_node *select_values(pset_new_t *preds, ir_node *block, ir_node **values)
{
	int                  num_preds;
	ir_node             *idom, *result, *pred;
	pset_new_t           reach;
	pset_new_iterator_t  it;

	num_preds = pset_new_size(preds);
	if (num_preds == 0) return NULL;

	/* Shortcut for exactly one pred. */
	if (num_preds == 1) {
		foreach_pset_new(preds, pred, it) {
			int index = get_pred_index(block, pred);
			assert(index >= 0);
			return values[index];
		}
	}

	/* Get the idom of the given pred blocks. */
	idom = NULL;
	foreach_pset_new(preds, pred, it) {
		if (idom == NULL) idom = get_Block_idom(pred);
		else idom = node_smallest_common_dominator(idom, pred);
		assert(idom);
	}

	/* Discover reachable deps for the preds. */
	pset_new_init(&reach);
	foreach_pset_new(preds, pred, it) {
		find_cdep_reach(&reach, pred, idom);
	}

	/* Walk down the reachable deps from the idom and create gammas. */
	result = select_values_walk(&reach, idom, block, values);
	pset_new_destroy(&reach);

	return result;
}

static ir_node **get_incoming_values(ir_node *block)
{
	int num_phis, i;
	int num_preds = get_Block_n_cfgpreds(block);

	ir_node **tuples = NEW_ARR_F(ir_node*, num_preds);
	ir_node  *phi;

	/* Count phi nodes in this block. */
	phi = get_Block_phis(block);
	num_phis = 0;

	while (phi != NULL) {
		num_phis++;
		phi = get_Phi_next(phi);
	}

	/* For each predecessor create a tuple of values selected by the phis. */
	for (i = 0; i < num_preds; i++) {
		int j = 0;

		/* Collect the values of all phis on the given predecessor. */
		ir_node **values = NEW_ARR_F(ir_node*, num_phis);

		phi = get_Block_phis(block);
		while (phi != NULL) {
			values[j++] = get_Phi_pred(phi, i);
			phi = get_Phi_next(phi);
		}

		/* Create a tuple from them. */
		tuples[i] = new_r_Tuple(block, num_phis, values);
		DEL_ARR_F(values);
	}

	return tuples;
}

static void free_incoming_values(ir_node **tuples)
{
	DEL_ARR_F(tuples);
}

static void replace_phis_by_projs(ir_node *block, ir_node *tuple) {

	ir_node *phi, *next_phi;
	int i = 0;

	/* For each node create a proj getting a value from the tuple. */
	phi = get_Block_phis(block);
	while (phi != NULL) {

		ir_mode *mode = get_irn_mode(phi);
		ir_node *proj = new_r_Proj(tuple, mode, i++);

		next_phi = get_Phi_next(phi);
		exchange(phi, proj);
		phi = next_phi;
	}
}

static ir_node *create_break_cond(ir_node *block, ir_loop *loop)
{
	/* Not supported yet. */
	(void)loop;
	return new_r_Unknown(get_irn_irg(block), mode_b);
}

static void create_phi_extracts(ir_node *block)
{
	int       i;
	ir_node  *phi;
	ir_loop  *loop = get_irn_loop(block);
	ir_graph *irg  = get_irn_irg(block);

	/* No extracts needed when accessing phis outside of loops. */
	if (!loop) return;

	/* Iterate all phi nodes in the current block. */
	phi = get_Block_phis(block);
	while (phi != NULL) {
		const ir_edge_t *edge, *tmp;

		foreach_out_edge_safe(phi, edge, tmp) {
			ir_node *src = get_edge_src_irn(edge);

			/* End processes no values. These are keep-alive edges. */
			if (src == get_irg_end(irg)) continue;

			/* When someone outside the loop accesses the phi node, construct
			 * an extract node to get the appropriate value. */
			if (!is_in_loop(src, loop)) {

				ir_mode  *mode      = get_irn_mode(phi);
				ir_node  *cond      = create_break_cond(block, loop);
				ir_node  *src_block = get_nodes_block(src);
				ir_node  *extract   = new_r_Extract(src_block, phi, cond, mode);

				/* Rewire src to access the extract node instead. */
				for (i = 0; i < get_irn_arity(src); i++) {
					ir_node *dep = get_irn_n(src, i);
					if (dep == phi) set_irn_n(src, i, extract);
				}
			}
		}

		phi = get_Phi_next(phi);
	}
}

static void replace_phis_walk(ir_node *block, void *ctx)
{
	int          i;
	ir_loop     *loop;
	ir_node    **values, *outer_tuple, *inner_tuple, *result;
	pset_new_t   outer_preds, inner_preds;

	(void)ctx;

	/* Skip blocks without phis. */
	if (get_Block_phis(block) == NULL) return;

	/* Obtain for each predecessor the values selected by phis. */
	values = get_incoming_values(block);

	/* Find out if we need to construct a theta node or just gamma nodes.
	 * If we are inside a loop header (there are incoming paths from the loop
	 * and from outside the loop), we need a theta node. The values from those
	 * predecessors outside the loop provide the init value and those from
	 * inside the loop the next value. Use the usual gamma tree construction
	 * for both values, but on a subset of predecessors only. */

	/* Collect pred nodes outside the current loop. */
	pset_new_init(&outer_preds);
	pset_new_init(&inner_preds);

	loop = get_irn_loop(block);
	for (i = 0; i < get_Block_n_cfgpreds(block); i++) {

		ir_node    *pred = get_nodes_block(get_Block_cfgpred(block, i));
		pset_new_t *set  = &inner_preds;

		/* Classify preds by their origin (inside the loop or outside). If
		 * there is no loop, put everything in inner_preds. */
		if (loop && !is_in_loop(pred, loop)) {
			set = &outer_preds;
		}

		pset_new_insert(set, pred);
	}

	/* Create gammas for the outer and inner preds. Can return NULL. */
	inner_tuple = select_values(&inner_preds, block, values);
	outer_tuple = select_values(&outer_preds, block, values);
	assert(inner_tuple);

	/* If there are outer and inner preds, construct a theta node. */
	result = inner_tuple;
	if (outer_tuple) {
		result = new_r_Theta(block, outer_tuple, inner_tuple, mode_T);
	}

	/* Construct projs on the result. */
	create_phi_extracts(block);
	replace_phis_by_projs(block, result);

	pset_new_destroy(&inner_preds);
	pset_new_destroy(&outer_preds);
	free_incoming_values(values);
}

static void replace_phis(ir_graph *irg)
{
	ir_resources_t resources =
		IR_RESOURCE_IRN_LINK |
		IR_RESOURCE_PHI_LIST;

	/* We need to walk the CFG in reverse order and access dominators. */
	assure_doms(irg);
	assure_cf_loop(irg);

	/* Create lists of phi nodes in each block. */
	ir_reserve_resources(irg, resources);
	collect_phiprojs(irg);

	/* Create the control dependence graph. */
	compute_cdep(irg);
	compute_rev_cdep(irg);

	/* Walk along the graph and replace phi nodes by gammas. */
	irg_block_walk_graph(irg, NULL, replace_phis_walk, NULL);

	free_rev_cdep(irg);
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
static ir_node *unfold_tuples_walk(ir_node *irn, int idx, ir_mode *mode)
{
	ir_node *block = get_nodes_block(irn);

	if (is_Gamma(irn)) {
		/* Construct a new gamma for the value. */
		ir_node *cond     = get_Gamma_cond(irn);
		ir_node *ir_true  = unfold_tuples_walk(get_Gamma_true(irn),  idx, mode);
		ir_node *ir_false = unfold_tuples_walk(get_Gamma_false(irn), idx, mode);
		return new_r_Gamma(block, cond, ir_false, ir_true, mode);
	}
	else if (is_Theta(irn)) {
		/* Same thing for the theta. */
		ir_node *init = unfold_tuples_walk(get_Theta_init(irn), idx, mode);
		ir_node *next = unfold_tuples_walk(get_Theta_next(irn), idx, mode);
		return new_r_Theta(block, init, next, mode);
	}
	else if (is_Tuple(irn)) {
		/* Extract the appropriate value from the tuple. */
		ir_node *res = get_Tuple_pred(irn, idx);
		assert((mode == get_irn_mode(res)) && "Unexpected mode.");
		return res;
	}

	assert(0 && "Invalid tupelized gamma tree.");
}

static void unfold_tuples_find(ir_node *irn, void *ctx)
{
	ir_node *tuple, *value;
	ir_mode *value_mode;
	int      value_pn;

	(void)ctx;

	/* Search for proj nodes. */
	if (!is_Proj(irn)) return;

	/* That point to gammas or thetas. */
	tuple      = get_Proj_pred(irn);
	value_pn   = get_Proj_proj(irn);
	value_mode = get_irn_mode(irn);

	if (is_Gamma(tuple) || is_Theta(tuple)) {
		value = unfold_tuples_walk(tuple, value_pn, value_mode);
		exchange(irn, value);
	}
}

static void unfold_tuples(ir_graph *irg)
{
	irg_walk_graph(irg, NULL, unfold_tuples_find, NULL);
}

void convert_to_peg(ir_graph *irg)
{
	/* Use automatic out edges. Makes things easier later. */
	int opt_level = get_optimize();
	int had_edges = edges_assure(irg);
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
	if (!had_edges) edges_deactivate(irg);
}
