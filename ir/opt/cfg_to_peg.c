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
#include "cdep.h"
#include "iredges.h"
#include "bitset.h"
#include "pmap.h"

/******************************************************************************
 * Utility functions.                                                         *
 ******************************************************************************/

/**
 * Given two blocks, where pred_block is a predecessor of block, returns the
 * index of that predecessor among blocks dependencies.
 */
static int get_Block_cfgpred_idx(ir_node *block, ir_node *pred_block)
{
	int i;
	assert(is_Block(block) && is_Block(pred_block));

	/* Get the predecessors of block until the given block is found. */
	for (i = 0; i < get_Block_n_cfgpreds(block); i++) {
		ir_node *pred = get_Block_cfgpred(block, i);
		if (get_nodes_block(pred) == pred_block) return i;
	}

	return -1;
}

/**
 * Given two blocks, where pred_block is a predecessor of block, returns the
 * node inside pred_block that block depends on.
 */
static ir_node *get_pred_in_block(ir_node *block, ir_node *pred_block)
{
	int pred_index = get_Block_cfgpred_idx(block, pred_block);
	assert((pred_index >= 0) && "Predecessor not found.");

	return get_Block_cfgpred(block, pred_index);
}

/**
 * Determines whether the given node is nested somewhere inside the specified
 * loop by walking up the loop tree.
 */
static int is_in_loop(ir_node *node, ir_loop *outer_loop)
{
	ir_loop *last, *loop;
	assert(node && outer_loop);

	/* get_irn_loop will return NULL on a node. */
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

/**
 * Determines the transitive closure of the predecessor relation between
 * blocks, that is all blocks that the given block transitively depends on,
 * up to a given end node, which is where computation stops.
 */
static void find_pred_closure(bitset_t *closure, ir_node *block, ir_node *end)
{
	int i;
	assert(is_Block(block));

	bitset_set(closure, get_irn_idx(block));
	if (block == end) return;

	/* Simple recurse to the pred blocks if they weren't visited before. */
	for (i = 0; i < get_Block_n_cfgpreds(block); i++) {
		ir_node *pred = get_Block_cfgpred(block, i);
		pred = get_nodes_block(pred);

		if (!bitset_is_set(closure, get_irn_idx(pred))) {
			find_pred_closure(closure, pred, end);
		}
	}
}

/******************************************************************************
 * Phase 1-1: Create gamma graphs for value selection.                        *
 ******************************************************************************/

/**
 * Associates a number of edges with a value. This is passed to build_gammas,
 * which selects the given value, based on the branches that have to be taken
 * from the start block to one of the mentioned edges.
 */

typedef struct select_edge {
	ir_node *src;
	ir_node *dst;
} select_edge;

typedef struct select_value {
	ir_node     *value;
	int          num_edges;
	select_edge *edges;
} select_value;

/**
 * For simple branching structures the start node is usually the closest dom
 * of all the involved edges (or more accurately the source blocks as they are
 * dominate their edge). This function is an easy way to determine it.
 */
static ir_node *find_values_dom(int num_values, select_value *values)
{
	int      i, j;
	ir_node *dom = NULL;

	assert((num_values > 0) && values);

	/* Iterate all the blocks. */
	for (i = 0; i < num_values; i++) {
		select_value *value = values + i;
		assert(value->num_edges > 0);

		for (j = 0; j < value->num_edges; j++) {
			ir_node *src = value->edges[j].src;
			dom = dom ? node_smallest_common_dominator(dom, src) : src;
		}
	}

	assert(dom);
	return dom;
}

typedef struct build_gammas_ctx
{
	bitset_t      *path;
	ir_node       *cons_block;
	int            num_values;
	select_value  *values;
	bitset_t     **closures;

} build_gammas_ctx;

static ir_node *build_gammas_walk(ir_node *block, build_gammas_ctx *ctx)
{
	const ir_edge_t *edge;

	int i, j, block_idx;
	int num_closures = 0, num_edges = 0;

	select_edge  edges[2];
	ir_node     *values[2];
	ir_node     *closure_value;

	/* Find overlapping closures and outgoing edges. */
	block_idx = get_irn_idx(block);

	/* Return NULL on backedges. If there would be a value, the previous node
	 * wouldn't have recursed here. Cancel recursion to avoid a loop. */
	if (bitset_is_set(ctx->path, block_idx)) return NULL;

	for (i = 0; i < ctx->num_values; i++) {
		/* Check the closure of every value. */
		if (bitset_is_set(ctx->closures[i], block_idx)) {
			select_value *value = &ctx->values[i];

			/* Store the closures value (to get it if there is only one). */
			num_closures++;
			closure_value = value->value;

			/* Scan edges for the closures we are in. */
			for (j = 0; j < value->num_edges; j++) {
				if (value->edges[j].src == block) {
					assert((num_edges < 2) && "More than two out-edges.");

					/* For any adjoining edge store edge and value. */
					edges[num_edges]  = value->edges[j];
					values[num_edges] = value->value;
					num_edges++;
				}
			}
		}
	}

	/* We are about to leave all transitive closures. Recurse back. */
	if (num_closures == 0) return NULL;

	/* If we overlap with only one closure, don't recurse further. */
	if (num_closures == 1) return closure_value;

	/* There are two successors and we are in more than one closure. We may
	 * already have <=2 values from adjoing edges in values and edges. Try
	 * get remaining values by recursion. */

	foreach_block_succ(block, edge) {
		int      have_value = 0;
		ir_node *succ       = get_edge_src_irn(edge);
		ir_node *value      = NULL;

		/* If this is an edge we have a value for, don't recurse there. */
		for (i = 0; i < num_edges; i++) {
			if (edges[i].dst == succ) {
				have_value = 1;
				break;
			}
		}

		/* Continue with the next edge instead. */
		if (have_value) continue;

		/* Try to recurse, to get another value. This may return NULL, if we
		 * leave all transitive hulls by that edge. */
		bitset_set(ctx->path, block_idx);
		value = build_gammas_walk(succ, ctx);
		bitset_clear(ctx->path, block_idx);

		/* Store the value and the edge we used. */
		if (value) {
			assert((num_edges < 2) && "More than two out-edges.");

			edges[num_edges].src = block;
			edges[num_edges].dst = succ;
			values[num_edges]    = value;
			num_edges++;
		}
	}

	/* Backtrack if we can get no value. This will happen on paths that end
	 * up in an already visited block without encountering a value edge (if
	 * such an edge had beed encountered, the last unvisited block would have
	 * had num_edges >= 1 and returned non-NULL). */
	if (num_edges <= 0) return NULL;

	/* One path must have reached a backedge and backtracked or it has left
	 * all closures and leads to nowhere. Return the value we have for sure. */
	if (num_edges == 1) return values[0];

	/* We have exactly two values to select from here. */
	assert((num_edges == 2) && "Unexpected value count.");
	{
		int      is_flipped;
		ir_node *jump_proj, *ir_false, *ir_true, *cond, *sel;
		ir_mode *mode;

		/* Get the jump proj to reach value[0]. */
		jump_proj = get_pred_in_block(edges[0].dst, block);
		assert(is_Proj(jump_proj) && "Unexpected jump nodes on branch.");

		/* We have the proj of values[0]. So if it has pn_Cond_false, ir_false
		 * is obviously values[0]. If not, indices are simply flipped. */

		is_flipped = (get_Proj_proj(jump_proj) != pn_Cond_false);
		ir_false   = values[is_flipped ? 1 : 0];
		ir_true    = values[is_flipped ? 0 : 1];

		/* Get the condition value. */
		cond = get_Proj_pred(jump_proj);
		sel  = get_Cond_selector(cond);

		mode = get_irn_mode(ir_false);
		assert((mode == get_irn_mode(ir_true)) && "Mode mismatch.");

		/* Create the gamma node. */
		return new_r_Gamma(ctx->cons_block, sel, ir_false, ir_true, mode);
	}
}

static ir_node *build_gammas(ir_node *start_block, ir_node *cons_block,
	int num_values, select_value *values)
{
	int        i, j, max_idx;
	ir_node   *result;
	ir_graph  *irg;
	bitset_t **closures;
	build_gammas_ctx ctx;

	assert(is_Block(start_block) && is_Block(cons_block));
	assert((num_values > 0) && values);

	/* Initialize bitsets for the transitive closure of each value. */
	irg      = get_irn_irg(cons_block);
	max_idx  = get_irg_last_idx(irg);
	closures = XMALLOCN(bitset_t*, num_values);

	for (i = 0; i < num_values; i++) {
		closures[i] = bitset_malloc(max_idx);

		/**
		 * Calculate the closure of the value by calculating the union of the
		 * individual source block closures. This is used later to determine
		 * which blocks can still reach the source blocks of any of the edges.
		 */
		assert(values[i].num_edges > 0);
		for (j = 0; j < values[i].num_edges; j++) {
			select_edge *edge = &values[i].edges[j];
			find_pred_closure(closures[i], edge->src, start_block);
		}
	}

	/* Store recursion-independ data in a structure to make things easier. */
	ctx.path       = bitset_malloc(max_idx);
	ctx.cons_block = cons_block;
	ctx.num_values = num_values;
	ctx.values     = values;
	ctx.closures   = closures;

	result = build_gammas_walk(start_block, &ctx);

	bitset_free(ctx.path);

	/* Free the transitive closures again. */
	for (i = 0; i < num_values; i++) {
		bitset_free(closures[i]);
	}

	xfree(closures);

	return result;
}

/******************************************************************************
 * Phase 1-2: Wrap and unwrap phi-selected values in tuples.                  *
 ******************************************************************************/

/**
 * Createss a tuple for one predecessors of given block list, that encapsulates
 * the values that all the phi nodes select for that predecessor. The values
 * inside the tuple are ordered like the linked list of phi nodes in the block.
 *
 * This makes it possible to handle all the phi nodes in the block at once, as
 * if there would only be one phi node with mode_T.
 */
static ir_node *get_incoming_tuple(ir_node *block, int pred_index)
{
	int       i;
	ir_node  *phi    = get_Block_phis(block);
	ir_node **values = NEW_ARR_F(ir_node*, 0);
	ir_node  *tuple;

	/* Collect the according preds of all phi nodes. */
	i = 0;
	while (phi != NULL) {
		ARR_APP1(ir_node*, values, get_Phi_pred(phi, pred_index));
		phi = get_Phi_next(phi);
		i++;
	}

	/* Create a tuple from the values and return it. */
	tuple = new_r_Tuple(block, ARR_LEN(values), values);
	DEL_ARR_F(values);

	return tuple;
}

/**
 * Used in conjunction with get_incoming_values. This will replace all phi
 * nodes in the block by projs, that select their respective value from the
 * given tuple.
 * This is used, once the gamma graph that selects the blocks phi nodes is
 * in place. The tuple returned by that graph is the tuple passed here.
 */
static void replace_phis_by_projs(ir_node *block, ir_node *tuple) {

	ir_node *phi, *next_phi;
	int i = 0;

	assert(is_Block(block) && (get_irn_mode(tuple) == mode_T));

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

/******************************************************************************
 * Phase 1-3: Replace phi nodes with gamma graphs or theta nodes.             *
 ******************************************************************************/

/**
 * This function is used to walk along the blocks and replace the phi nodes
 * by gamma graphs or theta nodes. This is used by replace_phis.
 */
static void replace_phis_walk(ir_node *block, void *ctx)
{
	int           i, num_preds, num_inner, num_outer;
	ir_node      *inner_value, *outer_value, *result, *start_block;
	ir_loop      *loop;
	select_value *values;

	(void)ctx;
	assert(is_Block(block));

	/* Skip blocks without phis. */
	if (get_Block_phis(block) == NULL) return;

	num_preds = get_Block_n_cfgpreds(block);
	loop      = get_irn_loop(block);

	/* Obtain a list of predecessors. Partition the list so that predecessors
	 * from inside the loop end up left in the list. All nodes are considered
	 * to be inside the loop if there is no loop at all. */
	values    = XMALLOCN(select_value, num_preds);
	num_inner = 0;
	num_outer = 0;

	for (i = 0; i < num_preds; i++) {
		int      index = 0;
		ir_node *pred  = get_nodes_block(get_Block_cfgpred(block, i));

		/* Add inner blocks on the left and outer nodes on the right. */
		if (!loop || is_in_loop(pred, loop)) {
			index = num_inner;
			num_inner++;
		} else {
			num_outer++;
			index = num_preds - num_outer;
		}

		/* For any incoming edge set a value to select. */
		values[index].value        = get_incoming_tuple(block, i);
		values[index].num_edges    = 1;
		values[index].edges        = XMALLOCN(select_edge, 1);
		values[index].edges[0].src = pred;
		values[index].edges[0].dst = block;
	}

	/* Select the inner value. This always works. */
	start_block = find_values_dom(num_inner, values);
	inner_value = build_gammas(start_block, block, num_inner, values);
	result = inner_value;

	/* For loops also get the outer value and build a theta. */
	if (num_outer > 0) {
		int depth = get_loop_depth(loop);

		start_block = find_values_dom(num_outer, values + num_inner);
		outer_value = build_gammas(
			start_block, block, num_outer, values + num_inner
		);

		result = new_r_Theta(block, outer_value, inner_value, mode_T, depth);
	}

	for (i = 0; i < num_preds; i++) {
		xfree(values[i].edges);
	}
	xfree(values);

	/* Construct projs on the result. */
	replace_phis_by_projs(block, result);
}

/**
 * Initiates the replacement of phi nodes by gamma and theta nodes in the
 * given graph. Note that after calling this function there will still be a
 * block structure and extract nodes, as well as their conditions are still
 * missing. They will be placed by the next phase.
 */
static void replace_phis(ir_graph *irg)
{
	ir_resources_t resources =
		IR_RESOURCE_IRN_LINK |
		IR_RESOURCE_PHI_LIST;

	assert(irg);

	/* Create lists of phi nodes in each block. */
	ir_reserve_resources(irg, resources);
	collect_phiprojs(irg);

	/* Walk along the graph and replace phi nodes by gammas. */
	irg_block_walk_graph(irg, NULL, replace_phis_walk, NULL);

	ir_free_resources(irg, resources);
}

/******************************************************************************
 * Phase 2: Insert extract nodes when accessing values in a loop.             *
 ******************************************************************************/

/**
 * Exit blocks of for a loop iteration. The loop header is a special kind of
 * "exit". It just exits the current iteration and begins the new one.
 */
typedef struct iter_exits {
	ir_loop     *root;
	ir_node     *header;
	int          num_back_edges;
	select_edge *back_edges;
	int          num_exit_edges;
	select_edge *exit_edges;
} iter_exits;

static void find_iter_exits_walk(ir_loop *loop, iter_exits *exits)
{
	int       i, j, is_header;
	ir_graph *irg;
	const ir_edge_t *edge;

	for (i = 0; i < get_loop_n_elements(loop); i++) {
		loop_element element = get_loop_element(loop, i);

		switch (*element.kind) {
		case k_ir_loop:
			/* Recurse into deeper loops. */
			find_iter_exits_walk(element.son, exits);
			break;

		case k_ir_node:
			irg = get_irn_irg(element.node);
			is_header = 0;

			/* Iterate in-edges. */
			for (j = 0; j < get_Block_n_cfgpreds(element.node); j++) {
				ir_node *pred = get_Block_cfgpred(element.node, j);
				pred = get_nodes_block(pred);

				/* Edge from the outside, must be the loop header. */
				if (!is_in_loop(pred, exits->root)) {
					is_header = 1;
					break;
				}
			}

			/* Add the back-edges. */
			if (is_header) {
				assert(!exits->header && "Multiple loop headers.");
				exits->header = element.node;

				for (j = 0; j < get_Block_n_cfgpreds(element.node); j++) {
					ir_node *pred = get_Block_cfgpred(element.node, j);
					pred = get_nodes_block(pred);

					if (is_in_loop(pred, exits->root)) {
						select_edge edge;
						edge.src = pred;
						edge.dst = element.node;
						ARR_APP1(select_edge, exits->back_edges, edge);
						exits->num_back_edges++;
					}
				}
			}

			/* Iterate out-edges. */
			foreach_block_succ(element.node, edge) {
				ir_node *src = get_edge_src_irn(edge);
				if (src == get_irg_end(irg)) continue; /* Skip keep-alives. */

				/* Edge from outside the loop. This is an exit. */
				if (!is_in_loop(src, exits->root)) {
					select_edge edge;
					edge.src = element.node;
					edge.dst = src;
					ARR_APP1(select_edge, exits->exit_edges, edge);
					exits->num_exit_edges++;
				}
			}

			break;

		default:
			assert(0 && "Unexpected element type.");
		}
	}
}

/**
 * Find all blocks that an iteration of the given loop can exit to.
 */
static iter_exits *find_iter_exits(ir_loop *loop)
{
	iter_exits *exits = XMALLOC(iter_exits);

	exits->root           = loop;
	exits->header         = NULL;
	exits->num_back_edges = 0;
	exits->back_edges     = NEW_ARR_F(select_edge, 0);
	exits->num_exit_edges = 0;
	exits->exit_edges     = NEW_ARR_F(select_edge, 0);

	find_iter_exits_walk(loop, exits);
	return exits;
}

static void free_iter_exits(iter_exits *exits)
{
	DEL_ARR_F(exits->back_edges);
	DEL_ARR_F(exits->exit_edges);
	xfree(exits);
}

/**
 * Creates a graph of gamma nodes that evaluates to true when one of the given
 * loops breaking conditions is true. In PEG semantics, the produced value is
 * part of loop and produces an infinite list of booleans.
 */
static ir_node *create_break_cond(ir_node *block, ir_loop *loop)
{
	select_value  values[2];
	iter_exits   *exits = find_iter_exits(loop);
	ir_node      *result;

	/* Select false for any backedge that enters the loop header. */
	values[0].value     = new_Const_long(mode_b, 0);
	values[0].num_edges = exits->num_back_edges;
	values[0].edges     = exits->back_edges;

	/* Select true for any edge that is leaving the loop. */
	values[1].value     = new_Const_long(mode_b, 1);
	values[1].num_edges = exits->num_exit_edges;
	values[1].edges     = exits->exit_edges;

	/* Create the gamma nodes for the selection. */
	result = build_gammas(exits->header, block, 2, values);
	free_iter_exits(exits);

	return result;
}

/**
 * This walker function walks through the graph and inserts extract nodes when
 * a node inside a loop is accessed from the outside.
 */
static void insert_extracts_walk(ir_node* node, void *ctx)
{
	int      i;
	ir_node *block;
	pmap    *break_conds = (pmap*)ctx;

	if (is_Block(node)) return;

	/* Ignore those keep-alive edges. */
	if (is_End(node)) return;

	block = get_nodes_block(node);

	for (i = 0; i < get_irn_arity(node); i++)
	{
		ir_node *in       = get_irn_n(node, i);
		ir_node *in_block = get_nodes_block(in);
		ir_loop *in_loop  = get_irn_loop(in_block);

		/* Shortcut. Can't be an extract. */
		if (in_loop == NULL)   continue;
		if (block == in_block) continue;

		/* Accessing a loop from the outside requires extract. */
		if (!is_in_loop(node, in_loop)) {
			ir_mode *mode    = get_irn_mode(in);
			ir_node *cond    = pmap_get(break_conds, in_loop);
			ir_node *extract;

			if (!cond) {
				cond = create_break_cond(in_block, in_loop);
				pmap_insert(break_conds, in_loop, cond);
			}

			extract = new_r_Extract(block, in, cond, mode);
			set_irn_n(node, i, extract);
		}
	}
}

/**
 * Initiates extract node creation in the graph. The block structure is still
 * important for this phase, to determine loop membership.
 */
static void insert_extracts(ir_graph* irg)
{
	pmap *break_conds;
	assert(irg);

	assure_cf_loop(irg);
	break_conds = pmap_create();

	irg_walk_graph(irg, NULL, insert_extracts_walk, break_conds);

	pmap_destroy(break_conds);
}

/******************************************************************************
 * Phase 3: Remove the graphs block structure.                                *
 ******************************************************************************/

/**
 * Moves all nodes except those in the start- and end-block to the target block.
 * The target block is given by the walkers context. See remove_blocks.
 */
static void remove_blocks_walk(ir_node *irn, void *ctx)
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

/**
 * Removes the graphs superfluous block structure, by creating a new block that
 * remains beside start and end block and then moving all nodes there.
 * Firm will remove the remaining blocks as they can't be reached from the end
 * block anymore.
 */
static void remove_blocks(ir_graph *irg)
{
	/* Create a single block to stuff everything else in. */
	ir_node *start = get_irg_start(irg);
	ir_node *exec  = new_r_Proj(start, mode_X, pn_Start_X_initial_exec);
	ir_node *block = new_r_Block(irg, 1, &exec);

	irg_walk_graph(irg, NULL, remove_blocks_walk, block);
}

/******************************************************************************
 * Phase 4: Unfold gamma and theta tuples.                                    *
 ******************************************************************************/

/**
 * Walks along gamma and theta nodes with mode_T and splits them up, so that
 * each value in the tuple gets its own copy of the gamma/theta graph.
 */
static ir_node *unfold_tuples_walk(ir_node *irn, int idx, ir_mode *mode)
{
	ir_node *block;
	assert((idx >= 0) && irn && mode);

	block = get_nodes_block(irn);

	if (is_Gamma(irn)) {
		/* Construct a new gamma for the value. */
		ir_node *cond     = get_Gamma_cond(irn);
		ir_node *ir_true  = unfold_tuples_walk(get_Gamma_true(irn),  idx, mode);
		ir_node *ir_false = unfold_tuples_walk(get_Gamma_false(irn), idx, mode);
		return new_r_Gamma(block, cond, ir_false, ir_true, mode);
	}
	else if (is_Theta(irn)) {
		/* Same thing for the theta. */
		int      depth = get_Theta_depth(irn);
		ir_node *init  = unfold_tuples_walk(get_Theta_init(irn), idx, mode);
		ir_node *next  = unfold_tuples_walk(get_Theta_next(irn), idx, mode);
		return new_r_Theta(block, init, next, mode, depth);
	}
	else if (is_Tuple(irn)) {
		/* Extract the appropriate value from the tuple. */
		ir_node *res = get_Tuple_pred(irn, idx);
		assert((mode == get_irn_mode(res)) && "Unexpected mode.");
		return res;
	}

	assert(0 && "Invalid tupelized gamma tree.");
}

/**
 * Walker function to search for gammas and theta nodes on tuples and splitting
 * them up into multiple graphs.
 */
static void unfold_tuples_find(ir_node *irn, void *ctx)
{
	ir_node *tuple, *value;
	ir_mode *value_mode;
	int      value_pn;

	assert(irn);
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

/**
 * Unfolds all gamma and theta nodes with mode_T to create graphs for all the
 * individual values stored inside those tuples.
 */
static void unfold_tuples(ir_graph *irg)
{
	assert(irg);
	irg_walk_graph(irg, NULL, unfold_tuples_find, NULL);
}

/******************************************************************************
 * Local optimizations.                                                       *
 ******************************************************************************/

static ir_node *equivalent_node_gamma(ir_node *gamma)
{
	ir_node *cond     = get_Gamma_cond(gamma);
	ir_node *ir_true  = get_Gamma_true(gamma);
	ir_node *ir_false = get_Gamma_false(gamma);

	if (get_irn_mode(gamma) != mode_b) return gamma;

	/* Gamma(cond, true, false) --> cond */
	if (is_Const(ir_true)  && is_Const_one(ir_true) &&
		is_Const(ir_false) && is_Const_null(ir_false)) {
		return get_Gamma_cond(gamma);
	}

	/* Gamma(Not(cond), a, b) --> Gamma(cond, b, a) */
	if (is_Not(cond)) {
		cond = get_Not_op(cond);
		set_Gamma_cond(gamma, cond);
		set_Gamma_false(gamma, ir_true);
		set_Gamma_true(gamma, ir_false);
		return gamma;
	}

	return gamma;
}

static ir_node *transform_node_gamma(ir_node *gamma)
{
	ir_node *ir_true  = get_Gamma_true(gamma);
	ir_node *ir_false = get_Gamma_false(gamma);

	if (get_irn_mode(gamma) != mode_b) return gamma;

	/* Gamma(cond, false, true) --> Not(cond) */
	if (is_Const(ir_true)  && is_Const_null(ir_true) &&
		is_Const(ir_false) && is_Const_one(ir_false)) {

		ir_node *block = get_nodes_block(gamma);
		return new_r_Not(block, get_Gamma_cond(gamma), mode_b);
	}

	return gamma;
}

/******************************************************************************
 * Public interface.                                                          *
 ******************************************************************************/

/**
 * TODO:
 * - Multiple loop entries
 * - Use an obstack for allocations
 * - What about keepalive edges?
 */

/**
 * Converts the given firm graph to the PEG representation.
 */
void cfg_to_peg(ir_graph *irg)
{
	/* Use automatic out edges. Makes things easier later. */
	int had_edges = edges_assure(irg);

	/* Register local optimizations. */
	op_Gamma->ops.equivalent_node = equivalent_node_gamma;
	op_Gamma->ops.transform_node  = transform_node_gamma;

	assert(irg);
	dump_ir_graph(irg, "cfg");

	/* Eliminate all switch nodes, we can't represent them in a PEG. */
	lower_switch(irg, 0);
	dump_ir_graph(irg, "switch");

	/* Eliminate multiple returns. The program is exactly one value in PEG. */
	normalize_one_return(irg);
	dump_ir_graph(irg, "return");

	/* We need to walk the CFG in reverse order and access dominators. */
	assure_doms(irg);
	assure_cf_loop(irg);

	/* Replace phi nodes by gamma trees selecting tuples. */
	replace_phis(irg);
	dump_ir_graph(irg, "gamma_theta");

	/* Add extract nodes on loop access. */
	insert_extracts(irg);
	dump_ir_graph(irg, "extracts");

	/* Remove the existing block structure. */
	remove_blocks(irg);

	ir_add_dump_flags(ir_dump_flag_hide_control_flow);
	dump_ir_graph(irg, "nocfg");

	/* Unfold tuples on gammas and thetas. */
	unfold_tuples(irg);
	dump_ir_graph(irg, "unfold");
	ir_remove_dump_flags(ir_dump_flag_hide_control_flow);

	/* Most data is probably inconsistent now. */
	set_irg_outs_inconsistent(irg);
	set_irg_doms_inconsistent(irg);
	set_irg_extblk_inconsistent(irg);
	set_irg_loopinfo_inconsistent(irg);

	if (!had_edges) edges_deactivate(irg);
}
