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
 * Walks down the control dependence graph and collects all reachable nodes
 * until the given idom node is reached. For each node that can be reached,
 * the bit corresponding to its index is set in the reach bitset.
 */
static void find_cdep_reach(bitset_t *reach, ir_node *block, ir_node *idom)
{
	ir_cdep *dep;

	assert(is_Block(block));

	bitset_set(reach, get_irn_idx(block));

	if (block == idom) return;
	dep = find_cdep(block);

	/* Recurse all unvisited blocks. */
	while (dep != NULL) {
		if (!bitset_is_set(reach, get_irn_idx(dep->node))) {
			find_cdep_reach(reach, dep->node, idom);
		}

		dep = dep->next;
	}
}

/**
 * Identifies the header node of the loop, which is where the loop is being
 * entered. If there are more than two loop headers, only one of them will be
 * returned. This should be normalized before.
 */
static ir_node *find_loop_header(ir_loop *loop)
{
	int i, j;

	/* This assumes, that for two nested loops the header node of the outer
	 * loop is not part of the inner loop. This shouldn't happen because the
	 * inner loop needs a backedge. Since the header node would be the first
	 * node along the inner loops path, the backedge has to go either there or
	 * to some node further down the path but before the branch that leads to
	 * the backedge.
	 * In the first case, there is just one loop whose body branches somewhere
	 * and eventually leads back to the header. In the second case, there is
	 * a nested loop that can't have the same header as the first one, because
	 * the backedge is behind the header. */

	for (i = 0; i < get_loop_n_nodes(loop); i++) {
		ir_node *node = get_loop_node(loop, i);

		/* Search for control flow from outside the loop. */
		for (j = 0; j < get_Block_n_cfgpreds(node); j++) {
			ir_node *pred = get_Block_cfgpred(node, j);
			pred = get_nodes_block(pred);

			/* TODO: Could be improved by either using a bitset to collect
			 * the nodes of the loop first or by augmenting the loop tree to
			 * provide loop membership lookup in O(1). */
			if (!is_in_loop(pred, loop)) {
				return node; /* Has to be the header. */
			}
		}
	}

	assert(0 && "No loop header found.");
}

/******************************************************************************
 * Phase 1-1: Create gamma graphs for value selection.                        *
 ******************************************************************************/

/**
 * Unfortunately select_value_walk requires quite a bit of context that doesn't
 * change and isn't tied to the recursion path. Passing this data as parameter
 * increases stack consumption and makes usage much more clumsy.
 */
typedef struct select_value_ctx {
	/* We can't use the visited flag here, because extract node construction
	 * requires select_value and happens in a walker function. */

	bitset_t  *visited;    /* The blocks we have already visited. */
	bitset_t  *reach;      /* Nodes that reach end blocks via revcdeps. */

	int        num_blocks; /* The number of end blocks. */
	ir_node  **blocks;     /* A list of end block we have values for. */
	ir_node  **values;     /* The values to select for each end block. */

	ir_node   *cons_block; /* The block where nodes shall be constructed. */
} select_value_ctx;

/**
 * This function is called by select_value and does the actual work, by
 * walking down the reverse control dependence graph and creating gamma nodes
 * on the way from the idom node to the predecessors.
 * See select_value for a real description, this is just the worker function.
 */
static ir_node *select_value_walk(ir_node *block, select_value_ctx *ctx)
{
	ir_cdep *dep;
	ir_node *deps[2];
	int      num_deps;
	int      val_index, i;

	assert(ctx->reach && ctx->visited);
	assert(is_Block(ctx->cons_block) && is_Block(block));
	assert((ctx->num_blocks > 0) && ctx->blocks && ctx->values);

	/* Find out if we have reached one of the end blocks. */
	val_index = -1;

	for (i = 0; i < ctx->num_blocks; i++) {
		if (block == ctx->blocks[i]) {
			val_index = i;
			break;
		}
	}

	/* Have we already visited this node? */
	if (bitset_is_set(ctx->visited, get_irn_idx(block))) {

		/* Under normal circumstances we can just ignore cycles along the path.
		 * Control flow may of course loop a few times, before it breaks out to
		 * reach one of the end blocks, but we don't have to consider that here.
		 * We only construct gamma nodes and going back to the path we already
		 * built gammas for will cause an infinite loop.
		 *
		 * The actual looping semantic is added later, when extract nodes for
		 * the branch conditions are constructed.
		 *
		 * However there is one exception: when building the break condition,
		 * the header node of the loop is both idom and an end node. We will
		 * later reach that node again and have to return its value. So if this
		 * is an end node, return its value. */

		if (val_index >= 0) {
			return ctx->values[val_index];
		} else {
			return
		}
	}

	bitset_set(ctx->visited, get_irn_idx(block));

	/* Get the reverse deps for the block. */
	dep = find_rev_cdep(block);

	/* Collect the dependees that can reach one of the blocks. */
	num_deps = 0;
	while (dep != NULL) {

		if (bitset_is_set(ctx->reach, get_irn_idx(dep->node))) {
			deps[num_deps] = dep->node;
			num_deps++;

			/* There are more than two deps, but only one branch. So one dep
			 * must dominate another. This violates the base assumption, that
			 * only one of the blocks is guaranteed to have been executed. */
			assert((num_deps <= 2) && "Base assumption violated.");
		}

		dep = dep->next;
	}

	assert(((val_index >= 0) || (num_deps > 0)) && "Nowhere to walk.");

	/* There are four situations that may happen. (1) This is a block we have a
	 * value for. (2) We have one dependee to recurse to. (3) This is a branch
	 * with two dependees to get the values from. (4) This is a branch with one
	 * dependee and we have a value for the block itself. */

	/* Case one. */
	if ((num_deps == 0) && (val_index >= 0)) {
		return ctx->values[val_index];
	}

	/* Case two. */
	if ((num_deps == 1) && (val_index < 0)) {
		return select_value_walk(deps[0], ctx);
	}

	/* Case three. */
	if ((num_deps == 2) && (val_index < 0)) {
		ir_mode *mode;
		ir_node *cond, *ir_true, *ir_false, *succ, *succ_proj;
		int      succ_req, succ_cur;

		/* Get the true and false values. They may still be flipped. */
		ir_true  = select_value_walk(deps[0], ctx);
		ir_false = select_value_walk(deps[1], ctx);

		/* Get the first successor and find out which branch it belongs to. */
		succ      = get_edge_src_irn(get_block_succ_first(block));
		succ_proj = get_pred_in_block(succ, block);

		/* Determine where the successor should lead to and where it really
		 * leads. Note that list[0] will postdominate the successor that
		 * depends on block but not block itself (and the other successor). */
		succ_req = (get_Proj_proj(succ_proj) == pn_Cond_true);
		succ_cur = block_postdominates(deps[0], succ);

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

		/* The condition might actually be nested inside a loop. In that case
		 * we would need an extract node to get it out. However we can just
		 * create a dependency to the condition here and since the gamma node
		 * is constructed in the end block, the extract insertion phase that
		 * is executed later will take care of this, too. */

		return new_r_Gamma(ctx->cons_block, cond, ir_false, ir_true, mode);
	}

	/* Case four is not handled yet. */

	assert(0 && "Unexpected control dependence graph.");
	return NULL;
}

/**
 * Given a set of blocks and a value for each block, builds a graph of gamma
 * nodes to select the value corresponding to the block that will be executed.
 * Speaking in CFG terms, the basic assumption is that exactly one of the given
 * blocks is guaranteed to be executed when the gamma graph is evaluated.
 *
 * If that assumtion doesn't hold, the returned gamma graph may either return
 * a value for a block that is actually never executed or it may fail with an
 * assertion (if one of the given blocks dominates another block).
 *
 * The simple example is the replacement of a phi node when control flow merges
 * after several branches. The resulting gamma graph will evaluate the branch
 * conditions and select the value of the old phi node.
 *
 * A more complex example are breaking conditions of loops. In that case, the
 * select_value function is used to determine if the loop header is executed
 * again or if a block behind one of the breaking edges will run. One of these
 * cases will inevitably happen in each iteration. Note that the result will
 * produce an infinite list with one selected value for each iteration.
 */
static ir_node *select_value(
	ir_node   *cons_block, /* The block to construct the gamma graph in. */
	int        num_blocks, /* The number of end blocks. */
	ir_node  **blocks,     /* The blocks we have values for. */
	ir_node  **values)     /* A value for each of the blocks. */
{
	int               i, last_idx;
	ir_node          *idom, *result;
	ir_graph         *irg;
	select_value_ctx  ctx;

	assert(is_Block(cons_block) && (num_blocks > 0) && blocks && values);

	/* One node shortcut. */
	if (num_blocks == 1) {
		return values[0];
	}

	/* Find the idom of the given blocks. This is where the first important
	 * decision takes place. So it is the starting point for construction. */
	idom = NULL;

	for (i = 0; i < num_blocks; i++) {
		assert(is_Block(blocks[i]));
		if (idom == NULL) idom = get_Block_idom(blocks[i]);
		else idom = node_smallest_common_dominator(idom, blocks[i]);
		assert(idom);
	}

	/* Discover all reachable deps of the given blocks. */
	irg         = get_irn_irg(cons_block);
	last_idx    = get_irg_last_idx(irg);
	ctx.reach   = bitset_malloc(last_idx);
	ctx.visited = bitset_malloc(last_idx);

	for (i = 0; i < num_blocks; i++) {
		find_cdep_reach(ctx.reach, blocks[i], idom);
	}

	/* Walk down the reachable deps from the idom and create gammas. */
	ctx.blocks     = blocks;
	ctx.values     = values;
	ctx.cons_block = cons_block;
	ctx.num_blocks = num_blocks;

	result = select_value_walk(idom, &ctx);

	bitset_free(ctx.reach);
	bitset_free(ctx.visited);

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
	int       i, num_preds, num_inner, num_outer;
	ir_node **blocks, **values;
	ir_node  *inner_value, *outer_value, *result;
	ir_loop  *loop;

	(void)ctx;
	assert(is_Block(block));

	/* Skip blocks without phis. */
	if (get_Block_phis(block) == NULL) return;

	num_preds = get_Block_n_cfgpreds(block);
	loop      = get_irn_loop(block);

	/* Obtain a list of predecessors. Partition the list so that predecessors
	 * from inside the loop end up left in the list. All nodes are considered
	 * to be inside the loop if there is no loop at all. */
	blocks    = XMALLOCN(ir_node*, num_preds);
	values    = XMALLOCN(ir_node*, num_preds);
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

		/* Store block and value in the same order. */
		blocks[index] = pred;
		values[index] = get_incoming_tuple(block, i);
	}

	/* Select the inner value. This always works. */
	inner_value = select_value(block, num_inner, blocks, values);
	result = inner_value;

	/* For loops also get the outer value and build a theta. */
	if (num_outer > 0) {
		outer_value = select_value(
			block, num_outer, blocks + num_inner, values + num_inner
		);

		result = new_r_Theta(block, outer_value, inner_value, mode_T);
	}

	xfree(values);
	xfree(blocks);

	/* Construct projs on the result. */
	replace_phis_by_projs(block, result);
}


static void print_cdeps(ir_node *irn, void* ctx)
{
	ir_cdep *dep = find_cdep(irn);
	while (dep)
	{
		printf("%li -> %li\n", get_irn_node_nr(irn), get_irn_node_nr(dep->node));

		dep = dep->next;
	}
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

	/* We need to walk the CFG in reverse order and access dominators. */
	assure_doms(irg);
	assure_cf_loop(irg);

	/* Create lists of phi nodes in each block. */
	ir_reserve_resources(irg, resources);
	collect_phiprojs(irg);

	/* Create the control dependence graph. */
	compute_cdep(irg);
	compute_rev_cdep(irg);

	irg_walk_graph(irg, NULL, print_cdeps, NULL);

	/* Walk along the graph and replace phi nodes by gammas. */
	irg_block_walk_graph(irg, NULL, replace_phis_walk, NULL);

	free_rev_cdep(irg);
	free_cdep(irg);
	ir_free_resources(irg, resources);
}

/******************************************************************************
 * Phase 2: Insert extract nodes when accessing values in a loop.             *
 ******************************************************************************/

/**
 * Creates a graph of gamma nodes that evaluates to true when one of the given
 * loops breaking conditions is true. In PEG semantics, the produced value is
 * part of loop and produces an infinite list of booleans.
 */
static ir_node *create_break_cond(ir_node *block, ir_loop *loop)
{
	ir_node *header;
	assert(is_Block(block) && loop);

	/* The break condition for an iteration can be found using select_value.
	 * For blocks at the end of edges that left the loop select true, because
	 * one of the breaking conditions had to be true then. For the loop header
	 * select false, because if it is reached again, another iteration will
	 * follow. The remaining question is: how does select_value cope with the
	 * loop on the header node (control dependant on itself) and what happens
	 * if the idom is one of the final nodes (the header). Will another idom
	 * be selected?
	 */

	/* Identify the loops header node. */
	header = find_loop_header(loop);

	/* Not supported yet. */
	(void)loop;
	return new_r_Unknown(get_irn_irg(block), mode_b);
}

/**
 * This walker function walks through the graph and inserts extract nodes when
 * a node inside a loop is accessed from the outside.
 */
static void insert_extracts_walk(ir_node* node, void *ctx)
{
	int      i;
	ir_node *block;
	(void)ctx;

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
			ir_node *cond    = create_break_cond(in_block, in_loop);
			ir_node *extract = new_r_Extract(block, in, cond, mode);
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
	assert(irg);

	assure_cf_loop(irg);
	irg_walk_graph(irg, NULL, insert_extracts_walk, NULL);
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
 * Public interface.                                                          *
 ******************************************************************************/

/**
 * TODO:
 * - Loop conditions
 * - Multiple loop entries
 * - Use an obstack for allocations
 * - What about keepalive edges?
 * - Generalize select_values
 */

/**
 * Converts the given firm graph to the PEG representation.
 */
void convert_to_peg(ir_graph *irg)
{
	/* Use automatic out edges. Makes things easier later. */
	int opt_level = get_optimize();
	int had_edges = edges_assure(irg);
	set_optimize(0);

	assert(irg);
	dump_ir_graph(irg, "cfg");

	/* Eliminate all switch nodes, we can't represent them in a PEG. */
	lower_switch(irg, 0);
	dump_ir_graph(irg, "switch");

	/* Eliminate multiple returns. The program is exactly one value in PEG. */
	normalize_one_return(irg);
	dump_ir_graph(irg, "return");

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

	/* Most data is probably inconsistent now. */
	set_irg_outs_inconsistent(irg);
	set_irg_doms_inconsistent(irg);
	set_irg_extblk_inconsistent(irg);
	set_irg_loopinfo_inconsistent(irg);

	optimize_cf(irg);
	set_optimize(opt_level);
	if (!had_edges) edges_deactivate(irg);
}
