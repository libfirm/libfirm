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
 * Walks down the control dependence graph and collects all reachable nodes
 * until the given end node is reached. For each node that can be reached,
 * the bit corresponding to its index is set in the reach bitset.
 */
static void find_cdep_reach(bitset_t *reach, ir_node *block, ir_node *end)
{
	ir_cdep *dep;

	assert(is_Block(block));

	bitset_set(reach, get_irn_idx(block));

	if (block == end) return;
	dep = find_cdep(block);

	/* Recurse all unvisited blocks. */
	while (dep != NULL) {
		if (!bitset_is_set(reach, get_irn_idx(dep->node))) {
			find_cdep_reach(reach, dep->node, end);
		}

		dep = dep->next;
	}
}

/******************************************************************************
 * Phase 1-1: Create gamma graphs for value selection.                        *
 ******************************************************************************/

/**
 * Values can be assigned to nodes only, not to edges. That is because we walk
 * along control dependencies and don't consider the control flow edges at all.
 * However in some cases we want to associate a value with certain edges of a
 * node only.
 *
 * To calculate breaking conditions of a loop for example, we start at the loop
 * header and go down the rev cdeps until we find either an exit node or reach
 * the header again. So we basically start at a block we have a value for, but
 * we only want to return that value, if we reach that block via a backedge.
 * This is exactly what value_type_backedge is for.
 */

typedef enum block_value_type {
	value_type_normal,
	value_type_backedge
} block_value_type;

typedef struct block_value {
	ir_node          *block;
	ir_node          *value;
	block_value_type  type;
} block_value;

/**
 * Unfortunately select_value_walk requires quite a bit of context that doesn't
 * change and isn't tied to the recursion path. Passing this data as parameter
 * increases stack consumption and makes usage much more clumsy.
 */
typedef struct select_value_ctx {
	/* We can't use the visited flag here, because extract node construction
	 * requires select_value and happens in a walker function. */

	bitset_t    *visited;    /* The blocks we have already visited. */
	bitset_t    *reach;      /* Nodes that reach end blocks via revcdeps. */

	int          num_values; /* The number of values to select from. */
	block_value *values;     /* A list of value for block associations. */

	ir_node     *cons_block; /* The block where nodes shall be constructed. */
} select_value_ctx;

/**
 * This function is called by select_value and does the actual work, by
 * walking down the reverse control dependence graph and creating gamma nodes
 * on the way from the idom node to the end blocks.
 *
 * If ctx->be_value is NULL and a backedge is found, the execution will back-
 * track to the last branch and throw the path and branch away. If both paths
 * of a branch are NULL, execution will backtrack even further.
 *
 * See select_value for a real description, this is just the worker function.
 */
static ir_node *select_value_walk(ir_node *block, select_value_ctx *ctx)
{
	ir_cdep     *dep;
	ir_node     *deps[2];
	ir_node     *dep_values[2];
	int          num_dep_values;
	int          i;
	block_value *block_value;

	assert(ctx->reach && ctx->visited);
	assert(is_Block(ctx->cons_block) && is_Block(block));
	assert((ctx->num_values > 0) && ctx->values);

	/* Find out if we have reached one of the value blocks. */
	block_value = NULL;

	for (i = 0; i < ctx->num_values; i++) {
		if (block == ctx->values[i].block) {
			block_value = &ctx->values[i];
			break;
		}
	}

	/* Find out if this is a backedge and return be_value then. */
	if (bitset_is_set(ctx->visited, get_irn_idx(block))) {
		/* If the block uses backedge retrieval, return the value. */
		if (block_value && (block_value->type == value_type_backedge)) {
			return block_value->value;
		} else {
			return NULL; /* Backtrack otherwise. */
		}
	}

	bitset_set(ctx->visited, get_irn_idx(block));

	/* Backedge values are ignored beyond this point. */
	if (block_value && (block_value->type == value_type_backedge)) {
		block_value = NULL;
	}

	/* Get the reverse deps for the block. */
	dep = find_rev_cdep(block);

	/* Collect values for all dependees. Some may backtrack and return NULL,
	 * in which case they are not considered any further. */
	num_dep_values = 0;
	while (dep != NULL) {

		if (bitset_is_set(ctx->reach, get_irn_idx(dep->node))) {
			ir_node *dep_value;

			/* Note that a dep might return NULL in case of loops. */
			dep_value = select_value_walk(dep->node, ctx);

			if (dep_value) {
				deps[num_dep_values]       = dep->node;
				dep_values[num_dep_values] = dep_value;
				num_dep_values++;
			}

			/* There are more than two deps, but only one branch. So one dep
			 * must dominate another. This violates the base assumption, that
			 * only one of the blocks is guaranteed to have been executed. */
			assert((num_dep_values <= 2) && "Base assumption violated.");
		}

		dep = dep->next;
	}

	assert((block_value || (num_dep_values > 0)) && "Nowhere to walk.");

	/**
	 * There are five situations that may happen:
	 *
	 *  (1) We have a value for the block and no other value.
	 *  (2) We have a value for the block and a value for one successor.
	 *  (3) We have no value, but we got one from a successor.
	 *  (4) We have no value, but we got two from the successors.
	 *  (5) We have no value at all.
	 */

	/* (1) Simply return the value we have. */
	if ((num_dep_values == 0) && block_value) {
		assert(block_value->type == value_type_normal);
		return block_value->value;
	}

	/* (2) One end block leads to another (or even itself). Create a gamma
	 *     node to select which value to return. */
	if ((num_dep_values == 1) && block_value) {
		assert(0); /* TODO */
		return NULL;
	}

	/* (3) Simply return the value we got. */
	if ((num_dep_values == 1) && !block_value) {
		return dep_values[0];
	}

	/* (4) The classical branch. Build a gamma node. */
	if ((num_dep_values == 2) && !block_value) {
		ir_mode *mode;
		ir_node *cond, *ir_true, *ir_false, *succ, *succ_proj;
		int      succ_req, succ_cur;

		/* Get the true and false values. They may still be flipped. */
		ir_true  = dep_values[0];
		ir_false = dep_values[1];

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

	/* (5) Backtrack. */
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
 * In some cases the control flow will branch back to the discovered path. This
 * happens when loop nodes are involved. For usual value selection, this can be
 * ignored (by passing NULL), because the gamma structure is static and the
 * decision whether to do another iteration is handled by the extract nodes.
 *
 * However this is also used to calculate the break condition for the extract
 * nodes. In that case, following a backedge means the breaking condition for
 * that iteration yields false. Decisions leading to a backedge are relevant
 * here and the false value can be given as the be_value parameter. */

static ir_node *select_value(
	ir_node     *cons_block, /* The block to construct the gamma graph in. */
	int          num_values, /* The number of values to select from. */
	block_value *values)     /* A list of value for block associations. */
{
	int               i, last_idx;
	ir_node          *result, *dom;
	ir_graph         *irg;
	select_value_ctx  ctx;

	assert(is_Block(cons_block) && (num_values > 0) && values);

	/* One node shortcut. */
	if (num_values == 1) {
		assert(values[0].type == value_type_normal);
		return values[0].value;
	}

	/* Find the clostes common dominator of the given blocks. This is where
	 * the first important decision takes place. So it is the starting
	 * point for construction. */

	dom = values[0].block; /* A block dominates itself. */

	for (i = 1; i < num_values; i++) {
		assert(is_Block(values[i].block));
		dom = node_smallest_common_dominator(dom, values[i].block);
	}

	assert(dom);

	/* Discover all reachable deps of the given blocks. */
	irg         = get_irn_irg(cons_block);
	last_idx    = get_irg_last_idx(irg);
	ctx.reach   = bitset_malloc(last_idx);
	ctx.visited = bitset_malloc(last_idx);

	for (i = 0; i < num_values; i++) {
		find_cdep_reach(ctx.reach, values[i].block, dom);
	}

	/* Walk down the reachable deps from the idom and create gammas. */
	ctx.values     = values;
	ctx.cons_block = cons_block;
	ctx.num_values = num_values;

	result = select_value_walk(dom, &ctx);
	assert(result && "Couldn't select a value.");

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
	int          i, num_preds, num_inner, num_outer;
	ir_node     *inner_value, *outer_value, *result;
	ir_loop     *loop;
	block_value *values;

	(void)ctx;
	assert(is_Block(block));

	/* Skip blocks without phis. */
	if (get_Block_phis(block) == NULL) return;

	num_preds = get_Block_n_cfgpreds(block);
	loop      = get_irn_loop(block);

	/* Obtain a list of predecessors. Partition the list so that predecessors
	 * from inside the loop end up left in the list. All nodes are considered
	 * to be inside the loop if there is no loop at all. */
	values    = XMALLOCN(block_value, num_preds);
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
		values[index].block = pred;
		values[index].value = get_incoming_tuple(block, i);
		values[index].type  = value_type_normal;
	}

	/* Select the inner value. This always works. */
	inner_value = select_value(block, num_inner, values);
	result = inner_value;

	/* For loops also get the outer value and build a theta. */
	if (num_outer > 0) {
		outer_value = select_value(block, num_outer, values + num_inner);
		result = new_r_Theta(block, outer_value, inner_value, mode_T);
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
typedef struct iter_exits
{
	ir_loop  *root;
	ir_node  *entry;
	ir_node **blocks;
	int       num_blocks;
} iter_exits;

static void find_iter_exits_walk(ir_loop *loop, iter_exits *exits)
{
	int       i, j;
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

			/* Iterate in-edges. */
			for (j = 0; j < get_Block_n_cfgpreds(element.node); j++) {
				ir_node *pred = get_Block_cfgpred(element.node, j);
				pred = get_nodes_block(pred);

				/* Edge from the outside, must be the loop entry. */
				if (!is_in_loop(pred, exits->root)) {
					assert(!exits->entry && "Multiple loop entries.");
					exits->entry = element.node;
				}
			}

			/* Iterate out-edges. */
			foreach_block_succ(element.node, edge) {
				ir_node *src = get_edge_src_irn(edge);

				/* Skip keep-alive edges. */
				if (src != get_irg_end(irg)) {

					/* Edge from outside the loop. This is an exit. */
					if (!is_in_loop(src, exits->root)) {
						ARR_APP1(ir_node*, exits->blocks, src);
						exits->num_blocks++;
					}
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

	exits->root       = loop;
	exits->entry      = NULL;
	exits->num_blocks = 0;
	exits->blocks      = NEW_ARR_F(ir_node*, 0);

	find_iter_exits_walk(loop, exits);
	return exits;
}

static void free_iter_exits(iter_exits *exits)
{
	DEL_ARR_F(exits->blocks);
	xfree(exits);
}

/**
 * Creates a graph of gamma nodes that evaluates to true when one of the given
 * loops breaking conditions is true. In PEG semantics, the produced value is
 * part of loop and produces an infinite list of booleans.
 */
static ir_node *create_break_cond(ir_node *block, ir_loop *loop)
{
	int          i;
	iter_exits  *exits    = find_iter_exits(loop);
	block_value *values   = XMALLOCN(block_value, exits->num_blocks + 1);
	ir_node     *ir_true  = new_Const_long(mode_b, 1);
	ir_node     *ir_false = new_Const_long(mode_b, 0);
	ir_node     *result;

	/* When entering the entry block via a backedge, select false. */
	values[0].block = exits->entry;
	values[0].value = ir_false;
	values[0].type  = value_type_backedge;

	/* Select true when entering any exit block. */
	for (i = 0; i < exits->num_blocks; i++) {
		values[i + 1].block = exits->blocks[i];
		values[i + 1].value = ir_true;
		values[i + 1].type  = value_type_normal;
	}

	/* Select one of the values. */
	result = select_value(block, exits->num_blocks + 1, values);

	/* TODO: dominator stimmt noch nicht. */
	xfree(values);
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

	/* We need to walk the CFG in reverse order and access dominators. */
	assure_doms(irg);
	assure_cf_loop(irg);

	/* Create the control dependence graph. */
	compute_cdep(irg);
	compute_rev_cdep(irg);

	/* Replace phi nodes by gamma trees selecting tuples. */
	replace_phis(irg);
	dump_ir_graph(irg, "gamma_theta");

	/* Add extract nodes on loop access. */
	insert_extracts(irg);
	dump_ir_graph(irg, "extracts");

	free_rev_cdep(irg);
	free_cdep(irg);

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
