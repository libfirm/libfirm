/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Move nodes to a block where they will be executed the least
 *           often.
 * @author   Christian Schaefer, Goetz Lindenmaier, Sebastian Felis,
 *           Michael Beck
 *
 * The idea here is to push nodes as deep into the dominance tree as their
 * dependencies allow. After pushing them back up out of as many loops as
 * possible.
 */
#include "iredges_t.h"
#include "irgopt.h"
#include "irnode_t.h"
#include "iroptimize.h"
#include "pdeq.h"
#include <stdbool.h>

#ifndef NDEBUG
static bool is_block_reachable(ir_node *block)
{
	return get_Block_dom_depth(block) >= 0;
}
#endif

/**
 * Find the earliest correct block for node n.  --- Place n into the
 * same Block as its dominance-deepest Input.
 *
 * move_out_of_loops() expects that place_floats_early() have placed
 * all "living" nodes into a living block. That is why we must
 * move nodes in dead block with "live" successors into a valid
 * block.
 * We move them just into the same block as its successor (or
 * in case of a Phi into the effective use block). For Phi successors,
 * this may still be a dead block, but then there is no real use, as
 * the control flow will be dead later.
 */
static void place_floats_early(ir_node *n, deq_t *worklist)
{
	/* we must not run into an infinite loop */
	if (irn_visited_else_mark(n))
		return;

	/* The algorithm relies on the fact that all predecessors of a block are
	 * moved up after a call to place_float_early of the predecessors
	 * (see the loop below).
	 * However we have to break cycles somewhere. Relying on the visited flag
	 * will result in nodes not being moved up despite their place_floats_early
	 * call.
	 * Instead we break cycles at pinned nodes which will not move anyway:
	 * This works because in firm each cycle contains a Phi or Block node
	 * (which are pinned)
	 */
	if (get_irn_pinned(n)) {
		/* we cannot move pinned nodes */
		foreach_irn_in(n, i, pred) {
			deq_push_pointer_right(worklist, pred);
		}
		if (!is_Block(n))
			deq_push_pointer_right(worklist, get_nodes_block(n));
		return;
	}

	ir_node *block = get_nodes_block(n);

	/* first move predecessors up */
	place_floats_early(block, worklist);
	foreach_irn_in(n, i, pred) {
		place_floats_early(pred, worklist);
	}

	/* determine earliest point */
	ir_node *new_block = NULL;
	int      new_depth = 0;
	foreach_irn_in(n, i, pred) {
		ir_node *pred_block = get_nodes_block(pred);
		int      pred_depth = get_Block_dom_depth(pred_block);
		if (pred_depth > new_depth) {
			new_depth = pred_depth;
			new_block = pred_block;
		}
	}

	/* Set the new block */
	if (new_block != NULL)
		set_nodes_block(n, new_block);
}

/**
 * Floating nodes form subgraphs that begin at nodes as Const, Load,
 * Start, Call and that end at pinned nodes as Store, Call.
 * Place_early places all floating nodes reachable from its argument through
 * floating nodes and adds all beginnings at pinned nodes to the worklist.
 *
 * @param worklist   a worklist, used for the algorithm, empty on in/output
 */
static void place_early(ir_graph *irg, deq_t *worklist)
{
	assert(worklist);
	inc_irg_visited(irg);

	/* this initializes the worklist */
	place_floats_early(get_irg_end(irg), worklist);

	/* Work the content of the worklist. */
	while (!deq_empty(worklist)) {
		ir_node *n = deq_pop_pointer_left(ir_node, worklist);
		if (!irn_visited(n))
			place_floats_early(n, worklist);
	}
	set_irg_pinned(irg, op_pin_state_pinned);
}

/**
 * Wrapper around ir_deepest_common_dominator() that allows the @p dca to be
 * NULL as a special case.
 */
static ir_node *calc_dom_dca(ir_node *dca, ir_node *block)
{
	assert(block != NULL);
	if (dca == NULL || dca == block)
		return block;

	return ir_deepest_common_dominator(dca, block);
}

/**
 * Deepest common dominance ancestor of DCA and CONSUMER of PRODUCER.
 * I.e., DCA is the block where we might place PRODUCER.
 * A data flow edge points from producer to consumer.
 */
static ir_node *consumer_dom_dca(ir_node *dca, ir_node *consumer,
                                 ir_node *producer)
{
	/* Compute the last block into which we can place a node so that it is
	   before consumer. */
	if (is_Phi(consumer)) {
		/* our consumer is a Phi-node, the effective use is in all those
		   blocks through which the Phi-node reaches producer */
		ir_node *phi_block = get_nodes_block(consumer);
		int      arity     = get_irn_arity(consumer);

		for (int i = 0;  i < arity; i++) {
			if (get_Phi_pred(consumer, i) == producer) {
				ir_node *new_block = get_Block_cfgpred_block(phi_block, i);
				if (new_block == NULL)
					continue;

				assert(is_block_reachable(new_block));
				dca = calc_dom_dca(dca, new_block);
			}
		}
	} else {
		dca = calc_dom_dca(dca, get_nodes_block(consumer));
	}
	return dca;
}

static inline int get_block_loop_depth(ir_node *block)
{
	return get_loop_depth(get_irn_loop(block));
}

/**
 * Move n to a block with less loop depth than its current block. The
 * new block must be dominated by early.
 *
 * @param n      the node that should be moved
 * @param early  the earliest block we can n move to
 */
static void move_out_of_loops(ir_node *n, ir_node *early)
{
	ir_node *block      = get_nodes_block(n);
	ir_node *best       = block;
	int      best_depth = get_block_loop_depth(best);

	/* Find the region deepest in the dominator tree dominating
	   dca with the least loop nesting depth, but still dominated
	   by our early placement. */
	while (block != early) {
		ir_node *idom       = get_Block_idom(block);
		int      idom_depth = get_block_loop_depth(idom);
		if (idom_depth < best_depth) {
			best       = idom;
			best_depth = idom_depth;
		}
		block = idom;
	}
	if (best != get_nodes_block(n))
		set_nodes_block(n, best);
}

/**
 * Calculate the deepest common ancestor in the dominator tree of all nodes'
 * blocks depending on node; our final placement has to dominate DCA.
 *
 * @param node  the definition node
 * @param dca   the deepest common ancestor block so far, initially
 *              NULL
 *
 * @return the deepest common dominator ancestor of all blocks of node's users
 */
static ir_node *get_deepest_common_dom_ancestor(ir_node *node, ir_node *dca)
{
	foreach_out_edge(node, edge) {
		ir_node *succ = get_edge_src_irn(edge);

		/* keepalive edges are special and do not respect the dominance */
		if (is_End(succ))
			continue;

		if (is_Proj(succ)) {
			/* Proj nodes are in the same block as node, so
			 * the users of Proj are our users. */
			dca = get_deepest_common_dom_ancestor(succ, dca);
		} else {
			assert(is_block_reachable(get_nodes_block(succ)));
			dca = consumer_dom_dca(dca, succ, node);
		}
	}

	return dca;
}

/**
 * Put all the Proj nodes of a node into a given block.
 *
 * @param node   the mode_T node
 * @param block  the block to put the Proj nodes to
 */
static void set_projs_block(ir_node *node, ir_node *block)
{
	foreach_out_edge(node, edge) {
		ir_node *succ = get_edge_src_irn(edge);

		if (!is_Proj(succ))
			continue;

		set_nodes_block(succ, block);
		if (get_irn_mode(succ) == mode_T) {
			set_projs_block(succ, block);
		}
	}
}

/**
 * Find the latest legal block for N and place N into the
 * `optimal' Block between the latest and earliest legal block.
 * The `optimal' block is the dominance-deepest block of those
 * with the least loop-nesting-depth.  This places N out of as many
 * loops as possible and then makes it as control dependent as
 * possible.
 */
static void place_floats_late(ir_node *n, deq_t *worklist)
{
	if (irn_visited_else_mark(n))
		return;

	/* break cycles at pinned nodes (see place place_floats_early) as to why */
	if (get_irn_pinned(n)) {
		foreach_out_edge(n, edge) {
			ir_node *succ = get_edge_src_irn(edge);
			deq_push_pointer_right(worklist, succ);
		}
		return;
	}

	/* place our users */
	foreach_out_edge(n, edge) {
		ir_node *succ = get_edge_src_irn(edge);
		place_floats_late(succ, worklist);
	}

	/* no point in moving Projs around, they are moved with their predecessor */
	if (is_Proj(n))
		return;
	/* some nodes should simply stay in the start block */
	if (is_irn_start_block_placed(n)) {
		assert(get_nodes_block(n) == get_irg_start_block(get_irn_irg(n)));
		return;
	}

	ir_node *block = get_nodes_block(n);
	assert(is_block_reachable(block));

	/* deepest common ancestor in the dominator tree of all nodes'
	   blocks depending on us; our final placement has to dominate
	   DCA. */
	ir_node *dca = get_deepest_common_dom_ancestor(n, NULL);
	/* this node had no user? This can happen if a node is only kept alive,
	 * do nothing in this case. */
	if (dca == NULL)
		return;
	set_nodes_block(n, dca);
	move_out_of_loops(n, block);
	if (get_irn_mode(n) == mode_T)
		set_projs_block(n, get_nodes_block(n));
}

/**
 * Place floating nodes on the given worklist as late as possible using
 * the dominance tree.
 *
 * @param worklist   the worklist containing the nodes to place
 */
static void place_late(ir_graph *irg, deq_t *worklist)
{
	assert(worklist);
	inc_irg_visited(irg);

	/* This fills the worklist initially. */
	place_floats_late(get_irg_start_block(irg), worklist);

	/* And now empty the worklist again... */
	while (!deq_empty(worklist)) {
		ir_node *n = deq_pop_pointer_left(ir_node, worklist);
		if (!irn_visited(n))
			place_floats_late(n, worklist);
	}
}

/* Code Placement. */
void place_code(ir_graph *irg)
{
	/* Handle graph state */
	assure_irg_properties(irg,
		IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES |
		IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE |
		IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES |
		IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE |
		IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO);

	/* Place all floating nodes as early as possible. This guarantees
	 a legal code placement. */
	deq_t worklist;
	deq_init(&worklist);
	place_early(irg, &worklist);

	/* While GCSE might place nodes in unreachable blocks,
	 * these are now placed in reachable blocks. */

	/* Note: place_early changes only blocks, no data edges. So, the
	 * data out edges are still valid, no need to recalculate them here. */

	/* Now move the nodes down in the dominator tree. This reduces the
	   unnecessary executions of the node. */
	place_late(irg, &worklist);

	deq_free(&worklist);
	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_CONTROL_FLOW);
}
