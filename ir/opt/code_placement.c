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
 * @brief    Code Placement.  Pins all floating nodes to a block where they
 *           will be executed only if needed.
 * @author   Christian Schaefer, Goetz Lindenmaier, Sebastian Felis,
 *           Michael Beck
 * @version  $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "adt/pdeq.h"
#include "irnode_t.h"
#include "irouts.h"
#include "irgopt.h"

/**
 * Returns non-zero, is a block is not reachable from Start.
 *
 * @param block  the block to test
 */
static int
is_Block_unreachable(ir_node *block) {
	return is_Block_dead(block) || get_Block_dom_depth(block) < 0;
}

/**
 * Find the earliest correct block for node n.  --- Place n into the
 * same Block as its dominance-deepest Input.
 *
 * We have to avoid calls to get_nodes_block() here
 * because the graph is floating.
 *
 * move_out_of_loops() expects that place_floats_early() have placed
 * all "living" nodes into a living block. That's why we must
 * move nodes in dead block with "live" successors into a valid
 * block.
 * We move them just into the same block as it's successor (or
 * in case of a Phi into the effective use block). For Phi successors,
 * this may still be a dead block, but then there is no real use, as
 * the control flow will be dead later.
 *
 * @param n         the node to be placed
 * @param worklist  a worklist, predecessors of non-floating nodes are placed here
 */
static void
place_floats_early(ir_node *n, waitq *worklist) {
	int i, irn_arity;

	/* we must not run into an infinite loop */
	assert(irn_not_visited(n));
	mark_irn_visited(n);

	/* Place floating nodes. */
	if (get_irn_pinned(n) == op_pin_state_floats) {
		ir_node *curr_block = get_nodes_block(n);
		int in_dead_block   = is_Block_unreachable(curr_block);
		int depth           = 0;
		ir_node *b          = NULL;   /* The block to place this node in */

		assert(is_no_Block(n));

		if (is_irn_start_block_placed(n)) {
			/* These nodes will not be placed by the loop below. */
			b = get_irg_start_block(current_ir_graph);
			depth = 1;
		}

		/* find the block for this node. */
		irn_arity = get_irn_arity(n);
		for (i = 0; i < irn_arity; i++) {
			ir_node *pred = get_irn_n(n, i);
			ir_node *pred_block;

			if ((irn_not_visited(pred))
			    && (get_irn_pinned(pred) == op_pin_state_floats)) {

				/*
				 * If the current node is NOT in a dead block, but one of its
				 * predecessors is, we must move the predecessor to a live block.
				 * Such thing can happen, if global CSE chose a node from a dead block.
				 * We move it simply to our block.
				 * Note that neither Phi nor End nodes are floating, so we don't
				 * need to handle them here.
				 */
				if (! in_dead_block) {
					if (get_irn_pinned(pred) == op_pin_state_floats &&
						is_Block_unreachable(get_nodes_block(pred)))
						set_nodes_block(pred, curr_block);
				}
				place_floats_early(pred, worklist);
			}

			/*
			 * A node in the Bad block must stay in the bad block,
			 * so don't compute a new block for it.
			 */
			if (in_dead_block)
				continue;

			/* Because all loops contain at least one op_pin_state_pinned node, now all
			   our inputs are either op_pin_state_pinned or place_early() has already
			   been finished on them.  We do not have any unfinished inputs!  */
			pred_block = get_nodes_block(pred);
			if ((!is_Block_dead(pred_block)) &&
				(get_Block_dom_depth(pred_block) > depth)) {
				b = pred_block;
				depth = get_Block_dom_depth(pred_block);
			}
			/* Avoid that the node is placed in the Start block */
			if (depth == 1 &&
					get_Block_dom_depth(get_nodes_block(n)) > 1 &&
					get_irg_phase_state(current_ir_graph) != phase_backend) {
				b = get_Block_cfg_out(get_irg_start_block(current_ir_graph), 0);
				assert(b != get_irg_start_block(current_ir_graph));
				depth = 2;
			}
		}
		if (b)
			set_nodes_block(n, b);
	}

	/*
	 * Add predecessors of non floating nodes and non-floating predecessors
	 * of floating nodes to worklist and fix their blocks if the are in dead block.
	 */
	irn_arity = get_irn_arity(n);

	if (is_End(n)) {
		/*
		 * Simplest case: End node. Predecessors are keep-alives,
		 * no need to move out of dead block.
		 */
		for (i = -1; i < irn_arity; ++i) {
			ir_node *pred = get_irn_n(n, i);
			if (irn_not_visited(pred))
				waitq_put(worklist, pred);
		}
	} else if (is_Block(n)) {
		/*
		 * Blocks: Predecessors are control flow, no need to move
		 * them out of dead block.
		 */
		for (i = irn_arity - 1; i >= 0; --i) {
			ir_node *pred = get_irn_n(n, i);
			if (irn_not_visited(pred))
				waitq_put(worklist, pred);
		}
	} else if (is_Phi(n)) {
		ir_node *pred;
		ir_node *curr_block = get_nodes_block(n);
		int in_dead_block   = is_Block_unreachable(curr_block);

		/*
		 * Phi nodes: move nodes from dead blocks into the effective use
		 * of the Phi-input if the Phi is not in a bad block.
		 */
		pred = get_nodes_block(n);
		if (irn_not_visited(pred))
			waitq_put(worklist, pred);

		for (i = irn_arity - 1; i >= 0; --i) {
			ir_node *pred = get_irn_n(n, i);

			if (irn_not_visited(pred)) {
				if (! in_dead_block &&
					get_irn_pinned(pred) == op_pin_state_floats &&
					is_Block_unreachable(get_nodes_block(pred))) {
					set_nodes_block(pred, get_Block_cfgpred_block(curr_block, i));
				}
				waitq_put(worklist, pred);
			}
		}
	} else {
		ir_node *pred;
		ir_node *curr_block = get_nodes_block(n);
		int in_dead_block   = is_Block_unreachable(curr_block);

		/*
		 * All other nodes: move nodes from dead blocks into the same block.
		 */
		pred = get_nodes_block(n);
		if (irn_not_visited(pred))
			waitq_put(worklist, pred);

		for (i = irn_arity - 1; i >= 0; --i) {
			ir_node *pred = get_irn_n(n, i);

			if (irn_not_visited(pred)) {
				if (! in_dead_block &&
					get_irn_pinned(pred) == op_pin_state_floats &&
					is_Block_unreachable(get_nodes_block(pred))) {
					set_nodes_block(pred, curr_block);
				}
				waitq_put(worklist, pred);
			}
		}
	}
}

/**
 * Floating nodes form subgraphs that begin at nodes as Const, Load,
 * Start, Call and that end at op_pin_state_pinned nodes as Store, Call.  Place_early
 * places all floating nodes reachable from its argument through floating
 * nodes and adds all beginnings at op_pin_state_pinned nodes to the worklist.
 *
 * @param worklist   a worklist, used for the algorithm, empty on in/output
 */
static void place_early(waitq *worklist) {
	assert(worklist);
	inc_irg_visited(current_ir_graph);

	/* this inits the worklist */
	place_floats_early(get_irg_end(current_ir_graph), worklist);

	/* Work the content of the worklist. */
	while (!waitq_empty(worklist)) {
		ir_node *n = waitq_get(worklist);
		if (irn_not_visited(n))
			place_floats_early(n, worklist);
	}

	set_irg_outs_inconsistent(current_ir_graph);
	set_irg_pinned(current_ir_graph, op_pin_state_pinned);
}

/**
 * Compute the deepest common ancestor of block and dca.
 */
static ir_node *calc_dca(ir_node *dca, ir_node *block) {
	assert(block);

	/* we do not want to place nodes in dead blocks */
	if (is_Block_dead(block))
		return dca;

	/* We found a first legal placement. */
	if (!dca) return block;

	/* Find a placement that is dominates both, dca and block. */
	while (get_Block_dom_depth(block) > get_Block_dom_depth(dca))
		block = get_Block_idom(block);

	while (get_Block_dom_depth(dca) > get_Block_dom_depth(block)) {
		dca = get_Block_idom(dca);
	}

	while (block != dca) {
		block = get_Block_idom(block); dca = get_Block_idom(dca);
	}

	return dca;
}

/** Deepest common dominance ancestor of DCA and CONSUMER of PRODUCER.
 * I.e., DCA is the block where we might place PRODUCER.
 * A data flow edge points from producer to consumer.
 */
static ir_node *consumer_dom_dca(ir_node *dca, ir_node *consumer, ir_node *producer)
{
	/* Compute the last block into which we can place a node so that it is
	   before consumer. */
	if (is_Phi(consumer)) {
		/* our consumer is a Phi-node, the effective use is in all those
		   blocks through which the Phi-node reaches producer */
		ir_node *phi_block = get_nodes_block(consumer);
		int      arity     = get_irn_arity(consumer);
		int      i;

		for (i = 0;  i < arity; i++) {
			if (get_Phi_pred(consumer, i) == producer) {
				ir_node *new_block = get_Block_cfgpred_block(phi_block, i);

				if (!is_Block_unreachable(new_block))
					dca = calc_dca(dca, new_block);
			}
		}
	} else {
		dca = calc_dca(dca, get_nodes_block(consumer));
	}

	return dca;
}

/* FIXME: the name clashes here with the function from ana/field_temperature.c
 * please rename. */
static INLINE int get_irn_loop_depth(ir_node *n) {
	return get_loop_depth(get_irn_loop(n));
}

/**
 * Move n to a block with less loop depth than it's current block. The
 * new block must be dominated by early.
 *
 * @param n      the node that should be moved
 * @param early  the earliest block we can n move to
 */
static void move_out_of_loops(ir_node *n, ir_node *early) {
	ir_node *best, *dca;
	assert(n && early);


	/* Find the region deepest in the dominator tree dominating
	   dca with the least loop nesting depth, but still dominated
	   by our early placement. */
	dca = get_nodes_block(n);

	best = dca;
	while (dca != early) {
		dca = get_Block_idom(dca);
		if (!dca || is_Bad(dca)) break; /* may be Bad if not reachable from Start */
		if (get_irn_loop_depth(dca) < get_irn_loop_depth(best)) {
			best = dca;
		}
	}
	if (best != get_nodes_block(n)) {
		/* debug output
		printf("Moving out of loop: "); DDMN(n);
		printf(" Outermost block: "); DDMN(early);
		printf(" Best block: "); DDMN(best);
		printf(" Innermost block: "); DDMN(get_nodes_block(n));
		*/
		set_nodes_block(n, best);
	}
}

/* deepest common ancestor in the dominator tree of all nodes'
   blocks depending on us; our final placement has to dominate DCA. */
static ir_node *get_deepest_common_ancestor(ir_node *node, ir_node *dca)
{
	int i;

	for (i = get_irn_n_outs(node) - 1; i >= 0; --i) {
		ir_node *succ = get_irn_out(node, i);

		if (is_End(succ)) {
			/*
			 * This consumer is the End node, a keep alive edge.
			 * This is not a real consumer, so we ignore it
			 */
			continue;
		}

		if (is_Proj(succ)) {
			dca = get_deepest_common_ancestor(succ, dca);
		} else {
			/* ignore if succ is in dead code */
			ir_node *succ_blk = get_nodes_block(succ);
			if (is_Block_unreachable(succ_blk))
				continue;
			dca = consumer_dom_dca(dca, succ, node);
		}
	}

	return dca;
}

static void set_projs_block(ir_node *node, ir_node *block)
{
	int i;

	for (i = get_irn_n_outs(node) - 1; i >= 0; --i) {
		ir_node *succ = get_irn_out(node, i);

		assert(is_Proj(succ));

		if(get_irn_mode(succ) == mode_T) {
			set_projs_block(succ, block);
		}
		set_nodes_block(succ, block);
	}
}

/**
 * Find the latest legal block for N and place N into the
 * `optimal' Block between the latest and earliest legal block.
 * The `optimal' block is the dominance-deepest block of those
 * with the least loop-nesting-depth.  This places N out of as many
 * loops as possible and then makes it as control dependent as
 * possible.
 *
 * @param n         the node to be placed
 * @param worklist  a worklist, all successors of non-floating nodes are
 *                  placed here
 */
static void place_floats_late(ir_node *n, pdeq *worklist) {
	int i, n_outs;
	ir_node *early_blk;

	assert(irn_not_visited(n)); /* no multiple placement */

	mark_irn_visited(n);

	/* no need to place block nodes, control nodes are already placed. */
	if (!is_Block(n) &&
	    (!is_cfop(n)) &&
	    (get_irn_mode(n) != mode_X)) {
		/* Remember the early_blk placement of this block to move it
		   out of loop no further than the early_blk placement. */
		early_blk = get_nodes_block(n);

		/*
		 * BEWARE: Here we also get code, that is live, but
		 * was in a dead block.  If the node is life, but because
		 * of CSE in a dead block, we still might need it.
		 */

		/* Assure that our users are all placed, except the Phi-nodes.
		--- Each data flow cycle contains at least one Phi-node.  We
		    have to break the `user has to be placed before the
		    producer' dependence cycle and the Phi-nodes are the
		    place to do so, because we need to base our placement on the
		    final region of our users, which is OK with Phi-nodes, as they
		    are op_pin_state_pinned, and they never have to be placed after a
		    producer of one of their inputs in the same block anyway. */
		for (i = get_irn_n_outs(n) - 1; i >= 0; --i) {
			ir_node *succ = get_irn_out(n, i);
			if (irn_not_visited(succ) && !is_Phi(succ))
				place_floats_late(succ, worklist);
		}

		if (! is_Block_dead(early_blk)) {
			/* do only move things that where not dead */
			ir_op *op = get_irn_op(n);

			/* We have to determine the final block of this node... except for
			   constants and Projs */
			if ((get_irn_pinned(n) == op_pin_state_floats) &&
			    (op != op_Const)    &&
			    (op != op_SymConst) &&
			    (op != op_Proj))
			{
				/* deepest common ancestor in the dominator tree of all nodes'
				   blocks depending on us; our final placement has to dominate
				   DCA. */
				ir_node *dca = get_deepest_common_ancestor(n, NULL);
				if (dca != NULL) {
					set_nodes_block(n, dca);
					move_out_of_loops(n, early_blk);
					if (get_irn_mode(n) == mode_T) {
						set_projs_block(n, get_nodes_block(n));
					}
				}
			}
		}
	}

	/* Add successors of all non-floating nodes on list. (Those of floating
	   nodes are placed already and therefore are marked.)  */
	n_outs = get_irn_n_outs(n);
	for (i = 0; i < n_outs; i++) {
		ir_node *succ = get_irn_out(n, i);
		if (irn_not_visited(get_irn_out(n, i))) {
			pdeq_putr(worklist, succ);
		}
	}
}

/**
 * Place floating nodes on the given worklist as late as possible using
 * the dominance tree.
 *
 * @param worklist   the worklist containing the nodes to place
 */
static void place_late(waitq *worklist) {
	assert(worklist);
	inc_irg_visited(current_ir_graph);

	/* This fills the worklist initially. */
	place_floats_late(get_irg_start_block(current_ir_graph), worklist);

	/* And now empty the worklist again... */
	while (!waitq_empty(worklist)) {
		ir_node *n = waitq_get(worklist);
		if (irn_not_visited(n))
			place_floats_late(n, worklist);
	}
}

/* Code Placement. */
void place_code(ir_graph *irg) {
	waitq *worklist;
	ir_graph *rem = current_ir_graph;

	current_ir_graph = irg;

	/* Handle graph state */
	assert(get_irg_phase_state(irg) != phase_building);
	assure_doms(irg);

	if (1 || get_irg_loopinfo_state(irg) != loopinfo_consistent) {
		free_loop_information(irg);
		construct_cf_backedges(irg);
	}

	/* Place all floating nodes as early as possible. This guarantees
	 a legal code placement. */
	worklist = new_waitq();
	place_early(worklist);

	/* place_early() invalidates the outs, place_late needs them. */
	compute_irg_outs(irg);

	/* Now move the nodes down in the dominator tree. This reduces the
	   unnecessary executions of the node. */
	place_late(worklist);

	set_irg_outs_inconsistent(current_ir_graph);
	set_irg_loopinfo_inconsistent(current_ir_graph);
	del_waitq(worklist);
	current_ir_graph = rem;
}
