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
 * @brief    Optimizations for a whole ir graph, i.e., a procedure.
 * @author   Christian Schaefer, Goetz Lindenmaier, Sebastian Felis,
 *           Michael Beck
 * @version  $Id$
 */
#include "config.h"

#include <assert.h>

#include "irnode_t.h"
#include "irgraph_t.h"

#include "iroptimize.h"
#include "iropt_t.h"
#include "irgopt.h"
#include "irgmod.h"
#include "irgwalk.h"

#include "adt/pdeq.h"

#include "irpass_t.h"
#include "irflag_t.h"
#include "iredges_t.h"
#include "irtools.h"

/*------------------------------------------------------------------*/
/* apply optimizations of iropt to all nodes.                       */
/*------------------------------------------------------------------*/

/**
 * A wrapper around optimize_inplace_2() to be called from a walker.
 */
static void optimize_in_place_wrapper(ir_node *n, void *env)
{
	ir_node *optimized = optimize_in_place_2(n);
	(void) env;

	if (optimized != n) {
		exchange(n, optimized);
	}
}

/**
 * Do local optimizations for a node.
 *
 * @param n  the IR-node where to start. Typically the End node
 *           of a graph
 *
 * @note current_ir_graph must be set
 */
static inline void do_local_optimize(ir_node *n)
{
	ir_graph *irg = get_irn_irg(n);

	/* Handle graph state */
	assert(get_irg_phase_state(irg) != phase_building);

	if (get_opt_global_cse())
		set_irg_pinned(irg, op_pin_state_floats);
	set_irg_doms_inconsistent(irg);
	set_irg_loopinfo_inconsistent(irg);

	/* Clean the value_table in irg for the CSE. */
	new_identities(irg);

	/* walk over the graph */
	irg_walk(n, firm_clear_link, optimize_in_place_wrapper, NULL);
}

/* Applies local optimizations (see iropt.h) to all nodes reachable from node n */
void local_optimize_node(ir_node *n)
{
	ir_graph *rem = current_ir_graph;
	current_ir_graph = get_irn_irg(n);

	do_local_optimize(n);

	current_ir_graph = rem;
}

/**
 * Enqueue all users of a node to a wait queue.
 * Handles mode_T nodes.
 */
static void enqueue_users(ir_node *n, pdeq *waitq)
{
	const ir_edge_t *edge;

	foreach_out_edge(n, edge) {
		ir_node *succ = get_edge_src_irn(edge);

		if (get_irn_link(succ) != waitq) {
			pdeq_putr(waitq, succ);
			set_irn_link(succ, waitq);
		}
		if (get_irn_mode(succ) == mode_T) {
		/* A mode_T node has Proj's. Because most optimizations
			run on the Proj's we have to enqueue them also. */
			enqueue_users(succ, waitq);
		}
	}
}

/**
 * Block-Walker: uses dominance depth to mark dead blocks.
 */
static void kill_dead_blocks(ir_node *block, void *env)
{
	pdeq *waitq = (pdeq*) env;

	if (get_Block_dom_depth(block) < 0) {
		/*
		 * Note that the new dominance code correctly handles
		 * the End block, i.e. it is always reachable from Start
		 */
		ir_graph *irg = get_irn_irg(block);
		enqueue_users(block, waitq);
		exchange(block, new_r_Bad(irg, mode_BB));
	}
}

/* Applies local optimizations (see iropt.h) to all nodes reachable from node n. */
void local_optimize_graph(ir_graph *irg)
{
	ir_graph *rem = current_ir_graph;
	current_ir_graph = irg;

	do_local_optimize(get_irg_end(irg));

	current_ir_graph = rem;
}

/**
 * Data flow optimization walker.
 * Optimizes all nodes and enqueue its users
 * if done.
 */
static void opt_walker(ir_node *n, void *env)
{
	pdeq *waitq = (pdeq*)env;
	ir_node *optimized;

	optimized = optimize_in_place_2(n);
	set_irn_link(optimized, NULL);

	if (optimized != n) {
		enqueue_users(n, waitq);
		exchange(n, optimized);
	}
}

static void clear_block_phis(ir_node *node, void *env) {
	(void) env;
	if (is_Block(node)) {
		set_Block_phis(node, NULL);
	}
}

static void collect_block_phis(ir_node *node, void *env) {
	(void) env;
	if (is_Phi(node)) {
		add_Block_phi(get_nodes_block(node), node);
	}
}

static int count_non_bads(ir_node *node) {
	int arity = get_irn_arity(node);
	int count = 0;
	int i;
	for (i=0; i<arity; ++i) {
		if (!is_Bad(get_irn_n(node, i)))
			count++;
	}
	return count;
}

static void block_remove_bads(ir_node *block, int *changed) {
	int i, j;
	ir_node **new_in;
	const int max = get_irn_arity(block);
	const int new_max = count_non_bads(block);
	assert (max >= new_max);

	if (is_Bad(block) || max == new_max) return;

	new_in = ALLOCAN(ir_node*, new_max);
	*changed = 1;

	assert (get_Block_dom_depth(block) >= 0);

	/* 1. Create a new block without Bad inputs */
	j = 0;
	for (i = 0; i < max; ++i) {
		ir_node *block_pred = get_irn_n(block, i);
		if (!is_Bad(block_pred)) {
			new_in[j++] = block_pred;
		}
	}
	assert (j == new_max);

	/* If the end block is unreachable, it might have zero predecessors. */
	ir_node *end_block = get_irg_end_block(get_irn_irg(block));
	if (new_max == 0 && block == end_block) {
		set_irn_in(block, new_max, new_in);
		return;
	}

	ir_node *new_block =  new_r_Block(get_irn_irg(block), new_max, new_in);

	/* 2. Remove inputs on Phis, where the block input is Bad. */
	ir_node *phi = get_Block_phis(block);
	if (phi != NULL) {
		do {
			ir_node* next = get_Phi_next(phi);
			if (get_irn_arity(phi) != new_max) {
				j = 0;
				for (i = 0; i < max; ++i) {
					ir_node *block_pred = get_irn_n(block, i);

					if (!is_Bad(block_pred)) {
						ir_node *pred = get_irn_n(phi, i);
						new_in[j++] = pred;
					}
				}
				assert (j == new_max);

				ir_node *new_phi = new_r_Phi(new_block, new_max, new_in, get_irn_mode(phi));
				exchange(phi, new_phi);
			}
			phi = next;
		} while (phi != NULL);
	}

	exchange(block, new_block);
}

/* Remove Bad nodes from Phi and Block inputs.
 *
 * Precondition: No unreachable code.
 * Postcondition: No Bad nodes.
 */
static int remove_Bads(ir_graph *irg) {
	int changed = 0;
	/* build phi list per block */
	irg_walk_graph(irg, clear_block_phis, collect_block_phis, NULL);

	/* actually remove Bads */
	irg_block_walk_graph(irg, NULL, (void (*)(struct ir_node *, void *)) block_remove_bads, &changed);

	return changed;
}

/* Applies local optimizations to all nodes in the graph until fixpoint. */
int optimize_graph_df(ir_graph *irg)
{
	pdeq     *waitq = new_pdeq();
	ir_graph *rem = current_ir_graph;
	ir_node  *end;
	int      state, changed;

	current_ir_graph = irg;

	state = edges_assure(irg);

	/* Clean the value_table in irg for the CSE. */
	new_identities(irg);

	if (get_opt_global_cse()) {
		set_irg_pinned(irg, op_pin_state_floats);
	}

	/* The following enables unreachable code elimination (=Blocks may be
	 * Bad). */
	set_irg_state(irg, IR_GRAPH_STATE_BAD_BLOCK);

	/* invalidate info */
	set_irg_doms_inconsistent(irg);
	set_irg_loopinfo_inconsistent(irg);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);

	/* walk over the graph, but don't touch keep-alives */
	irg_walk_graph(irg, NULL, opt_walker, waitq);

	/* any optimized nodes are stored in the wait queue,
	 * so if it's not empty, the graph has been changed */
	changed = !pdeq_empty(waitq);

	do {
		/* finish the wait queue */
		while (! pdeq_empty(waitq)) {
			ir_node *n = (ir_node*)pdeq_getl(waitq);
			opt_walker(n, waitq);
		}
		/* kill newly generated unreachable code */
		compute_doms(irg);
		irg_block_walk_graph(irg, NULL, kill_dead_blocks, waitq);
	} while (! pdeq_empty(waitq));

	del_pdeq(waitq);

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	if (! state)
		edges_deactivate(irg);

	/* Finally kill BAD and doublets from the keep alives.
	   Doing this AFTER edges where deactivated saves cycles */
	end = get_irg_end(irg);
	remove_End_Bads_and_doublets(end);

	if (remove_Bads(irg)) {
		edges_deactivate(irg);
	}

	clear_irg_state(irg, IR_GRAPH_STATE_BAD_BLOCK);

	current_ir_graph = rem;
	return changed;
}

/* Creates an ir_graph pass for optimize_graph_df. */
ir_graph_pass_t *optimize_graph_df_pass(const char *name)
{
	return def_graph_pass_ret(name ? name : "optimize_graph_df", optimize_graph_df);
}  /* optimize_graph_df_pass */
