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
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <assert.h>

#include "irnode_t.h"
#include "irgraph_t.h"

#include "iroptimize.h"
#include "iropt_t.h"
#include "irgopt.h"
#include "irgmod.h"
#include "irgwalk.h"

#include "adt/pdeq.h"

#include "irflag_t.h"
#include "iredges_t.h"
#include "irtools.h"

/*------------------------------------------------------------------*/
/* apply optimizations of iropt to all nodes.                       */
/*------------------------------------------------------------------*/

/**
 * A wrapper around optimize_inplace_2() to be called from a walker.
 */
static void optimize_in_place_wrapper (ir_node *n, void *env) {
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
static INLINE void do_local_optimize(ir_node *n) {
	/* Handle graph state */
	assert(get_irg_phase_state(current_ir_graph) != phase_building);

	if (get_opt_global_cse())
		set_irg_pinned(current_ir_graph, op_pin_state_floats);
	set_irg_outs_inconsistent(current_ir_graph);
	set_irg_doms_inconsistent(current_ir_graph);
	set_irg_loopinfo_inconsistent(current_ir_graph);

	/* Clean the value_table in irg for the CSE. */
	del_identities(current_ir_graph->value_table);
	current_ir_graph->value_table = new_identities();

	/* walk over the graph */
	irg_walk(n, firm_clear_link, optimize_in_place_wrapper, NULL);
}

/* Applies local optimizations (see iropt.h) to all nodes reachable from node n */
void local_optimize_node(ir_node *n) {
	ir_graph *rem = current_ir_graph;
	current_ir_graph = get_irn_irg(n);

	do_local_optimize(n);

	current_ir_graph = rem;
}

/**
 * Block-Walker: uses dominance depth to mark dead blocks.
 */
static void kill_dead_blocks(ir_node *block, void *env) {
	(void) env;

	if (get_Block_dom_depth(block) < 0) {
		/*
		 * Note that the new dominance code correctly handles
		 * the End block, i.e. it is always reachable from Start
		 */
		set_Block_dead(block);
	}
}

/* Applies local optimizations (see iropt.h) to all nodes reachable from node n. */
void local_optimize_graph(ir_graph *irg) {
	ir_graph *rem = current_ir_graph;
	current_ir_graph = irg;

	if (get_irg_dom_state(irg) == dom_consistent)
		irg_block_walk_graph(irg, NULL, kill_dead_blocks, NULL);

	do_local_optimize(get_irg_end(irg));

	current_ir_graph = rem;
}

/**
 * Enqueue all users of a node to a wait queue.
 * Handles mode_T nodes.
 */
static void enqueue_users(ir_node *n, pdeq *waitq) {
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
 * Data flow optimization walker.
 * Optimizes all nodes and enqueue it's users
 * if done.
 */
static void opt_walker(ir_node *n, void *env) {
	pdeq *waitq = env;
	ir_node *optimized;

	optimized = optimize_in_place_2(n);
	set_irn_link(optimized, NULL);

	if (optimized != n) {
		enqueue_users(n, waitq);
		exchange(n, optimized);
	}
}

/* Applies local optimizations to all nodes in the graph until fixpoint. */
int optimize_graph_df(ir_graph *irg) {
	pdeq     *waitq = new_pdeq();
	ir_graph *rem = current_ir_graph;
	ir_node  *end;
	int      i, state, n_ka, changed;

	current_ir_graph = irg;

	state = edges_assure(irg);

	if (get_opt_global_cse())
		set_irg_pinned(current_ir_graph, op_pin_state_floats);

	/* Clean the value_table in irg for the CSE. */
	del_identities(irg->value_table);
	irg->value_table = new_identities();

	if (get_irg_dom_state(irg) == dom_consistent)
		irg_block_walk_graph(irg, NULL, kill_dead_blocks, NULL);

	/* invalidate info */
	set_irg_outs_inconsistent(irg);
	set_irg_doms_inconsistent(irg);
	set_irg_loopinfo_inconsistent(irg);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);

	end  = get_irg_end(irg);
	n_ka = get_End_n_keepalives(end);

	/* walk over the graph, but don't touch keep-alives */
	irg_walk(get_irg_end_block(irg), NULL, opt_walker, waitq);

	/*
	 * Optimize keep-alives by removing superfluous ones.
	 * Beware: the last transformation might add new keep-alives
	 * that keep blocks that are where visited! So, check only the
	 * "old" keep-alives, not the new ones!
	 *
	 * FIXME: it might be better to completely remove this
	 * optimization here ...
	 */
	for (i = n_ka - 1; i >= 0; --i) {
		ir_node *ka = get_End_keepalive(end, i);

		if (irn_visited(ka) && !is_irn_keep(ka)) {
			/* this node can be regularly visited, no need to keep it */
			set_End_keepalive(end, i, get_irg_bad(irg));
		}
	}
	/* now walk again and visit all not yet visited nodes */
	set_irg_visited(current_ir_graph, get_irg_visited(irg) - 1);
	irg_walk(get_irg_end(irg), NULL, opt_walker, waitq);

	/* any optimized nodes are stored in the wait queue,
	 * so if it's not empty, the graph has been changed */
	changed = !pdeq_empty(waitq);

	/* finish the wait queue */
	while (! pdeq_empty(waitq)) {
		ir_node *n = pdeq_getl(waitq);
		if (! is_Bad(n))
			opt_walker(n, waitq);
	}

	del_pdeq(waitq);

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	if (! state)
		edges_deactivate(irg);

	current_ir_graph = rem;
	return changed;
}
