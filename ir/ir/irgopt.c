/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Optimizations for a whole ir graph, i.e., a procedure.
 * @author   Christian Schaefer, Goetz Lindenmaier, Sebastian Felis,
 *           Michael Beck
 */
#include <assert.h>

#include "irnode_t.h"
#include "irgraph_t.h"

#include "iroptimize.h"
#include "iropt_t.h"
#include "irgopt.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "ircons.h"

#include "adt/pdeq.h"

#include "irpass_t.h"
#include "irflag_t.h"
#include "iredges_t.h"
#include "irtools.h"

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

	if (get_opt_global_cse())
		set_irg_pinned(irg, op_pin_state_floats);
	clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);

	/* Clean the value_table in irg for the CSE. */
	new_identities(irg);

	/* walk over the graph */
	irg_walk(n, firm_clear_link, optimize_in_place_wrapper, NULL);
}

void local_optimize_node(ir_node *n)
{
	ir_graph *rem = current_ir_graph;
	current_ir_graph = get_irn_irg(n);

	do_local_optimize(n);

	current_ir_graph = rem;
}

static void enqueue_node(ir_node *node, pdeq *waitq)
{
	if (get_irn_link(node) == waitq)
		return;
	pdeq_putr(waitq, node);
	set_irn_link(node, waitq);
}

/**
 * Enqueue all users of a node to a wait queue.
 * Handles mode_T nodes.
 */
static void enqueue_users(ir_node *n, pdeq *waitq)
{
	foreach_out_edge(n, edge) {
		ir_node *succ  = get_edge_src_irn(edge);

		enqueue_node(succ, waitq);

		/* Also enqueue Phis to prevent inconsistencies. */
		if (is_Block(succ)) {
			foreach_out_edge(succ, edge2) {
				ir_node *succ2 = get_edge_src_irn(edge2);

				if (is_Phi(succ2)) {
					enqueue_node(succ2, waitq);
				}
			}
		} else if (get_irn_mode(succ) == mode_T) {
		/* A mode_T node has Proj's. Because most optimizations
			run on the Proj's we have to enqueue them also. */
			enqueue_users(succ, waitq);
		}
	}
}

/**
 * Block-Walker: uses dominance depth to mark dead blocks.
 */
static void find_unreachable_blocks(ir_node *block, void *env)
{
	pdeq *waitq = (pdeq*) env;

	if (get_Block_dom_depth(block) < 0) {
		ir_graph *irg = get_irn_irg(block);
		ir_node  *end = get_irg_end(irg);

		foreach_block_succ(block, edge) {
			ir_node *succ_block = get_edge_src_irn(edge);
			enqueue_node(succ_block, waitq);
			foreach_out_edge(succ_block, edge2) {
				ir_node *succ = get_edge_src_irn(edge2);
				if (is_Phi(succ))
					enqueue_node(succ, waitq);
			}
		}
		enqueue_node(end, waitq);
	}
}

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

int optimize_graph_df(ir_graph *irg)
{
	pdeq     *waitq = new_pdeq();
	ir_graph *rem = current_ir_graph;
	ir_node  *end;

	current_ir_graph = irg;

	if (get_opt_global_cse())
		set_irg_pinned(irg, op_pin_state_floats);

	/* enable unreachable code elimination */
	assert(!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_OPTIMIZE_UNREACHABLE_CODE));
	add_irg_constraints(irg, IR_GRAPH_CONSTRAINT_OPTIMIZE_UNREACHABLE_CODE);

	new_identities(irg);
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES
	                         | IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	irg_walk_graph(irg, NULL, opt_walker, waitq);

	/* any optimized nodes are stored in the wait queue,
	 * so if it's not empty, the graph has been changed */
	while (!pdeq_empty(waitq)) {
		/* finish the wait queue */
		while (! pdeq_empty(waitq)) {
			ir_node *n = (ir_node*)pdeq_getl(waitq);
			opt_walker(n, waitq);
		}
		/* Calculate dominance so we can kill unreachable code
		 * We want this intertwined with localopts for better optimization
		 * (phase coupling) */
		compute_doms(irg);
		irg_block_walk_graph(irg, NULL, find_unreachable_blocks, waitq);
	}
	del_pdeq(waitq);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	/* disable unreachable code elimination */
	clear_irg_constraints(irg, IR_GRAPH_CONSTRAINT_OPTIMIZE_UNREACHABLE_CODE);
	add_irg_properties(irg, IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE);

	/* invalidate infos */
	clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);
	clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO);
	edges_deactivate(irg);

	/* Finally kill BAD and doublets from the keep alives.
	 * Doing this AFTER edges where deactivated saves cycles */
	end = get_irg_end(irg);
	remove_End_Bads_and_doublets(end);

	current_ir_graph = rem;

	/* Note we do not have a reliable way to detect changes, since some
	 * localopt rules change the inputs of a node and do not return a new
	 * node, so we conservatively say true here */
	return true;
}

void local_opts_const_code(void)
{
	ir_graph *irg = get_const_code_irg();
	/* Clean the value_table in irg for the CSE. */
	new_identities(irg);

	walk_const_code(firm_clear_link, optimize_in_place_wrapper, NULL);
}

ir_graph_pass_t *optimize_graph_df_pass(const char *name)
{
	return def_graph_pass_ret(name ? name : "optimize_graph_df", optimize_graph_df);
}
