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

#include "constbits.h"
#include "iroptimize.h"
#include "iropt_t.h"
#include "irgopt.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "ircons.h"

#include "adt/pdeq.h"

#include "irflag_t.h"
#include "iredges_t.h"
#include "irtools.h"

/**
 * A wrapper around optimize_inplace_2() to be called from a walker.
 */
static void optimize_in_place_wrapper(ir_node *n, void *env)
{
	(void)env;
	ir_node *optimized = optimize_in_place_2(n);

	if (optimized != n)
		exchange(n, optimized);
}

void local_optimize_node(ir_node *n)
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
	local_optimize_node(get_irg_end(irg));
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

void optimize_graph_df(ir_graph *irg)
{
	pdeq *waitq = new_pdeq();

	if (get_opt_global_cse())
		set_irg_pinned(irg, op_pin_state_floats);

	/* enable unreachable code elimination,
	 * not that currently disabling algebraic simplifications disables all
	 * transform_node_XXX() functions and therefore unreachable code
	 * elimination. */
	if (get_opt_algebraic_simplification()) {
		assert(!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_OPTIMIZE_UNREACHABLE_CODE));
		add_irg_constraints(irg, IR_GRAPH_CONSTRAINT_OPTIMIZE_UNREACHABLE_CODE);
	}

	new_identities(irg);
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES
	                         | IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);

	constbits_analyze(irg);

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

	constbits_clear(irg);

	confirm_irg_properties(irg, IR_GRAPH_PROPERTY_ONE_RETURN
	                            | IR_GRAPH_PROPERTY_MANY_RETURNS
	                            | IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES);

	if (get_opt_algebraic_simplification()) {
		/* Unreachable code elimination was enabled. */
		clear_irg_constraints(irg, IR_GRAPH_CONSTRAINT_OPTIMIZE_UNREACHABLE_CODE);
		add_irg_properties(irg, IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE);
	}

	/* Finally kill BAD and doublets from the keep alives.
	 * Doing this AFTER edges where deactivated saves cycles */
	ir_node *end = get_irg_end(irg);
	remove_End_Bads_and_doublets(end);
}

void local_opts_const_code(void)
{
	ir_graph *irg = get_const_code_irg();
	/* Clean the value_table in irg for the CSE. */
	new_identities(irg);

	walk_const_code(firm_clear_link, optimize_in_place_wrapper, NULL);
}
