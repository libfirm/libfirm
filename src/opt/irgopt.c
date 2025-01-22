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
#include "irgopt.h"

#include "constbits.h"
#include "ircons.h"
#include "iredges_t.h"
#include "irflag_t.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "iroptimize.h"
#include "irtools.h"
#include "pdeq.h"
#include <assert.h>

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
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	irg_walk(n, firm_clear_link, optimize_in_place_wrapper, NULL);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
}

static void enqueue_node(ir_node *node, deq_t *waitq)
{
	if (get_irn_link(node) == waitq)
		return;
	deq_push_pointer_right(waitq, node);
	set_irn_link(node, waitq);
}

static void enqueue_node_init(ir_node *node, void *env)
{
	deq_t *waitq = (deq_t *)env;
	deq_push_pointer_right(waitq, node);
	set_irn_link(node, waitq);
}

/**
 * Enqueue all users of a node to a wait queue.
 * Handles mode_T nodes.
 */
static void enqueue_users(ir_node *n, deq_t *waitq)
{
	foreach_out_edge(n, edge) {
		ir_node *succ = get_edge_src_irn(edge);

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
	if (get_Block_dom_depth(block) >= 0)
		return;

	deq_t *waitq = (deq_t *)env;
	foreach_block_succ(block, edge) {
		ir_node *succ_block = get_edge_src_irn(edge);
		enqueue_node(succ_block, waitq);
		foreach_out_edge(succ_block, edge2) {
			ir_node *succ = get_edge_src_irn(edge2);
			if (is_Phi(succ))
				enqueue_node(succ, waitq);
		}
	}

	ir_graph *irg = get_irn_irg(block);
	ir_node *end = get_irg_end(irg);
	enqueue_node(end, waitq);
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
static void opt_walker(ir_node *n, deq_t *waitq)
{
	/* If CSE occurs during the optimization,
	 * our operands have fewer users than before.
	 * Thus, we may be able to apply a rule that
	 * requires an operand with only one user.
	 * Hence, we need a loop to reach the fixpoint. */
	ir_node *optimized = n;
	ir_node *last;
	do {
		last      = optimized;
		optimized = optimize_in_place_2(last);

		if (optimized != last) {
			enqueue_users(last, waitq);
			exchange(last, optimized);
		}
	} while (optimized != last);
}

void optimize_graph_df(ir_graph *irg)
{
	ir_graph_properties_t props = IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES;
	if (get_opt_global_cse()) {
		set_irg_pinned(irg, op_pin_state_floats);
		props |= IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE | IR_GRAPH_PROPERTY_CONSISTENT_POSTDOMINANCE;
	}

	/* enable unreachable code elimination,
	 * not that currently disabling algebraic simplifications disables all
	 * transform_node_XXX() functions and therefore unreachable code
	 * elimination. */
	if (get_opt_algebraic_simplification()) {
		assert(!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_OPTIMIZE_UNREACHABLE_CODE));
		add_irg_constraints(irg, IR_GRAPH_CONSTRAINT_OPTIMIZE_UNREACHABLE_CODE);
	}

	new_identities(irg);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);

	constbits_analyze(irg);

	deq_t waitq;
	deq_init(&waitq);
	irg_walk_graph(irg, NULL, enqueue_node_init, &waitq);

	/* any optimized nodes are stored in the wait queue,
	 * so if it's not empty, the graph has been changed */
	while (!deq_empty(&waitq)) {
		assure_irg_properties(irg, props);

		/* finish the wait queue */
		while (!deq_empty(&waitq)) {
			ir_node *n = deq_pop_pointer_left(ir_node, &waitq);
			set_irn_link(n, NULL);
			opt_walker(n, &waitq);
		}
		if (irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_OPTIMIZE_UNREACHABLE_CODE)) {
			/* Calculate dominance so we can kill unreachable code
			 * We want this intertwined with localopts for better optimization
			 * (phase coupling) */
			compute_doms(irg);
			assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);
			irg_block_walk_graph(irg, NULL, find_unreachable_blocks, &waitq);
		}
	}
	deq_free(&waitq);
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

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	walk_const_code(firm_clear_link, optimize_in_place_wrapper, NULL);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
}
