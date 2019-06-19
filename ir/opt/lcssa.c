/*
 * This file is part of libFirm.
 * Copyright (C) 2018 Karlsruhe Institute of Technology
 */

/**
 * @file
 * @brief   loop-closed SSA transformation
 * @author  Elias Aebi
 */
#include "lcssa_t.h"
#include "irtools.h"
#include "xmalloc.h"
#include "debug.h"
#include <stdbool.h>
#include <assert.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/* Returns true iff inner is nested inside outer.
 * Also returns true if inner and outer are the same loop. */
static bool is_loop_nested_inside(ir_loop const *inner, ir_loop const *const outer)
{
	unsigned inner_depth = get_loop_depth(inner);
	unsigned outer_depth = get_loop_depth(outer);
	if (outer_depth > inner_depth) {
		return false;
	}

	for (unsigned i = inner_depth; i > outer_depth; i--) {
		inner = get_loop_outer_loop(inner);
	}

	return inner == outer;
}

/* Returns true if control flow has to exit a loop on the path from
 * "from_block" to "to_block". */
static bool cf_has_loop_exit(ir_node const *const from_block, ir_node const *const to_block)
{
	DB((dbg, LEVEL_2, "Checking whether %+F --> %+F exits a loop: ", from_block, to_block));

	ir_loop const *const from_loop = get_irn_loop(from_block);
	if (!from_loop) {
		DB((dbg, LEVEL_2, "no from_loop -> false\n"));
		return false;
	}
	ir_loop const *to_loop = get_irn_loop(to_block);
	if (!to_loop) {
		// from_block is in a loop (otherwise early return), but to_block is not
		// ==> Have to leave a loop somewhere
		DB((dbg, LEVEL_2, "no to_loop -> true\n"));
		return true;
	}

	unsigned const from_depth = get_loop_depth(from_loop);
	unsigned const to_depth   = get_loop_depth(to_loop);

	if (from_depth == 0) {
		DB((dbg, LEVEL_2, "from_loop depth 0 -> false\n"));
		return false;
	}
	if (from_depth > to_depth) {
		// Have to leave deeper nest at some point
		DB((dbg, LEVEL_2, "from_loop deeper than to_loop -> true\n"));
		return true;
	}
	assert(from_depth > 0 && to_depth > 0);

	// to_block is deeper in the loop or equally deep.
	// Check if to_block's loop is an inner loop (or the same) of from_block's loop.
	bool nested = is_loop_nested_inside(to_loop, from_loop);

	// If loops are not equal, we have to leave from_loop before entering to_loop.
	DB((dbg, LEVEL_2, "checking nesting -> %s\n", !nested ? "true" : "false"));
	return !nested;
}

/* Returns true if control flow from pred_block to block is an exit of any loop containing target. */
static bool is_nested_loop_exit(ir_node const *const block, ir_node const *const pred_block, ir_node const *const target)
{
	ir_loop const *const block_loop = get_irn_loop(block);
	ir_loop const *const pred_loop = get_irn_loop(pred_block);

	if (!block_loop || !pred_loop || block_loop == pred_loop) {
		return false;
	}

	ir_node const *const target_block = get_nodes_block(target);
	ir_loop const *const target_loop = get_irn_loop(target_block);
	assert(target_loop && get_loop_depth(target_loop) > 0);

	unsigned const block_depth = get_loop_depth(block_loop);
	unsigned const pred_depth  = get_loop_depth(pred_loop);

	return is_loop_nested_inside(target_loop, pred_loop) &&
		pred_depth > block_depth;
}

static ir_node *new_linked_Phi(ir_node *block, ir_mode *mode)
{
	ir_node *unknown = new_r_Unknown(get_irn_irg(block), mode);
	unsigned arity = get_Block_n_cfgpreds(block);
	ir_node *in[arity];

	for (unsigned i = 0; i < arity; i++) {
		in[i] = unknown;
	}

	int const opt  = get_optimize();
	set_optimize(0);
	ir_node *const phi = new_r_Phi(block, arity, in, mode);
	set_optimize(opt);

	set_irn_link(block, phi);

	return phi;
}

static ir_node *insert_phis_recursive(ir_node *const target, ir_node *const block)
{
	DB((dbg, LEVEL_2, "\tinsert_phis_recursive at %+F: ", block));

	ir_node *const link = get_irn_link(block);
	if (link) {
		DB((dbg, LEVEL_2, "Already visited, return %+F\n", link));
		return link;
	}

	ir_node *result;
	unsigned const arity = get_irn_arity(block);
	ir_mode *mode = get_irn_mode(target);

	if (block == get_nodes_block(target)) {
		DB((dbg, LEVEL_2, "Target block found, return %+F\n", target));
		result = target;

	} else if (arity == 1) {
		ir_node *pred_block = get_Block_cfgpred_block(block, 0);

		if (is_nested_loop_exit(block, pred_block, target)) {
			ir_node *phi  = new_linked_Phi(block, mode);
			DB((dbg, LEVEL_2, "Loop exit, constructing %+F\n", phi));

			ir_node *pred = insert_phis_recursive(target, pred_block);
			set_irn_n(phi, 0, pred);
			DB((dbg, LEVEL_2, "\tsetting pred of loop-exiting %+F to %+F\n", phi, pred));

			result = phi;
		} else {
			DB((dbg, LEVEL_2, "One predecessor, pass-through\n"));
			result = insert_phis_recursive(target, pred_block);
		}

	} else {
		ir_node *phi = new_linked_Phi(block, mode);
		DB((dbg, LEVEL_2, "Multiple preds, constructing tentative %+F\n", phi));

		for (unsigned i = 0; i < arity; i++) {
			ir_node *const pred_block = get_Block_cfgpred_block(block, i);
			ir_node *const pred_phi   = insert_phis_recursive(target, pred_block);
			DB((dbg, LEVEL_2, "\t\t%+F @ %+F: Predecessor %i is %+F\n", phi, block, i, pred_phi));
			set_irn_n(phi, i, pred_phi);
		}

#ifdef DEBUG_libfirm
		DB((dbg, LEVEL_2, "\tChecking if %+F can be optimized away (preds are", phi));
		for (unsigned i = 0; i < arity; i++) {
			DB((dbg, LEVEL_2, " %+F", get_irn_n(phi, i)));
		}
		DB((dbg, LEVEL_2, "): "));
#endif
		bool phi_has_single_pred = true;
		ir_node *single_pred = NULL;

		for (unsigned i = 0; i < arity; i++) {
			ir_node *pred = get_irn_n(phi, i);
			if (pred == phi) {
				// Self-loops can be ignored
				// TODO This is only an approximation to SCCs
				continue;
			}
			if (single_pred && pred != single_pred) {
				phi_has_single_pred = false;
				break;
			}
			single_pred = pred;
		}

		if (phi_has_single_pred) {
			// single_pred dominates phi because it dominates every CF predecessor of block
			// ==> We can use it as block's phi
			DB((dbg, LEVEL_2, "Yes, replacing with %+F\n", single_pred));
			result = single_pred;
		} else {
			DB((dbg, LEVEL_2, "No\n"));
			result = phi;
		}
	}

	set_irn_link(block, result);
	DB((dbg, LEVEL_2, "\t%+F done, return %+F\n", block, result));
	return result;
}

static void clear_link_recursive(ir_node *const block)
{
	ir_node *const link = get_irn_link(block);
	if (link == NULL)
		return;

	set_irn_link(block, NULL);

	int const arity = get_irn_arity(block);
	for (int i = 0; i < arity; ++i) {
		ir_node *const pred_block = get_Block_cfgpred_block(block, i);
		clear_link_recursive(pred_block);
	}
}

// insert phi nodes for the edge between node and its nth predecessor
static void insert_phis_for_edge(ir_node *node, int n)
{
	ir_node *const pred = get_irn_n(node, n);
	ir_mode *const mode = get_irn_mode(pred);
	if (!(mode_is_data(mode) || mode == mode_M))
		return;

	ir_node *block = get_nodes_block(node);
	ir_node *const pred_block = get_nodes_block(pred);
	if (!cf_has_loop_exit(pred_block, block))
		return;

	if (is_Phi(node)) {
		// if node is a phi, start at the nth predecessor of block
		block = get_nodes_block(get_irn_n(block, n));
	}
	DB((dbg, LEVEL_1, "inserting phis for %+F (%+F), edge %i (pred %+F)\n", node, block, n, pred));
	ir_node *const phi = insert_phis_recursive(pred, block);
	set_irn_n(node, n, phi);
	clear_link_recursive(block);
}

static void insert_phis_for_node(ir_node *const node, void *const env)
{
	(void)env;
	// ignore blocks and keep-alive edges
	if (is_Block(node) || is_End(node))
		return;
	int const arity = get_irn_arity(node);
	for (int i = 0; i < arity; ++i) {
		insert_phis_for_edge(node, i);
	}
}

static void insert_phis_for_node_out(ir_node *const node)
{
	if (irn_visited(node))
		return;
	unsigned int const n_outs = get_irn_n_outs(node);
	for (unsigned int i = 0; i < n_outs; ++i) {
		int n;
		ir_node *const succ = get_irn_out_ex(node, i, &n);
		insert_phis_for_edge(succ, n);
	}
}

static void insert_phis_for_block(ir_node *const block)
{
	// iterate over the nodes of the block
	unsigned int const n_outs = get_irn_n_outs(block);
	for (unsigned int i = 0; i < n_outs; ++i) {
		ir_node *const node = get_irn_out(block, i);
		assert(!is_Block(node));
		insert_phis_for_node_out(node);
	}
}

static void insert_phis_for_loop(ir_loop *const loop)
{
	size_t const n_elements = get_loop_n_elements(loop);
	for (size_t i = 0; i < n_elements; ++i) {
		loop_element const element = get_loop_element(loop, i);
		if (*element.kind == k_ir_node) {
			assert(is_Block(element.node));
			insert_phis_for_block(element.node);
		} else if (*element.kind == k_ir_loop) {
			insert_phis_for_loop(element.son);
		}
	}
}

#ifdef DEBUG_libfirm

static bool is_inner_loop(ir_loop *const outer_loop, ir_loop *inner_loop)
{
	ir_loop *old_inner_loop;
	do {
		old_inner_loop = inner_loop;
		inner_loop = get_loop_outer_loop(inner_loop);
	} while (inner_loop != old_inner_loop && inner_loop != outer_loop);
	return inner_loop != old_inner_loop;
}

static void verify_lcssa_node(ir_node *const node, void *const env)
{
	(void)env;
	if (is_Block(node))
		return;
	ir_loop *const loop  = get_irn_loop(get_nodes_block(node));
	int      const arity = get_irn_arity(node);
	for (int i = 0; i < arity; ++i) {
		ir_node *const pred = get_irn_n(node, i);
		if (!mode_is_data(get_irn_mode(pred)))
			return;
		ir_loop *const pred_loop = get_irn_loop(get_nodes_block(pred));
		if (is_inner_loop(loop, pred_loop)) {
			assert(is_Phi(node));
		}
	}
}

static void verify_lcssa(ir_graph *const irg)
{
	irg_walk_graph(irg, verify_lcssa_node, NULL, NULL);
}

#endif /* DEBUG_libfirm */

void assure_lcssa(ir_graph *const irg)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.lcssa");
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO | IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE | IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES);
	dump_ir_graph(irg, "before-lcssa");
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	irg_walk_graph(irg, firm_clear_link, NULL, NULL);
	DB((dbg, LEVEL_1, "Begin LCSSA construction on %+F\n", irg));
	irg_walk_graph(irg, insert_phis_for_node, NULL, NULL);
	DB((dbg, LEVEL_1, "LCSSA done on %+F\n", irg));
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
	DEBUG_ONLY(verify_lcssa(irg);)
	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);
	dump_ir_graph(irg, "after-lcssa");
}

void assure_loop_lcssa(ir_graph *const irg, ir_loop *const loop)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.lcssa");
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO | IR_GRAPH_PROPERTY_CONSISTENT_OUTS | IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);
	inc_irg_visited(irg);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	irg_walk_graph(irg, firm_clear_link, NULL, NULL);
	insert_phis_for_loop(loop);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);
}
