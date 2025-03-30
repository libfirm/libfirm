/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       lower mode_b operations to something the backend can handle
 * @author      Matthias Braun, Christoph Mallon
 */
#include "lower_mode_b.h"

#include "array.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "irflag.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "irtools.h"
#include "lowering.h"
#include "panic.h"
#include "tv.h"
#include "util.h"
#include <stdbool.h>
#include <stdlib.h>

typedef struct needs_lowering_t {
	ir_node *node;
	int      input;
} needs_lowering_t;

static ir_mode          *lowered_mode;
static needs_lowering_t *needs_lowering;

static ir_node *create_not(dbg_info *dbgi, ir_node *node)
{
	ir_node  *block = get_nodes_block(node);
	ir_mode  *mode  = lowered_mode;
	ir_graph *irg   = get_irn_irg(node);
	ir_node  *one   = new_rd_Const_one(dbgi, irg, mode);

	return new_rd_Eor(dbgi, block, node, one);
}

static ir_node *convert_to_modeb(ir_node *node)
{
	ir_node  *block = get_nodes_block(node);
	ir_graph *irg   = get_irn_irg(node);
	ir_node  *zero  = new_r_Const_null(irg, lowered_mode);
	ir_node  *cmp   = new_r_Cmp(block, node, zero, ir_relation_less_greater);
	return cmp;
}

/**
 * implementation of create_set_func which produces a cond with control
 * flow
 */
static ir_node *create_cond_set(ir_node *cond_value, ir_mode *dest_mode)
{
	ir_node *lower_block = part_block_edges(cond_value);
	ir_node *upper_block = get_nodes_block(cond_value);
	foreach_out_edge_safe(upper_block, edge) {
		/* The cached nodes might belong to the lower block, so we have
		 * to clear the cache for moved nodes to avoid dominance problems. */
		ir_node *node = get_edge_src_irn(edge);
		set_irn_link(node, NULL);
	}
	ir_graph *irg         = get_irn_irg(cond_value);
	ir_node  *cond        = new_r_Cond(upper_block, cond_value);
	ir_node  *proj_true   = new_r_Proj(cond, mode_X, pn_Cond_true);
	ir_node  *proj_false  = new_r_Proj(cond, mode_X, pn_Cond_false);
	ir_node  *in_true[1]  = { proj_true };
	ir_node  *in_false[1] = { proj_false };
	ir_node  *true_block  = new_r_Block(irg, ARRAY_SIZE(in_true), in_true);
	ir_node  *false_block = new_r_Block(irg, ARRAY_SIZE(in_false),in_false);
	ir_node  *true_jmp    = new_r_Jmp(true_block);
	ir_node  *false_jmp   = new_r_Jmp(false_block);
	ir_node  *lower_in[2] = { true_jmp, false_jmp };
	ir_node  *one         = new_r_Const_one(irg, dest_mode);
	ir_node  *zero        = new_r_Const_null(irg, dest_mode);
	ir_node  *phi_in[2]   = { one, zero };

	set_irn_in(lower_block, ARRAY_SIZE(lower_in), lower_in);
	ir_node *phi = new_r_Phi(lower_block, ARRAY_SIZE(phi_in), phi_in, dest_mode);

	return phi;
}

static ir_node *lower_node(ir_node *node)
{
	ir_node *res = (ir_node *)get_irn_link(node);

	if (res != NULL)
		return res;

	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = get_nodes_block(node);
	ir_mode  *mode  = lowered_mode;
	ir_graph *irg   = get_irn_irg(node);

	node = skip_Tuple(node);

	assert(get_irn_mode(node) == mode_b);

	switch (get_irn_opcode(node)) {
	case iro_Phi: {
		int       arity = get_irn_arity(node);
		ir_node **in    = ALLOCAN(ir_node*, arity);
		ir_node  *dummy = new_r_Dummy(irg, mode);

		for (int i = 0; i < arity; ++i) {
			in[i] = dummy;
		}
		ir_node *new_phi = new_r_Phi(block, arity, in, mode);
		/* FIXME This does not correctly break cycles: The Phi might not be the
		 * first in the recursion, so the caller(s) are some yet un-lowered
		 * nodes and this Phi might have them (indirectly) as operands, so they
		 * would be replaced twice. */
		set_irn_link(node, new_phi);

		foreach_irn_in(node, i, in) {
			ir_node *lowered_in = lower_node(in);
			set_irn_n(new_phi, i, lowered_in);
		}

		return new_phi;
	}

	case iro_And: {
		ir_node *lowered_left  = lower_node(get_And_left(node));
		ir_node *lowered_right = lower_node(get_And_right(node));
		res = new_rd_And(dbgi, block, lowered_left, lowered_right);
		break;
	}
	case iro_Or: {
		ir_node *lowered_left  = lower_node(get_Or_left(node));
		ir_node *lowered_right = lower_node(get_Or_right(node));
		res = new_rd_Or(dbgi, block, lowered_left, lowered_right);
		break;
	}
	case iro_Eor: {
		ir_node *lowered_left  = lower_node(get_Eor_left(node));
		ir_node *lowered_right = lower_node(get_Eor_right(node));
		res = new_rd_Eor(dbgi, block, lowered_left, lowered_right);
		break;
	}

	case iro_Not: {
		ir_node *op     = get_Not_op(node);
		ir_node *low_op = lower_node(op);

		res = create_not(dbgi, low_op);
		break;
	}

	case iro_Mux: {
		ir_node *cond        = get_Mux_sel(node);
		ir_node *low_cond    = lower_node(cond);
		ir_node *v_true      = get_Mux_true(node);
		ir_node *low_v_true  = lower_node(v_true);
		ir_node *v_false     = get_Mux_false(node);
		ir_node *low_v_false = lower_node(v_false);

		ir_node *and0     = new_rd_And(dbgi, block, low_cond, low_v_true);
		ir_node *not_cond = create_not(dbgi, low_cond);
		ir_node *and1     = new_rd_And(dbgi, block, not_cond, low_v_false);
		res = new_rd_Or(dbgi, block, and0, and1);
		break;
	}

	case iro_Cmp:
		res = create_cond_set(node, mode);
		break;

	case iro_Const: {
		ir_tarval *tv = get_Const_tarval(node);
		if (tv == get_tarval_b_true()) {
			res = new_rd_Const_one(dbgi, irg, mode);
		} else if (tv == get_tarval_b_false()) {
			res = new_rd_Const_null(dbgi, irg, mode);
		} else {
			panic("invalid boolean const %+F", node);
		}
		break;
	}

	case iro_Unknown:
		res = new_r_Unknown(irg, mode);
		break;

	case iro_Bad:
		res = new_r_Bad(irg, mode);
		break;

	default:
		panic("don't know how to lower mode_b node %+F", node);
	}

	set_irn_link(node, res);
	return res;
}

static bool needs_mode_b_input(const ir_node *node, int input)
{
	return (is_Cond(node) && input == n_Cond_selector)
	    || (is_Mux(node) && input == n_Mux_sel);
}

/**
 * Collects "roots" of a mode_b calculation. These are nodes which require a
 * mode_b input (Cond, Mux)
 */
static void collect_needs_lowering(ir_node *node, void *env)
{
	(void)env;

	/* if the node produces mode_b then it is not a root (but should be
	 * something our lower_node function can handle) */
	if (get_irn_mode(node) == mode_b) {
		assert(is_And(node) || is_Or(node) || is_Eor(node) || is_Phi(node)
		       || is_Not(node) || is_Mux(node) || is_Cmp(node)
		       || is_Const(node) || is_Unknown(node) || is_Bad(node));
		return;
	}

	foreach_irn_in(node, i, in) {
		if (get_irn_mode(in) != mode_b)
			continue;
		if (is_Cmp(in) && needs_mode_b_input(node, i))
			continue;

		needs_lowering_t entry = { .node  = node, .input = i };
		ARR_APP1(needs_lowering_t, needs_lowering, entry);
	}
}

void ir_lower_mode_b(ir_graph *const irg, ir_mode *const nlowered_mode)
{
	lowered_mode = nlowered_mode;

	/* edges are used by part_block_edges in the ir_create_cond_set variant. */
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES
	                         | IR_GRAPH_PROPERTY_NO_TUPLES);

	add_irg_constraints(irg, IR_GRAPH_CONSTRAINT_MODEB_LOWERED);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);

	needs_lowering = NEW_ARR_F(needs_lowering_t, 0);

	irg_walk_graph(irg, firm_clear_link, collect_needs_lowering, NULL);

	size_t n = ARR_LEN(needs_lowering);
	for (size_t i = 0; i < n; ++i) {
		const needs_lowering_t *entry   = &needs_lowering[i];
		ir_node                *node    = entry->node;
		int                     input   = entry->input;
		ir_node                *in      = get_irn_n(node, input);
		ir_node                *lowered = lower_node(in);

		if (needs_mode_b_input(node, input))
			lowered = convert_to_modeb(lowered);
		set_irn_n(node, input, lowered);
	}

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	DEL_ARR_F(needs_lowering);

	confirm_irg_properties(irg, n > 0 ? IR_GRAPH_PROPERTIES_NONE
	                                  : IR_GRAPH_PROPERTIES_ALL);
}
