/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   conv node optimization
 * @author  Matthias Braun, Christoph Mallon
 *
 * Try to minimize the number of conv nodes by changing modes of operations.
 * The typical example is the following structure:
 *    (some node mode_Hs)
 *            |                                       (some node_Hs)
 *         Conv Is                                          |
 *            |                                          Add Hs
 *          Add Is            gets transformed to           |
 *            |
 *         Conv Hs
 *
 * TODO: * try to optimize cmp modes
 *       * decide when it is useful to move the convs through phis
 */
#include "iroptimize.h"

#include <stdbool.h>
#include "debug.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "tv.h"
#include "vrp.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

static inline int imin(int a, int b) { return a < b ? a : b; }

static bool is_optimizable_node(const ir_node *node, ir_mode *dest_mode)
{
	switch (get_irn_opcode(node)) {
	case iro_Minus:
	case iro_Phi:
	case iro_And:
	case iro_Eor:
	case iro_Or:
	case iro_Not:
		return true;
	case iro_Add:
	case iro_Mul:
	case iro_Sub:
		if (mode_is_float(get_irn_mode(node)))
			return false;
		return true;
	case iro_Shl: {
		int modulo_shift = get_mode_modulo_shift(dest_mode);
		int old_shift    = get_mode_modulo_shift(get_irn_mode(node));
		/* bail out if modulo shift changes */
		if (modulo_shift != old_shift)
			return false;
		return true;
	}

	default:
		return false;
	}
}

static ir_tarval* conv_const_tv(const ir_node* cnst, ir_mode* dest_mode)
{
	return tarval_convert_to(get_Const_tarval(cnst), dest_mode);
}

static bool is_downconv(ir_mode *src_mode, ir_mode *dest_mode)
{
	return ((mode_is_int(src_mode) && mode_is_int(dest_mode))
		|| (mode_is_float(src_mode) && mode_is_float(dest_mode)))
		&& get_mode_size_bits(dest_mode) <= get_mode_size_bits(src_mode);
}

static int get_conv_costs(const ir_node *node, ir_mode *dest_mode)
{
	ir_mode *mode = get_irn_mode(node);
	int arity;
	int i;
	int costs;

	if (mode == dest_mode)
		return 0;

	if (is_Const(node)) {
		return conv_const_tv(node, dest_mode) == tarval_bad ? 1 : 0;
	}

	if (is_Conv(node) &&
			is_downconv(mode, dest_mode) &&
			get_irn_mode(get_Conv_op(node)) == dest_mode) {
		return -1;
	}

	if (get_irn_n_edges(node) > 1) {
		DB((dbg, LEVEL_3, "multi outs at %+F\n", node));
		return 1;
	}

	if (ir_zero_when_converted(node, dest_mode)) {
		return -1;
	}

	/* TODO: Phi nodes */

	if (!is_downconv(mode, dest_mode)) {
		return 1;
	}

	if (is_Conv(node)) {
		ir_node *pred      = get_Conv_op(node);
		ir_mode *pred_mode = get_irn_mode(pred);

		if (smaller_mode(pred_mode, dest_mode)) {
			return get_conv_costs(get_Conv_op(node), dest_mode) - 1;
		}
		if (may_leave_out_middle_conv(pred_mode, mode, dest_mode)) {
			return 0;
		} else {
			return 1;
		}
	}

	if (!is_optimizable_node(node, dest_mode)) {
		return 1;
	}

	costs = 0;
	// The shift count does not participate in the conv optimization
	arity = is_Shl(node) ? 1 : get_irn_arity(node);
	for (i = 0; i < arity; ++i) {
		ir_node *pred = get_irn_n(node, i);
		costs += imin(get_conv_costs(pred, dest_mode), 1);
	}

	return costs;
}

static ir_node *place_conv(ir_node *node, ir_mode *dest_mode)
{
	ir_node *block = get_nodes_block(node);
	ir_node *conv = new_r_Conv(block, node, dest_mode);
	return conv;
}

static ir_node *conv_transform(ir_node *node, ir_mode *dest_mode)
{
	ir_mode  *mode = get_irn_mode(node);
	ir_graph *irg  = get_irn_irg(node);
	int       arity;
	int       conv_arity;
	int       i;
	ir_node  *new_node;
	ir_node **ins;

	if (mode == dest_mode)
		return node;

	if (is_Const(node)) {
		ir_tarval *tv = conv_const_tv(node, dest_mode);
		if (tv == tarval_bad) {
			return place_conv(node, dest_mode);
		} else {
			return new_r_Const(irg, tv);
		}
	}

	if (is_Conv(node) &&
			is_downconv(mode, dest_mode) &&
			get_irn_mode(get_Conv_op(node)) == dest_mode) {
		return get_Conv_op(node);
	}

	if (get_irn_n_edges(node) > 1) {
		return place_conv(node, dest_mode);
	}

	if (!is_downconv(mode, dest_mode)) {
		return place_conv(node, dest_mode);
	}

	if (is_Conv(node)) {
		ir_node *pred      = get_Conv_op(node);
		ir_mode *pred_mode = get_irn_mode(pred);

		if (smaller_mode(pred_mode, dest_mode)) {
			return conv_transform(get_Conv_op(node), dest_mode);
		}
		return place_conv(node, dest_mode);
	}

	if (!is_optimizable_node(node, dest_mode)) {
		return place_conv(node, dest_mode);
	}

	// We want to create a new node with the right mode
	arity = get_irn_arity(node);
	ins = ALLOCAN(ir_node *, arity);

	// The shift count does not participate in the conv optimization
	conv_arity = is_Shl(node) ? 1 : arity;
	for (i = 0; i < conv_arity; i++) {
		ir_node *pred = get_irn_n(node, i);
		ir_node *transformed;
		if (get_conv_costs(pred, dest_mode) > 0) {
			transformed = place_conv(pred, dest_mode);
		} else {
			transformed = conv_transform(pred, dest_mode);
		}
		ins[i] = transformed;
	}

	for (i = conv_arity; i < arity; i++) {
		ins[i] = get_irn_n(node, i);
	}

	new_node = new_ir_node(get_irn_dbg_info(node),
				irg,
				get_nodes_block(node),
				get_irn_op(node),
				dest_mode,
				arity,
				ins);
	copy_node_attr(irg, node, new_node);

	return new_node;
}

static void conv_opt_walker(ir_node *node, void *data)
{
	ir_node *transformed;
	ir_node *pred;
	ir_mode *pred_mode;
	ir_mode *mode;
	int costs;
	bool *changed = (bool*)data;

	if (!is_Conv(node))
		return;

	pred      = get_Conv_op(node);
	mode      = get_irn_mode(node);
	pred_mode = get_irn_mode(pred);

	if (mode_is_reference(mode) || mode_is_reference(pred_mode))
		return;

	if (!is_Phi(pred) && !is_downconv(pred_mode, mode))
		return;

	/* - 1 for the initial conv */
	costs = get_conv_costs(pred, mode) - 1;
	DB((dbg, LEVEL_2, "Costs for %+F -> %+F: %d\n", node, pred, costs));
	if (costs > 0)
		return;

	transformed = conv_transform(pred, mode);
	if (node != transformed) {
		exchange(node, transformed);
		*changed = true;
	}
}

void conv_opt(ir_graph *irg)
{
	bool global_changed = false;
	bool changed;
	FIRM_DBG_REGISTER(dbg, "firm.opt.conv");

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);

	DB((dbg, LEVEL_1, "===> Performing conversion optimization on %+F\n", irg));

	do {
		changed = false;
		irg_walk_graph(irg, NULL, conv_opt_walker, &changed);
		if (changed)
			local_optimize_graph(irg);
		global_changed |= changed;
	} while (changed);

	confirm_irg_properties(irg,
		global_changed ? IR_GRAPH_PROPERTIES_NONE : IR_GRAPH_PROPERTIES_ALL);
}
