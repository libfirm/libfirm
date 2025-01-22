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
#include "debug.h"
#include "ircons.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "iroptimize.h"
#include "tv.h"
#include "util.h"
#include "vrp.h"
#include <stdbool.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

static bool is_optimizable_node(const ir_node *node, ir_mode *dest_mode)
{
	switch (get_irn_opcode(node)) {
	case iro_And:
	case iro_Eor:
	case iro_Minus:
	case iro_Not:
	case iro_Or:
	case iro_Phi:
		return true;

	case iro_Add:
	case iro_Mul:
	case iro_Sub:
		return !mode_is_float(get_irn_mode(node));

	case iro_Shr:
	case iro_Shrs: {
		int dest_size = get_mode_size_bits(dest_mode);
		int size      = get_mode_size_bits(get_irn_mode(node));
		if (dest_size != size)
			return false;
	} /* FALLTHROUGH */
	case iro_Shl: {
		int modulo_shift = get_mode_modulo_shift(dest_mode);
		int old_shift    = get_mode_modulo_shift(get_irn_mode(node));
		/* bail out if modulo shift changes */
		return modulo_shift == old_shift;
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

static bool is_shift(ir_node const *const node)
{
	return is_Shl(node) || is_Shr(node) || is_Shrs(node);
}

static int get_conv_costs(const ir_node *const node, ir_mode *const dest_mode)
{
	ir_mode *const mode = get_irn_mode(node);
	if (mode == dest_mode)
		return 0;

	switch (get_irn_opcode(node)) {
	case iro_Bad:
		return 0;

	case iro_Const: {
		ir_tarval *conved = conv_const_tv(node, dest_mode);
		return (conved != tarval_bad && conved != tarval_unknown) ? 0 : 1;
	}

	case iro_Conv:
		if (is_downconv(mode, dest_mode) &&
		    get_irn_mode(get_Conv_op(node)) == dest_mode) {
			return -1;
		}
		break;

	case iro_Unknown:
		return 0;
	}

	if (get_irn_n_edges(node) > 1) {
		DB((dbg, LEVEL_3, "multi outs at %+F\n", node));
		return 1;
	}

	if (ir_zero_when_converted(node, dest_mode))
		return -1;

	/* TODO: Phi nodes */

	if (!is_downconv(mode, dest_mode))
		return 1;

	if (is_Conv(node)) {
		ir_node *pred      = get_Conv_op(node);
		ir_mode *pred_mode = get_irn_mode(pred);
		if (smaller_mode(pred_mode, dest_mode)) {
			return get_conv_costs(pred, dest_mode) - 1;
		} else if (may_leave_out_middle_conv(pred_mode, mode, dest_mode)) {
			return 0;
		} else {
			return 1;
		}
	}

	if (!is_optimizable_node(node, dest_mode))
		return 1;

	int costs = 0;
	// The shift count does not participate in the conv optimization
	int const arity = is_shift(node) ? 1 : get_irn_arity(node);
	for (int i = 0; i < arity; ++i) {
		ir_node *const pred   = get_irn_n(node, i);
		int      const pcosts = get_conv_costs(pred, dest_mode);
		costs += MIN(pcosts, 1);
	}

	return costs;
}

static ir_node *place_conv(ir_node *node, ir_mode *dest_mode)
{
	ir_node *block = get_nodes_block(node);
	ir_node *conv  = new_r_Conv(block, node, dest_mode);
	return conv;
}

static ir_node *conv_transform(ir_node *node, ir_mode *dest_mode)
{
	ir_mode *const mode = get_irn_mode(node);
	if (mode == dest_mode)
		return node;

	ir_graph *const irg  = get_irn_irg(node);
	switch (get_irn_opcode(node)) {
	case iro_Bad:
		return new_r_Bad(irg, dest_mode);

	case iro_Const: {
		ir_tarval *tv = conv_const_tv(node, dest_mode);
		if (tv == tarval_bad || tv == tarval_unknown) {
			return place_conv(node, dest_mode);
		} else {
			dbg_info *const dbgi = get_irn_dbg_info(node);
			return new_rd_Const(dbgi, irg, tv);
		}
	}

	case iro_Conv:
		if (is_downconv(mode, dest_mode) &&
		    get_irn_mode(get_Conv_op(node)) == dest_mode) {
			return get_Conv_op(node);
		}
		break;

	case iro_Unknown:
		return new_r_Unknown(irg, dest_mode);
	}

	if (get_irn_n_edges(node) > 1)
		return place_conv(node, dest_mode);

	if (!is_downconv(mode, dest_mode))
		return place_conv(node, dest_mode);

	if (is_Conv(node)) {
		ir_node *pred      = get_Conv_op(node);
		ir_mode *pred_mode = get_irn_mode(pred);
		if (smaller_mode(pred_mode, dest_mode))
			return conv_transform(get_Conv_op(node), dest_mode);
		return place_conv(node, dest_mode);
	}

	if (!is_optimizable_node(node, dest_mode))
		return place_conv(node, dest_mode);

	// We want to create a new node with the right mode
	int       const arity = get_irn_arity(node);
	ir_node **const ins   = ALLOCAN(ir_node *, arity);

	// The shift count does not participate in the conv optimization
	int const conv_arity = is_shift(node) ? 1 : arity;
	for (int i = 0; i < conv_arity; i++) {
		ir_node *pred = get_irn_n(node, i);
		ir_node *transformed;
		if (get_conv_costs(pred, dest_mode) > 1) {
			transformed = place_conv(pred, dest_mode);
		} else {
			transformed = conv_transform(pred, dest_mode);
		}
		ins[i] = transformed;
	}

	for (int i = conv_arity; i < arity; i++) {
		ins[i] = get_irn_n(node, i);
	}

	dbg_info *const dbg      = get_irn_dbg_info(node);
	ir_node  *const block    = get_nodes_block(node);
	ir_op    *const op       = get_irn_op(node);
	ir_node  *const new_node = new_ir_node(dbg, irg, block, op, dest_mode, arity, ins);
	copy_node_attr(irg, node, new_node);

	return new_node;
}

static void conv_opt_walker(ir_node *node, void *data)
{
	bool *const changed = (bool*)data;

	if (!is_Conv(node))
		return;

	ir_mode *const mode = get_irn_mode(node);
	if (mode_is_reference(mode))
		return;

	ir_node *const pred      = get_Conv_op(node);
	ir_mode *const pred_mode = get_irn_mode(pred);
	if (mode_is_reference(pred_mode))
		return;

	if (!is_Phi(pred) && !is_downconv(pred_mode, mode))
		return;

	/* - 1 for the initial conv */
	int const costs = get_conv_costs(pred, mode) - 1;
	DB((dbg, LEVEL_2, "Costs for %+F -> %+F: %d\n", node, pred, costs));
	if (costs > 0)
		return;

	ir_node *const transformed = conv_transform(pred, mode);
	if (node != transformed) {
		exchange(node, transformed);
		*changed = true;
	}
}

void conv_opt(ir_graph *irg)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.conv");

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);

	DB((dbg, LEVEL_1, "===> Performing conversion optimization on %+F\n", irg));

	bool global_changed = false;
	bool changed;
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
