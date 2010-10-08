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
 * @brief   conv node optimisation
 * @author  Matthias Braun, Christoph Mallon
 * @version $Id$
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
#include "config.h"

#include "iroptimize.h"

#include <assert.h>
#include <stdbool.h>
#include "debug.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irnode_t.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "irpass_t.h"
#include "tv.h"
#include "vrp.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg);

static inline int imin(int a, int b) { return a < b ? a : b; }

static bool is_optimizable_node(const ir_node *node)
{
	switch (get_irn_opcode(node)) {
	case iro_Add:
	case iro_And:
	case iro_Eor:
	case iro_Minus:
	case iro_Mul:
	case iro_Not:
	case iro_Or:
	case iro_Phi:
	case iro_Shl:
	case iro_Sub:
		return true;
	default:
		return false;
	}
}

static ir_tarval* conv_const_tv(const ir_node* cnst, ir_mode* dest_mode)
{
	return tarval_convert_to(get_Const_tarval(cnst), dest_mode);
}

static int is_downconv(ir_mode *src_mode, ir_mode *dest_mode)
{
	return
		mode_is_int(src_mode) &&
		mode_is_int(dest_mode) &&
		get_mode_size_bits(dest_mode) <= get_mode_size_bits(src_mode);
}

static int get_conv_costs(const ir_node *node, ir_mode *dest_mode)
{
	ir_mode *mode = get_irn_mode(node);
	size_t arity;
	size_t i;
	int costs;

	if (mode == dest_mode)
		return 0;

	if (is_Const(node)) {
		/* TODO tarval module is incomplete and can't convert floats to ints */
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

#if 0 // TODO
	/* Take the minimum of the conversion costs for Phi predecessors as only one
	 * branch is actually executed at a time */
	if (is_Phi(node)) {
		size_t i;
		size_t arity = get_Phi_n_preds(node);
		int costs;

		costs = get_conv_costs(get_Phi_pred(node, 0), dest_mode);
		for (i = 1; i < arity; ++i) {
			ir_node *pred = get_Phi_pred(node, i);
			int c = get_conv_costs(pred, dest_mode);
			if (c < costs) costs = c;
		}

		return costs;
	}
#endif

	if (!is_downconv(mode, dest_mode)) {
		return 1;
	}

	if (is_Conv(node)) {
		ir_node *pred      = get_Conv_op(node);
		ir_mode *pred_mode = get_irn_mode(pred);

		if (!values_in_mode(dest_mode, pred_mode)) {
			return 1;
		}
		return get_conv_costs(get_Conv_op(node), dest_mode) - 1;
	}

	if (!is_optimizable_node(node)) {
		return 1;
	}

	costs = 0;
	// The shift count does not participate in the conv optimisation
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
	size_t    arity;
	size_t    conv_arity;
	size_t    i;
	ir_node  *new_node;
	ir_node **ins;

	if (mode == dest_mode)
		return node;

	if (is_Const(node)) {
		/* TODO tarval module is incomplete and can't convert floats to ints */
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

		if (!values_in_mode(dest_mode, pred_mode)) {
			return place_conv(node, dest_mode);
		}
		return conv_transform(get_Conv_op(node), dest_mode);
	}

	if (!is_optimizable_node(node)) {
		return place_conv(node, dest_mode);
	}

	// We want to create a new node with the right mode
	arity = get_irn_arity(node);
	ins = ALLOCAN(ir_node *, arity);

	// The shift count does not participate in the conv optimisation
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
	bool *changed = data;

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
		vrp_attr *vrp;

		exchange(node, transformed);
		vrp = vrp_get_info(transformed);
		if (vrp && vrp->valid) {
			vrp->range_type = VRP_VARYING;
			vrp->bits_set = tarval_convert_to(vrp->bits_set, mode);
			vrp->bits_not_set = tarval_convert_to(vrp->bits_not_set, mode);
		}

		*changed = true;
	}
}

int conv_opt(ir_graph *irg)
{
	bool changed;
	bool invalidate = false;
	FIRM_DBG_REGISTER(dbg, "firm.opt.conv");

	DB((dbg, LEVEL_1, "===> Performing conversion optimization on %+F\n", irg));

	edges_assure(irg);
	do {
		changed = false;
		irg_walk_graph(irg, NULL, conv_opt_walker, &changed);
		local_optimize_graph(irg);
		invalidate |= changed;
	} while (changed);

	if (invalidate) {
		set_irg_outs_inconsistent(irg);
	}
	return invalidate;
}

/* Creates an ir_graph pass for conv_opt. */
ir_graph_pass_t *conv_opt_pass(const char *name)
{
	ir_graph_pass_t *path = def_graph_pass_ret(name ? name : "conv_opt", conv_opt);

	/* safe to run parallel on all irgs */
	ir_graph_pass_set_parallel(path, 1);

	return path;
}
