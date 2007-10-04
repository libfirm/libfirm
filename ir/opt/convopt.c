/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "iroptimize.h"

#include <assert.h>
#include "debug.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irnode_t.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "irprintf.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg);

static INLINE int imin(int a, int b) { return a < b ? a : b; }

static
int is_optimizable_node(const ir_node *node)
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
			return 1;

		default: return 0;
	}
}

static tarval* conv_const_tv(const ir_node* cnst, ir_mode* dest_mode)
{
	return tarval_convert_to(get_Const_tarval(cnst), dest_mode);
}

static
int is_downconv(ir_mode *src_mode, ir_mode *dest_mode)
{
	return
		mode_is_int(src_mode) &&
		mode_is_int(dest_mode) &&
		get_mode_size_bits(dest_mode) < get_mode_size_bits(src_mode);
}

static
int get_conv_costs(const ir_node *node, ir_mode *dest_mode)
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

	if (get_irn_n_edges(node) > 1) {
		DB((dbg, LEVEL_3, "multi outs at %+F\n", node));
		return 1;
	}

	if (is_Conv(node) && is_downconv(get_irn_mode(node), dest_mode)) {
		return get_conv_costs(get_Conv_op(node), dest_mode) - 1;
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
	ir_node *conv = new_r_Conv(current_ir_graph, block, node, dest_mode);
	return conv;
}

static
ir_node *conv_transform(ir_node *node, ir_mode *dest_mode)
{
	size_t arity;
	size_t i;

	if (get_irn_mode(node) == dest_mode)
		return node;

	if (is_Const(node)) {
		/* TODO tarval module is incomplete and can't convert floats to ints */
		tarval *tv = conv_const_tv(node, dest_mode);
		if (tv == tarval_bad) {
			return place_conv(node, dest_mode);
		} else {
			return new_Const(dest_mode, tv);
		}
	}

	if (get_irn_n_edges(node) > 1) {
		return place_conv(node, dest_mode);
	}

	if (is_Conv(node) && is_downconv(get_irn_mode(node), dest_mode)) {
		return conv_transform(get_Conv_op(node), dest_mode);
	}

	if (!is_optimizable_node(node)) {
		return place_conv(node, dest_mode);
	}

	// The shift count does not participate in the conv optimisation
	arity = is_Shl(node) ? 1 : get_irn_arity(node);
	for (i = 0; i < arity; i++) {
		ir_node *pred = get_irn_n(node, i);
		ir_node *transformed;
		if (get_conv_costs(pred, dest_mode) > 0) {
			transformed = place_conv(pred, dest_mode);
		} else {
			transformed = conv_transform(pred, dest_mode);
		}
		set_irn_n(node, i, transformed);
	}
	set_irn_mode(node, dest_mode);
	return node;
}

/* TODO, backends (at least ia32) can't handle it at the moment,
   and it's probably not more efficient on most archs */
#if 0
static
void try_optimize_cmp(ir_node *node)
{
	ir_node *left  = get_Cmp_left(node);
	ir_node *right = get_Cmp_right(node);
	ir_node *conv  = NULL;

	if(is_downconv
}
#endif

static char changed;

static
void conv_opt_walker(ir_node *node, void *data)
{
	ir_node *transformed;
	ir_node *pred;
	ir_mode *pred_mode;
	ir_mode *mode;
	int costs;
	(void) data;

#if 0
	if(is_Cmp(node)) {
		try_optimize_cmp(node);
		return;
	}
#endif

	if (!is_Conv(node))
		return;

	pred      = get_Conv_op(node);
	mode      = get_irn_mode(node);
	pred_mode = get_irn_mode(pred);

	if (!is_Phi(pred) && !is_downconv(pred_mode, mode))
		return;

	/* - 1 for the initial conv */
	costs = get_conv_costs(pred, mode) - 1;
	DB((dbg, LEVEL_2, "Costs for %+F -> %+F: %d\n", node, pred, costs));
	if (costs > 0) return;

	transformed = conv_transform(pred, mode);
	if (node != transformed) {
		exchange(node, transformed);
		changed = 1;
	}
}

void conv_opt(ir_graph *irg)
{
	char invalidate = 0;
	FIRM_DBG_REGISTER(dbg, "firm.opt.conv");

	DB((dbg, LEVEL_1, "===> Performing conversion optimization on %+F\n", irg));

	edges_assure(irg);
	do {
		changed = 0;
		irg_walk_graph(irg, NULL, conv_opt_walker, NULL);
		local_optimize_graph(irg);
		invalidate |= changed;
	} while (changed);

	if (invalidate) {
		set_irg_outs_inconsistent(irg);
	}
}
