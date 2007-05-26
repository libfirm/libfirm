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

#include <assert.h>
#include "convopt.h"
#include "debug.h"
#include "ircons.h"
#include "irgmod.h"
#include "irnode_t.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "irprintf.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg);

static
int is_optimizable_node(const ir_node *node)
{
	if(is_Const(node)) {
		ir_mode *mode = get_irn_mode(node);
		/* tarval module is incomplete and can't convert floats to ints */
		if(!mode_is_int(mode))
			return 0;
		return 1;
	}
	return is_Add(node) || is_Sub(node) || is_Mul(node) || is_Phi(node);
}

static
int get_conv_costs(const ir_node *node, ir_mode *dest_mode)
{
	ir_mode *mode = get_irn_mode(node);

	if(mode == dest_mode)
		return 0;

	/* TODO... */
	if(!is_Const(node) && get_irn_n_edges(node) > 1) {
		DB((dbg, LEVEL_3, "multi outs at %+F\n", node));
		return 1;
	}

	if(is_Conv(node)) {
		return get_conv_costs(get_Conv_op(node), dest_mode) - 1;
	}

	if(is_optimizable_node(node)) {
		int i;
		int arity = get_irn_arity(node);
		int costs = 0;

		for(i = 0; i < arity; ++i) {
			ir_node *pred = get_irn_n(node, i);
			costs += get_conv_costs(pred, dest_mode);
		}

		return costs;
	}

	return 1;
}

static
ir_node *conv_transform(ir_node *node, ir_mode *dest_mode)
{
	size_t arity;
	size_t i;

	if (get_irn_mode(node) == dest_mode)
		return node;

	if (is_Const(node)) {
		tarval *tv = tarval_convert_to(get_Const_tarval(node), dest_mode);
		assert(get_tarval_mode(tv) == dest_mode);
		return new_Const(dest_mode, tv);
	}

	if (is_Conv(node)) {
		return conv_transform(get_Conv_op(node), dest_mode);
	}

	if (!is_optimizable_node(node) || get_irn_n_edges(node) > 1) {
		ir_node *block = get_nodes_block(node);
		ir_node *conv = new_r_Conv(current_ir_graph, block, node, dest_mode);
		return conv;
	}

	arity = get_irn_arity(node);
	for (i = 0; i < arity; i++) {
		ir_node *pred = get_irn_n(node, i);
		ir_node *transformed = conv_transform(pred, dest_mode);
		set_irn_n(node, i, transformed);
	}
	set_irn_mode(node, dest_mode);
	return node;
}

static
int is_downconv(ir_mode *src_mode, ir_mode *dest_mode)
{
	if(!mode_is_int(src_mode) || !mode_is_int(dest_mode))
		return 0;
	if(get_mode_size_bits(dest_mode) >= get_mode_size_bits(src_mode))
		return 0;

	return 1;
}

/* TODO, backends can't handle and it's probably not more efficient on most
   archs */
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

#if 0
	if(is_Cmp(node)) {
		try_optimize_cmp(node);
		return;
	}
#endif

	if(!is_Conv(node))
		return;

	pred      = get_Conv_op(node);
	mode      = get_irn_mode(node);
	pred_mode = get_irn_mode(pred);

	if (!is_Phi(pred) && !is_downconv(pred_mode, mode))
		return;

	costs = get_conv_costs(pred, mode);
	DB((dbg, LEVEL_2, "Costs for %+F -> %+F: %d\n", node, pred, costs));
	if (costs > 0) return;

	transformed = conv_transform(pred, mode);
	exchange(node, transformed);
	changed = 1;
}

void conv_opt(ir_graph *irg)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.conv");

	DB((dbg, LEVEL_1, "===> Performing conversion optimization on %+F\n", irg));

	edges_assure(irg);
	do {
		changed = 0;
		irg_walk_graph(irg, NULL, conv_opt_walker, NULL);
		local_optimize_graph(irg);
	} while (changed);
}
