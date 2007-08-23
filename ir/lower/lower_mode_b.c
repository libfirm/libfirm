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
 * @author      Matthias Braun, Christoph Mallon
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "irnode_t.h"
#include "ircons_t.h"
#include "irgwalk.h"
#include "irtools.h"
#include "tv.h"
#include "error.h"
#include "lowering.h"
#include "benode_t.h"

static ir_mode *lowered_mode     = NULL;
static int      lower_direct_cmp = 0;

static ir_node *create_not(dbg_info *dbgi, ir_node *node)
{
	ir_graph *irg    = current_ir_graph;
	ir_node  *block  = get_nodes_block(node);
	tarval   *tv_one = get_tarval_one(lowered_mode);
	ir_node  *one    = new_d_Const(dbgi, lowered_mode, tv_one);

	return new_rd_Eor(dbgi,	irg, block, node, one, lowered_mode);
}

static ir_node *create_convb(ir_node *node)
{
	ir_graph *irg   = current_ir_graph;
	ir_node  *block = get_nodes_block(node);
	ir_node  *conv  = new_rd_Conv(NULL, irg, block, node, mode_b);

	return conv;
}

static ir_node *create_conv_lowered_mode(ir_node *node)
{
	ir_graph *irg   = current_ir_graph;
	ir_node  *block = get_nodes_block(node);
	ir_node  *conv  = new_rd_Conv(NULL, irg, block, node, lowered_mode);

	return conv;
}

static ir_node *lower_node(ir_node *node)
{
	ir_graph *irg   = current_ir_graph;
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_op    *op    = get_irn_op(node);
	ir_node  *block = get_nodes_block(node);
	ir_node  *res;

	assert(get_irn_mode(node) == mode_b);

	res = get_irn_link(node);
	if(res != NULL)
		return res;

	/* TODO: be robust against phi-loops... */
	if(op == op_Phi) {
		int       i, arity;
		ir_node **in;
		ir_node  *unknown, *new_phi;

		arity = get_irn_arity(node);
		in    = alloca(arity * sizeof(in[0]));
		unknown = new_Unknown(lowered_mode);
		for(i = 0; i < arity; ++i) {
			in[i] = unknown;
		}
		new_phi = new_rd_Phi(dbgi, irg, block, arity, in,
		                              lowered_mode);
		set_irn_link(node, new_phi);

		for(i = 0; i < arity; ++i) {
			ir_node *in     = get_irn_n(node, i);
			ir_node *low_in = lower_node(in);

			set_irn_n(new_phi, i, low_in);
		}

		return new_phi;
	}

	if(op == op_And || op == op_Or || op == op_Eor) {
		int i, arity;
		ir_node *copy = exact_copy(node);

		arity = get_irn_arity(node);
		for(i = 0; i < arity; ++i) {
			ir_node *in     = get_irn_n(node, i);
			ir_node *low_in = lower_node(in);

			set_irn_n(copy, i, low_in);
		}
		set_irn_mode(copy, lowered_mode);

		set_irn_link(node, copy);
		return copy;
	}
	if(op == op_Not) {
		ir_node *op     = get_Not_op(node);
		ir_node *low_op = lower_node(op);

		res = create_not(dbgi, low_op);
		set_irn_link(node, res);
		return res;
	}
	if(op == op_Psi) {
		ir_node *cond        = get_Psi_cond(node, 0);
		ir_node *low_cond    = lower_node(cond);
		ir_node *v_true      = get_Psi_val(node, 0);
		ir_node *low_v_true  = lower_node(v_true);
		ir_node *v_false     = get_Psi_default(node);
		ir_node *low_v_false = lower_node(v_false);

		ir_node *and0     = new_rd_And(dbgi, irg, block, low_cond, low_v_true,
		                               lowered_mode);

		ir_node *not_cond = create_not(dbgi, low_cond);

		ir_node *and1     = new_rd_And(dbgi, irg, block, not_cond, low_v_false,
		                               lowered_mode);

		ir_node *or       = new_rd_Or(dbgi, irg, block, and0, and1,
		                              lowered_mode);

		set_irn_link(node, or);
		return or;
	}
	if(op == op_Conv) {
		tarval  *tv_one   = get_tarval_one(lowered_mode);
		ir_node *one      = new_d_Const(dbgi, lowered_mode, tv_one);
		tarval  *tv_zero  = get_tarval_null(lowered_mode);
		ir_node *zero     = new_d_Const(dbgi, lowered_mode, tv_zero);
		ir_node *pred     = get_Conv_op(node);
		ir_mode *mode     = get_irn_mode(pred);
		tarval  *tv_zeroc = get_tarval_null(mode);
		ir_node *zero_cmp = new_d_Const(dbgi, mode, tv_zeroc);

		ir_node *cmp      = new_rd_Cmp(dbgi, irg, block, pred, zero_cmp);
		ir_node *proj     = new_rd_Proj(dbgi, irg, block, cmp, mode_b,
		                                pn_Cmp_Lg);
		ir_node *vals[2]  = { one, zero };
		ir_node *psi      = new_rd_Psi(dbgi, irg, block, 1, &proj, vals,
		                               lowered_mode);

		set_irn_link(node, psi);
		return psi;
	}
	if(op == op_Proj) {
		ir_node *pred  = get_Proj_pred(node);

		assert(!is_Load(pred));
		if(is_Cmp(pred)) {
			ir_mode *mode  = get_irn_mode(pred);
			ir_node *left  = get_Cmp_left(pred);
			ir_node *right = get_Cmp_right(pred);

			if( (mode_is_int(mode) || mode_is_reference(mode)) &&
				(get_mode_size_bits(mode) < get_mode_size_bits(lowered_mode)
			         || classify_Const(right) == CNST_NULL)) {
				int      pnc      = get_Proj_proj(node);
				int      need_not = 0;
				ir_node *a        = NULL;
				ir_node *b        = NULL;

				if(pnc == pn_Cmp_Lt) {
					/* a < b  ->  (a - b) >> 31 */
					a = left;
					b = right;
				} else if(pnc == pn_Cmp_Le) {
					/* a <= b  -> ((a - b) >> 31) ^ 1 */
					a        = right;
					b        = left;
					need_not = 1;
				} else if(pnc == pn_Cmp_Gt) {
					/* a > b   -> (b - a) >> 31 */
					a = right;
					b = left;
				} else if(pnc == pn_Cmp_Ge) {
					/* a >= b   -> ((a - b) >> 31) ^ 1 */
					a        = left;
					b        = right;
					need_not = 1;
				}

				if(a != NULL) {
					tarval  *tv        = new_tarval_from_long(31, mode_Iu);
					ir_node *shift_cnt = new_d_Const(dbgi, mode_Iu, tv);

					if(mode != lowered_mode) {
						a = new_rd_Conv(dbgi, irg, block, a, lowered_mode);
						b = new_rd_Conv(dbgi, irg, block, b, lowered_mode);
					}

					res = new_rd_Sub(dbgi, irg, block, a, b, lowered_mode);
					res = new_rd_Shr(dbgi, irg, block, res, shift_cnt,
					                 lowered_mode);

					if(need_not) {
						res = create_not(dbgi, res);
					}
					set_irn_link(node, res);
					return res;
				}
			}

			{
			/* synthesize the 0/1 value */
			tarval  *tv_one  = get_tarval_one(lowered_mode);
			ir_node *one     = new_d_Const(dbgi, lowered_mode, tv_one);
			tarval  *tv_zero = get_tarval_null(lowered_mode);
			ir_node *zero    = new_d_Const(dbgi, lowered_mode, tv_zero);

			ir_node *vals[2] = { one, zero };
			ir_node *psi     = new_rd_Psi(dbgi, irg, block, 1, &node, vals,
			                              lowered_mode);

			set_irn_link(node, psi);
			return psi;
			}
		} else if(is_Proj(pred) && is_Call(get_Proj_pred(pred))) {
			return create_conv_lowered_mode(node);
		} else if(is_Proj(pred) && is_Start(get_Proj_pred(pred))) {
			return create_conv_lowered_mode(node);
		} else if (be_is_Barrier(pred)) {
			/* nothing to do */
			return node;
		}

		panic("unexpected projb: %+F (pred: %+F)", node, pred);
	}
	if(op == op_Const) {
		tarval *tv = get_Const_tarval(node);
		if(tv == get_tarval_b_true()) {
			tarval  *tv_one  = get_tarval_one(lowered_mode);
			res              = new_d_Const(dbgi, lowered_mode, tv_one);
		} else if(tv == get_tarval_b_false()) {
			tarval  *tv_zero = get_tarval_null(lowered_mode);
			res              = new_d_Const(dbgi, lowered_mode, tv_zero);
		} else {
			panic("invalid boolean const %+F", node);
		}
		set_irn_link(node, res);
		return res;
	}

	panic("didn't expect %+F to have mode_b", node);
}

static void lower_mode_b_walker(ir_node *node, void *env)
{
	int i, arity;
	(void) env;

	arity = get_irn_arity(node);
	for(i = 0; i < arity; ++i) {
		ir_node *lowered_in;
		ir_node *in = get_irn_n(node, i);
		if(get_irn_mode(in) != mode_b)
			continue;

		if(! lower_direct_cmp) {
			if(is_Cond(node)
					|| (is_Psi(node) && get_irn_mode(node) != mode_b)) {
				if(is_Proj(in)) {
					ir_node *pred = get_Proj_pred(in);
					if(is_Cmp(pred))
						continue;
				}
			}
		}

		lowered_in = lower_node(in);
		lowered_in = create_convb(lowered_in);
		set_irn_n(node, i, lowered_in);
	}
}

static void clear_links(ir_node *node, void *env)
{
	(void) env;
	set_irn_link(node, NULL);
}

void ir_lower_mode_b(ir_graph *irg, ir_mode *mode, int do_lower_direct_cmp)
{
	lowered_mode     = mode;
	lower_direct_cmp = do_lower_direct_cmp;
	set_using_irn_link(irg);
	irg_walk_graph(irg, clear_links, NULL, NULL);
	irg_walk_graph(irg, lower_mode_b_walker, NULL, NULL);
	clear_using_irn_link(irg);
}
