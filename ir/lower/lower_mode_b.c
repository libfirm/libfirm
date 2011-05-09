/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @brief       lowers operations with mode_b. The result is a graph which
 *              might still contains some convs from/to mode_b, but no
 *              operations are performed on them anymore, they are just there
 *              so modes match. A backend can safely skip all mode_b convs.
 * @author      Matthias Braun, Christoph Mallon
 * @version     $Id$
 *
 * After this pass the following should hold:
 *   - The only inputs with mode_b are for the Cond node or the
 *     Sel input of a Mux node.
 *   - The only nodes producing mode_b are: Proj(Cmp) and ConvB(X) (where X
 *     is some mode that can be converted to the lowered mode).
 *     ConvB will usually be implemented by a comparison with 0 producing some
 *     flags in the backends. It's debatable whether ConvB(X) is a good idea.
 *     Maybe we should rather introduce a Test node.
 * All other former uses should be converted to manipulations with an integer
 * mode that was specified in the pass configuration.
 */
#include "config.h"

#include <stdlib.h>
#include <stdbool.h>

#include "irnode_t.h"
#include "ircons_t.h"
#include "irflag.h"
#include "irgwalk.h"
#include "irtools.h"
#include "iredges.h"
#include "iropt_t.h"
#include "irgmod.h"
#include "tv.h"
#include "error.h"
#include "lowering.h"
#include "pdeq.h"
#include "irpass_t.h"
#include "util.h"
#include "array.h"

static const lower_mode_b_config_t *config;
static ir_type                     *lowered_type;
static ir_node                    **lowered_nodes;
static ir_node                    **check_later;

/**
 * Removes a node if its out-edge count has reached 0.
 * temporary hack until we have proper automatic dead code elimination.
 */
static void maybe_kill_node(ir_node *node)
{
	if (get_irn_n_edges(node) != 0)
		return;

	kill_node(node);
}

static ir_node *create_not(dbg_info *dbgi, ir_node *node)
{
	ir_node   *block  = get_nodes_block(node);
	ir_mode   *mode   = config->lowered_mode;
	ir_tarval *tv_one = get_mode_one(mode);
	ir_graph  *irg    = get_irn_irg(node);
	ir_node   *one    = new_rd_Const(dbgi, irg, tv_one);

	return new_rd_Eor(dbgi, block, node, one, mode);
}

static ir_node *create_convb(ir_node *node)
{
	ir_node  *block = get_nodes_block(node);
	ir_node  *conv  = new_rd_Conv(NULL, block, node, mode_b);

	return conv;
}

static ir_type *create_lowered_type(void)
{
	if (lowered_type == NULL) {
		lowered_type = new_type_primitive(config->lowered_mode);
	}
	return lowered_type;
}

ir_node *ir_create_mux_set(ir_node *cond, ir_mode *dest_mode)
{
	ir_graph  *irg     = get_irn_irg(cond);
	ir_node   *block   = get_nodes_block(cond);
	ir_tarval *tv_one  = get_mode_one(dest_mode);
	ir_node   *one     = new_r_Const(irg, tv_one);
	ir_tarval *tv_zero = get_mode_null(dest_mode);
	ir_node   *zero    = new_r_Const(irg, tv_zero);
	ir_node   *set     = new_r_Mux(block, cond, zero, one, dest_mode);
	return set;
}

ir_node *ir_create_cond_set(ir_node *cond_value, ir_mode *dest_mode)
{
	ir_node  *lower_block = part_block_edges(cond_value);
	ir_node  *upper_block = get_nodes_block(cond_value);
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
	ir_node  *one         = new_r_Const(irg, get_mode_one(dest_mode));
	ir_node  *zero        = new_r_Const(irg, get_mode_null(dest_mode));
	ir_node  *phi_in[2]   = { one, zero };
	ir_node  *phi;

	set_irn_in(lower_block, ARRAY_SIZE(lower_in), lower_in);
	phi = new_r_Phi(lower_block, ARRAY_SIZE(phi_in), phi_in, dest_mode);

	/* make sure we do visit the cond_value later... */
	ARR_APP1(ir_node*, check_later, cond_value);

	return phi;
}

static void adjust_method_type(ir_type *method_type)
{
	size_t i;
	size_t n_params;
	size_t n_res;

	n_params = get_method_n_params(method_type);
	for (i = 0; i < n_params; ++i) {
		ir_type *param = get_method_param_type(method_type, i);
		if (get_type_mode(param) == mode_b) {
			set_method_param_type(method_type, i, create_lowered_type());
		}
	}

	n_res = get_method_n_ress(method_type);
	for (i = 0; i < n_res; ++i) {
		ir_type *res_type = get_method_res_type(method_type, i);
		if (get_type_mode(res_type) == mode_b) {
			set_method_res_type(method_type, i, create_lowered_type());
		}
	}
}

static ir_node *lower_node(ir_node *node)
{
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = get_nodes_block(node);
	ir_mode  *mode  = config->lowered_mode;
	ir_node  *res   = (ir_node*)get_irn_link(node);
	ir_graph *irg;

	if (res != NULL)
		return res;

	assert(get_irn_mode(node) == mode_b);

	irg = get_irn_irg(node);
	switch (get_irn_opcode(node)) {
	case iro_Phi: {
		int       i, arity;
		ir_node **in;
		ir_node  *dummy;
		ir_node  *new_phi;

		arity = get_irn_arity(node);
		in    = ALLOCAN(ir_node*, arity);
		dummy = new_r_Dummy(irg, mode);
		for (i = 0; i < arity; ++i) {
			in[i] = dummy;
		}
		new_phi = new_r_Phi(block, arity, in, mode);
		/* FIXME This does not correctly break cycles: The Phi might not be the
		 * first in the recursion, so the caller(s) are some yet un-lowered
		 * nodes and this Phi might have them (indirectly) as operands, so they
		 * would be replaced twice. */
		set_irn_link(node, new_phi);
		ARR_APP1(ir_node*, lowered_nodes, node);

		for (i = 0; i < arity; ++i) {
			ir_node *in         = get_irn_n(node, i);
			ir_node *lowered_in = is_Bad(in) ? in : lower_node(in);

			set_irn_n(new_phi, i, lowered_in);
		}

		return new_phi;
	}

	case iro_And: {
		ir_node *lowered_left  = lower_node(get_And_left(node));
		ir_node *lowered_right = lower_node(get_And_right(node));
		res = new_rd_And(dbgi, block, lowered_left, lowered_right, mode);
		break;
	}
	case iro_Or: {
		ir_node *lowered_left  = lower_node(get_Or_left(node));
		ir_node *lowered_right = lower_node(get_Or_right(node));
		res = new_rd_Or(dbgi, block, lowered_left, lowered_right, mode);
		break;
	}
	case iro_Eor: {
		ir_node *lowered_left  = lower_node(get_Eor_left(node));
		ir_node *lowered_right = lower_node(get_Eor_right(node));
		res = new_rd_Eor(dbgi, block, lowered_left, lowered_right, mode);
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

		ir_node *and0     = new_rd_And(dbgi, block, low_cond, low_v_true, mode);
		ir_node *not_cond = create_not(dbgi, low_cond);
		ir_node *and1     = new_rd_And(dbgi, block, not_cond, low_v_false, mode);
		res = new_rd_Or(dbgi, block, and0, and1, mode);
		break;
	}

	case iro_Conv: {
		ir_node   *pred     = get_Conv_op(node);
		ir_mode   *mode     = get_irn_mode(pred);
		ir_tarval *tv_zeroc = get_mode_null(mode);
		ir_node   *zero_cmp = new_rd_Const(dbgi, irg, tv_zeroc);

		ir_node *cmp      = new_rd_Cmp(dbgi, block, pred, zero_cmp, ir_relation_less_greater);
		res = config->create_set(cmp);
		break;
	}

	case iro_Cmp: {
		ir_node    *left     = get_Cmp_left(node);
		ir_node    *right    = get_Cmp_right(node);
		ir_mode    *cmp_mode = get_irn_mode(left);
		ir_relation relation = get_Cmp_relation(node);

		if ((mode_is_int(cmp_mode) || mode_is_reference(cmp_mode)) &&
			(get_mode_size_bits(cmp_mode) < get_mode_size_bits(mode) ||
			(mode_is_signed(cmp_mode) && is_Const(right) && is_Const_null(right) && relation != ir_relation_greater))) {
			int         need_not = 0;
			ir_node    *a        = NULL;
			ir_node    *b        = NULL;
			int         bits;
			ir_tarval  *tv;
			ir_node    *shift_cnt;

			if (relation == ir_relation_less) {
				/* a < b  ->  (a - b) >> 31 */
				a = left;
				b = right;
			} else if (relation == ir_relation_less_equal) {
				/* a <= b  -> ~(a - b) >> 31 */
				a        = right;
				b        = left;
				need_not = 1;
			} else if (relation == ir_relation_greater) {
				/* a > b   -> (b - a) >> 31 */
				a = right;
				b = left;
			} else if (relation == ir_relation_greater_equal) {
				/* a >= b   -> ~(a - b) >> 31 */
				a        = left;
				b        = right;
				need_not = 1;
			} else {
				goto synth_zero_one;
			}

			bits      = get_mode_size_bits(mode);
			tv        = new_tarval_from_long(bits-1, mode_Iu);
			shift_cnt = new_rd_Const(dbgi, irg, tv);

			if (cmp_mode != mode) {
				a = new_rd_Conv(dbgi, block, a, mode);
				b = new_rd_Conv(dbgi, block, b, mode);
			}

			res = new_rd_Sub(dbgi, block, a, b, mode);
			if (need_not) {
				res = new_rd_Not(dbgi, block, res, mode);
			}
			res = new_rd_Shr(dbgi, block, res, shift_cnt, mode);
		} else {
			/* synthesize the 0/1 value */
synth_zero_one:
			res = config->create_set(node);
		}
		break;
	}

	case iro_Proj: {
		ir_node *pred = get_Proj_pred(node);

		if (is_Proj(pred) && is_Call(get_Proj_pred(pred))) {
			ir_type *type = get_Call_type(get_Proj_pred(pred));
			adjust_method_type(type);
			set_irn_mode(node, mode);
			res = node;
			goto own_replacement;
		} else if (is_Proj(pred) && is_Start(get_Proj_pred(pred))) {
			ir_graph  *irg    = get_irn_irg(node);
			ir_entity *entity = get_irg_entity(irg);
			ir_type   *type   = get_entity_type(entity);
			adjust_method_type(type);
			set_irn_mode(node, mode);
			res = node;
			goto own_replacement;
		} else {
			panic("unexpected projb: %+F (pred: %+F)", node, pred);
		}
		break;
	}

	case iro_Const: {
		ir_tarval *tv = get_Const_tarval(node);
		if (tv == get_tarval_b_true()) {
			ir_tarval *tv_one = get_mode_one(mode);
			res               = new_rd_Const(dbgi, irg, tv_one);
		} else if (tv == get_tarval_b_false()) {
			ir_tarval *tv_zero = get_mode_null(mode);
			res                = new_rd_Const(dbgi, irg, tv_zero);
		} else {
			panic("invalid boolean const %+F", node);
		}
		break;
	}

	case iro_Unknown:
		res = new_r_Unknown(irg, mode);
		break;

	default:
		panic("didn't expect %+F to have mode_b", node);
	}

	ARR_APP1(ir_node*, lowered_nodes, node);
own_replacement:
	set_irn_link(node, res);
	return res;
}

static void lower_mode_b_walker(ir_node *node, void *env)
{
	int i, arity;
	bool changed = false;

	arity = get_irn_arity(node);
	for (i = 0; i < arity; ++i) {
		ir_node *lowered_in;
		ir_node *in = get_irn_n(node, i);
		if (get_irn_mode(in) != mode_b)
			continue;

		if (! config->lower_direct_cmp) {
			/* Cmp as input for Cond and Mux nodes needs no changes.
			   (Mux with mode_b is an exception as it gets replaced by and/or
			    anyway so we still lower the inputs then) */
			if (is_Cond(node) ||
			    (is_Mux(node) && get_irn_mode(node) != mode_b)) {
				if (is_Cmp(in)) {
					continue;
				}
			}
		}

		lowered_in = lower_node(in);

		if (is_Call(node)) {
			ir_type *type = get_Call_type(node);
			adjust_method_type(type);
		} else if (is_Cond(node) || (is_Mux(node) && i == 0)) {
			lowered_in = create_convb(lowered_in);
		}
		set_irn_n(node, i, lowered_in);
		changed = true;
	}
	if (changed) {
		bool *global_changed = (bool*)env;
		*global_changed = true;
		add_identities(node);
	}
}

void ir_lower_mode_b(ir_graph *irg, const lower_mode_b_config_t *nconfig)
{
	ir_entity *entity  = get_irg_entity(irg);
	ir_type   *type    = get_entity_type(entity);
	bool       changed = false;
	size_t     i;
	size_t     n;

	config        = nconfig;
	lowered_nodes = NEW_ARR_F(ir_node*, 0);
	check_later   = NEW_ARR_F(ir_node*, 0);
	lowered_type  = NULL;

	edges_assure(irg);

	/* ensure no optimisation touches muxes anymore */
	set_irg_state(irg, IR_GRAPH_STATE_KEEP_MUX | IR_GRAPH_STATE_BCONV_ALLOWED);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);

	adjust_method_type(type);

	irg_walk_graph(irg, firm_clear_link, NULL, NULL);
	irg_walk_graph(irg, lower_mode_b_walker, NULL, &changed);

	for (i = 0, n = ARR_LEN(check_later); i < n; ++i) {
		ir_node *node = check_later[i];
		irg_walk_core(node, lower_mode_b_walker, NULL, &changed);
	}

	for (i = 0, n = ARR_LEN(lowered_nodes); i < n; ++i) {
		ir_node *node = lowered_nodes[i];
		maybe_kill_node(node);
	}
	DEL_ARR_F(check_later);
	DEL_ARR_F(lowered_nodes);

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	if (changed) {
		/* lowering might create new blocks, so be sure to handle this */
		set_irg_extblk_inconsistent(irg);
		set_irg_doms_inconsistent(irg);
		set_irg_loopinfo_inconsistent(irg);
	}
}
