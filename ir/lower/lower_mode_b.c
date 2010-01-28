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
 *     flags in the backends. It's debatable wether ConvB(X) is a goode idea.
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
#include "tv.h"
#include "error.h"
#include "lowering.h"
#include "pdeq.h"
#include "irpass_t.h"

static lower_mode_b_config_t  config;
static ir_type               *lowered_type  = NULL;
static pdeq                  *lowered_nodes = NULL;

/**
 * Removes a node if its out-edge count has reached 0.
 * temporary hack until we have proper automatic dead code elimination.
 */
static void maybe_kill_node(ir_node *node)
{
	ir_graph *irg;
	int       i, arity;

	if (get_irn_n_edges(node) != 0)
		return;

	irg = get_irn_irg(node);

	assert(!is_Bad(node));

	arity = get_irn_arity(node);
	for (i = 0; i < arity; ++i) {
		set_irn_n(node, i, new_Bad());
	}
	set_nodes_block(node, new_Bad());

	edges_node_deleted(node, irg);
}

static ir_node *create_not(dbg_info *dbgi, ir_node *node)
{
	ir_node  *block  = get_nodes_block(node);
	ir_mode  *mode   = config.lowered_mode;
	tarval   *tv_one = get_tarval_one(mode);
	ir_node  *one    = new_d_Const(dbgi, tv_one);

	return new_rd_Eor(dbgi,	block, node, one, mode);
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
		lowered_type = new_type_primitive(config.lowered_mode);
	}
	return lowered_type;
}

/**
 * creates a "set" node that produces a 0 or 1 based on a Cmp result
 */
static ir_node *create_set(ir_node *node)
{
	dbg_info *dbgi    = get_irn_dbg_info(node);
	ir_mode  *mode    = config.lowered_set_mode;
	tarval   *tv_one  = get_tarval_one(mode);
	ir_node  *one     = new_d_Const(dbgi, tv_one);
	ir_node  *block   = get_nodes_block(node);
	tarval   *tv_zero = get_tarval_null(mode);
	ir_node  *zero    = new_d_Const(dbgi, tv_zero);

	ir_node *set      = new_rd_Mux(dbgi, block, node, zero, one, mode);

	if (mode != config.lowered_mode) {
		set = new_r_Conv(block, set, config.lowered_mode);
	}

	return set;
}

static void adjust_method_type(ir_type *method_type)
{
	int i;
	int n_params;
	int n_res;

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
	ir_mode  *mode  = config.lowered_mode;
	ir_node  *res;

	assert(get_irn_mode(node) == mode_b);

	res = get_irn_link(node);
	if (res != NULL)
		return res;

	switch (get_irn_opcode(node)) {
	case iro_Phi: {
		int       i, arity;
		ir_node **in;
		ir_node  *unknown, *new_phi;

		arity   = get_irn_arity(node);
		in      = ALLOCAN(ir_node*, arity);
		unknown = new_Unknown(mode);
		for (i = 0; i < arity; ++i) {
			in[i] = unknown;
		}
		new_phi = new_r_Phi(block, arity, in, mode);
		set_irn_link(node, new_phi);
		pdeq_putr(lowered_nodes, node);

		for (i = 0; i < arity; ++i) {
			ir_node *in     = get_irn_n(node, i);
			ir_node *low_in = lower_node(in);

			set_irn_n(new_phi, i, low_in);
		}

		return new_phi;
	}

	case iro_And:
	case iro_Or:
	case iro_Eor: {
		int i, arity;
		ir_node *copy = exact_copy(node);

		arity = get_irn_arity(node);
		for (i = 0; i < arity; ++i) {
			ir_node *in     = get_irn_n(node, i);
			ir_node *low_in = lower_node(in);

			set_irn_n(copy, i, low_in);
		}
		set_irn_mode(copy, mode);

		set_irn_link(node, copy);
		pdeq_putr(lowered_nodes, node);
		return copy;
	}

	case iro_Not: {
		ir_node *op     = get_Not_op(node);
		ir_node *low_op = lower_node(op);

		res = create_not(dbgi, low_op);
		set_irn_link(node, res);
		pdeq_putr(lowered_nodes, node);
		return res;
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
		ir_node *or       = new_rd_Or(dbgi, block, and0, and1, mode);

		set_irn_link(node, or);
		pdeq_putr(lowered_nodes, node);
		return or;
	}

	case iro_Conv: {
		ir_node *pred     = get_Conv_op(node);
		ir_mode *mode     = get_irn_mode(pred);
		tarval  *tv_zeroc = get_tarval_null(mode);
		ir_node *zero_cmp = new_d_Const(dbgi, tv_zeroc);
		ir_node *set;

		ir_node *cmp      = new_rd_Cmp(dbgi, block, pred, zero_cmp);
		ir_node *proj     = new_rd_Proj(dbgi, block, cmp, mode_b, pn_Cmp_Lg);
		set = create_set(proj);

		set_irn_link(node, set);
		pdeq_putr(lowered_nodes, node);
		return set;
	}

	case iro_Proj: {
		ir_node *pred = get_Proj_pred(node);

		if (is_Cmp(pred)) {
			ir_node *left  = get_Cmp_left(pred);
			ir_node *right = get_Cmp_right(pred);
			ir_mode *cmp_mode  = get_irn_mode(left);
			ir_node *set;

			if ((mode_is_int(cmp_mode) || mode_is_reference(cmp_mode)) && (
						get_mode_size_bits(cmp_mode) < get_mode_size_bits(mode) ||
						(mode_is_signed(cmp_mode) && is_Const(right) && is_Const_null(right))
					)) {
				int      pnc      = get_Proj_proj(node);
				int      need_not = 0;
				ir_node *a        = NULL;
				ir_node *b        = NULL;

				if (pnc == pn_Cmp_Lt) {
					/* a < b  ->  (a - b) >> 31 */
					a = left;
					b = right;
				} else if (pnc == pn_Cmp_Le) {
					/* a <= b  -> ~(a - b) >> 31 */
					a        = right;
					b        = left;
					need_not = 1;
				} else if (pnc == pn_Cmp_Gt) {
					/* a > b   -> (b - a) >> 31 */
					a = right;
					b = left;
				} else if (pnc == pn_Cmp_Ge) {
					/* a >= b   -> ~(a - b) >> 31 */
					a        = left;
					b        = right;
					need_not = 1;
				}

				if (a != NULL) {
					int      bits      = get_mode_size_bits(mode);
					tarval  *tv        = new_tarval_from_long(bits-1, mode_Iu);
					ir_node *shift_cnt = new_d_Const(dbgi, tv);

					if (cmp_mode != mode) {
						a = new_rd_Conv(dbgi, block, a, mode);
						b = new_rd_Conv(dbgi, block, b, mode);
					}

					res = new_rd_Sub(dbgi, block, a, b, mode);
					if (need_not) {
						res = new_rd_Not(dbgi, block, res, mode);
					}
					res = new_rd_Shr(dbgi, block, res, shift_cnt, mode);

					set_irn_link(node, res);
					pdeq_putr(lowered_nodes, node);
					return res;
				}
			}

			/* synthesize the 0/1 value */
			set = create_set(node);
			set_irn_link(node, set);
			pdeq_putr(lowered_nodes, node);
			return set;
		} else if (is_Proj(pred) && is_Call(get_Proj_pred(pred))) {
			ir_type   *type   = get_Call_type(get_Proj_pred(pred));
			adjust_method_type(type);
			set_irn_mode(node, mode);
			return node;
		} else if (is_Proj(pred) && is_Start(get_Proj_pred(pred))) {
			ir_entity *entity = get_irg_entity(current_ir_graph);
			ir_type   *type   = get_entity_type(entity);
			adjust_method_type(type);
			set_irn_mode(node, mode);
			return node;
		}

		panic("unexpected projb: %+F (pred: %+F)", node, pred);
	}

	case iro_Const: {
		tarval *tv = get_Const_tarval(node);
		if (tv == get_tarval_b_true()) {
			tarval  *tv_one  = get_tarval_one(mode);
			res              = new_d_Const(dbgi, tv_one);
		} else if (tv == get_tarval_b_false()) {
			tarval  *tv_zero = get_tarval_null(mode);
			res              = new_d_Const(dbgi, tv_zero);
		} else {
			panic("invalid boolean const %+F", node);
		}
		set_irn_link(node, res);
		pdeq_putr(lowered_nodes, node);
		return res;
	}

	case iro_Unknown:
		return new_Unknown(mode);

	default:
		panic("didn't expect %+F to have mode_b", node);
	}
}

static void lower_mode_b_walker(ir_node *node, void *env)
{
	int i, arity;
	bool changed = 0;
	(void) env;

	arity = get_irn_arity(node);
	for (i = 0; i < arity; ++i) {
		ir_node *lowered_in;
		ir_node *in = get_irn_n(node, i);
		if (get_irn_mode(in) != mode_b)
			continue;

		if (! config.lower_direct_cmp) {
			/* Proj(Cmp) as input for Cond and Mux nodes needs no changes.
			   (Mux with mode_b is an exception as it gets replaced by and/or
			    anyway so we still lower the inputs then) */
			if (is_Cond(node) ||
			    (is_Mux(node) && get_irn_mode(node) != mode_b)) {
				if (is_Proj(in)) {
					ir_node *pred = get_Proj_pred(in);
					if (is_Cmp(pred))
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
		add_identities(current_ir_graph->value_table, node);
	}
}

void ir_lower_mode_b(ir_graph *irg, const lower_mode_b_config_t *nconfig)
{
	ir_entity *entity = get_irg_entity(irg);
	ir_type   *type   = get_entity_type(entity);

	config        = *nconfig;
	lowered_nodes = new_pdeq();
	lowered_type  = NULL;

	/* ensure no optimisation touches muxes anymore */
	set_irg_state(irg, IR_GRAPH_STATE_KEEP_MUX);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);

	adjust_method_type(type);

	set_opt_allow_conv_b(0);
	irg_walk_graph(irg, firm_clear_link, NULL, NULL);
	irg_walk_graph(irg, lower_mode_b_walker, NULL, NULL);

	while(!pdeq_empty(lowered_nodes)) {
		ir_node *node = (ir_node*) pdeq_getr(lowered_nodes);
		maybe_kill_node(node);
	}
	del_pdeq(lowered_nodes);

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
}

struct pass_t {
	ir_graph_pass_t             pass;
	const lower_mode_b_config_t *config;
};

/**
 * Wrapper to run ir_lower_mode_b() as an ir_graph pass
 */
static int pass_wrapper(ir_graph *irg, void *context)
{
	struct pass_t *pass = context;

	ir_lower_mode_b(irg, pass->config);
	return 0;
}

ir_graph_pass_t *ir_lower_mode_b_pass(
	const char *name, const lower_mode_b_config_t *config)
{
	struct pass_t *pass = XMALLOCZ(struct pass_t);

	pass->config = config;
	return def_graph_pass_constructor(
		&pass->pass, name ? name : "lower_mode_b", pass_wrapper);
}
