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
 * @brief       This file implements the mapping of 64Bit intrinsic
 *              functions to code or library calls.
 * @author      Michael Beck
 * @version     $Id$
 */
#include "config.h"

#include "iredges.h"
#include "irgmod.h"
#include "irop.h"
#include "irnode_t.h"
#include "ircons.h"
#include "irprog_t.h"
#include "lowering.h"
#include "array.h"
#include "error.h"

#include "ia32_new_nodes.h"
#include "bearch_ia32_t.h"
#include "gen_ia32_regalloc_if.h"

/** The array of all intrinsics that must be mapped. */
static i_record *intrinsics;

/** An array to cache all entities. */
static ir_entity *i_ents[iro_Last + 1];

/*
 * Maps all intrinsic calls that the backend support
 * and map all instructions the backend did not support
 * to runtime calls.
 */
void ia32_handle_intrinsics(void) {
	if (intrinsics && ARR_LEN(intrinsics) > 0) {
		lower_intrinsics(intrinsics, ARR_LEN(intrinsics), /*part_block_used=*/1);
	}
}

#define BINOP_Left_Low   0
#define BINOP_Left_High  1
#define BINOP_Right_Low  2
#define BINOP_Right_High 3

/**
 * Reroute edges from the pn_Call_T_result proj of a call.
 *
 * @param proj   the pn_Call_T_result Proj
 * @param l_res  the lower 32 bit result
 * @param h_res  the upper 32 bit result or NULL
 * @param irg    the graph to replace on
 */
static void reroute_result(ir_node *proj, ir_node *l_res, ir_node *h_res, ir_graph *irg) {
	const ir_edge_t *edge, *next;

	foreach_out_edge_safe(proj, edge, next) {
		ir_node *proj = get_edge_src_irn(edge);
		long    pn    = get_Proj_proj(proj);

		if (pn == 0) {
			edges_reroute(proj, l_res, irg);
		} else if (pn == 1 && h_res != NULL) {
			edges_reroute(proj, h_res, irg);
		} else {
			panic("Unsupported Result-Proj from Call found");
		}
	}
}

/**
 * Replace a call be a tuple of l_res, h_res.
 *
 * @param call   the call node to replace
 * @param l_res  the lower 32 bit result
 * @param h_res  the upper 32 bit result or NULL
 * @param irg    the graph to replace on
 * @param block  the block to replace on (always the call block)
 */
static void resolve_call(ir_node *call, ir_node *l_res, ir_node *h_res, ir_graph *irg, ir_node *block) {
	ir_node *jmp, *res, *in[2];
	ir_node *bad   = get_irg_bad(irg);
	ir_node *nomem = get_irg_no_mem(irg);
	int     old_cse;

	if (edges_activated(irg)) {
		/* use rerouting to prevent some warning in the backend */
		const ir_edge_t *edge, *next;

		foreach_out_edge_safe(call, edge, next) {
			ir_node *proj = get_edge_src_irn(edge);
			pn_Call pn    = get_Proj_proj(proj);

			switch (pn) {
			case pn_Call_X_regular:
				/* Beware:
				 * We do not check here if this call really has exception and regular Proj's.
				 * new_r_Jmp might than be CSEd with the real exit jmp and then bad things happen
				 * (in movgen.c from 186.crafty for example).
				 * So be sure the newly created Jmp cannot CSE.
				 */
				old_cse = get_opt_cse();
				set_opt_cse(0);
				jmp = new_r_Jmp(block);
				set_opt_cse(old_cse);
				edges_reroute(proj, jmp, irg);
				break;

			case pn_Call_X_except:
			case pn_Call_P_value_res_base:
				/* should not happen here */
				edges_reroute(proj, bad, irg);
				break;
			case pn_Call_M_except:
				/* should not happen here */
				edges_reroute(proj, nomem, irg);
				break;
			case pn_Call_T_result:
				reroute_result(proj, l_res, h_res, irg);
				break;
			default:
				panic("Wrong Proj from Call");
			}
			kill_node(proj);
		}
		kill_node(call);
	} else {
		/* no edges, build Tuple */
		if (h_res == NULL)
			res = l_res;
		else {
			in[0] = l_res;
			in[1] = h_res;
			res = new_r_Tuple(block, 2, in);
		}

		turn_into_tuple(call, pn_Call_max);
		set_Tuple_pred(call, pn_Call_M_regular,        nomem);
		/*
		 * Beware:
		 * We do not check here if this call really has exception and regular Proj's.
		 * new_r_Jmp might than be CSEd with the real exit jmp and then bad things happen
		 * (in movgen.c from 186.crafty for example).
		 * So be sure the newly created Jmp cannot CSE.
		 */
		old_cse = get_opt_cse();
		set_opt_cse(0);
		jmp = new_r_Jmp(block);
		set_opt_cse(old_cse);

		set_Tuple_pred(call, pn_Call_X_regular,        jmp);
		set_Tuple_pred(call, pn_Call_X_except,         bad);
		set_Tuple_pred(call, pn_Call_T_result,         res);
		set_Tuple_pred(call, pn_Call_M_except,         nomem);
		set_Tuple_pred(call, pn_Call_P_value_res_base, bad);
	}
}

/**
 * Map an Add (a_l, a_h, b_l, b_h)
 */
static int map_Add(ir_node *call, void *ctx) {
	dbg_info *dbg        = get_irn_dbg_info(call);
	ir_node  *block      = get_nodes_block(call);
	ir_node  **params    = get_Call_param_arr(call);
	ir_type  *method     = get_Call_type(call);
	ir_node  *a_l        = params[BINOP_Left_Low];
	ir_node  *a_h        = params[BINOP_Left_High];
	ir_node  *b_l        = params[BINOP_Right_Low];
	ir_node  *b_h        = params[BINOP_Right_High];
	ir_mode  *l_mode     = get_type_mode(get_method_res_type(method, 0));
	ir_mode  *h_mode     = get_type_mode(get_method_res_type(method, 1));
	ir_mode  *mode_flags = ia32_reg_classes[CLASS_ia32_flags].mode;
	ir_node  *add_low, *add_high, *flags;
	ir_node  *l_res, *h_res;
	(void) ctx;

	/* l_res = a_l + b_l */
	/* h_res = a_h + b_h + carry */

	add_low  = new_bd_ia32_l_Add(dbg, block, a_l, b_l, mode_T);
	flags    = new_r_Proj(block, add_low, mode_flags, pn_ia32_flags);
	add_high = new_bd_ia32_l_Adc(dbg, block, a_h, b_h, flags, h_mode);

	l_res = new_r_Proj(block, add_low, l_mode, pn_ia32_res);
	h_res = add_high;

	resolve_call(call, l_res, h_res, current_ir_graph, block);
	return 1;
}

/**
 * Map a Sub (a_l, a_h, b_l, b_h)
 */
static int map_Sub(ir_node *call, void *ctx)
{
	dbg_info *dbg        = get_irn_dbg_info(call);
	ir_node  *block      = get_nodes_block(call);
	ir_node  **params    = get_Call_param_arr(call);
	ir_type  *method     = get_Call_type(call);
	ir_node  *a_l        = params[BINOP_Left_Low];
	ir_node  *a_h        = params[BINOP_Left_High];
	ir_node  *b_l        = params[BINOP_Right_Low];
	ir_node  *b_h        = params[BINOP_Right_High];
	ir_mode  *l_mode     = get_type_mode(get_method_res_type(method, 0));
	ir_mode  *h_mode     = get_type_mode(get_method_res_type(method, 1));
	ir_mode  *mode_flags = ia32_reg_classes[CLASS_ia32_flags].mode;
	ir_node  *sub_low, *sub_high, *flags;
	ir_node  *l_res, *h_res;
	(void) ctx;

	/* l_res = a_l - b_l */
	/* h_res = a_h - b_h - carry */

	sub_low  = new_bd_ia32_l_Sub(dbg, block, a_l, b_l, mode_T);
	flags    = new_r_Proj(block, sub_low, mode_flags, pn_ia32_flags);
	sub_high = new_bd_ia32_l_Sbb(dbg, block, a_h, b_h, flags, h_mode);

	l_res = new_r_Proj( block, sub_low, l_mode, pn_ia32_res);
	h_res = sub_high;

	resolve_call(call, l_res, h_res, current_ir_graph, block);
	return 1;
}

/**
 * Map a Shl (a_l, a_h, count)
 */
static int map_Shl(ir_node *call, void *ctx) {
	ir_graph *irg     = current_ir_graph;
	dbg_info *dbg     = get_irn_dbg_info(call);
	ir_node  *block   = get_nodes_block(call);
	ir_node  **params = get_Call_param_arr(call);
	ir_type  *method  = get_Call_type(call);
	ir_node  *a_l     = params[BINOP_Left_Low];
	ir_node  *a_h     = params[BINOP_Left_High];
	ir_node  *cnt     = params[BINOP_Right_Low];
	ir_mode  *l_mode  = get_type_mode(get_method_res_type(method, 0));
	ir_mode  *h_mode  = get_type_mode(get_method_res_type(method, 1));
	ir_mode  *c_mode;
	ir_node  *l_res, *h_res, *irn, *cond, *upper, *n_block, *l1, *l2, *h1, *h2, *in[2];
	(void) ctx;

	if (is_Const(cnt)) {
		/* the shift count is a const, create better code */
		tarval *tv = get_Const_tarval(cnt);

		if (tarval_cmp(tv, new_tarval_from_long(32, l_mode)) & (pn_Cmp_Gt|pn_Cmp_Eq)) {
			/* simplest case: shift only the lower bits. Note that there is no
			   need to reduce the constant here, this is done by the hardware.  */
			ir_node *conv = new_rd_Conv(dbg, block, a_l, h_mode);
			h_res = new_rd_Shl(dbg, block, conv, cnt, h_mode);
			l_res = new_rd_Const(dbg, irg, get_mode_null(l_mode));

		} else {
			/* h_res = SHLD a_h, a_l, cnt */
			h_res = new_bd_ia32_l_ShlD(dbg, block, a_h, a_l, cnt, h_mode);

			/* l_res = SHL a_l, cnt */
			l_res = new_bd_ia32_l_ShlDep(dbg, block, a_l, cnt, h_res, l_mode);
		}

		resolve_call(call, l_res, h_res, irg, block);
		return 1;
	}

	part_block(call);
	upper = get_nodes_block(call);

	/* h_res = SHLD a_h, a_l, cnt */
	h1 = new_bd_ia32_l_ShlD(dbg, upper, a_h, a_l, cnt, h_mode);

	/* l_res = SHL a_l, cnt */
	l1 = new_bd_ia32_l_ShlDep(dbg, upper, a_l, cnt, h1, l_mode);

	c_mode = get_irn_mode(cnt);
	irn    = new_r_Const_long(irg, c_mode, 32);
	irn    = new_rd_And(dbg, upper, cnt, irn, c_mode);
	irn    = new_rd_Cmp(dbg, upper, irn, new_r_Const(irg, get_mode_null(c_mode)));
	irn    = new_r_Proj(upper, irn, mode_b, pn_Cmp_Eq);
	cond   = new_rd_Cond(dbg, upper, irn);

	in[0]  = new_r_Proj(upper, cond, mode_X, pn_Cond_true);
	in[1]  = new_r_Proj(upper, cond, mode_X, pn_Cond_false);

	/* the block for cnt >= 32 */
	n_block = new_rd_Block(dbg, irg, 1, &in[1]);
	h2      = new_rd_Conv(dbg, n_block, l1, h_mode);
	l2      = new_r_Const(irg, get_mode_null(l_mode));
	in[1]   = new_r_Jmp(n_block);

	set_irn_in(block, 2, in);

	in[0] = l1;
	in[1] = l2;
	l_res = new_r_Phi(block, 2, in, l_mode);
	set_Block_phis(block, l_res);

	in[0] = h1;
	in[1] = h2;
	h_res = new_r_Phi(block, 2, in, h_mode);
	set_Phi_next(l_res, h_res);
	set_Phi_next(h_res, NULL);

	/* move it down */
	set_nodes_block(call, block);
	for (irn = get_irn_link(call); irn != NULL; irn = get_irn_link(irn))
		set_nodes_block(irn, block);

	resolve_call(call, l_res, h_res, irg, block);
	return 1;
}

/**
 * Map a Shr (a_l, a_h, count)
 */
static int map_Shr(ir_node *call, void *ctx) {
	ir_graph *irg     = current_ir_graph;
	dbg_info *dbg     = get_irn_dbg_info(call);
	ir_node  *block   = get_nodes_block(call);
	ir_node  **params = get_Call_param_arr(call);
	ir_type  *method  = get_Call_type(call);
	ir_node  *a_l     = params[BINOP_Left_Low];
	ir_node  *a_h     = params[BINOP_Left_High];
	ir_node  *cnt     = params[BINOP_Right_Low];
	ir_mode  *l_mode  = get_type_mode(get_method_res_type(method, 0));
	ir_mode  *h_mode  = get_type_mode(get_method_res_type(method, 1));
	ir_mode  *c_mode;
	ir_node  *l_res, *h_res, *irn, *cond, *upper, *n_block, *l1, *l2, *h1, *h2, *in[2];
	(void) ctx;

	if (is_Const(cnt)) {
		/* the shift count is a const, create better code */
		tarval *tv = get_Const_tarval(cnt);

		if (tarval_cmp(tv, new_tarval_from_long(32, l_mode)) & (pn_Cmp_Gt|pn_Cmp_Eq)) {
			/* simplest case: shift only the higher bits. Note that there is no
			   need to reduce the constant here, this is done by the hardware.  */
			ir_node *conv = new_rd_Conv(dbg, block, a_h, l_mode);
			h_res = new_rd_Const(dbg, irg, get_mode_null(h_mode));
			l_res = new_rd_Shr(dbg, block, conv, cnt, l_mode);
		} else {
			/* l_res = SHRD a_h:a_l, cnt */
			l_res = new_bd_ia32_l_ShrD(dbg, block, a_l, a_h, cnt, l_mode);

			/* h_res = SHR a_h, cnt */
			h_res = new_bd_ia32_l_ShrDep(dbg, block, a_h, cnt, l_res, h_mode);
		}
		resolve_call(call, l_res, h_res, irg, block);
		return 1;
	}

	part_block(call);
	upper = get_nodes_block(call);

	/* l_res = SHRD a_h:a_l, cnt */
	l1 = new_bd_ia32_l_ShrD(dbg, upper, a_l, a_h, cnt, l_mode);

	/* h_res = SHR a_h, cnt */
	h1 = new_bd_ia32_l_ShrDep(dbg, upper, a_h, cnt, l1, h_mode);

	c_mode = get_irn_mode(cnt);
	irn    = new_r_Const_long(irg, c_mode, 32);
	irn    = new_rd_And(dbg, upper, cnt, irn, c_mode);
	irn    = new_rd_Cmp(dbg, upper, irn, new_r_Const(irg, get_mode_null(c_mode)));
	irn    = new_r_Proj(upper, irn, mode_b, pn_Cmp_Eq);
	cond   = new_rd_Cond(dbg, upper, irn);

	in[0]  = new_r_Proj(upper, cond, mode_X, pn_Cond_true);
	in[1]  = new_r_Proj(upper, cond, mode_X, pn_Cond_false);

	/* the block for cnt >= 32 */
	n_block = new_rd_Block(dbg, irg, 1, &in[1]);
	l2      = new_rd_Conv(dbg, n_block, h1, l_mode);
	h2      = new_r_Const(irg, get_mode_null(h_mode));
	in[1]   = new_r_Jmp(n_block);

	set_irn_in(block, 2, in);

	in[0] = l1;
	in[1] = l2;
	l_res = new_r_Phi(block, 2, in, l_mode);
	set_Block_phis(block, l_res);

	in[0] = h1;
	in[1] = h2;
	h_res = new_r_Phi(block, 2, in, h_mode);
	set_Phi_next(l_res, h_res);
	set_Phi_next(h_res, NULL);

	/* move it down */
	set_nodes_block(call, block);
	for (irn = get_irn_link(call); irn != NULL; irn = get_irn_link(irn))
		set_nodes_block(irn, block);

	resolve_call(call, l_res, h_res, irg, block);
	return 1;
}

/**
 * Map a Shrs (a_l, a_h, count)
 */
static int map_Shrs(ir_node *call, void *ctx) {
	ir_graph *irg     = current_ir_graph;
	dbg_info *dbg     = get_irn_dbg_info(call);
	ir_node  *block   = get_nodes_block(call);
	ir_node  **params = get_Call_param_arr(call);
	ir_type  *method  = get_Call_type(call);
	ir_node  *a_l     = params[BINOP_Left_Low];
	ir_node  *a_h     = params[BINOP_Left_High];
	ir_node  *cnt     = params[BINOP_Right_Low];
	ir_mode  *l_mode  = get_type_mode(get_method_res_type(method, 0));
	ir_mode  *h_mode  = get_type_mode(get_method_res_type(method, 1));
	ir_mode  *c_mode;
	ir_node  *l_res, *h_res, *irn, *cond, *upper, *n_block, *l1, *l2, *h1, *h2, *in[2];
	(void) ctx;

	if (is_Const(cnt)) {
		/* the shift count is a const, create better code */
		tarval *tv = get_Const_tarval(cnt);

		if (tarval_cmp(tv, new_tarval_from_long(32, l_mode)) & (pn_Cmp_Gt|pn_Cmp_Eq)) {
			/* simplest case: shift only the higher bits. Note that there is no
			   need to reduce the constant here, this is done by the hardware.  */
			ir_node *conv    = new_rd_Conv(dbg, block, a_h, l_mode);
			ir_mode *c_mode  = get_irn_mode(cnt);

			h_res = new_rd_Shrs(dbg, block, a_h, new_r_Const_long(irg, c_mode, 31), h_mode);
			l_res = new_rd_Shrs(dbg, block, conv, cnt, l_mode);
		} else {
			/* l_res = SHRD a_h:a_l, cnt */
			l_res = new_bd_ia32_l_ShrD(dbg, block, a_l, a_h, cnt, l_mode);

			/* h_res = SAR a_h, cnt */
			h_res = new_bd_ia32_l_SarDep(dbg, block, a_h, cnt, l_res, h_mode);
		}
		resolve_call(call, l_res, h_res, irg, block);
		return 1;
	}

	part_block(call);
	upper = get_nodes_block(call);

	/* l_res = SHRD a_h:a_l, cnt */
	l1 = new_bd_ia32_l_ShrD(dbg, upper, a_l, a_h, cnt, l_mode);

	/* h_res = SAR a_h, cnt */
	h1 = new_bd_ia32_l_SarDep(dbg, upper, a_h, cnt, l1, h_mode);

	c_mode = get_irn_mode(cnt);
	irn    = new_r_Const_long(irg, c_mode, 32);
	irn    = new_rd_And(dbg, upper, cnt, irn, c_mode);
	irn    = new_rd_Cmp(dbg, upper, irn, new_r_Const(irg, get_mode_null(c_mode)));
	irn    = new_r_Proj(upper, irn, mode_b, pn_Cmp_Eq);
	cond   = new_rd_Cond(dbg, upper, irn);

	in[0]  = new_r_Proj(upper, cond, mode_X, pn_Cond_true);
	in[1]  = new_r_Proj(upper, cond, mode_X, pn_Cond_false);

	/* the block for cnt >= 32 */
	n_block = new_rd_Block(dbg, irg, 1, &in[1]);
	l2      = new_rd_Conv(dbg, n_block, h1, l_mode);
	h2      = new_rd_Shrs(dbg, n_block, a_h, new_r_Const_long(irg, c_mode, 31), h_mode);
	in[1]   = new_r_Jmp(n_block);

	set_irn_in(block, 2, in);

	in[0] = l1;
	in[1] = l2;
	l_res = new_r_Phi(block, 2, in, l_mode);
	set_Block_phis(block, l_res);

	in[0] = h1;
	in[1] = h2;
	h_res = new_r_Phi(block, 2, in, h_mode);
	set_Phi_next(l_res, h_res);
	set_Phi_next(h_res, NULL);

	/* move it down */
	set_nodes_block(call, block);
	for (irn = get_irn_link(call); irn != NULL; irn = get_irn_link(irn))
		set_nodes_block(irn, block);

	resolve_call(call, l_res, h_res, irg, block);
	return 1;
}

/**
 * Checks where node high is a sign extension of low.
 */
static int is_sign_extend(ir_node *low, ir_node *high)
{
	if (is_Shrs(high)) {
		ir_node *high_l;
		ir_node *high_r;
		tarval  *shift_count;

		high_r = get_Shrs_right(high);
		if (!is_Const(high_r)) return 0;

		shift_count = get_Const_tarval(high_r);
		if (!tarval_is_long(shift_count))       return 0;
		if (get_tarval_long(shift_count) != 31) return 0;

		high_l = get_Shrs_left(high);

		if (is_Conv(low)    && get_Conv_op(low)    == high_l) return 1;
		if (is_Conv(high_l) && get_Conv_op(high_l) == low)    return 1;
	} else if (is_Const(low) && is_Const(high)) {
		tarval *tl = get_Const_tarval(low);
		tarval *th = get_Const_tarval(high);

		if (tarval_is_long(th) && tarval_is_long(tl)) {
			long l = get_tarval_long(tl);
			long h = get_tarval_long(th);

			return (h == 0  && l >= 0) || (h == -1 && l <  0);
		}
	}

	return 0;
}

/**
 * Map a Mul (a_l, a_h, b_l, b_h)
 */
static int map_Mul(ir_node *call, void *ctx) {
	dbg_info *dbg     = get_irn_dbg_info(call);
	ir_node  *block   = get_nodes_block(call);
	ir_node  **params = get_Call_param_arr(call);
	ir_type  *method  = get_Call_type(call);
	ir_node  *a_l     = params[BINOP_Left_Low];
	ir_node  *a_h     = params[BINOP_Left_High];
	ir_node  *b_l     = params[BINOP_Right_Low];
	ir_node  *b_h     = params[BINOP_Right_High];
	ir_mode  *l_mode  = get_type_mode(get_method_res_type(method, 0));
	ir_mode  *h_mode  = get_type_mode(get_method_res_type(method, 1));
	ir_node  *l_res, *h_res, *mul, *pEDX, *add;
	(void) ctx;

	/*
		EDX:EAX = a_l * b_l
		l_res   = EAX

		t1 = b_l * a_h
		t2 = t1 + EDX
		t3 = a_l * b_h
		h_res = t2 + t3
	*/

	/* handle the often used case of 32x32=64 mul */
	if (is_sign_extend(a_l, a_h) && is_sign_extend(b_l, b_h)) {
		mul   = new_bd_ia32_l_IMul(dbg, block, a_l, b_l);
		h_res = new_rd_Proj(dbg, block, mul, h_mode, pn_ia32_l_IMul_res_high);
		l_res = new_rd_Proj(dbg, block, mul, l_mode, pn_ia32_l_IMul_res_low);
	} else {
		/* note that zero extension is handled hare efficiently */
		mul   = new_bd_ia32_l_Mul(dbg, block, a_l, b_l);
		pEDX  = new_rd_Proj(dbg, block, mul, h_mode, pn_ia32_l_Mul_res_high);
		l_res = new_rd_Proj(dbg, block, mul, l_mode, pn_ia32_l_Mul_res_low);

		b_l   = new_rd_Conv(dbg, block, b_l, h_mode);
		mul   = new_rd_Mul( dbg, block, a_h, b_l, h_mode);
		add   = new_rd_Add( dbg, block, mul, pEDX, h_mode);
		a_l   = new_rd_Conv(dbg, block, a_l, h_mode);
		mul   = new_rd_Mul( dbg, block, a_l, b_h, h_mode);
		h_res = new_rd_Add( dbg, block, add, mul, h_mode);
	}
	resolve_call(call, l_res, h_res, current_ir_graph, block);

	return 1;
}

/**
 * Map a Minus (a_l, a_h)
 */
static int map_Minus(ir_node *call, void *ctx) {
	dbg_info *dbg     = get_irn_dbg_info(call);
	ir_node  *block   = get_nodes_block(call);
	ir_node  **params = get_Call_param_arr(call);
	ir_type  *method  = get_Call_type(call);
	ir_node  *a_l     = params[BINOP_Left_Low];
	ir_node  *a_h     = params[BINOP_Left_High];
	ir_mode  *l_mode  = get_type_mode(get_method_res_type(method, 0));
	ir_mode  *h_mode  = get_type_mode(get_method_res_type(method, 1));
	ir_node  *l_res, *h_res, *res;
	(void) ctx;

	res   = new_bd_ia32_Minus64Bit(dbg, block, a_l, a_h);
	l_res = new_r_Proj(block, res, l_mode, pn_ia32_Minus64Bit_low_res);
	h_res = new_r_Proj(block, res, h_mode, pn_ia32_Minus64Bit_high_res);

	resolve_call(call, l_res, h_res, current_ir_graph, block);

	return 1;
}

/**
 * Map a Abs (a_l, a_h)
 */
static int map_Abs(ir_node *call, void *ctx) {
	dbg_info *dbg        = get_irn_dbg_info(call);
	ir_node  *block      = get_nodes_block(call);
	ir_node  **params    = get_Call_param_arr(call);
	ir_type  *method     = get_Call_type(call);
	ir_node  *a_l        = params[BINOP_Left_Low];
	ir_node  *a_h        = params[BINOP_Left_High];
	ir_mode  *l_mode     = get_type_mode(get_method_res_type(method, 0));
	ir_mode  *h_mode     = get_type_mode(get_method_res_type(method, 1));
	ir_mode  *mode_flags = ia32_reg_classes[CLASS_ia32_flags].mode;
	ir_node  *l_res, *h_res, *sign, *sub_l, *sub_h;
	ir_node  *sign_l;
	ir_node  *l_sub;
	ir_node  *flags;
	(void) ctx;

	/*
		Code inspired by gcc output :) (although gcc doubles the
		operation for t1 as t2 and uses t1 for operations with low part
		and t2 for operations with high part which is actually unnecessary
		because t1 and t2 represent the same value)

		t1    = SHRS a_h, 31
		t2    = a_l ^ t1
		t3    = a_h ^ t1
		l_res = t2 - t1
		h_res = t3 - t1 - carry

	*/

	/* TODO: give a hint to the backend somehow to not create a cltd here... */
	sign   = new_rd_Shrs(dbg, block, a_h, new_Const_long(l_mode, 31), h_mode);
	sign_l = new_rd_Conv(dbg, block, sign, l_mode);
	sub_l  = new_rd_Eor(dbg, block, a_l, sign_l, l_mode);
	sub_h  = new_rd_Eor(dbg, block, a_h, sign,   h_mode);

	l_sub  = new_bd_ia32_l_Sub(dbg, block, sub_l, sign_l, mode_T);
	l_res  = new_r_Proj(block, l_sub, l_mode,     pn_ia32_res);
	flags  = new_r_Proj(block, l_sub, mode_flags, pn_ia32_flags);
	h_res  = new_bd_ia32_l_Sbb(dbg, block, sub_h, sign, flags, h_mode);

	resolve_call(call, l_res, h_res, current_ir_graph, block);

	return 1;
}

#define ID(x) new_id_from_chars(x, sizeof(x)-1)

/**
 * Maps a Div. Change into a library call.
 */
static int map_Div(ir_node *call, void *ctx) {
	ia32_intrinsic_env_t *env = ctx;
	ir_type   *method    = get_Call_type(call);
	ir_mode   *h_mode    = get_type_mode(get_method_res_type(method, 1));
	ir_node   *ptr;
	ir_entity *ent;
	symconst_symbol sym;

	if (mode_is_signed(h_mode)) {
		/* 64bit signed Division */
		ent = env->divdi3;
		if (ent == NULL) {
			/* create library entity */
			ent = env->divdi3 = new_entity(get_glob_type(), ID("__divdi3"), method);
			set_entity_visibility(ent, visibility_external_allocated);
			set_entity_ld_ident(ent, ID("__divdi3"));
		}
	} else {
		/* 64bit unsigned Division */
		ent = env->udivdi3;
		if (ent == NULL) {
			/* create library entity */
			ent = env->udivdi3 = new_entity(get_glob_type(), ID("__udivdi3"), method);
			set_entity_visibility(ent, visibility_external_allocated);
			set_entity_ld_ident(ent, ID("__udivdi3"));
		}
	}
	sym.entity_p = ent;
	ptr = get_Call_ptr(call);
	set_SymConst_symbol(ptr, sym);
	return 1;
}

/**
 * Maps a Mod. Change into a library call
 */
static int map_Mod(ir_node *call, void *ctx) {
	ia32_intrinsic_env_t *env = ctx;
	ir_type   *method    = get_Call_type(call);
	ir_mode   *h_mode    = get_type_mode(get_method_res_type(method, 1));
	ir_node   *ptr;
	ir_entity *ent;
	symconst_symbol sym;

	if (mode_is_signed(h_mode)) {
		/* 64bit signed Modulo */
		ent = env->moddi3;
		if (ent == NULL) {
			/* create library entity */
			ent = env->moddi3 = new_entity(get_glob_type(), ID("__moddi3"), method);
			set_entity_visibility(ent, visibility_external_allocated);
			set_entity_ld_ident(ent, ID("__moddi3"));
		}
	} else {
		/* 64bit signed Modulo */
		ent = env->umoddi3;
		if (ent == NULL) {
			/* create library entity */
			ent = env->umoddi3 = new_entity(get_glob_type(), ID("__umoddi3"), method);
			set_entity_visibility(ent, visibility_external_allocated);
			set_entity_ld_ident(ent, ID("__umoddi3"));
		}
	}
	sym.entity_p = ent;
	ptr = get_Call_ptr(call);
	set_SymConst_symbol(ptr, sym);
	return 1;
}

/**
 * Maps a Conv.
 */
static int map_Conv(ir_node *call, void *ctx) {
	ir_graph  *irg     = current_ir_graph;
	dbg_info  *dbg     = get_irn_dbg_info(call);
	ir_node   *block   = get_nodes_block(call);
	ir_node   **params = get_Call_param_arr(call);
	ir_type   *method  = get_Call_type(call);
	int       n        = get_Call_n_params(call);
	ir_node   *l_res, *h_res;
	(void) ctx;

	if (n == 1) {
		ir_node *float_to_ll;

		/* We have a Conv float -> long long here */
		ir_node *a_f        = params[0];
		ir_mode *l_res_mode = get_type_mode(get_method_res_type(method, 0));
		ir_mode *h_res_mode = get_type_mode(get_method_res_type(method, 1));

		assert(mode_is_float(get_irn_mode(a_f)) && "unexpected Conv call");

		if (mode_is_signed(h_res_mode)) {
			/* convert from float to signed 64bit */
			float_to_ll = new_bd_ia32_l_FloattoLL(dbg, block, a_f);

			l_res = new_r_Proj(block, float_to_ll, l_res_mode,
							   pn_ia32_l_FloattoLL_res_low);
			h_res = new_r_Proj(block, float_to_ll, h_res_mode,
							   pn_ia32_l_FloattoLL_res_high);
		} else {
			/* convert from float to signed 64bit */
			ir_mode *flt_mode = get_irn_mode(a_f);
			tarval  *flt_tv   = new_tarval_from_str("9223372036854775808", 19, flt_mode);
			ir_node *flt_corr = new_Const(flt_tv);
			ir_node *lower_blk = block;
			ir_node *upper_blk;
			ir_node *cmp, *proj, *cond, *blk, *int_phi, *flt_phi;
			ir_node *in[2];

			part_block(call);
			upper_blk = get_nodes_block(call);

			cmp   = new_rd_Cmp(dbg, upper_blk, a_f, flt_corr);
			proj  = new_r_Proj(upper_blk, cmp, mode_b, pn_Cmp_Lt);
			cond  = new_rd_Cond(dbg, upper_blk, proj);
			in[0] = new_r_Proj(upper_blk, cond, mode_X, pn_Cond_true);
			in[1] = new_r_Proj(upper_blk, cond, mode_X, pn_Cond_false);
			blk   = new_r_Block(irg, 1, &in[1]);
			in[1] = new_r_Jmp(blk);

			set_irn_in(lower_blk, 2, in);

			/* create to Phis */
			in[0] = new_Const(get_mode_null(h_res_mode));
			in[1] = new_Const_long(h_res_mode, 0x80000000);

			int_phi = new_r_Phi(lower_blk, 2, in, h_res_mode);

			in[0] = a_f;
			in[1] = new_rd_Sub(dbg, upper_blk, a_f, flt_corr, flt_mode);

			flt_phi = new_r_Phi(lower_blk, 2, in, flt_mode);

			/* fix Phi links for next part_block() */
			set_Block_phis(lower_blk, int_phi);
			set_Phi_next(int_phi, flt_phi);
			set_Phi_next(flt_phi, NULL);

			float_to_ll = new_bd_ia32_l_FloattoLL(dbg, lower_blk, flt_phi);

			l_res = new_r_Proj(lower_blk, float_to_ll, l_res_mode,
							   pn_ia32_l_FloattoLL_res_low);
			h_res = new_r_Proj(lower_blk, float_to_ll, h_res_mode,
							   pn_ia32_l_FloattoLL_res_high);

			h_res = new_rd_Add(dbg, lower_blk, h_res, int_phi, h_res_mode);

			/* move the call and its Proj's to the lower block */
			set_nodes_block(call, lower_blk);

			for (proj = get_irn_link(call); proj != NULL; proj = get_irn_link(proj))
				set_nodes_block(proj, lower_blk);
			block = lower_blk;
		}
		/* lower the call */
		resolve_call(call, l_res, h_res, irg, block);
	} else if (n == 2) {
		ir_node *ll_to_float;

		/* We have a Conv long long -> float here */
		ir_node *a_l       = params[BINOP_Left_Low];
		ir_node *a_h       = params[BINOP_Left_High];
		ir_mode *fres_mode = get_type_mode(get_method_res_type(method, 0));

		assert(! mode_is_float(get_irn_mode(a_l))
				&& ! mode_is_float(get_irn_mode(a_h)));

		ll_to_float = new_bd_ia32_l_LLtoFloat(dbg, block, a_h, a_l, fres_mode);

		/* lower the call */
		resolve_call(call, ll_to_float, NULL, irg, block);
	} else {
		panic("unexpected Conv call %+F", call);
	}

	return 1;
}

/* Ia32 implementation of intrinsic mapping. */
ir_entity *ia32_create_intrinsic_fkt(ir_type *method, const ir_op *op,
                                     const ir_mode *imode, const ir_mode *omode,
                                     void *context)
{
	i_record      elt;
	ir_entity     **ent = NULL;
	i_mapper_func mapper;

	if (! intrinsics)
		intrinsics = NEW_ARR_F(i_record, 0);

	switch (get_op_code(op)) {
	case iro_Add:
		ent    = &i_ents[iro_Add];
		mapper = map_Add;
		break;
	case iro_Sub:
		ent    = &i_ents[iro_Sub];
		mapper = map_Sub;
		break;
	case iro_Shl:
		ent    = &i_ents[iro_Shl];
		mapper = map_Shl;
		break;
	case iro_Shr:
		ent    = &i_ents[iro_Shr];
		mapper = map_Shr;
		break;
	case iro_Shrs:
		ent    = &i_ents[iro_Shrs];
		mapper = map_Shrs;
		break;
	case iro_Mul:
		ent    = &i_ents[iro_Mul];
		mapper = map_Mul;
		break;
	case iro_Minus:
		ent    = &i_ents[iro_Minus];
		mapper = map_Minus;
		break;
	case iro_Abs:
		ent    = &i_ents[iro_Abs];
		mapper = map_Abs;
		break;
	case iro_Div:
		ent    = &i_ents[iro_Div];
		mapper = map_Div;
		break;
	case iro_Mod:
		ent    = &i_ents[iro_Mod];
		mapper = map_Mod;
		break;
	case iro_Conv:
		ent    = &i_ents[iro_Conv];
		mapper = map_Conv;
		break;
	default:
		fprintf(stderr, "FIXME: unhandled op for ia32 intrinsic function %s\n", get_id_str(op->name));
		return def_create_intrinsic_fkt(method, op, imode, omode, context);
	}

	if (ent && ! *ent) {
#define IDENT(s)  new_id_from_chars(s, sizeof(s)-1)

		ident *id = id_mangle(IDENT("L"), get_op_ident(op));
		*ent = new_entity(get_glob_type(), id, method);
	}

	elt.i_call.kind     = INTRINSIC_CALL;
	elt.i_call.i_ent    = *ent;
	elt.i_call.i_mapper = mapper;
	elt.i_call.ctx      = context;
	elt.i_call.link     = NULL;

	ARR_APP1(i_record, intrinsics, elt);
	return *ent;
}
