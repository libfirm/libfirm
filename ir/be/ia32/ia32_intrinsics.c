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
 * @brief       This file implements the mapping of 64Bit intrinsic
 *              functions to code or library calls.
 * @author      Michael Beck
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

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

/** An array to cache all entities */
static ir_entity *i_ents[iro_MaxOpcode];

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
 * Replace a call be a tuple of l_res, h_res.
 */
static void resolve_call(ir_node *call, ir_node *l_res, ir_node *h_res, ir_graph *irg, ir_node *block) {
	ir_node *res, *in[2];
	ir_node *bad   = get_irg_bad(irg);
	ir_node *nomem = get_irg_no_mem(irg);

	in[0] = l_res;
	in[1] = h_res;
	res = new_r_Tuple(irg, block, h_res == NULL ? 1 : 2, in);

	turn_into_tuple(call, pn_Call_max);
	set_Tuple_pred(call, pn_Call_M_regular,        nomem);
	/* Matze: the new_r_Jmp here sometimes CSEs and then bad things happen
	 * (in movgen.c from 186.crafty for example) I don't know why it is here
	 * and if this fix is correct... */
	/*set_Tuple_pred(call, pn_Call_X_regular,        new_r_Jmp(irg, block));*/
	set_Tuple_pred(call, pn_Call_X_regular,        bad);
	set_Tuple_pred(call, pn_Call_X_except,         bad);
	set_Tuple_pred(call, pn_Call_T_result,         res);
	set_Tuple_pred(call, pn_Call_M_except,         nomem);
	set_Tuple_pred(call, pn_Call_P_value_res_base, bad);
}

/**
 * Map an Add (a_l, a_h, b_l, b_h)
 */
static int map_Add(ir_node *call, void *ctx) {
	ir_graph *irg        = current_ir_graph;
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

	add_low  = new_rd_ia32_l_Add(dbg, irg, block, a_l, b_l, mode_T);
	flags    = new_r_Proj(irg, block, add_low, mode_flags, pn_ia32_flags);
	add_high = new_rd_ia32_l_Adc(dbg, irg, block, a_h, b_h, flags, h_mode);

	l_res = new_r_Proj(irg, block, add_low, l_mode, pn_ia32_res);
	h_res = add_high;

	resolve_call(call, l_res, h_res, irg, block);
	return 1;
}

/**
 * Map a Sub (a_l, a_h, b_l, b_h)
 */
static int map_Sub(ir_node *call, void *ctx)
{
	ir_graph *irg        = current_ir_graph;
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

	sub_low  = new_rd_ia32_l_Sub(dbg, irg, block, a_l, b_l, mode_T);
	flags    = new_r_Proj(irg, block, sub_low, mode_flags, pn_ia32_flags);
	sub_high = new_rd_ia32_l_Sbb(dbg, irg, block, a_h, b_h, flags, h_mode);

	l_res = new_r_Proj(irg, block, sub_low, l_mode, pn_ia32_res);
	h_res = sub_high;

	resolve_call(call, l_res, h_res, irg, block);
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
			ir_node *conv = new_rd_Conv(dbg, irg, block, a_l, h_mode);
			h_res = new_rd_Shl(dbg, irg, block, conv, cnt, h_mode);
			l_res = new_rd_Const(dbg, irg, block, l_mode, get_mode_null(l_mode));

		} else {
			/* h_res = SHLD a_h, a_l, cnt */
			h_res = new_rd_ia32_l_ShlD(dbg, irg, block, a_h, a_l, cnt, h_mode);

			/* l_res = SHL a_l, cnt */
			l_res = new_rd_ia32_l_ShlDep(dbg, irg, block, a_l, cnt, h_res, l_mode);
		}

		resolve_call(call, l_res, h_res, irg, block);
		return 1;
	}

	part_block(call);
	upper = get_nodes_block(call);

	/* h_res = SHLD a_h, a_l, cnt */
	h1 = new_rd_ia32_l_ShlD(dbg, irg, upper, a_h, a_l, cnt, h_mode);

	/* l_res = SHL a_l, cnt */
	l1 = new_rd_ia32_l_ShlDep(dbg, irg, upper, a_l, cnt, h1, l_mode);

	c_mode = get_irn_mode(cnt);
	irn    = new_r_Const_long(irg, upper, c_mode, 32);
	irn    = new_rd_And(dbg, irg, upper, cnt, irn, c_mode);
	irn    = new_rd_Cmp(dbg, irg, upper, irn, new_r_Const(irg, upper, c_mode, get_mode_null(c_mode)));
	irn    = new_r_Proj(irg, upper, irn, mode_b, pn_Cmp_Eq);
	cond   = new_rd_Cond(dbg, irg, upper, irn);

	in[0]  = new_r_Proj(irg, upper, cond, mode_X, pn_Cond_true);
	in[1]  = new_r_Proj(irg, upper, cond, mode_X, pn_Cond_false);

	/* the block for cnt >= 32 */
	n_block = new_rd_Block(dbg, irg, 1, &in[1]);
	h2      = new_rd_Conv(dbg, irg, n_block, l1, h_mode);
	l2      = new_r_Const(irg, n_block, l_mode, get_mode_null(l_mode));
	in[1]   = new_r_Jmp(irg, n_block);

	set_irn_in(block, 2, in);

	in[0] = l1;
	in[1] = l2;
	l_res = new_r_Phi(irg, block, 2, in, l_mode);
	set_irn_link(block, l_res);

	in[0] = h1;
	in[1] = h2;
	h_res = new_r_Phi(irg, block, 2, in, h_mode);
	set_irn_link(l_res, h_res);
	set_irn_link(h_res, NULL);

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
			ir_node *conv = new_rd_Conv(dbg, irg, block, a_h, l_mode);
			h_res = new_rd_Const(dbg, irg, block, h_mode, get_mode_null(h_mode));
			l_res = new_rd_Shr(dbg, irg, block, conv, cnt, l_mode);
		} else {
			/* l_res = SHRD a_h:a_l, cnt */
			l_res = new_rd_ia32_l_ShrD(dbg, irg, block, a_l, a_h, cnt, l_mode);

			/* h_res = SHR a_h, cnt */
			h_res = new_rd_ia32_l_ShrDep(dbg, irg, block, a_h, cnt, l_res, h_mode);
		}
		resolve_call(call, l_res, h_res, irg, block);
		return 1;
	}

	part_block(call);
	upper = get_nodes_block(call);

	/* l_res = SHRD a_h:a_l, cnt */
	l1 = new_rd_ia32_l_ShrD(dbg, irg, upper, a_l, a_h, cnt, l_mode);

	/* h_res = SHR a_h, cnt */
	h1 = new_rd_ia32_l_ShrDep(dbg, irg, upper, a_h, cnt, l1, h_mode);

	c_mode = get_irn_mode(cnt);
	irn    = new_r_Const_long(irg, upper, c_mode, 32);
	irn    = new_rd_And(dbg, irg, upper, cnt, irn, c_mode);
	irn    = new_rd_Cmp(dbg, irg, upper, irn, new_r_Const(irg, upper, c_mode, get_mode_null(c_mode)));
	irn    = new_r_Proj(irg, upper, irn, mode_b, pn_Cmp_Eq);
	cond   = new_rd_Cond(dbg, irg, upper, irn);

	in[0]  = new_r_Proj(irg, upper, cond, mode_X, pn_Cond_true);
	in[1]  = new_r_Proj(irg, upper, cond, mode_X, pn_Cond_false);

	/* the block for cnt >= 32 */
	n_block = new_rd_Block(dbg, irg, 1, &in[1]);
	l2      = new_rd_Conv(dbg, irg, n_block, h1, l_mode);
	h2      = new_r_Const(irg, n_block, l_mode, get_mode_null(h_mode));
	in[1]   = new_r_Jmp(irg, n_block);

	set_irn_in(block, 2, in);

	in[0] = l1;
	in[1] = l2;
	l_res = new_r_Phi(irg, block, 2, in, l_mode);
	set_irn_link(block, l_res);

	in[0] = h1;
	in[1] = h2;
	h_res = new_r_Phi(irg, block, 2, in, h_mode);
	set_irn_link(l_res, h_res);
	set_irn_link(h_res, NULL);

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
			ir_node *conv    = new_rd_Conv(dbg, irg, block, a_h, l_mode);
			ir_mode *c_mode  = get_irn_mode(cnt);

			h_res = new_rd_Shrs(dbg, irg, block, a_h, new_r_Const_long(irg, block, c_mode, 31), h_mode);
			l_res = new_rd_Shrs(dbg, irg, block, conv, cnt, l_mode);
		} else {
			/* l_res = SHRD a_h:a_l, cnt */
			l_res = new_rd_ia32_l_ShrD(dbg, irg, block, a_l, a_h, cnt, l_mode);

			/* h_res = SAR a_h, cnt */
			h_res = new_rd_ia32_l_SarDep(dbg, irg, block, a_h, cnt, l_res, h_mode);
		}
		resolve_call(call, l_res, h_res, irg, block);
		return 1;
	}

	part_block(call);
	upper = get_nodes_block(call);

	/* l_res = SHRD a_h:a_l, cnt */
	l1 = new_rd_ia32_l_ShrD(dbg, irg, upper, a_l, a_h, cnt, l_mode);

	/* h_res = SAR a_h, cnt */
	h1 = new_rd_ia32_l_SarDep(dbg, irg, upper, a_h, cnt, l1, h_mode);

	c_mode = get_irn_mode(cnt);
	irn    = new_r_Const_long(irg, upper, c_mode, 32);
	irn    = new_rd_And(dbg, irg, upper, cnt, irn, c_mode);
	irn    = new_rd_Cmp(dbg, irg, upper, irn, new_r_Const(irg, upper, c_mode, get_mode_null(c_mode)));
	irn    = new_r_Proj(irg, upper, irn, mode_b, pn_Cmp_Eq);
	cond   = new_rd_Cond(dbg, irg, upper, irn);

	in[0]  = new_r_Proj(irg, upper, cond, mode_X, pn_Cond_true);
	in[1]  = new_r_Proj(irg, upper, cond, mode_X, pn_Cond_false);

	/* the block for cnt >= 32 */
	n_block = new_rd_Block(dbg, irg, 1, &in[1]);
	l2      = new_rd_Conv(dbg, irg, n_block, h1, l_mode);
	h2      = new_rd_Shrs(dbg, irg, n_block, a_h, new_r_Const_long(irg, block, c_mode, 31), h_mode);
	in[1]   = new_r_Jmp(irg, n_block);

	set_irn_in(block, 2, in);

	in[0] = l1;
	in[1] = l2;
	l_res = new_r_Phi(irg, block, 2, in, l_mode);
	set_irn_link(block, l_res);

	in[0] = h1;
	in[1] = h2;
	h_res = new_r_Phi(irg, block, 2, in, h_mode);
	set_irn_link(l_res, h_res);
	set_irn_link(h_res, NULL);

	/* move it down */
	set_nodes_block(call, block);
	for (irn = get_irn_link(call); irn != NULL; irn = get_irn_link(irn))
		set_nodes_block(irn, block);

	resolve_call(call, l_res, h_res, irg, block);
	return 1;
}

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
	ir_graph *irg     = current_ir_graph;
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
		mul   = new_rd_ia32_l_IMul(dbg, irg, block, a_l, b_l);
		h_res = new_rd_Proj(dbg, irg, block, mul, h_mode, pn_ia32_l_Mul_EDX);
		l_res = new_rd_Proj(dbg, irg, block, mul, l_mode, pn_ia32_l_Mul_EAX);

		goto end;
	}

	mul   = new_rd_ia32_l_Mul(dbg, irg, block, a_l, b_l);
	pEDX  = new_rd_Proj(dbg, irg, block, mul, h_mode, pn_ia32_l_Mul_EDX);
	l_res = new_rd_Proj(dbg, irg, block, mul, l_mode, pn_ia32_l_Mul_EAX);

	b_l   = new_rd_Conv(dbg, irg, block, b_l, h_mode);
	mul   = new_rd_Mul( dbg, irg, block, a_h, b_l, h_mode);
	add   = new_rd_Add( dbg, irg, block, mul, pEDX, h_mode);
	a_l   = new_rd_Conv(dbg, irg, block, a_l, h_mode);
	mul   = new_rd_Mul( dbg, irg, block, a_l, b_h, h_mode);
	h_res = new_rd_Add( dbg, irg, block, add, mul, h_mode);

end:
	resolve_call(call, l_res, h_res, irg, block);

	return 1;
}

/**
 * Map a Minus (a_l, a_h)
 */
static int map_Minus(ir_node *call, void *ctx) {
	ir_graph *irg     = current_ir_graph;
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

	res   = new_rd_ia32_Minus64Bit(dbg, irg, block, a_l, a_h);
	l_res = new_r_Proj(irg, block, res, l_mode, pn_ia32_Minus64Bit_low_res);
	h_res = new_r_Proj(irg, block, res, h_mode, pn_ia32_Minus64Bit_high_res);

	resolve_call(call, l_res, h_res, irg, block);

	return 1;
}

/**
 * Map a Abs (a_l, a_h)
 */
static int map_Abs(ir_node *call, void *ctx) {
	ir_graph *irg        = current_ir_graph;
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
	sign   = new_rd_Shrs(dbg, irg, block, a_h, new_Const_long(l_mode, 31), h_mode);
	sign_l = new_rd_Conv(dbg, irg, block, sign, l_mode);
	sub_l  = new_rd_Eor(dbg, irg, block, a_l, sign_l, l_mode);
	sub_h  = new_rd_Eor(dbg, irg, block, a_h, sign,   h_mode);

	l_sub  = new_rd_ia32_l_Sub(dbg, irg, block, sub_l, sign_l, mode_T);
	l_res  = new_r_Proj(irg, block, l_sub, l_mode,     pn_ia32_res);
	flags  = new_r_Proj(irg, block, l_sub, mode_flags, pn_ia32_flags);
	h_res  = new_rd_ia32_l_Sbb(dbg, irg, block, sub_h, sign, flags, h_mode);

	resolve_call(call, l_res, h_res, irg, block);

	return 1;
}

#define ID(x) new_id_from_chars(x, sizeof(x)-1)

/**
 * Maps a Div. Change into a library call
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
	ir_mode   *h_mode    = get_type_mode(get_method_res_type(method, 0));
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
	ia32_intrinsic_env_t *env = ctx;
	ir_graph  *irg        = current_ir_graph;
	dbg_info  *dbg        = get_irn_dbg_info(call);
	ir_node   *block      = get_nodes_block(call);
	ir_node   **params    = get_Call_param_arr(call);
	ir_type   *method     = get_Call_type(call);
	int       n           = get_Call_n_params(call);
	int       gp_bytes    = get_mode_size_bytes(ia32_reg_classes[CLASS_ia32_gp].mode);
	ir_entity *ent;
	ir_node   *l_res, *h_res, *frame, *fres;
	ir_node   *store_l, *store_h;
	ir_node   *op_mem[2], *mem;

	if (n == 1) {
		/* We have a Conv float -> long long here */
		ir_node *a_f        = params[0];
		ir_mode *l_res_mode = get_type_mode(get_method_res_type(method, 0));
		ir_mode *h_res_mode = get_type_mode(get_method_res_type(method, 1));

		assert(mode_is_float(get_irn_mode(a_f)) && "unexpected Conv call");

		/* allocate memory on frame to store args */
		ent = env->irg == irg ? env->d_ll_conv : NULL;
		if (! ent) {
			ent      = env->d_ll_conv = frame_alloc_area(get_irg_frame_type(irg), 2 * gp_bytes, 16, 0);
			env->irg = irg;
		}

		/* Store arg */
		frame = get_irg_frame(irg);

		/*
			Now we create a node to move the value from a XMM register into
			x87 FPU because it is unknown here, which FPU is used.
			This node is killed in transformation phase when not needed.
			Otherwise it is split up into a movsd + fld
		*/
		a_f = new_rd_ia32_l_SSEtoX87(dbg, irg, block, frame, a_f, get_irg_no_mem(irg), mode_D);
		set_ia32_frame_ent(a_f, ent);
		set_ia32_use_frame(a_f);
		set_ia32_ls_mode(a_f, mode_D);

		if (mode_is_signed(h_res_mode)) {
			/* a float to signed conv, the simple case */

			/* store from FPU as Int */
			a_f = new_rd_ia32_l_vfist(dbg, irg, block, frame, a_f, get_irg_no_mem(irg));
			set_ia32_frame_ent(a_f, ent);
			set_ia32_use_frame(a_f);
			set_ia32_ls_mode(a_f, mode_Ls);
			mem = a_f;

			/* load low part of the result */
			l_res = new_rd_ia32_l_Load(dbg, irg, block, frame, mem);
			set_ia32_frame_ent(l_res, ent);
			set_ia32_use_frame(l_res);
			set_ia32_ls_mode(l_res, l_res_mode);
			l_res = new_r_Proj(irg, block, l_res, l_res_mode, pn_ia32_l_Load_res);

			/* load hight part of the result */
			h_res = new_rd_ia32_l_Load(dbg, irg, block, frame, mem);
			set_ia32_frame_ent(h_res, ent);
			add_ia32_am_offs_int(h_res, gp_bytes);
			set_ia32_use_frame(h_res);
			set_ia32_ls_mode(h_res, h_res_mode);
			h_res = new_r_Proj(irg, block, h_res, h_res_mode, pn_ia32_l_Load_res);
		} else {
			/* a float to unsigned conv, more complicated */
			panic("Float->unsigned64 NYI\n");
		}

		/* lower the call */
		resolve_call(call, l_res, h_res, irg, block);
	}
	else if (n == 2) {
		/* We have a Conv long long -> float here */
		ir_node *a_l       = params[BINOP_Left_Low];
		ir_node *a_h       = params[BINOP_Left_High];
		ir_mode *fres_mode = get_type_mode(get_method_res_type(method, 0));

		assert(! mode_is_float(get_irn_mode(a_l))
				&& ! mode_is_float(get_irn_mode(a_h)));

		/* allocate memory on frame to store args */
		ent = env->irg == irg ? env->ll_d_conv : NULL;
		if (! ent) {
			ent = env->ll_d_conv = frame_alloc_area(get_irg_frame_type(irg), 2 * gp_bytes, 16, 0);
			env->irg = irg;
		}

		/* Store arg */
		frame = get_irg_frame(irg);

		/* store first arg (low part) */
		store_l   = new_rd_ia32_l_Store(dbg, irg, block, frame, a_l, get_irg_no_mem(irg));
		set_ia32_frame_ent(store_l, ent);
		set_ia32_use_frame(store_l);
		set_ia32_ls_mode(store_l, get_irn_mode(a_l));
		op_mem[0] = store_l;

		/* store second arg (high part) */
		store_h   = new_rd_ia32_l_Store(dbg, irg, block, frame, a_h, get_irg_no_mem(irg));
		set_ia32_frame_ent(store_h, ent);
		add_ia32_am_offs_int(store_h, gp_bytes);
		set_ia32_use_frame(store_h);
		set_ia32_ls_mode(store_h, get_irn_mode(a_h));
		op_mem[1] = store_h;

		mem = new_r_Sync(irg, block, 2, op_mem);

		/* Load arg into x87 FPU (implicit convert) */
		fres = new_rd_ia32_l_vfild(dbg, irg, block, frame, mem);
		set_ia32_frame_ent(fres, ent);
		set_ia32_use_frame(fres);
		set_ia32_ls_mode(fres, mode_D);
		mem  = new_r_Proj(irg, block, fres, mode_M, pn_ia32_l_vfild_M);
		fres = new_r_Proj(irg, block, fres, fres_mode, pn_ia32_l_vfild_res);

		/*
			Now we create a node to move the loaded value into a XMM
			register because it is unknown here, which FPU is used.
			This node is killed in transformation phase when not needed.
			Otherwise it is split up into a fst + movsd
		*/
		fres = new_rd_ia32_l_X87toSSE(dbg, irg, block, frame, fres, mem, fres_mode);
		set_ia32_frame_ent(fres, ent);
		set_ia32_use_frame(fres);
		set_ia32_ls_mode(fres, fres_mode);

		/* lower the call */
		resolve_call(call, fres, NULL, irg, block);
	}
	else {
		assert(0 && "unexpected Conv call");
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

		ident *id = mangle(IDENT("L"), get_op_ident(op));
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
