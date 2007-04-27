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
 * This file implements the mapping of 64Bit intrinsic functions to
 * code or library calls.
 * @author Michael Beck
 * $Id$
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irgmod.h"
#include "irop.h"
#include "irnode_t.h"
#include "ircons.h"
#include "irprog_t.h"
#include "lower_intrinsics.h"
#include "lower_dw.h"
#include "mangle.h"
#include "array.h"

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
	if (intrinsics && ARR_LEN(intrinsics) > 0)
		lower_intrinsics(intrinsics, ARR_LEN(intrinsics));
}

#define BINOP_Left_Low   0
#define BINOP_Left_High  1
#define BINOP_Right_Low  2
#define BINOP_Right_High 3

static void resolve_call(ir_node *call, ir_node *l_res, ir_node *h_res, ir_graph *irg, ir_node *block) {
	ir_node *res, *in[2];

	in[0] = l_res;
	in[1] = h_res;
	res = new_r_Tuple(irg, block, h_res == NULL ? 1 : 2, in);

	turn_into_tuple(call, pn_Call_max);
	set_Tuple_pred(call, pn_Call_M_regular,        get_irg_no_mem(irg));
	set_Tuple_pred(call, pn_Call_X_except,         get_irg_bad(irg));
	set_Tuple_pred(call, pn_Call_T_result,         res);
	set_Tuple_pred(call, pn_Call_M_except,         get_irg_no_mem(irg));
	set_Tuple_pred(call, pn_Call_P_value_res_base, get_irg_bad(irg));
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
	ir_mode  *l_res_mode = get_type_mode(get_method_res_type(method, 0));
	ir_mode  *h_res_mode = get_type_mode(get_method_res_type(method, 1));
	ir_node  *l_res, *h_res, *add;

	/* l_res = a_l + b_l */
	/* h_res = a_h + b_h + carry */

	add   = new_rd_ia32_Add64Bit(dbg, irg, block, a_l, a_h, b_l, b_h);
	l_res = new_r_Proj(irg, block, add, l_res_mode, pn_ia32_Add64Bit_low_res);
	h_res = new_r_Proj(irg, block, add, h_res_mode, pn_ia32_Add64Bit_high_res);

	resolve_call(call, l_res, h_res, irg, block);
	return 1;
}

/**
 * Map a Sub (a_l, a_h, b_l, b_h)
 */
static int map_Sub(ir_node *call, void *ctx) {
	ir_graph *irg        = current_ir_graph;
	dbg_info *dbg        = get_irn_dbg_info(call);
	ir_node  *block      = get_nodes_block(call);
	ir_node  **params    = get_Call_param_arr(call);
	ir_type  *method     = get_Call_type(call);
	ir_node  *a_l        = params[BINOP_Left_Low];
	ir_node  *a_h        = params[BINOP_Left_High];
	ir_node  *b_l        = params[BINOP_Right_Low];
	ir_node  *b_h        = params[BINOP_Right_High];
	ir_mode  *l_res_mode = get_type_mode(get_method_res_type(method, 0));
	ir_mode  *h_res_mode = get_type_mode(get_method_res_type(method, 1));
	ir_node  *l_res, *h_res, *res;

	/* l_res = a_l - b_l */
	/* h_res = a_h - b_h - carry */

	res   = new_rd_ia32_Sub64Bit(dbg, irg, block, a_l, a_h, b_l, b_h);
	l_res = new_r_Proj(irg, block, res, l_res_mode, pn_ia32_Sub64Bit_low_res);
	h_res = new_r_Proj(irg, block, res, h_res_mode, pn_ia32_Sub64Bit_high_res);

	resolve_call(call, l_res, h_res, irg, block);
	return 1;
}

/**
 * Map a Shl (a_l, a_h, count)
 */
static int map_Shl(ir_node *call, void *ctx) {
	ir_graph *irg        = current_ir_graph;
	dbg_info *dbg        = get_irn_dbg_info(call);
	ir_node  *block      = get_nodes_block(call);
	ir_node  **params    = get_Call_param_arr(call);
	ir_type  *method     = get_Call_type(call);
	ir_node  *a_l        = params[BINOP_Left_Low];
	ir_node  *a_h        = params[BINOP_Left_High];
	ir_node  *cnt        = params[BINOP_Right_Low];
	ir_mode  *l_res_mode = get_type_mode(get_method_res_type(method, 0));
	ir_mode  *h_res_mode = get_type_mode(get_method_res_type(method, 1));
	ir_node  *l_res, *h_res;

	/* h_res = SHLD a_h, a_l, cnt */
	h_res = new_rd_ia32_l_ShlD(dbg, irg, block, a_h, a_l, cnt, l_res_mode);

	/* l_res = SHL a_l, cnt */
	l_res = new_rd_ia32_l_Shl(dbg, irg, block, a_l, cnt, h_res_mode);

	//add_irn_dep(l_res, h_res);

	resolve_call(call, l_res, h_res, irg, block);
	return 1;
}

/**
 * Map a Shr (a_l, a_h, count)
 */
static int map_Shr(ir_node *call, void *ctx) {
	ir_graph *irg        = current_ir_graph;
	dbg_info *dbg        = get_irn_dbg_info(call);
	ir_node  *block      = get_nodes_block(call);
	ir_node  **params    = get_Call_param_arr(call);
	ir_type  *method     = get_Call_type(call);
	ir_node  *a_l        = params[BINOP_Left_Low];
	ir_node  *a_h        = params[BINOP_Left_High];
	ir_node  *cnt        = params[BINOP_Right_Low];
	ir_mode  *l_res_mode = get_type_mode(get_method_res_type(method, 0));
	ir_mode  *h_res_mode = get_type_mode(get_method_res_type(method, 1));
	ir_node  *l_res, *h_res;

	/* l_res = SHRD a_l, a_h, cnt */
	l_res = new_rd_ia32_l_ShrD(dbg, irg, block, a_l, a_h, cnt, l_res_mode);

	/* h_res = SHR a_h, cnt */
	h_res = new_rd_ia32_l_Shr(dbg, irg, block, a_h, cnt, h_res_mode);

	//add_irn_dep(h_res, l_res);

	resolve_call(call, l_res, h_res, irg, block);
	return 1;
}

/**
 * Map a Shrs (a_l, a_h, count)
 */
static int map_Shrs(ir_node *call, void *ctx) {
	ir_graph *irg        = current_ir_graph;
	dbg_info *dbg        = get_irn_dbg_info(call);
	ir_node  *block      = get_nodes_block(call);
	ir_node  **params    = get_Call_param_arr(call);
	ir_type  *method     = get_Call_type(call);
	ir_node  *a_l        = params[BINOP_Left_Low];
	ir_node  *a_h        = params[BINOP_Left_High];
	ir_node  *cnt        = params[BINOP_Right_Low];
	ir_mode  *l_res_mode = get_type_mode(get_method_res_type(method, 0));
	ir_mode  *h_res_mode = get_type_mode(get_method_res_type(method, 1));
	ir_node  *l_res, *h_res;

	/* l_res = SHRD a_l, a_h, cnt */
	l_res = new_rd_ia32_l_ShrD(dbg, irg, block, a_l, a_h, cnt, l_res_mode);

	/* h_res = SAR a_h, cnt */
	h_res = new_rd_ia32_l_Sar(dbg, irg, block, a_h, cnt, h_res_mode);

	//add_irn_dep(h_res, l_res);

	resolve_call(call, l_res, h_res, irg, block);
	return 1;
}

/**
 * Map a Mul (a_l, a_h, b_l, b_h)
 */
static int map_Mul(ir_node *call, void *ctx) {
	ir_graph *irg        = current_ir_graph;
	dbg_info *dbg        = get_irn_dbg_info(call);
	ir_node  *block      = get_nodes_block(call);
	ir_node  **params    = get_Call_param_arr(call);
	ir_type  *method     = get_Call_type(call);
	ir_node  *a_l        = params[BINOP_Left_Low];
	ir_node  *a_h        = params[BINOP_Left_High];
	ir_node  *b_l        = params[BINOP_Right_Low];
	ir_node  *b_h        = params[BINOP_Right_High];
	ir_mode  *l_res_mode = get_type_mode(get_method_res_type(method, 0));
	ir_mode  *h_res_mode = get_type_mode(get_method_res_type(method, 1));
	ir_node  *l_res, *h_res, *mul, *pEDX, *add;

	/*
		EDX:EAX = a_l * b_l
		l_res   = EAX

		t1 = b_l * a_h
		t2 = t1 + EDX
		t3 = a_l * b_h
		h_res = t2 + t3
	*/
	mul   = new_rd_ia32_l_Mul(dbg, irg, block, a_l, b_l);
	pEDX  = new_rd_Proj(dbg, irg, block, mul, l_res_mode, pn_ia32_l_Mul_EDX);
	l_res = new_rd_Proj(dbg, irg, block, mul, l_res_mode, pn_ia32_l_Mul_EAX);

	mul   = new_rd_ia32_l_Mul(dbg, irg, block, a_h, b_l);
	add   = new_rd_ia32_l_Add(dbg, irg, block, mul, pEDX, h_res_mode);
	mul   = new_rd_ia32_l_Mul(dbg, irg, block, a_l, b_h);
	h_res = new_rd_ia32_l_Add(dbg, irg, block, add, mul, h_res_mode);

	resolve_call(call, l_res, h_res, irg, block);

	return 1;
}

/**
 * Map a Minus (a_l, a_h)
 */
static int map_Minus(ir_node *call, void *ctx) {
	ir_graph *irg        = current_ir_graph;
	dbg_info *dbg        = get_irn_dbg_info(call);
	ir_node  *block      = get_nodes_block(call);
	ir_node  **params    = get_Call_param_arr(call);
	ir_type  *method     = get_Call_type(call);
	ir_node  *a_l        = params[BINOP_Left_Low];
	ir_node  *a_h        = params[BINOP_Left_High];
	ir_mode  *l_res_mode = get_type_mode(get_method_res_type(method, 0));
	ir_mode  *h_res_mode = get_type_mode(get_method_res_type(method, 1));
	ir_node  *l_res, *h_res, *cnst, *res;

	/* too bad: we need 0 in a register here */
	cnst  = new_Const_long(h_res_mode, 0);

	/* l_res = 0 - a_l */
	/* h_res = 0 - a_h - carry */

	res   = new_rd_ia32_Minus64Bit(dbg, irg, block, cnst, a_l, a_h);
	l_res = new_r_Proj(irg, block, res, l_res_mode, pn_ia32_Minus64Bit_low_res);
	h_res = new_r_Proj(irg, block, res, h_res_mode, pn_ia32_Minus64Bit_high_res);

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
	ir_mode  *l_res_mode = get_type_mode(get_method_res_type(method, 0));
	ir_mode  *h_res_mode = get_type_mode(get_method_res_type(method, 1));
	ir_node  *l_res, *h_res, *sign, *sub_l, *sub_h, *res;

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

	sign  = new_rd_ia32_l_Sar(dbg, irg, block, a_h, new_Const_long(h_res_mode, 31), h_res_mode);
	sub_l = new_rd_ia32_l_Xor(dbg, irg, block, a_l, sign, l_res_mode);
	sub_h = new_rd_ia32_l_Xor(dbg, irg, block, a_h, sign, h_res_mode);
	res   = new_rd_ia32_Sub64Bit(dbg, irg, block, sub_l, sub_h, sign, sign);
	l_res = new_r_Proj(irg, block, res, l_res_mode, pn_ia32_Sub64Bit_low_res);
	h_res = new_r_Proj(irg, block, res, h_res_mode, pn_ia32_Sub64Bit_high_res);

	resolve_call(call, l_res, h_res, irg, block);

	return 1;
}

typedef enum {
	IA32_INTRINSIC_DIV,
	IA32_INTRINSIC_MOD,
} ia32_intrinsic_divmod_t;

/**
 * Maps a Div/Mod (a_l, a_h, b_l, b_h)
 */
static int DivMod_mapper(ir_node *call, void *ctx, ia32_intrinsic_divmod_t dmtp) {
	ia32_intrinsic_env_t *env = ctx;
	ir_graph  *irg        = current_ir_graph;
	dbg_info  *dbg        = get_irn_dbg_info(call);
	ir_node   *block      = get_nodes_block(call);
	ir_node   **params    = get_Call_param_arr(call);
	ir_type   *method     = get_Call_type(call);
	ir_node   *a_l        = params[BINOP_Left_Low];
	ir_node   *a_h        = params[BINOP_Left_High];
	ir_node   *b_l        = params[BINOP_Right_Low];
	ir_node   *b_h        = params[BINOP_Right_High];
	ir_mode   *l_res_mode = get_type_mode(get_method_res_type(method, 0));
	ir_mode   *h_res_mode = get_type_mode(get_method_res_type(method, 1));
	int       mode_bytes  = get_mode_size_bytes(ia32_reg_classes[CLASS_ia32_gp].mode);
	ir_entity *ent_a      = env->irg == irg ? env->ll_div_op1 : NULL;
	ir_entity *ent_b      = env->irg == irg ? env->ll_div_op2 : NULL;
	ir_node   *l_res, *h_res, *frame;
	ir_node   *store_l, *store_h;
	ir_node   *op_mem[2], *mem, *fa_mem, *fb_mem;
	ir_node   *fa, *fb, *fres;

	/* allocate memory on frame to store args */
	if (! ent_a) {
		ent_a = env->ll_div_op1 =
			frame_alloc_area(get_irg_frame_type(irg), 2 * mode_bytes, 16, 0);
		env->irg = irg;
	}

	if (! ent_b) {
		ent_b = env->ll_div_op2 =
			frame_alloc_area(get_irg_frame_type(irg), 2 * mode_bytes, 16, 0);
		env->irg = irg;
	}

	frame = get_irg_frame(irg);

	/* store first arg */
	store_l   = new_rd_ia32_l_Store(dbg, irg, block, frame, a_l, get_irg_no_mem(irg));
	set_ia32_frame_ent(store_l, ent_a);
	set_ia32_use_frame(store_l);
	set_ia32_ls_mode(store_l, get_irn_mode(a_l));
	op_mem[0] = store_l;

	store_h   = new_rd_ia32_l_Store(dbg, irg, block, frame, a_h, get_irg_no_mem(irg));
	set_ia32_frame_ent(store_h, ent_a);
	add_ia32_am_offs_int(store_h, mode_bytes);
	set_ia32_use_frame(store_h);
	set_ia32_ls_mode(store_h, get_irn_mode(a_h));
	op_mem[1] = store_h;

	mem = new_r_Sync(irg, block, 2, op_mem);

	/* load first arg into FPU */
	fa = new_rd_ia32_l_vfild(dbg, irg, block, frame, mem);
	set_ia32_frame_ent(fa, ent_a);
	set_ia32_use_frame(fa);
	set_ia32_ls_mode(fa, mode_D);
	fa_mem = new_r_Proj(irg, block, fa, mode_M, pn_ia32_l_vfild_M);
	fa     = new_r_Proj(irg, block, fa, mode_E, pn_ia32_l_vfild_res);

	/* store second arg */
	store_l   = new_rd_ia32_l_Store(dbg, irg, block, frame, b_l, get_irg_no_mem(irg));
	set_ia32_frame_ent(store_l, ent_b);
	set_ia32_use_frame(store_l);
	set_ia32_ls_mode(store_l, get_irn_mode(b_l));
	op_mem[0] = store_l;

	store_h   = new_rd_ia32_l_Store(dbg, irg, block, frame, b_h, get_irg_no_mem(irg));
	set_ia32_frame_ent(store_h, ent_b);
	add_ia32_am_offs_int(store_h, mode_bytes);
	set_ia32_use_frame(store_h);
	set_ia32_ls_mode(store_h, get_irn_mode(b_h));
	op_mem[1] = store_h;

	mem = new_r_Sync(irg, block, 2, op_mem);

	/* load second arg into FPU */
	fb = new_rd_ia32_l_vfild(dbg, irg, block, frame, mem);
	set_ia32_frame_ent(fb, ent_b);
	set_ia32_use_frame(fb);
	set_ia32_ls_mode(fb, mode_D);
	fb_mem = new_r_Proj(irg, block, fb, mode_M, pn_ia32_l_vfild_M);
	fb     = new_r_Proj(irg, block, fb, mode_E, pn_ia32_l_vfild_res);

	op_mem[0] = fa_mem;
	op_mem[1] = fb_mem;

	mem = new_r_Sync(irg, block, 2, op_mem);

	/* perform division */
	switch (dmtp) {
		case IA32_INTRINSIC_DIV:
			fres = new_rd_ia32_l_vfdiv(dbg, irg, block, fa, fb);
			fres = new_rd_Proj(dbg, irg, block, fres, mode_E, pn_ia32_l_vfdiv_res);
			break;
		case IA32_INTRINSIC_MOD:
			fres = new_rd_ia32_l_vfprem(dbg, irg, block, fa, fb, mode_E);
			break;
		default:
			assert(0);
	}

	/* store back result, we use ent_a here */
	fres = new_rd_ia32_l_vfist(dbg, irg, block, frame, fres, mem);
	set_ia32_frame_ent(fres, ent_a);
	set_ia32_use_frame(fres);
	set_ia32_ls_mode(fres, mode_D);
	mem = fres;

	/* load low part of the result */
	l_res = new_rd_ia32_l_Load(dbg, irg, block, frame, mem);
	set_ia32_frame_ent(l_res, ent_a);
	set_ia32_use_frame(l_res);
	set_ia32_ls_mode(l_res, l_res_mode);
	l_res = new_r_Proj(irg, block, l_res, l_res_mode, pn_ia32_l_Load_res);

	/* load hight part of the result */
	h_res = new_rd_ia32_l_Load(dbg, irg, block, frame, mem);
	set_ia32_frame_ent(h_res, ent_a);
	add_ia32_am_offs_int(h_res, mode_bytes);
	set_ia32_use_frame(h_res);
	set_ia32_ls_mode(h_res, h_res_mode);
	h_res = new_r_Proj(irg, block, h_res, h_res_mode, pn_ia32_l_Load_res);

	/* lower the call */
	resolve_call(call, l_res, h_res, irg, block);

	return 1;
}

static int map_Div(ir_node *call, void *ctx) {
	return DivMod_mapper(call, ctx, IA32_INTRINSIC_DIV);
}

static int map_Mod(ir_node *call, void *ctx) {
	return DivMod_mapper(call, ctx, IA32_INTRINSIC_MOD);
}

/**
 * Maps a Conv (a_l, a_h)
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

		/* store from FPU as Int */
		a_f = new_rd_ia32_l_vfist(dbg, irg, block, frame, a_f, get_irg_no_mem(irg));
		set_ia32_frame_ent(a_f, ent);
		set_ia32_use_frame(a_f);
		set_ia32_ls_mode(a_f, mode_D);
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

		/* lower the call */
		resolve_call(call, l_res, h_res, irg, block);
	}
	else if (n == 2) {
		/* We have a Conv long long -> float here */
		ir_node *a_l       = params[BINOP_Left_Low];
		ir_node *a_h       = params[BINOP_Left_High];
		ir_mode *mode_a_l  = get_irn_mode(a_l);
		ir_mode *mode_a_h  = get_irn_mode(a_h);
		ir_mode *fres_mode = get_type_mode(get_method_res_type(method, 0));

		assert(! mode_is_float(mode_a_l) && ! mode_is_float(mode_a_h) && "unexpected Conv call");

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
