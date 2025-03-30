/*
 * This file is part of libFirm.
 * Copyright (C) 2014 University of Karlsruhe.
 */

/**
 * @file
 * @brief    ARM 64bit lowering
 * @author   Matthias Braun
 */
#include "arm_bearch_t.h"
#include "arm_nodes_attr.h"
#include "benode.h"
#include "gen_arm_new_nodes.h"
#include "gen_arm_regalloc_if.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "lower_dw.h"
#include "panic.h"
#include "util.h"

static void lower64_add(ir_node *const node)
{
	dbg_info *dbgi       = get_irn_dbg_info(node);
	ir_node  *block      = get_nodes_block(node);
	ir_node  *left       = get_Add_left(node);
	ir_node  *right      = get_Add_right(node);
	ir_node  *left_low   = get_lowered_low(left);
	ir_node  *left_high  = get_lowered_high(left);
	ir_node  *right_low  = get_lowered_low(right);
	ir_node  *right_high = get_lowered_high(right);
	ir_node  *adds       = new_bd_arm_AddS_t(dbgi, block, left_low, right_low);
	ir_mode  *mode_low   = get_irn_mode(left_low);
	ir_node  *res_low    = new_r_Proj(adds, mode_low, pn_arm_AddS_t_res);
	ir_node  *res_flags  = new_r_Proj(adds, mode_ANY, pn_arm_AddS_t_flags);
	ir_mode  *mode       = get_node_high_mode(node);
	ir_node  *adc        = new_bd_arm_AdC_t(dbgi, block, left_high,
	                                        right_high, res_flags, mode);
	ir_set_dw_lowered(node, res_low, adc);
}

static void lower64_sub(ir_node *const node)
{
	dbg_info *dbgi       = get_irn_dbg_info(node);
	ir_node  *block      = get_nodes_block(node);
	ir_node  *left       = get_Sub_left(node);
	ir_node  *right      = get_Sub_right(node);
	ir_node  *left_low   = get_lowered_low(left);
	ir_node  *left_high  = get_lowered_high(left);
	ir_node  *right_low  = get_lowered_low(right);
	ir_node  *right_high = get_lowered_high(right);
	ir_node  *subs       = new_bd_arm_SubS_t(dbgi, block, left_low, right_low);
	ir_mode  *mode_low   = get_irn_mode(left_low);
	ir_node  *res_low    = new_r_Proj(subs, mode_low, pn_arm_SubS_t_res);
	ir_node  *res_flags  = new_r_Proj(subs, mode_ANY, pn_arm_SubS_t_flags);
	ir_mode  *mode       = get_node_high_mode(node);
	ir_node  *sbc        = new_bd_arm_SbC_t(dbgi, block, left_high,
	                                        right_high, res_flags, mode);
	ir_set_dw_lowered(node, res_low, sbc);
}

static void lower64_minus(ir_node *const node)
{
	dbg_info *dbgi         = get_irn_dbg_info(node);
	ir_graph *irg          = get_irn_irg(node);
	ir_node  *block        = get_nodes_block(node);
	ir_node  *op           = get_Minus_op(node);
	ir_node  *right_low    = get_lowered_low(op);
	ir_node  *right_high   = get_lowered_high(op);
	ir_mode  *low_unsigned = get_irn_mode(right_low);
	ir_node  *left_low     = new_r_Const_null(irg, low_unsigned);
	ir_mode  *mode         = get_node_high_mode(node);
	ir_node  *left_high    = new_r_Const_null(irg, mode);
	ir_node  *subs         = new_bd_arm_SubS_t(dbgi, block, left_low,
	                                           right_low);
	ir_node  *res_low      = new_r_Proj(subs, low_unsigned, pn_arm_SubS_t_res);
	ir_node  *res_flags    = new_r_Proj(subs, mode_ANY, pn_arm_SubS_t_flags);
	ir_node  *sbc          = new_bd_arm_SbC_t(dbgi, block, left_high,
	                                          right_high, res_flags, mode);
	ir_set_dw_lowered(node, res_low, sbc);
}

static void lower64_mul(ir_node *const node)
{
	dbg_info *dbgi       = get_irn_dbg_info(node);
	ir_node  *block      = get_nodes_block(node);
	ir_node  *left       = get_Mul_left(node);
	ir_node  *right      = get_Mul_right(node);
	ir_node  *left_low   = get_lowered_low(left);
	ir_node  *left_high  = get_lowered_high(left);
	ir_node  *right_low  = get_lowered_low(right);
	ir_node  *right_high = get_lowered_high(right);
	ir_mode  *mode       = get_node_high_mode(node);
	ir_node  *conv_l_low = new_rd_Conv(dbgi, block, left_low, mode);
	ir_node  *mul1       = new_rd_Mul(dbgi, block, conv_l_low, right_high);
	ir_node  *umull      = new_bd_arm_UMulL_t(dbgi, block, left_low, right_low);
	ir_mode  *umode      = get_irn_mode(right_low);
	ir_node  *umull_low  = new_r_Proj(umull, umode, pn_arm_UMulL_t_low);
	ir_node  *umull_high = new_r_Proj(umull, mode, pn_arm_UMulL_t_high);
	ir_node  *conv_r_low = new_rd_Conv(dbgi, block, right_low, mode);
	ir_node  *mul2       = new_rd_Mul(dbgi, block, conv_r_low, left_high);
	ir_node  *add1       = new_rd_Add(dbgi, block, mul2, mul1);
	ir_node  *add2       = new_rd_Add(dbgi, block, add1, umull_high);
	ir_set_dw_lowered(node, umull_low, add2);
}

static ir_entity *ldivmod;
static ir_entity *uldivmod;

static ir_entity *make_divmod(char const *const name, ir_type *const even, ir_type *const odd)
{
	ir_type *const mtp = new_type_method(4, 4, false, cc_cdecl_set, mtp_no_property);
	set_method_param_type(mtp, 0, even);
	set_method_param_type(mtp, 1, odd);
	set_method_param_type(mtp, 2, even);
	set_method_param_type(mtp, 3, odd);
	set_method_res_type(mtp, 0, even);
	set_method_res_type(mtp, 1, odd);
	set_method_res_type(mtp, 2, even);
	set_method_res_type(mtp, 3, odd);

	return create_compilerlib_entity(name, mtp);
}

static void create_divmod_intrinsics(ir_mode *mode_unsigned,
                                     ir_mode *mode_signed)
{
	ir_type *const tp_unsigned = get_type_for_mode(mode_unsigned);
	uldivmod = make_divmod("__aeabi_uldivmod", tp_unsigned, tp_unsigned);

	ir_type *const tp_signed = get_type_for_mode(mode_signed);
	ir_type *const even      = arm_cg_config.big_endian ? tp_signed   : tp_unsigned;
	ir_type *const odd       = arm_cg_config.big_endian ? tp_unsigned : tp_signed;
	ldivmod = make_divmod("__aeabi_ldivmod", even, odd);
}

static void lower_divmod(ir_node *const node, ir_node *const left, ir_node *const right, ir_node *const mem, int const res_offset)
{
	dbg_info  *dbgi       = get_irn_dbg_info(node);
	ir_node   *block      = get_nodes_block(node);
	ir_node   *left_low   = get_lowered_low(left);
	ir_node   *left_high  = get_lowered_high(left);
	ir_node   *right_low  = get_lowered_low(right);
	ir_node   *right_high = get_lowered_high(right);
	ir_mode   *node_mode  = get_irn_mode(left);
	ir_entity *entity     = mode_is_signed(node_mode) ? ldivmod : uldivmod;
	ir_type   *mtp        = get_entity_type(entity);
	ir_graph  *irg        = get_irn_irg(node);
	ir_node   *addr       = new_r_Address(irg, entity);
	ir_node   *in[4];
	if (arm_cg_config.big_endian) {
		in[0] = left_high;
		in[1] = left_low;
		in[2] = right_high;
		in[3] = right_low;
	} else {
		in[0] = left_low;
		in[1] = left_high;
		in[2] = right_low;
		in[3] = right_high;
	}
	ir_node *call    = new_rd_Call(dbgi, block, mem, addr, ARRAY_SIZE(in), in,
	                               mtp);
	ir_node *resproj = new_r_Proj(call, mode_T, pn_Call_T_result);
	set_irn_pinned(call, get_irn_pinned(node));
	foreach_out_edge_safe(node, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		if (!is_Proj(proj))
			continue;

		switch ((pn_Div)get_Proj_num(proj)) {
		case pn_Div_M:
			/* reroute to the call */
			set_Proj_pred(proj, call);
			set_Proj_num(proj, pn_Call_M);
			break;
		case pn_Div_X_regular:
			set_Proj_pred(proj, call);
			set_Proj_num(proj, pn_Call_X_regular);
			break;
		case pn_Div_X_except:
			set_Proj_pred(proj, call);
			set_Proj_num(proj, pn_Call_X_except);
			break;
		case pn_Div_res: {
			ir_mode *low_mode = get_irn_mode(left_low);
			ir_mode *mode     = get_node_high_mode(node);
			if (arm_cg_config.big_endian) {
				ir_node *res_low  = new_r_Proj(resproj, low_mode, res_offset+1);
				ir_node *res_high = new_r_Proj(resproj, mode,     res_offset);
				ir_set_dw_lowered(proj, res_low, res_high);
			} else {
				ir_node *res_low  = new_r_Proj(resproj, low_mode, res_offset);
				ir_node *res_high = new_r_Proj(resproj, mode,     res_offset+1);
				ir_set_dw_lowered(proj, res_low, res_high);
			}
			break;
		}
		}
		/* mark this proj: we have handled it already, otherwise we might fall
		 * into out new nodes. */
		mark_irn_visited(proj);
	}
}

static void lower64_div(ir_node *const node)
{
	ir_node *left  = get_Div_left(node);
	ir_node *right = get_Div_right(node);
	ir_node *mem   = get_Div_mem(node);
	lower_divmod(node, left, right, mem, 0);
}

static void lower64_mod(ir_node *const node)
{
	ir_node *left  = get_Mod_left(node);
	ir_node *right = get_Mod_right(node);
	ir_node *mem   = get_Mod_mem(node);
	lower_divmod(node, left, right, mem, 2);
}

static void lower64_shl(ir_node *const node)
{
	/* the following algo works, because we have modulo shift 256 */
	ir_mode  *mode       = get_node_high_mode(node);
	assert(get_mode_modulo_shift(mode) == 256);
	dbg_info *dbgi       = get_irn_dbg_info(node);
	ir_node  *block      = get_nodes_block(node);
	ir_node  *left       = get_Shl_left(node);
	ir_node  *left_low   = get_lowered_low(left);
	ir_node  *left_high  = get_lowered_high(left);
	ir_node  *right      = get_Shl_right(node);
	ir_mode  *umode      = get_irn_mode(left_low);
	ir_node  *right_low;
	if (get_mode_size_bits(get_irn_mode(right)) == 64) {
		right_low = get_lowered_low(right);
	} else {
		right_low = new_rd_Conv(dbgi, block, right, umode);
	}
	/* Res_hi = L_hi << R | L_lo << (R - 32) | L_lo >> (32 - R) */
	ir_node  *shl1     = new_rd_Shl(dbgi, block, left_high, right_low);
	ir_graph *irg      = get_irn_irg(node);
	ir_node  *c32      = new_r_Const_long(irg, umode, 32);
	ir_node  *sub      = new_rd_Sub(dbgi, block, right_low, c32);
	ir_node  *llo_conv = new_rd_Conv(dbgi, block, left_low, mode);
	ir_node  *shl2     = new_rd_Shl(dbgi, block, llo_conv, sub);
	ir_node  *or       = new_rd_Or(dbgi, block, shl1, shl2);
	ir_node  *sub2     = new_rd_Sub(dbgi, block, c32, right_low);
	ir_node  *shr      = new_rd_Shr(dbgi, block, llo_conv, sub2);
	ir_node  *or2      = new_rd_Or(dbgi, block, or, shr);
	/* Res_lo = L_lo << R */
	ir_node  *low      = new_rd_Shl(dbgi, block, left_low, right_low);
	ir_set_dw_lowered(node, low, or2);
}

static void lower64_shr(ir_node *const node)
{
	/* the following algo works, because we have modulo shift 256 */
	assert(get_mode_modulo_shift(get_node_high_mode(node)) == 256);
	dbg_info *dbgi       = get_irn_dbg_info(node);
	ir_node  *block      = get_nodes_block(node);
	ir_node  *left       = get_Shr_left(node);
	ir_node  *left_low   = get_lowered_low(left);
	ir_node  *left_high  = get_lowered_high(left);
	ir_node  *right      = get_Shr_right(node);
	ir_mode  *umode      = get_irn_mode(left_low);
	ir_node  *right_low;
	if (get_mode_size_bits(get_irn_mode(right)) == 64) {
		right_low = get_lowered_low(right);
	} else {
		right_low = new_rd_Conv(dbgi, block, right, umode);
	}
	/* Res_lo = L_lo >> R | L_hi >> (R - 32) | L_hi << (32 - R) */
	ir_node  *shr1     = new_rd_Shr(dbgi, block, left_low, right_low);
	ir_graph *irg      = get_irn_irg(node);
	ir_node  *c32      = new_r_Const_long(irg, umode, 32);
	ir_node  *sub      = new_rd_Sub(dbgi, block, right_low, c32);
	ir_node  *lhi_conv = new_rd_Conv(dbgi, block, left_high, umode);
	ir_node  *shr2     = new_rd_Shr(dbgi, block, lhi_conv, sub);
	ir_node  *or       = new_rd_Or(dbgi, block, shr1, shr2);
	ir_node  *sub2     = new_rd_Sub(dbgi, block, c32, right_low);
	ir_node  *shl      = new_rd_Shl(dbgi, block, lhi_conv, sub2);
	ir_node  *or2      = new_rd_Or(dbgi, block, or, shl);
	/* Res_hi = L_hi >> R */
	ir_node  *shr3       = new_rd_Shr(dbgi, block, left_high, right_low);
	ir_set_dw_lowered(node, or2, shr3);
}

static void lower64_shrs(ir_node *const node)
{
	/* the following algo works, because we have modulo shift 256 */
	assert(get_mode_modulo_shift(get_node_high_mode(node)) == 256);
	dbg_info *dbgi       = get_irn_dbg_info(node);
	ir_node  *block      = get_nodes_block(node);
	ir_node  *left       = get_Shrs_left(node);
	ir_node  *left_low   = get_lowered_low(left);
	ir_node  *left_high  = get_lowered_high(left);
	ir_node  *right      = get_Shrs_right(node);
	ir_mode  *umode      = get_irn_mode(left_low);
	ir_node  *right_low;
	if (get_mode_size_bits(get_irn_mode(right)) == 64) {
		right_low = get_lowered_low(right);
	} else {
		right_low = new_rd_Conv(dbgi, block, right, umode);
	}
	ir_node  *shr        = new_rd_Shr(dbgi, block, left_low, right_low);
	ir_graph *irg        = get_irn_irg(node);
	ir_node  *c32        = new_r_Const_long(irg, umode, 32);
	ir_node  *sub        = new_rd_Sub(dbgi, block, c32, right_low);
	ir_node  *subs       = new_bd_arm_SubS_t(dbgi, block, right_low, c32);
	ir_node  *subs_res   = new_r_Proj(subs, umode, pn_arm_SubS_t_res);
	ir_node  *subs_flags = new_r_Proj(subs, mode_ANY, pn_arm_SubS_t_flags);
	ir_node  *left_highu = new_rd_Conv(dbgi, block, left_high, umode);
	ir_node  *shl        = new_rd_Shl(dbgi, block, left_highu, sub);
	ir_node  *or         = new_rd_Or(dbgi, block, shr, shl);
	ir_node  *shrs       = new_rd_Shrs(dbgi, block, left_highu, subs_res);
	ir_node  *orpl       = new_bd_arm_OrPl_t(dbgi, block, or, shrs, or,
	                                         subs_flags, umode);
	ir_node  *shrs2      = new_rd_Shrs(dbgi, block, left_high, right_low);
	ir_set_dw_lowered(node, orpl, shrs2);
}

void arm_lower_64bit(void)
{
	ir_mode *word_unsigned = arm_mode_gp;
	ir_mode *word_signed   = find_signed_mode(word_unsigned);
	lwrdw_param_t lower_dw_params = {
		word_unsigned,
		word_signed,
		64  /* doubleword size */
	};

	create_divmod_intrinsics(word_unsigned, word_signed);

	/* make sure opcodes are initialized */
	arm_create_opcodes();

	ir_prepare_dw_lowering(&lower_dw_params);
	ir_register_dw_lower_function(op_Add,   lower64_add);
	ir_register_dw_lower_function(op_Div,   lower64_div);
	ir_register_dw_lower_function(op_Minus, lower64_minus);
	ir_register_dw_lower_function(op_Mod,   lower64_mod);
	ir_register_dw_lower_function(op_Mul,   lower64_mul);
	ir_register_dw_lower_function(op_Shl,   lower64_shl);
	ir_register_dw_lower_function(op_Shr,   lower64_shr);
	ir_register_dw_lower_function(op_Shrs,  lower64_shrs);
	ir_register_dw_lower_function(op_Sub,   lower64_sub);
	ir_lower_dw_ops();
}
