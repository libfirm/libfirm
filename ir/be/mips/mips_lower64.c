/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */
#include "mips_lower64.h"

#include "gen_mips_regalloc_if.h"
#include "lower_dw.h"

static void mips_lower_add64(ir_node *const node)
{
	dbg_info *const dbg        = get_irn_dbg_info(node);
	ir_node  *const block      = get_nodes_block(node);
	ir_node  *const left       = get_Add_left(node);
	ir_node  *const right      = get_Add_right(node);
	ir_node  *const left_low   = get_lowered_low(left);
	ir_node  *const left_high  = get_lowered_high(left);
	ir_node  *const right_low  = get_lowered_low(right);
	ir_node  *const right_high = get_lowered_high(right);

	ir_node  *const res_low   = new_rd_Add(dbg, block, left_low,  right_low);
	ir_node  *const cmp_carry = new_rd_Cmp(dbg, block, res_low,   right_low, ir_relation_less);
	ir_graph *const irg       = get_irn_irg(node);
	ir_mode  *const mode      = get_node_high_mode(node);
	ir_node  *const one       = new_r_Const(irg, get_mode_one(mode));
	ir_node  *const zero      = new_r_Const(irg, get_mode_null(mode));
	ir_node  *const carry     = new_rd_Mux(dbg, block, cmp_carry, zero, one);
	ir_node  *const sum_high  = new_rd_Add(dbg, block, left_high, right_high);
	ir_node  *const res_high  = new_rd_Add(dbg, block, sum_high,  carry);
	ir_set_dw_lowered(node, res_low, res_high);
}

static void mips_lower_sub64(ir_node *const node)
{
	dbg_info *const dbg        = get_irn_dbg_info(node);
	ir_node  *const block      = get_nodes_block(node);
	ir_node  *const left       = get_Sub_left(node);
	ir_node  *const right      = get_Sub_right(node);
	ir_node  *const left_low   = get_lowered_low(left);
	ir_node  *const left_high  = get_lowered_high(left);
	ir_node  *const right_low  = get_lowered_low(right);
	ir_node  *const right_high = get_lowered_high(right);

	ir_node  *const res_low   = new_rd_Sub(dbg, block, left_low,  right_low);
	ir_node  *const cmp_carry = new_rd_Cmp(dbg, block, res_low,   left_low, ir_relation_greater);
	ir_graph *const irg       = get_irn_irg(node);
	ir_mode  *const mode      = get_node_high_mode(node);
	ir_node  *const one       = new_r_Const(irg, get_mode_one(mode));
	ir_node  *const zero      = new_r_Const(irg, get_mode_null(mode));
	ir_node  *const carry     = new_rd_Mux(dbg, block, cmp_carry, zero, one);
	ir_node  *const sum_high  = new_rd_Sub(dbg, block, left_high, right_high);
	ir_node  *const res_high  = new_rd_Sub(dbg, block, sum_high,  carry);
	ir_set_dw_lowered(node, res_low, res_high);
}

void mips_lower64(void)
{
	ir_mode *const word_unsigned = mips_reg_classes[CLASS_mips_gp].mode;
	ir_mode *const word_signed   = find_signed_mode(word_unsigned);
	lwrdw_param_t const lower_dw_params = {
		.word_unsigned    = word_unsigned,
		.word_signed      = word_signed,
		.doubleword_size  = 64,
	};

	ir_prepare_dw_lowering(&lower_dw_params);
	ir_register_dw_lower_function(op_Add, mips_lower_add64);
	ir_register_dw_lower_function(op_Sub, mips_lower_sub64);
	ir_lower_dw_ops();
}
