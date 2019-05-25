/*
 * This file is part of libFirm.
 * Copyright (C) 2018 Christoph Mallon.
 */
#include "riscv_lower64.h"
#include "gen_riscv_new_nodes.h" 
#include "gen_riscv_regalloc_if.h"
#include "ircons_t.h"
#include "lower_dw.h"

static void lower64_minus(ir_node *const node)
{
	dbg_info *dbgi         = get_irn_dbg_info(node);
	ir_graph *irg          = get_irn_irg(node);
	ir_node  *block        = get_nodes_block(node);
	ir_node  *op           = get_Minus_op(node);
	ir_node  *right_low    = get_lowered_low(op);
	ir_node  *right_high   = get_lowered_high(op);
	
    ir_mode  *munsigned = get_irn_mode(right_low);
	ir_node  *cnull     = new_r_Const_null(irg, munsigned);
	ir_node  *inv_low   = new_rd_Eor(dbgi, block, cnull, right_low);        
	ir_node  *inv_high  = new_rd_Eor(dbgi, block, cnull, right_high);
    ir_node  *one       = new_r_Const_long(irg, munsigned, 1);
    ir_node  *res_low   = new_rd_Add(dbgi, block, inv_low, one); 
    ir_node  *sltu      = new_bd_riscv_sltu_t(dbgi, block, res_low, inv_low, munsigned);
    ir_node  *res_high  = new_rd_Add(dbgi, block, inv_high, sltu);
    
	ir_set_dw_lowered(node, res_low, res_high);
} 

static void lower64_add(ir_node *const node)
{
	dbg_info *dbgi       = get_irn_dbg_info(node);
	ir_node  *block      = get_nodes_block(node);
	ir_node  *left       = get_Add_left(node);
	ir_node  *right      = get_Add_right(node);
	ir_node  *left_low   = get_lowered_low(left);
	ir_node  *left_high  = get_lowered_high(left)
    ;
	ir_node  *right_low  = get_lowered_low(right);
	ir_node  *right_high = get_lowered_high(right);
	
	ir_node  *addl       = new_rd_Add(dbgi, block, left_low, right_low);   
    ir_node  *addh       = new_rd_Add(dbgi, block, left_high, right_high);
	ir_mode  *mode       = get_node_high_mode(node); 
    ir_node  *sltu      =  new_bd_riscv_sltu_t(dbgi, block, addl, left_low, mode);
    ir_node  *addh2       = new_rd_Add(dbgi, block, addh, sltu);
	ir_set_dw_lowered(node, addl, addh2); 
} 

static void lower64_sub(ir_node *const node)
{
	dbg_info *dbgi       = get_irn_dbg_info(node);
	ir_node  *block      = get_nodes_block(node);
	ir_node  *left       = get_Sub_left(node);
	ir_node  *right      = get_Sub_right(node);
	ir_node  *left_low   = get_lowered_low(left);
	ir_node  *left_high  = get_lowered_high(left)
    ;
	ir_node  *right_low  = get_lowered_low(right);
	ir_node  *right_high = get_lowered_high(right);
    
	ir_node  *subl       = new_rd_Sub(dbgi, block, left_low, right_low);
	ir_node  *subh       = new_rd_Sub(dbgi, block, left_high, right_high);
    ir_mode  *mode       = get_node_high_mode(node); 
    ir_node  *sltu      =  new_bd_riscv_sltu_t(dbgi, block, left_low, subl, mode);
    ir_node  *subh2       = new_rd_Sub(dbgi, block, subh, sltu);
	ir_set_dw_lowered(node, subl, subh2); 
} 


void riscv_lower64(void)
{
	ir_mode *const word_unsigned = riscv_reg_classes[CLASS_riscv_gp].mode;
	ir_mode *const word_signed   = find_signed_mode(word_unsigned);
	lwrdw_param_t lower_dw_params = {
		.word_unsigned    = word_unsigned,
		.word_signed      = word_signed,
		.doubleword_size  = 64,
	};

	ir_prepare_dw_lowering(&lower_dw_params);
    ir_register_dw_lower_function(op_Add, lower64_add); 
    ir_register_dw_lower_function(op_Sub, lower64_sub); 
    ir_register_dw_lower_function(op_Minus, lower64_minus); 
	ir_lower_dw_ops();
}
