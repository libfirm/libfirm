/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Function prototypes for the assembler ir node constructors.
 */
#ifndef FIRM_BE_AMD64_AMD64_NEW_NODES_H
#define FIRM_BE_AMD64_AMD64_NEW_NODES_H

#include "amd64_nodes_attr.h"
#include "gen_amd64_new_nodes.h"

static inline amd64_attr_t *get_amd64_attr(ir_node *node)
{
	assert(is_amd64_irn(node));
	return (amd64_attr_t*)get_irn_generic_attr(node);
}

static inline const amd64_attr_t *get_amd64_attr_const(const ir_node *node)
{
	assert(is_amd64_irn(node));
	return (const amd64_attr_t*)get_irn_generic_attr_const(node);
}

static inline bool amd64_has_binop_attr(const ir_node *node)
{
	const amd64_attr_t *attr = get_amd64_attr_const(node);
	return attr->op_mode == AMD64_OP_REG_REG
	    || attr->op_mode == AMD64_OP_REG_IMM
	    || attr->op_mode == AMD64_OP_REG_ADDR
	    || attr->op_mode == AMD64_OP_ADDR_REG
	    || attr->op_mode == AMD64_OP_ADDR_IMM;
}

static inline bool amd64_has_addr_attr(const ir_node *node)
{
	const amd64_attr_t *attr = get_amd64_attr_const(node);
	return (amd64_has_binop_attr(node)
		|| attr->op_mode == AMD64_OP_ADDR
		|| attr->op_mode == AMD64_OP_REG
	    || attr->op_mode == AMD64_OP_UNOP_IMM32
	    || attr->op_mode == AMD64_OP_UNOP_REG)
	    && !is_amd64_xor_0(node)
	    && !is_amd64_xorpd_0(node);
}

static inline amd64_addr_attr_t *get_amd64_addr_attr(ir_node *node)
{
	assert(amd64_has_addr_attr(node));
	return (amd64_addr_attr_t*)get_irn_generic_attr(node);
}

static inline const amd64_addr_attr_t *get_amd64_addr_attr_const(
		const ir_node *node)
{
	assert(amd64_has_addr_attr(node));
	return (const amd64_addr_attr_t*)get_irn_generic_attr_const(node);
}

static inline amd64_binop_addr_attr_t *get_amd64_binop_addr_attr(ir_node *node)
{
	assert(amd64_has_binop_attr(node));
	return (amd64_binop_addr_attr_t*)get_irn_generic_attr(node);
}

static inline const amd64_binop_addr_attr_t *get_amd64_binop_addr_attr_const(
		const ir_node *node)
{
	assert(amd64_has_binop_attr(node));
	return (const amd64_binop_addr_attr_t*)get_irn_generic_attr_const(node);
}

static inline bool amd64_has_shift_attr(const ir_node *node)
{
	const amd64_attr_t *attr = get_amd64_attr_const(node);
	return attr->op_mode == AMD64_OP_SHIFT_REG
	    || attr->op_mode == AMD64_OP_SHIFT_IMM;
}

static inline const amd64_shift_attr_t *get_amd64_shift_attr_const(
		const ir_node *node)
{
	assert(amd64_has_shift_attr(node));
	return (const amd64_shift_attr_t*)get_irn_generic_attr_const(node);
}

static inline amd64_shift_attr_t *get_amd64_shift_attr(ir_node *node)
{
	assert(amd64_has_shift_attr(node));
	return (amd64_shift_attr_t*)get_irn_generic_attr(node);
}

static inline const amd64_switch_jmp_attr_t *get_amd64_switch_jmp_attr_const(
		const ir_node *node)
{
	assert(is_amd64_jmp_switch(node));
	return (const amd64_switch_jmp_attr_t*)get_irn_generic_attr_const(node);
}

static inline amd64_switch_jmp_attr_t *get_amd64_switch_jmp_attr(ir_node *node)
{
	assert(is_amd64_jmp_switch(node));
	return (amd64_switch_jmp_attr_t*)get_irn_generic_attr(node);
}

static inline const amd64_cc_attr_t *get_amd64_cc_attr_const(
	const ir_node *node)
{
	assert(is_amd64_jcc(node));
	return (const amd64_cc_attr_t*)get_irn_generic_attr_const(node);
}

static inline amd64_cc_attr_t *get_amd64_cc_attr(ir_node *node)
{
	assert(is_amd64_jcc(node));
	return (amd64_cc_attr_t*)get_irn_generic_attr(node);
}

static inline const amd64_movimm_attr_t *get_amd64_movimm_attr_const(
		const ir_node *node)
{
	assert(is_amd64_mov_imm(node));
	return (const amd64_movimm_attr_t*)get_irn_generic_attr_const(node);
}

static inline amd64_movimm_attr_t *get_amd64_movimm_attr(ir_node *node)
{
	assert(is_amd64_mov_imm(node));
	return (amd64_movimm_attr_t*)get_irn_generic_attr(node);
}

/* Include the generated headers */
#include "gen_amd64_new_nodes.h"

#endif
