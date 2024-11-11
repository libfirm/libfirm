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

static inline bool amd64_has_binop_attr(amd64_op_mode_t const op_mode)
{
	switch (op_mode) {
	case AMD64_OP_REG_REG:
	case AMD64_OP_REG_IMM:
	case AMD64_OP_REG_ADDR:
	case AMD64_OP_ADDR_REG:
	case AMD64_OP_ADDR_IMM:
	case AMD64_OP_X87_ADDR_REG:
		return true;
	default:
		return false;
	}
}

static inline bool amd64_has_addr_attr(amd64_op_mode_t const op_mode)
{
	switch (op_mode) {
	case AMD64_OP_ADDR:
	case AMD64_OP_REG:
	case AMD64_OP_IMM32:
	case AMD64_OP_REG_REG_REG:
	case AMD64_OP_REG_REG_ADDR:
		return true;
	default:
		return amd64_has_binop_attr(op_mode);
	}
}

static inline bool amd64_loads(const ir_node *node)
{
	amd64_op_mode_t op_mode = get_amd64_attr_const(node)->op_mode;
	switch (op_mode) {
	case AMD64_OP_ADDR:
		return !is_amd64_lea(node);
	case AMD64_OP_REG_ADDR:
	case AMD64_OP_REG_REG_ADDR:
		return true;
	/* Note: AMD64_OP_ADDR_REG, AMD64_OP_X87_ADDR_REG are stores */
	default:
		return false;
	}
}

static inline amd64_addr_attr_t *get_amd64_addr_attr(ir_node *node)
{
	assert(amd64_has_addr_attr(get_amd64_attr_const(node)->op_mode));
	return (amd64_addr_attr_t*)get_irn_generic_attr(node);
}

static inline const amd64_addr_attr_t *get_amd64_addr_attr_const(
		const ir_node *node)
{
	assert(amd64_has_addr_attr(get_amd64_attr_const(node)->op_mode));
	return (const amd64_addr_attr_t*)get_irn_generic_attr_const(node);
}

static inline amd64_binop_addr_attr_t *get_amd64_binop_addr_attr(ir_node *node)
{
	assert(amd64_has_binop_attr(get_amd64_attr_const(node)->op_mode));
	return (amd64_binop_addr_attr_t*)get_irn_generic_attr(node);
}

static inline const amd64_binop_addr_attr_t *get_amd64_binop_addr_attr_const(
		const ir_node *node)
{
	assert(amd64_has_binop_attr(get_amd64_attr_const(node)->op_mode));
	return (const amd64_binop_addr_attr_t*)get_irn_generic_attr_const(node);
}

static inline bool amd64_has_shift_attr(amd64_op_mode_t const op_mode)
{
	return op_mode == AMD64_OP_SHIFT_REG || op_mode == AMD64_OP_SHIFT_IMM;
}

static inline const amd64_shift_attr_t *get_amd64_shift_attr_const(
		const ir_node *node)
{
	assert(amd64_has_shift_attr(get_amd64_attr_const(node)->op_mode));
	return (const amd64_shift_attr_t*)get_irn_generic_attr_const(node);
}

static inline amd64_shift_attr_t *get_amd64_shift_attr(ir_node *node)
{
	assert(amd64_has_shift_attr(get_amd64_attr_const(node)->op_mode));
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
	assert(get_amd64_attr_const(node)->op_mode == AMD64_OP_CC);
	return (const amd64_cc_attr_t*)get_irn_generic_attr_const(node);
}

static inline amd64_cc_attr_t *get_amd64_cc_attr(ir_node *node)
{
	assert(get_amd64_attr_const(node)->op_mode == AMD64_OP_CC);
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

static inline const amd64_copyb_attr_t *get_amd64_copyb_attr_const(
		const ir_node *node)
{
	assert(is_amd64_copyB(node) || is_amd64_copyB_i(node));
	return (const amd64_copyb_attr_t*)get_irn_generic_attr_const(node);
}

static inline amd64_copyb_attr_t *get_amd64_copyb_attr(ir_node *node)
{
	assert(is_amd64_copyB(node) || is_amd64_copyB_i(node));
	return (amd64_copyb_attr_t*)get_irn_generic_attr_const(node);
}

static inline const amd64_call_addr_attr_t *get_amd64_call_addr_attr_const(
		const ir_node *node)
{
	assert(is_amd64_call(node));
	return (const amd64_call_addr_attr_t*)get_irn_generic_attr_const(node);
}

static inline amd64_call_addr_attr_t *get_amd64_call_addr_attr(ir_node *node)
{
	assert(is_amd64_call(node));
	return (amd64_call_addr_attr_t*)get_irn_generic_attr(node);
}

static inline const amd64_x87_attr_t *get_amd64_x87_attr_const(
		const ir_node *node)
{
	assert(get_amd64_attr_const(node)->op_mode == AMD64_OP_X87);
	return (const amd64_x87_attr_t*)get_irn_generic_attr_const(node);
}

static inline amd64_x87_attr_t *get_amd64_x87_attr(ir_node *node)
{
	assert(get_amd64_attr_const(node)->op_mode == AMD64_OP_X87);
	return (amd64_x87_attr_t*)get_irn_generic_attr(node);
}

static inline amd64_x87_binop_addr_attr_t *get_amd64_x87_binop_addr_attr(
		ir_node *const node)
{
	assert(get_amd64_attr_const(node)->op_mode == AMD64_OP_X87_ADDR_REG);
	return (amd64_x87_binop_addr_attr_t*)get_irn_generic_attr(node);
}

x87_attr_t *amd64_get_x87_attr(ir_node *node);
x87_attr_t const *amd64_get_x87_attr_const(ir_node const *node);

/* Include the generated headers */
#include "gen_amd64_new_nodes.h"

#endif
