/*
 * This file is part of libFirm.
 * Copyright (C) 2018 Christoph Mallon.
 */

#ifndef FIRM_BE_RISCV_RISCV_NODES_ATTR_H
#define FIRM_BE_RISCV_RISCV_NODES_ATTR_H

#include "beasm.h"
#include "benode.h"
#include "irnode_t.h"

typedef struct riscv_attr_t {
	except_attr exc; /**< the exception attribute. MUST be the first one. */
} riscv_attr_t;

enum riscv_arch_irn_flags_t {
	riscv_arch_irn_flag_ignore_fp_offset_fix = arch_irn_flag_backend << 0,
};

typedef enum riscv_cond_t {
	/* Flipping the lowest bit negates the condition. */
	riscv_cc_eq,
	riscv_cc_ne,
	riscv_cc_lt,
	riscv_cc_ge,
	riscv_cc_ltu,
	riscv_cc_geu,
} riscv_cond_t;

static inline riscv_cond_t riscv_negate_cond(riscv_cond_t const c)
{
	return (riscv_cond_t)(c ^ 1U);
}

typedef struct riscv_asm_operand_t {
	be_asm_operand_t op;
	int32_t          val;
	ir_entity       *ent;
} riscv_asm_operand_t;

typedef struct riscv_cond_attr_t {
	riscv_attr_t attr;
	riscv_cond_t cond;
} riscv_cond_attr_t;

typedef struct riscv_immediate_attr_t {
	riscv_attr_t attr;
	ir_entity   *ent;
	int32_t      val;
} riscv_immediate_attr_t;

typedef struct riscv_switch_attr_t {
	riscv_attr_t     attr;
	be_switch_attr_t swtch;
} riscv_switch_attr_t;

static inline riscv_attr_t const *get_riscv_attr_const(ir_node const *const node)
{
	return (riscv_attr_t const*)get_irn_generic_attr_const(node);
}

static inline riscv_cond_attr_t const *get_riscv_cond_attr_const(ir_node const *const node)
{
	return (riscv_cond_attr_t const*)get_irn_generic_attr_const(node);
}

static inline riscv_immediate_attr_t *get_riscv_immediate_attr(ir_node *const node)
{
	return (riscv_immediate_attr_t*)get_irn_generic_attr(node);
}

static inline riscv_immediate_attr_t const *get_riscv_immediate_attr_const(ir_node const *const node)
{
	return (riscv_immediate_attr_t const*)get_irn_generic_attr_const(node);
}

static inline riscv_switch_attr_t const *get_riscv_switch_attr_const(ir_node const *const node)
{
	return (riscv_switch_attr_t const*)get_irn_generic_attr_const(node);
}

char const *riscv_get_cond_name(riscv_cond_t cond);

#endif
