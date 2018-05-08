/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */

#ifndef FIRM_BE_MIPS_MIPS_NODES_ATTR_H
#define FIRM_BE_MIPS_MIPS_NODES_ATTR_H

#include <stdint.h>

#include "beasm.h"
#include "benode.h"
#include "irnode_t.h"

typedef struct mips_attr_t {
	except_attr exc; /**< the exception attribute. MUST be the first one. */
} mips_attr_t;

typedef enum mips_cond_t {
	/* Flipping the lowest bit negates the condition. */
	mips_cc_eq,
	mips_cc_ne,
	mips_cc_ltz,
	mips_cc_gez,
	mips_cc_lez,
	mips_cc_gtz,
} mips_cond_t;

static inline mips_cond_t mips_negate_cond(mips_cond_t const c)
{
	return (mips_cond_t)(c ^ 1U);
}

typedef struct mips_asm_operand_t {
	be_asm_operand_t op;
	int32_t          val;
	ir_entity       *ent;
} mips_asm_operand_t;

typedef struct mips_cond_attr_t {
	mips_attr_t attr;
	mips_cond_t cond;
} mips_cond_attr_t;

typedef struct mips_immediate_attr_t {
	mips_attr_t attr;
	ir_entity  *ent;
	int32_t     val;
} mips_immediate_attr_t;

typedef struct mips_switch_attr_t {
	mips_attr_t      attr;
	be_switch_attr_t swtch;
} mips_switch_attr_t;

static inline mips_attr_t const *get_mips_attr_const(ir_node const *const node)
{
	return (mips_attr_t const*)get_irn_generic_attr_const(node);
}

static inline mips_cond_attr_t const *get_mips_cond_attr_const(ir_node const *const node)
{
	return (mips_cond_attr_t const*)get_irn_generic_attr_const(node);
}

static inline mips_immediate_attr_t *get_mips_immediate_attr(ir_node *const node)
{
	return (mips_immediate_attr_t*)get_irn_generic_attr_const(node);
}

static inline mips_immediate_attr_t const *get_mips_immediate_attr_const(ir_node const *const node)
{
	return (mips_immediate_attr_t const*)get_irn_generic_attr_const(node);
}

static inline mips_switch_attr_t const *get_mips_switch_attr_const(ir_node const *const node)
{
	return (mips_switch_attr_t const*)get_irn_generic_attr_const(node);
}

char const *mips_get_cond_name(mips_cond_t cond);

#endif
