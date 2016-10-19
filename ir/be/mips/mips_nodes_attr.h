/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */

#ifndef FIRM_BE_MIPS_MIPS_NODES_ATTR_H
#define FIRM_BE_MIPS_MIPS_NODES_ATTR_H

#include <stdint.h>

#include "irnode_t.h"

typedef struct mips_attr_t {
	except_attr exc; /**< the exception attribute. MUST be the first one. */
} mips_attr_t;

typedef struct mips_immediate_attr_t {
	mips_attr_t attr;
	int32_t     val;
} mips_immediate_attr_t;

static inline mips_attr_t const *get_mips_attr_const(ir_node const *const node)
{
	return (mips_attr_t const*)get_irn_generic_attr_const(node);
}

static inline mips_immediate_attr_t const *get_mips_immediate_attr_const(ir_node const *const node)
{
	return (mips_immediate_attr_t const*)get_irn_generic_attr_const(node);
}

#endif
