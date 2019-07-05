/*
 * This file is part of libFirm.
 * Copyright (C) 2019 University of Karlsruhe.
 */

#ifndef FIRM_BE_VHDL_VHDL_NODES_ATTR_H
#define FIRM_BE_VHDL_VHDL_NODES_ATTR_H

#include <stdint.h>

#include "beasm.h"
#include "benode.h"
#include "irnode_t.h"

typedef struct vhdl_attr_t {
	except_attr exc; /**< the exception attribute. MUST be the first one. */
} vhdl_attr_t;

static inline vhdl_attr_t const *get_vhdl_attr_const(ir_node const *const node)
{
	return (vhdl_attr_t const*)get_irn_generic_attr_const(node);
}

#endif
