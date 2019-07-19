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

typedef struct vhdl_cmp_attr_t {
	vhdl_attr_t attr;
	ir_relation rel;
} vhdl_cmp_attr_t;

typedef struct vhdl_immediate_attr_t {
	vhdl_attr_t attr;
	ir_entity  *ent;
	int32_t     val;
} vhdl_immediate_attr_t;

typedef struct vhdl_varsig_attr_t {
	vhdl_attr_t attr;
	char name[16];
} vhdl_varsig_attr_t;

typedef struct vhdl_start_attr_t {
	vhdl_attr_t attr;
	int n_signals;
	vhdl_varsig_attr_t *signals;
} vhdl_start_attr_t;


static inline vhdl_attr_t const *get_vhdl_attr_const(ir_node const *const node)
{
	return (vhdl_attr_t const*)get_irn_generic_attr_const(node);
}

static inline vhdl_immediate_attr_t const *get_vhdl_immediate_attr_const(ir_node const *const node)
{
	return (vhdl_immediate_attr_t const*)get_irn_generic_attr_const(node);
}

static inline vhdl_cmp_attr_t const *get_vhdl_cmp_attr_const(ir_node const *const node)
{
	return (vhdl_cmp_attr_t const*)get_irn_generic_attr_const(node);
}

static inline vhdl_varsig_attr_t const *get_vhdl_varsig_attr_const(ir_node const *const node)
{
	return (vhdl_varsig_attr_t const*)get_irn_generic_attr_const(node);
}

static inline vhdl_varsig_attr_t *get_vhdl_varsig_attr(ir_node const *const node)
{
	return (vhdl_varsig_attr_t*)get_irn_generic_attr_const(node);
}

static inline vhdl_start_attr_t const *get_vhdl_start_attr_const(ir_node const *const node)
{
	return (vhdl_start_attr_t const*)get_irn_generic_attr_const(node);
}

#endif
