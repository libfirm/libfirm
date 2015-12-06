/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Processor architecture specification.
 * @author      Sebastian Hack
 */
#include "be_t.h"
#include "bearch.h"
#include "benode.h"
#include "beinfo.h"
#include "beirg.h"
#include "bemodule.h"
#include "ircons_t.h"
#include "irnode_t.h"
#include "irop_t.h"
#include "raw_bitset.h"
#include "util.h"

static arch_register_class_t arch_exec_cls = {
	.name      = "exec",
	.mode      = NULL, /* Filled in at initialization. */
	.regs      = NULL,
	.class_req = arch_exec_req,
	.index     = (unsigned)-1,
	.n_regs    = 0,
	.manual_ra = true,
};

arch_register_req_t const arch_exec_requirement = {
	.cls = &arch_exec_cls,
};

static arch_register_class_t arch_memory_cls = {
	.name      = "memory",
	.mode      = NULL, /* Filled in at initialization. */
	.regs      = NULL,
	.class_req = arch_memory_req,
	.index     = (unsigned)-1,
	.n_regs    = 0,
	.manual_ra = true,
};

arch_register_req_t const arch_memory_requirement = {
	.cls = &arch_memory_cls,
};

static arch_register_class_t arch_none_cls = {
	.name      = "none",
	.mode      = NULL,
	.regs      = NULL,
	.class_req = arch_no_register_req,
	.index     = (unsigned)-1,
	.n_regs    = 0,
	.manual_ra = true,
};

arch_register_req_t const arch_no_requirement = {
	.cls = &arch_none_cls,
};

const arch_register_t *arch_get_irn_register(const ir_node *node)
{
	const reg_out_info_t *out = get_out_info(node);
	return out->reg;
}

const arch_register_t *arch_get_irn_register_out(const ir_node *node,
                                                 unsigned pos)
{
	const reg_out_info_t *out = get_out_info_n(node, pos);
	return out->reg;
}

const arch_register_t *arch_get_irn_register_in(const ir_node *node, int pos)
{
	ir_node *op = get_irn_n(node, pos);
	return arch_get_irn_register(op);
}

void arch_set_irn_register_out(ir_node *node, unsigned pos,
                               const arch_register_t *reg)
{
	reg_out_info_t *out = get_out_info_n(node, pos);
	out->reg            = reg;
}

void arch_set_irn_register(ir_node *node, const arch_register_t *reg)
{
	reg_out_info_t *out = get_out_info(node);
	out->reg = reg;
}

bool arch_reg_is_allocatable(const arch_register_req_t *req,
                             const arch_register_t *reg)
{
	if (req->cls != reg->cls)
		return false;
	if (reg->is_virtual)
		return true;
	if (req->limited != NULL)
		return rbitset_is_set(req->limited, reg->index);
	return true;
}

arch_register_t const *arch_find_register(char const *const name)
{
	arch_register_t const *const regs = isa_if->registers;
	for (size_t i = 0, n = isa_if->n_registers; i < n; ++i) {
		arch_register_t const *const reg = &regs[i];
		if (streq(reg->name, name))
			return reg;
	}
	return NULL;
}

void arch_set_additional_pressure(ir_node *const node,
                                  arch_register_class_t const *const cls,
                                  uint8_t const pressure)
{
	backend_info_t *const info = be_get_info(node);
	/* restricted to the first few classes for now */
	unsigned index = cls->index;
	assert(index < ARRAY_SIZE(info->add_pressure));
	info->add_pressure[index] = pressure;
}

uint8_t arch_get_additional_pressure(ir_node const *const node,
                                     arch_register_class_t const *const cls)
{
	backend_info_t *const info = be_get_info(node);
	/* restricted to the first few classes for now */
	unsigned index = cls->index;
	if (index < ARRAY_SIZE(info->add_pressure))
		return info->add_pressure[index];
	return 0;
}

void arch_copy_irn_out_info(ir_node *const dst, unsigned const dst_pos, ir_node const *const src)
{
	reg_out_info_t *const src_info = get_out_info(src);
	reg_out_info_t *const dst_info = get_out_info_n(dst, dst_pos);
	*dst_info = *src_info;
}

int be_default_is_valid_clobber(char const *const clobber)
{
	(void)clobber;
	return false;
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch)
void be_init_arch(void)
{
	arch_exec_cls.mode   = mode_X;
	arch_memory_cls.mode = mode_M;
}
