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
#include "ircons_t.h"
#include "irnode_t.h"
#include "irop_t.h"
#include "raw_bitset.h"

arch_register_req_t const arch_no_requirement = {
	.cls             = NULL,
	.limited         = NULL,
	.type            = arch_register_req_type_none,
	.other_same      = 0,
	.other_different = 0,
	.width           = 0,
};

static reg_out_info_t *get_out_info_n(const ir_node *node, unsigned pos)
{
	const backend_info_t *info = be_get_info(node);
	assert(pos < (unsigned)ARR_LEN(info->out_infos));
	return &info->out_infos[pos];
}


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
		if (strcmp(reg->name, name) == 0)
			return reg;
	}
	return NULL;
}

void be_make_start_mem(be_start_info_t *const info, ir_node *const start, unsigned const pos)
{
  info->pos = pos;
  info->irn = NULL;
  arch_set_irn_register_req_out(start, pos, arch_no_register_req);
}

void be_make_start_out(be_start_info_t *const info, ir_node *const start, unsigned const pos, arch_register_t const *const reg, arch_register_req_type_t const flags)
{
	info->pos = pos;
	info->irn = NULL;
	arch_register_req_t const *const req =
		flags == arch_register_req_type_none ? reg->single_req :
		be_create_reg_req(be_get_be_obst(get_irn_irg(start)), reg, flags);
	arch_set_irn_register_req_out(start, pos, req);
	arch_set_irn_register_out(start, pos, reg);
}

ir_node *be_get_start_proj(ir_graph *const irg, be_start_info_t *const info)
{
	if (!info->irn) {
		/* This is already the transformed start node. */
		ir_node                     *const start = get_irg_start(irg);
		arch_register_class_t const *const cls   = arch_get_irn_register_req_out(start, info->pos)->cls;
		info->irn = new_r_Proj(start, cls ? cls->mode : mode_M, info->pos);
	}
	return info->irn;
}

void arch_copy_irn_out_info(ir_node *const dst, unsigned const dst_pos, ir_node const *const src)
{
	reg_out_info_t *const src_info = get_out_info(src);
	reg_out_info_t *const dst_info = get_out_info_n(dst, dst_pos);
	*dst_info = *src_info;
}
