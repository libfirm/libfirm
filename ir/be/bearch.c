/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Processor architecture specification.
 * @author      Sebastian Hack
 */
#include "bearch.h"
#include "benode.h"
#include "beinfo.h"
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

/**
 * Get the isa responsible for a node.
 * @param irn The node to get the responsible isa for.
 * @return The irn operations given by the responsible isa.
 */
static const arch_irn_ops_t *get_irn_ops(const ir_node *irn)
{
	ir_op          const *const op     = get_irn_op(irn);
	arch_irn_ops_t const *const be_ops = get_op_ops(op)->be_ops;
	assert(be_ops);
	return be_ops;
}

void arch_set_frame_offset(ir_node *irn, int offset)
{
	const arch_irn_ops_t *ops = get_irn_ops(irn);
	ops->set_frame_offset(irn, offset);
}

ir_entity *arch_get_frame_entity(const ir_node *irn)
{
	const arch_irn_ops_t *ops = get_irn_ops(irn);
	return ops->get_frame_entity(irn);
}

int arch_get_sp_bias(ir_node *irn)
{
	const arch_irn_ops_t *ops = get_irn_ops(irn);
	return ops->get_sp_bias(irn);
}

void arch_perform_memory_operand(ir_node *irn, unsigned int i)
{
	const arch_irn_ops_t *ops = get_irn_ops(irn);
	if (ops->perform_memory_operand)
		ops->perform_memory_operand(irn, i);
}

int arch_get_op_estimated_cost(const ir_node *irn)
{
	const arch_irn_ops_t *ops = get_irn_ops(irn);

	if (ops->get_op_estimated_cost) {
		return ops->get_op_estimated_cost(irn);
	} else {
		return 1;
	}
}

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

void arch_set_irn_flags(ir_node *node, arch_irn_flags_t flags)
{
	backend_info_t *const info = be_get_info(node);
	info->flags = flags;
}

void arch_add_irn_flags(ir_node *node, arch_irn_flags_t flags)
{
	backend_info_t *const info = be_get_info(node);
	info->flags |= flags;
}

bool arch_reg_is_allocatable(const arch_register_req_t *req,
                             const arch_register_t *reg)
{
	assert(req->type != arch_register_req_type_none);
	if (req->cls != reg->reg_class)
		return false;
	if (reg->type & arch_register_type_virtual)
		return true;
	if (arch_register_req_is(req, limited))
		return rbitset_is_set(req->limited, reg->index);
	return true;
}

void be_make_start_mem(be_start_info_t *const info, ir_node *const start, unsigned const pos)
{
  info->pos = pos;
  info->irn = NULL;
  arch_set_irn_register_req_out(start, pos, arch_no_register_req);
}

void be_make_start_out(be_start_info_t *const info, struct obstack *const obst, ir_node *const start, unsigned const pos, arch_register_t const *const reg, arch_register_req_type_t const flags)
{
	info->pos = pos;
	info->irn = NULL;
	arch_register_req_t const *const req =
		flags == arch_register_req_type_none ? reg->single_req :
		be_create_reg_req(obst, reg, flags);
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
