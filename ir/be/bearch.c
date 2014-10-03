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

int arch_possible_memory_operand(const ir_node *irn, unsigned int i)
{
	const arch_irn_ops_t *ops = get_irn_ops(irn);

	if (ops->possible_memory_operand) {
		return ops->possible_memory_operand(irn, i);
	} else {
		return 0;
	}
}

void arch_perform_memory_operand(ir_node *irn, unsigned int i)
{
	const arch_irn_ops_t *ops = get_irn_ops(irn);
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

/**
 * Print information about a register requirement in human readable form
 * @param F   output stream/file
 * @param req The requirements structure to format.
 */
static void arch_dump_register_req(FILE *const F, arch_register_req_t const *const req)
{
	if (req == NULL || req->type == arch_register_req_type_none) {
		fprintf(F, "n/a");
		return;
	}

	fprintf(F, "%s", req->cls->name);

	if (arch_register_req_is(req, limited)) {
		unsigned n_regs = req->cls->n_regs;
		unsigned i;

		fprintf(F, " limited to");
		for (i = 0; i < n_regs; ++i) {
			if (rbitset_is_set(req->limited, i)) {
				const arch_register_t *reg = &req->cls->regs[i];
				fprintf(F, " %s", reg->name);
			}
		}
	}

	if (arch_register_req_is(req, should_be_same)) {
		const unsigned other = req->other_same;
		int i;

		fprintf(F, " same as");
		for (i = 0; 1U << i <= other; ++i) {
			if (other & (1U << i)) {
				ir_fprintf(F, " #%d", i);
			}
		}
	}

	if (arch_register_req_is(req, must_be_different)) {
		const unsigned other = req->other_different;
		int i;

		fprintf(F, " different from");
		for (i = 0; 1U << i <= other; ++i) {
			if (other & (1U << i)) {
				ir_fprintf(F, " #%d", i);
			}
		}
	}

	if (req->width != 1) {
		fprintf(F, " width:%d", req->width);
	}
	if (arch_register_req_is(req, aligned)) {
		fprintf(F, " aligned");
	}
	if (arch_register_req_is(req, ignore)) {
		fprintf(F, " ignore");
	}
	if (arch_register_req_is(req, produces_sp)) {
		fprintf(F, " produces_sp");
	}
}

void arch_dump_reqs_and_registers(FILE *F, const ir_node *node)
{
	backend_info_t *const info = be_get_info(node);
	/* don't fail on invalid graphs */
	if (!info || (!info->in_reqs && get_irn_arity(node) != 0) || !info->out_infos) {
		fprintf(F, "invalid register requirements!!!\n");
		return;
	}

	foreach_irn_in(node, i, op) {
		const arch_register_req_t *req = arch_get_irn_register_req_in(node, i);
		fprintf(F, "inreq #%d = ", i);
		arch_dump_register_req(F, req);
		arch_register_t const *const reg = be_get_info(skip_Proj_const(op))->out_infos ? arch_get_irn_register(op) : NULL;
		fprintf(F, " [%s]\n", reg ? reg->name : "n/a");
	}
	be_foreach_out(node, o) {
		const arch_register_req_t *req = arch_get_irn_register_req_out(node, o);
		fprintf(F, "outreq #%u = ", o);
		arch_dump_register_req(F, req);
		const arch_register_t *reg = arch_get_irn_register_out(node, o);
		fprintf(F, " [%s]\n", reg != NULL ? reg->name : "n/a");
	}

	fprintf(F, "flags =");
	arch_irn_flags_t flags = arch_get_irn_flags(node);
	if (flags == arch_irn_flags_none) {
		fprintf(F, " none");
	} else {
		if (flags & arch_irn_flag_dont_spill)
			fprintf(F, " unspillable");
		if (flags & arch_irn_flag_rematerializable)
			fprintf(F, " remat");
		if (flags & arch_irn_flag_modify_flags)
			fprintf(F, " modify_flags");
		if (flags & arch_irn_flag_simple_jump)
			fprintf(F, " simple_jump");
		if (flags & arch_irn_flag_not_scheduled)
			fprintf(F, " not_scheduled");
		if (flags & arch_irn_flag_schedule_first)
			fprintf(F, " schedule_first");
	}
	fprintf(F, " (0x%x)\n", (unsigned)flags);
}
