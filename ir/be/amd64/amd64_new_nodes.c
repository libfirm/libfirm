/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   This file implements the creation of the achitecture specific firm
 *          opcodes and the coresponding node constructors for the amd64
 *          assembler irg.
 */
#include <stdlib.h>
#include <inttypes.h>

#include "error.h"
#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irmode_t.h"
#include "ircons_t.h"
#include "iropt_t.h"
#include "irop.h"
#include "irprintf.h"
#include "xmalloc.h"

#include "bearch.h"

#include "amd64_nodes_attr.h"
#include "amd64_new_nodes.h"
#include "gen_amd64_regalloc_if.h"

/**
 * Dumper interface for dumping amd64 nodes in vcg.
 * @param F        the output file
 * @param n        the node to dump
 * @param reason   indicates which kind of information should be dumped
 */
static void amd64_dump_node(FILE *F, const ir_node *n, dump_reason_t reason)
{
	ir_mode *mode = NULL;

	switch (reason) {
	case dump_node_opcode_txt:
		fprintf(F, "%s", get_irn_opname(n));
		break;

	case dump_node_mode_txt:
		mode = get_irn_mode(n);

		if (mode) {
			fprintf(F, "[%s]", get_mode_name(mode));
		} else {
			fprintf(F, "[?NOMODE?]");
		}
		break;

	case dump_node_nodeattr_txt:
		break;

	case dump_node_info_txt:
		arch_dump_reqs_and_registers(F, n);
		const amd64_attr_t *attr = get_amd64_attr_const(n);
		fputs("mode = ", F);
		switch (attr->op_mode) {
		case AMD64_OP_REG_REG:  fputs("reg+reg\n", F);  break;
		case AMD64_OP_REG_IMM:  fputs("reg+imm\n", F);  break;
		case AMD64_OP_ADDR_REG: fputs("load+reg\n", F); break;
		case AMD64_OP_ADDR:     fputs("load\n", F);     break;
		case AMD64_OP_REG:      fputs("reg\n", F);      break;
		}
		if (attr->op_mode != AMD64_OP_NONE) {
			const amd64_addr_attr_t *addr_attr
				= (const amd64_addr_attr_t*)attr;
			fputs("size = ", F);
			switch (addr_attr->insn_mode) {
			case INSN_MODE_8:  fputs("8\n", F); break;
			case INSN_MODE_16: fputs("16\n", F); break;
			case INSN_MODE_32: fputs("32\n", F); break;
			case INSN_MODE_64: fputs("64\n", F); break;
			}
		}
		if (attr->op_mode == AMD64_OP_ADDR_REG) {
			const amd64_binop_addr_attr_t *binop_attr
				= (const amd64_binop_addr_attr_t*)attr;
			fprintf(F, "reg input: %d\n", binop_attr->u.reg_input);
		}
		if (attr->op_mode == AMD64_OP_ADDR_REG
		 || attr->op_mode == AMD64_OP_ADDR) {
			const amd64_addr_attr_t *addr_attr
				= (const amd64_addr_attr_t*)attr;
			fprintf(F, "base input: %d\n", addr_attr->addr.base_input);
			fprintf(F, "index input: %d\n", addr_attr->addr.index_input);
			ir_fprintf(F, "am imm: %+F%+" PRId32 "\n",
			           addr_attr->addr.immediate.entity,
					   addr_attr->addr.immediate.offset);
		}
		break;
	}
}

static void init_be_info(ir_node *node, arch_irn_flags_t flags,
                         const arch_register_req_t **in_reqs, int n_res)
{
	arch_set_irn_flags(node, flags);
	arch_set_irn_register_reqs_in(node, in_reqs);

	ir_graph       *irg  = get_irn_irg(node);
	struct obstack *obst = get_irg_obstack(irg);
	backend_info_t *info = be_get_info(node);
	info->out_infos = NEW_ARR_DZ(reg_out_info_t, obst, n_res);
}

static void init_amd64_attributes(ir_node *node, arch_irn_flags_t flags,
                                  const arch_register_req_t **in_reqs,
                                  int n_res, amd64_op_mode_t op_mode)
{
	init_be_info(node, flags, in_reqs, n_res);
	amd64_attr_t *attr = get_amd64_attr(node);
	attr->op_mode = op_mode;
}

static void init_amd64_switch_attributes(ir_node *node,
                                         const ir_switch_table *table,
                                         ir_entity *table_entity)
{
	amd64_switch_jmp_attr_t *attr = get_amd64_switch_jmp_attr(node);
	attr->table        = table;
	attr->table_entity = table_entity;

	for (unsigned o = 0, n_outs = arch_get_irn_n_outs(node); o < n_outs; o++) {
		arch_set_irn_register_req_out(node, o, arch_no_register_req);
	}
}

static void init_amd64_cc_attributes(ir_node *node, x86_condition_code_t cc)
{
	amd64_cc_attr_t *attr = get_amd64_cc_attr(node);
	attr->cc = cc;
}

static void init_amd64_movimm_attributes(ir_node *node,
                                         amd64_insn_mode_t insn_mode,
                                         ir_entity *entity, int64_t offset)
{
	amd64_movimm_attr_t *attr = get_amd64_movimm_attr(node);
	attr->insn_mode        = insn_mode;
	attr->immediate.entity = entity;
	attr->immediate.offset = offset;
}

static int cmp_imm32(const amd64_imm32_t *const imm0,
                     const amd64_imm32_t *const imm1)
{
	return imm0->offset != imm1->offset || imm0->entity != imm1->entity;
}

static int cmp_imm64(const amd64_imm64_t *const imm0,
                     const amd64_imm64_t *const imm1)
{
	return imm0->offset != imm1->offset || imm0->entity != imm1->entity;
}

static int cmp_addr(const amd64_addr_t *const am0,
                    const amd64_addr_t *const am1)
{
	return cmp_imm32(&am0->immediate, &am1->immediate)
	    || am0->base_input != am1->base_input
	    || am0->index_input != am1->index_input
	    || am0->log_scale != am1->log_scale
	    || am0->segment != am1->segment;
}

static int cmp_amd64_attr(const ir_node *a, const ir_node *b)
{
	const amd64_attr_t *attr_a = get_amd64_attr_const(a);
	const amd64_attr_t *attr_b = get_amd64_attr_const(b);
	return attr_a->op_mode != attr_b->op_mode;
}

static int cmp_amd64_addr_attr(const ir_node *a, const ir_node *b)
{
	const amd64_addr_attr_t *attr_a = get_amd64_addr_attr_const(a);
	const amd64_addr_attr_t *attr_b = get_amd64_addr_attr_const(b);
	return cmp_amd64_attr(a, b)
	    || cmp_addr(&attr_a->addr, &attr_b->addr)
	    || attr_a->insn_mode != attr_b->insn_mode
	    || attr_a->needs_frame_ent != attr_b->needs_frame_ent;
}

static int cmp_amd64_unop_attr(const ir_node *a, const ir_node *b)
{
	const amd64_unop_attr_t *attr_a = get_amd64_unop_attr_const(a);
	const amd64_unop_attr_t *attr_b = get_amd64_unop_attr_const(b);
	return cmp_amd64_attr(a, b)
	    || attr_a->insn_mode != attr_b->insn_mode;
}

static int cmp_amd64_binop_addr_attr(const ir_node *a,
                                     const ir_node *b)
{
	const amd64_binop_addr_attr_t *attr_a = get_amd64_binop_addr_attr_const(a);
	const amd64_binop_addr_attr_t *attr_b = get_amd64_binop_addr_attr_const(b);
	if (cmp_amd64_addr_attr(a, b))
		return 1;
	amd64_op_mode_t op_mode = attr_a->base.base.op_mode;
	if (op_mode == AMD64_OP_REG_IMM || op_mode == AMD64_OP_ADDR_IMM) {
		return cmp_imm32(&attr_a->u.immediate, &attr_b->u.immediate);
	} else {
		return attr_a->u.reg_input != attr_b->u.reg_input;
	}
}

static int cmp_amd64_movimm_attr(const ir_node *const a,
                                 const ir_node *const b)
{
	const amd64_movimm_attr_t *const attr_a = get_amd64_movimm_attr_const(a);
	const amd64_movimm_attr_t *const attr_b = get_amd64_movimm_attr_const(b);
	return cmp_amd64_attr(a, b)
	    || cmp_imm64(&attr_a->immediate, &attr_b->immediate)
	    || attr_a->insn_mode != attr_b->insn_mode;
}

static int cmp_amd64_shift_attr(const ir_node *const a,
                                const ir_node *const b)
{
	const amd64_shift_attr_t *const attr_a = get_amd64_shift_attr_const(a);
	const amd64_shift_attr_t *const attr_b = get_amd64_shift_attr_const(b);
	return cmp_amd64_attr(a, b)
	    || attr_a->immediate != attr_b->immediate
	    || attr_a->insn_mode != attr_b->insn_mode;
}

static int cmp_amd64_cc_attr(const ir_node *const a,
                             const ir_node *const b)
{
	if (cmp_amd64_attr(a, b))
		return true;
	const amd64_cc_attr_t *const attr_a = get_amd64_cc_attr_const(a);
	const amd64_cc_attr_t *const attr_b = get_amd64_cc_attr_const(b);
	return attr_a->cc != attr_b->cc;
}

static int cmp_amd64_switch_jmp_attr(const ir_node *const a,
                                    const ir_node *const b)
{
	if (cmp_amd64_attr(a, b))
		return true;
	const amd64_switch_jmp_attr_t *const attr_a
		= get_amd64_switch_jmp_attr_const(a);
	const amd64_switch_jmp_attr_t *const attr_b
		= get_amd64_switch_jmp_attr_const(b);
	return attr_a->table != attr_b->table;
}

static void amd64_copy_attr(ir_graph *irg, const ir_node *old_node,
                            ir_node *new_node)
{
	struct obstack   *obst       = get_irg_obstack(irg);
	const amd64_attr_t *attr_old = get_amd64_attr_const(old_node);
	amd64_attr_t     *attr_new   = get_amd64_attr(new_node);
	backend_info_t   *old_info   = be_get_info(old_node);
	backend_info_t   *new_info   = be_get_info(new_node);

	/* copy the attributes */
	memcpy(attr_new, attr_old, get_op_attr_size(get_irn_op(old_node)));

	/* copy out flags */
	new_info->flags = old_info->flags;
	new_info->out_infos =
		DUP_ARR_D(reg_out_info_t, obst, old_info->out_infos);
	new_info->in_reqs = old_info->in_reqs;
}

/* Include the generated constructor functions */
#include "gen_amd64_new_nodes.c.inl"
