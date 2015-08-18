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

#include "panic.h"
#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irmode_t.h"
#include "ircons_t.h"
#include "iropt_t.h"
#include "irop.h"
#include "irprintf.h"
#include "xmalloc.h"
#include "bedump.h"

#include "amd64_nodes_attr.h"
#include "amd64_new_nodes.h"
#include "bearch_amd64_t.h"
#include "gen_amd64_regalloc_if.h"

static const char *get_op_mode_string(amd64_op_mode_t mode)
{
	switch (mode) {
	case AMD64_OP_ADDR_IMM:   return "addr+imm";
	case AMD64_OP_ADDR_REG:   return "addr+reg";
	case AMD64_OP_ADDR:       return "addr";
	case AMD64_OP_IMM32:      return "imm32";
	case AMD64_OP_IMM64:      return "imm64";
	case AMD64_OP_NONE:       return "none";
	case AMD64_OP_RAX_ADDR:   return "rax_addr";
	case AMD64_OP_RAX_REG:    return "rax_reg";
	case AMD64_OP_REG_ADDR:   return "reg+addr";
	case AMD64_OP_REG_IMM:    return "reg+imm";
	case AMD64_OP_REG_REG:    return "reg+reg";
	case AMD64_OP_REG:        return "reg";
	case AMD64_OP_SHIFT_IMM:  return "shift_imm";
	case AMD64_OP_SHIFT_REG:  return "shift_reg";
	case AMD64_OP_UNOP_ADDR:  return "unop_addr";
	case AMD64_OP_UNOP_IMM32: return "unop_imm32";
	case AMD64_OP_UNOP_REG:   return "unop_reg";
	}
	panic("invalid op_mode");
}

static const char *get_insn_mode_string(amd64_insn_mode_t mode)
{
	switch (mode) {
	case INSN_MODE_8:   return "8";
	case INSN_MODE_16:  return "16";
	case INSN_MODE_32:  return "32";
	case INSN_MODE_64:  return "64";
	case INSN_MODE_128: return "128";
	case INSN_MODE_INVALID: break;
	}
	panic("invalid insn_mode");
}

/**
 * Dumper interface for dumping amd64 nodes in vcg.
 * @param F        the output file
 * @param n        the node to dump
 * @param reason   indicates which kind of information should be dumped
 */
static void amd64_dump_node(FILE *F, const ir_node *n, dump_reason_t reason)
{
	switch (reason) {
	case dump_node_opcode_txt:
		fprintf(F, "%s", get_irn_opname(n));
		break;

	case dump_node_mode_txt:
		fprintf(F, "[%s]", get_mode_name(get_irn_mode(n)));
		break;

	case dump_node_nodeattr_txt:
		break;

	case dump_node_info_txt: {
		const amd64_attr_t *attr = get_amd64_attr_const(n);
		amd64_op_mode_t const op_mode = attr->op_mode;
		fprintf(F, "mode = %s\n", get_op_mode_string(op_mode));
		switch (op_mode) {
		case AMD64_OP_ADDR_REG:
		case AMD64_OP_REG_ADDR: {
			const amd64_binop_addr_attr_t *binop_attr = get_amd64_binop_addr_attr_const(n);
			fprintf(F, "reg input: %d\n", binop_attr->u.reg_input);
			break;
		}
		case AMD64_OP_IMM64: {
			const amd64_imm64_t *const imm
				= &get_amd64_movimm_attr_const(n)->immediate;
			ir_fprintf(F, "imm64 entity: %+F\n", imm->entity);
			fprintf(F, "imm64 offset: 0x%" PRIX64 "\n", (uint64_t)imm->offset);
			break;
		}
		default:
			break;
		}
		if (amd64_has_addr_attr(n)) {
			const amd64_addr_attr_t *addr_attr = get_amd64_addr_attr_const(n);
			fprintf(F, "size = %s\n", get_insn_mode_string(addr_attr->insn_mode));
			fprintf(F, "base input: %d\n", addr_attr->addr.base_input);
			fprintf(F, "index input: %d\n", addr_attr->addr.index_input);
			fputs("am imm: ", F);
			x86_dump_imm32(&addr_attr->addr.immediate, F);
			fputc('\n', F);
			break;
		}
	}
	}
}

static void init_amd64_attributes(ir_node *node, arch_irn_flags_t flags,
                                  const arch_register_req_t **in_reqs,
                                  int n_res, amd64_op_mode_t op_mode)
{
	be_info_init_irn(node, flags, in_reqs, n_res);
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

	be_foreach_out(node, o) {
		arch_set_irn_register_req_out(node, o, arch_exec_req);
	}
}

static void init_amd64_cc_attributes(ir_node *node, x86_condition_code_t cc)
{
	amd64_cc_attr_t *attr = get_amd64_cc_attr(node);
	attr->cc = cc;
}

static void init_amd64_movimm_attributes(ir_node *node,
                                         amd64_insn_mode_t insn_mode,
                                         const amd64_imm64_t *imm)
{
	amd64_movimm_attr_t *attr = get_amd64_movimm_attr(node);
	attr->insn_mode = insn_mode;
	attr->immediate = *imm;
}

static bool imm64s_equal(const amd64_imm64_t *const imm0,
                         const amd64_imm64_t *const imm1)
{
	return imm0->offset == imm1->offset && imm0->entity == imm1->entity;
}

static bool amd64_addrs_equal(const amd64_addr_t *const am0,
                              const amd64_addr_t *const am1)
{
	return x86_imm32_equal(&am0->immediate, &am1->immediate)
	    && am0->base_input == am1->base_input
	    && am0->index_input == am1->index_input
	    && am0->log_scale == am1->log_scale
	    && am0->segment == am1->segment;
}

static int amd64_attrs_equal(const ir_node *a, const ir_node *b)
{
	const amd64_attr_t *attr_a = get_amd64_attr_const(a);
	const amd64_attr_t *attr_b = get_amd64_attr_const(b);
	return attr_a->op_mode == attr_b->op_mode;
}

static int amd64_addr_attrs_equal(const ir_node *a, const ir_node *b)
{
	const amd64_addr_attr_t *attr_a = get_amd64_addr_attr_const(a);
	const amd64_addr_attr_t *attr_b = get_amd64_addr_attr_const(b);
	return amd64_attrs_equal(a, b)
	    && amd64_addrs_equal(&attr_a->addr, &attr_b->addr)
	    && attr_a->insn_mode == attr_b->insn_mode
	    && attr_a->needs_frame_ent == attr_b->needs_frame_ent;
}

static int amd64_binop_addr_attrs_equal(const ir_node *a,
                                        const ir_node *b)
{
	const amd64_binop_addr_attr_t *attr_a = get_amd64_binop_addr_attr_const(a);
	const amd64_binop_addr_attr_t *attr_b = get_amd64_binop_addr_attr_const(b);
	if (!amd64_addr_attrs_equal(a, b))
		return false;
	amd64_op_mode_t op_mode = attr_a->base.base.op_mode;
	if (op_mode == AMD64_OP_REG_IMM || op_mode == AMD64_OP_ADDR_IMM) {
		return x86_imm32_equal(&attr_a->u.immediate, &attr_b->u.immediate);
	} else {
		return attr_a->u.reg_input == attr_b->u.reg_input;
	}
}

static int amd64_movimm_attrs_equal(const ir_node *const a,
                                    const ir_node *const b)
{
	const amd64_movimm_attr_t *const attr_a = get_amd64_movimm_attr_const(a);
	const amd64_movimm_attr_t *const attr_b = get_amd64_movimm_attr_const(b);
	return amd64_attrs_equal(a, b)
	    && imm64s_equal(&attr_a->immediate, &attr_b->immediate)
	    && attr_a->insn_mode == attr_b->insn_mode;
}

static int amd64_shift_attrs_equal(const ir_node *const a,
                                   const ir_node *const b)
{
	const amd64_shift_attr_t *const attr_a = get_amd64_shift_attr_const(a);
	const amd64_shift_attr_t *const attr_b = get_amd64_shift_attr_const(b);
	return amd64_attrs_equal(a, b)
	    && attr_a->immediate == attr_b->immediate
	    && attr_a->insn_mode == attr_b->insn_mode;
}

static int amd64_cc_attrs_equal(const ir_node *const a,
                                const ir_node *const b)
{
	const amd64_cc_attr_t *const attr_a = get_amd64_cc_attr_const(a);
	const amd64_cc_attr_t *const attr_b = get_amd64_cc_attr_const(b);
	return amd64_attrs_equal(a, b) && attr_a->cc == attr_b->cc;
}

static int amd64_switch_jmp_attrs_equal(const ir_node *const a,
                                        const ir_node *const b)
{
	const amd64_switch_jmp_attr_t *const attr_a
		= get_amd64_switch_jmp_attr_const(a);
	const amd64_switch_jmp_attr_t *const attr_b
		= get_amd64_switch_jmp_attr_const(b);
	return amd64_attrs_equal(a, b) && attr_a->table == attr_b->table;
}

/* Include the generated constructor functions */
#include "gen_amd64_new_nodes.c.inl"
