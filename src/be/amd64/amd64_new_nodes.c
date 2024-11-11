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
#include "amd64_new_nodes.h"

#include "amd64_bearch_t.h"
#include "amd64_new_nodes_t.h"
#include "amd64_nodes_attr.h"
#include "bedump.h"
#include "gen_amd64_regalloc_if.h"
#include "ircons_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "irprintf.h"
#include "irprog_t.h"
#include "panic.h"
#include "xmalloc.h"
#include <inttypes.h>
#include <stdlib.h>

x87_attr_t *amd64_get_x87_attr(ir_node *const node)
{
	amd64_attr_t const *const attr = get_amd64_attr_const(node);
	switch (attr->op_mode) {
	case AMD64_OP_X87:
		return &get_amd64_x87_attr(node)->x87;
	case AMD64_OP_X87_ADDR_REG:
		return &get_amd64_x87_binop_addr_attr(node)->x87;
	default:
		break;
	}
	panic("try to get x87 attr from invalid node '%s'", node);
}

x87_attr_t const *amd64_get_x87_attr_const(ir_node const *const node)
{
	/* hacky */
	return amd64_get_x87_attr((ir_node *)node);
}

static const char *get_op_mode_string(amd64_op_mode_t const op_mode)
{
	switch (op_mode) {
	case AMD64_OP_ADDR_IMM:     return "addr+imm";
	case AMD64_OP_ADDR_REG:     return "addr+reg";
	case AMD64_OP_ADDR:         return "addr";
	case AMD64_OP_IMM32:        return "imm32";
	case AMD64_OP_IMM64:        return "imm64";
	case AMD64_OP_NONE:         return "none";
	case AMD64_OP_REG_ADDR:     return "reg+addr";
	case AMD64_OP_REG_IMM:      return "reg+imm";
	case AMD64_OP_REG_REG:      return "reg+reg";
	case AMD64_OP_REG:          return "reg";
	case AMD64_OP_SHIFT_IMM:    return "shift_imm";
	case AMD64_OP_SHIFT_REG:    return "shift_reg";
	case AMD64_OP_X87:          return "x87";
	case AMD64_OP_X87_ADDR_REG: return "x87+addr+reg";
	case AMD64_OP_REG_REG_REG:  return "reg+reg+reg";
	case AMD64_OP_REG_REG_ADDR: return "reg+reg+addr";
	case AMD64_OP_CC:           return "cc";
	}
	return "invalid op_mode";
}

void amd64_dump_node(FILE *F, const ir_node *n, dump_reason_t reason)
{
	switch (reason) {
	case dump_node_opcode_txt:
		fprintf(F, "%s", get_irn_opname(n));
		break;

	case dump_node_mode_txt:
		break;

	case dump_node_nodeattr_txt:
		break;

	case dump_node_info_txt: {
		const amd64_attr_t *attr = get_amd64_attr_const(n);
		amd64_op_mode_t const op_mode = attr->op_mode;
		fprintf(F, "mode = %s\n", get_op_mode_string(op_mode));
		fprintf(F, "size = %u\n", x86_bytes_from_size(attr->size));
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
		if (amd64_has_addr_attr(op_mode)) {
			const amd64_addr_attr_t *addr_attr = get_amd64_addr_attr_const(n);
			x86_addr_variant_t const variant = addr_attr->addr.variant;
			fprintf(F, "am variant = %s\n", x86_get_addr_variant_str(variant));
			if (x86_addr_variant_has_base(variant))
				fprintf(F, "base input: %d\n", addr_attr->addr.base_input);
			if (x86_addr_variant_has_index(variant))
				fprintf(F, "index input: %d\n", addr_attr->addr.index_input);
			fputs("am imm: ", F);
			x86_dump_imm32(&addr_attr->addr.immediate, F);
			fputc('\n', F);
			break;
		}
	}
	}
}

void init_amd64_attributes(ir_node *node, amd64_op_mode_t op_mode,
                           x86_insn_size_t size)
{
	amd64_attr_t *attr = get_amd64_attr(node);
	attr->op_mode = op_mode;
	attr->size    = size;
}

void init_amd64_cc_attributes(ir_node *node, x86_condition_code_t cc)
{
	amd64_cc_attr_t *attr = get_amd64_cc_attr(node);
	attr->cc = cc;
}

void init_amd64_movimm_attributes(ir_node *node, const amd64_imm64_t *imm)
{
	amd64_movimm_attr_t *attr = get_amd64_movimm_attr(node);
	attr->immediate = *imm;
}

void init_amd64_copyb_attributes(ir_node *node, unsigned size)
{
	amd64_copyb_attr_t *attr = get_amd64_copyb_attr(node);
	attr->size = size;
}

static bool imm64s_equal(const amd64_imm64_t *const imm0,
                         const amd64_imm64_t *const imm1)
{
	return imm0->offset == imm1->offset && imm0->entity == imm1->entity;
}

int amd64_attrs_equal(const ir_node *a, const ir_node *b)
{
	const amd64_attr_t *attr_a = get_amd64_attr_const(a);
	const amd64_attr_t *attr_b = get_amd64_attr_const(b);
	return attr_a->op_mode == attr_b->op_mode
	    && attr_a->size == attr_b->size;
}

int amd64_addr_attrs_equal(const ir_node *a, const ir_node *b)
{
	const amd64_addr_attr_t *attr_a = get_amd64_addr_attr_const(a);
	const amd64_addr_attr_t *attr_b = get_amd64_addr_attr_const(b);
	return amd64_attrs_equal(a, b)
	    && x86_addrs_equal(&attr_a->addr, &attr_b->addr);
}

int amd64_binop_addr_attrs_equal(const ir_node *a, const ir_node *b)
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

int amd64_movimm_attrs_equal(const ir_node *const a, const ir_node *const b)
{
	const amd64_movimm_attr_t *const attr_a = get_amd64_movimm_attr_const(a);
	const amd64_movimm_attr_t *const attr_b = get_amd64_movimm_attr_const(b);
	return amd64_attrs_equal(a, b)
	    && imm64s_equal(&attr_a->immediate, &attr_b->immediate);
}

int amd64_copyb_attrs_equal(const ir_node *const a, const ir_node *const b)
{
	const amd64_copyb_attr_t *const attr_a = get_amd64_copyb_attr_const(a);
	const amd64_copyb_attr_t *const attr_b = get_amd64_copyb_attr_const(b);
	return amd64_attrs_equal(a, b)
	    && attr_a->size == attr_b->size;
}

int amd64_shift_attrs_equal(const ir_node *const a, const ir_node *const b)
{
	const amd64_shift_attr_t *const attr_a = get_amd64_shift_attr_const(a);
	const amd64_shift_attr_t *const attr_b = get_amd64_shift_attr_const(b);
	return amd64_attrs_equal(a, b)
	    && attr_a->immediate == attr_b->immediate;
}

int amd64_cc_attrs_equal(const ir_node *const a, const ir_node *const b)
{
	const amd64_cc_attr_t *const attr_a = get_amd64_cc_attr_const(a);
	const amd64_cc_attr_t *const attr_b = get_amd64_cc_attr_const(b);
	return amd64_attrs_equal(a, b) && attr_a->cc == attr_b->cc;
}

int amd64_switch_jmp_attrs_equal(const ir_node *const a, const ir_node *const b)
{
	const amd64_switch_jmp_attr_t *const attr_a
		= get_amd64_switch_jmp_attr_const(a);
	const amd64_switch_jmp_attr_t *const attr_b
		= get_amd64_switch_jmp_attr_const(b);
	return amd64_attrs_equal(a, b) && be_switch_attrs_equal(&attr_a->swtch, &attr_b->swtch);
}

int amd64_call_addr_attrs_equal(const ir_node *const a, const ir_node *const b)
{
	const amd64_call_addr_attr_t *const attr_a
		= get_amd64_call_addr_attr_const(a);
	const amd64_call_addr_attr_t *const attr_b
		= get_amd64_call_addr_attr_const(b);
	return amd64_addr_attrs_equal(a, b)
	    && attr_a->n_reg_results == attr_b->n_reg_results;
}

int amd64_x87_attrs_equal(const ir_node *const a, const ir_node *const b)
{
	/* we ignore x87 attributes for now */
	return amd64_attrs_equal(a, b);
}

int amd64_x87_addr_attrs_equal(const ir_node *const a, const ir_node *const b)
{
	/* ignore x87 part for now */
	return amd64_addr_attrs_equal(a, b);
}

int amd64_x87_binop_addr_attrs_equal(const ir_node *const a,
                                     const ir_node *const b)
{
	/* ignore x87 part for now */
	return amd64_binop_addr_attrs_equal(a, b);
}
