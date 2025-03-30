/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief  This file implements the creation of the architecture specific firm
 *         opcodes and the corresponding node constructors for the arm
 *         assembler irg.
 * @author Oliver Richter, Tobias Gneist
 */
#include "arm_new_nodes_t.h"

#include "arm_bearch_t.h"
#include "arm_nodes_attr.h"
#include "arm_optimize.h"
#include "bedump.h"
#include "ircons_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "irop_t.h"
#include "iropt_t.h"
#include "irprintf.h"
#include "irprog_t.h"
#include "xmalloc.h"
#include <stdbool.h>
#include <stdlib.h>

static bool arm_has_address_attr(const ir_node *node)
{
	return is_arm_Address(node) || is_arm_FrameAddr(node) || is_arm_Bl(node);
}

static bool has_load_store_attr(const ir_node *node)
{
	return is_arm_Ldr(node) || is_arm_Str(node) || is_arm_LinkLdrPC(node)
		|| is_arm_Ldf(node) || is_arm_Stf(node);
}

static bool has_shifter_operand(const ir_node *node)
{
	return is_arm_Add(node) || is_arm_And(node) || is_arm_Orr(node)
		|| is_arm_Eor(node) || is_arm_Bic(node) || is_arm_Sub(node)
		|| is_arm_Rsb(node) || is_arm_Mov(node) || is_arm_Mvn(node)
		|| is_arm_Cmn(node) || is_arm_Cmp(node) || is_arm_Tst(node)
		|| is_arm_LinkMovPC(node);
}

static bool has_cmp_attr(const ir_node *node)
{
	return is_arm_Cmn(node) || is_arm_Cmp(node) || is_arm_Tst(node);
}

static bool has_farith_attr(const ir_node *node)
{
	return is_arm_Adf(node) || is_arm_Muf(node) || is_arm_Suf(node)
	    || is_arm_Dvf(node) || is_arm_Mvf(node) || is_arm_Flt(node);
}

void arm_dump_node(FILE *F, const ir_node *n, dump_reason_t reason)
{
	switch (reason) {
	case dump_node_opcode_txt:
		fprintf(F, "%s", get_irn_opname(n));

		if (arm_has_address_attr(n)) {
			const arm_Address_attr_t *attr = get_arm_Address_attr_const(n);
			if (attr->entity != NULL)
				ir_fprintf(F, " %F", attr->entity);
		}
		break;

	case dump_node_mode_txt:
		/* mode isn't relevant in the backend */
		break;

	case dump_node_nodeattr_txt:
		/* TODO: dump shift modifiers */
		break;

	case dump_node_info_txt:
		if (has_load_store_attr(n)) {
			const arm_load_store_attr_t *attr
				= get_arm_load_store_attr_const(n);
			ir_fprintf(F, "load_store_mode = %+F\n", attr->load_store_mode);
			ir_fprintf(F, "entity = %+F\n", attr->entity);
			fprintf(F, "offset = %ld\n", attr->offset);
			fprintf(F, "is_frame_entity = %s\n", be_dump_yesno(attr->is_frame_entity));
			fprintf(F, "entity_sign = %s\n", be_dump_yesno(attr->entity_sign));
		}
		if (has_shifter_operand(n)) {
			const arm_shifter_operand_t *attr
				= get_arm_shifter_operand_attr_const(n);
			switch (attr->shift_modifier) {
			case ARM_SHF_REG:
				break;
			case ARM_SHF_IMM:
				fprintf(F, "modifier = imm %d ror %d\n",
				        attr->immediate_value, attr->shift_immediate);
				break;
			case ARM_SHF_ASR_IMM:
				fprintf(F, "modifier = V >>s %d\n", attr->shift_immediate);
				break;
			case ARM_SHF_ASR_REG:
				fprintf(F, "modifier = V >>s R\n");
				break;
			case ARM_SHF_LSL_IMM:
				fprintf(F, "modifier = V << %d\n", attr->shift_immediate);
				break;
			case ARM_SHF_LSL_REG:
				fprintf(F, "modifier = V << R\n");
				break;
			case ARM_SHF_LSR_IMM:
				fprintf(F, "modifier = V >> %d\n", attr->shift_immediate);
				break;
			case ARM_SHF_LSR_REG:
				fprintf(F, "modifier = V >> R\n");
				break;
			case ARM_SHF_ROR_IMM:
				fprintf(F, "modifier = V ROR %d\n", attr->shift_immediate);
				break;
			case ARM_SHF_ROR_REG:
				fprintf(F, "modifier = V ROR R\n");
				break;
			case ARM_SHF_RRX:
				fprintf(F, "modifier = RRX\n");
				break;
			default:
			case ARM_SHF_INVALID:
				fprintf(F, "modifier = INVALID SHIFT MODIFIER\n");
				break;
			}
		}
		if (has_cmp_attr(n)) {
			const arm_cmp_attr_t *attr = get_arm_cmp_attr_const(n);
			fprintf(F, "cmp_attr =");
			if (attr->is_unsigned) {
				fprintf(F, " unsigned");
			}
			if (attr->ins_permuted) {
				fprintf(F, " inputs swapped");
			}
			fputc('\n', F);
		}
		if (arm_has_address_attr(n)) {
			const arm_Address_attr_t *attr = get_arm_Address_attr_const(n);

			fprintf(F, "entity = ");
			if (attr->entity != NULL) {
				ir_fprintf(F, "'%F'", attr->entity);
			} else {
				fputs("NULL", F);
			}
			fputc('\n', F);
			fprintf(F, "frame offset = %d\n", attr->fp_offset);
		}
		if (has_farith_attr(n)) {
			const arm_farith_attr_t *attr = get_arm_farith_attr_const(n);
			ir_fprintf(F, "arithmetic mode = %+F\n", attr->mode);
		}
		break;
	}
}

arm_attr_t *get_arm_attr(ir_node *node)
{
	assert(is_arm_irn(node));
	return (arm_attr_t*)get_irn_generic_attr(node);
}

const arm_attr_t *get_arm_attr_const(const ir_node *node)
{
	assert(is_arm_irn(node));
	return (const arm_attr_t*)get_irn_generic_attr_const(node);
}

arm_Address_attr_t *get_arm_Address_attr(ir_node *node)
{
	assert(arm_has_address_attr(node));
	return (arm_Address_attr_t*)get_irn_generic_attr(node);
}

const arm_Address_attr_t *get_arm_Address_attr_const(const ir_node *node)
{
	assert(arm_has_address_attr(node));
	return (const arm_Address_attr_t*)get_irn_generic_attr_const(node);
}

static const arm_fConst_attr_t *get_arm_fConst_attr_const(const ir_node *node)
{
	assert(is_arm_fConst(node));
	return (const arm_fConst_attr_t*)get_irn_generic_attr_const(node);
}

static arm_fConst_attr_t *get_arm_fConst_attr(ir_node *node)
{
	assert(is_arm_fConst(node));
	return (arm_fConst_attr_t*)get_irn_generic_attr(node);
}

arm_farith_attr_t *get_arm_farith_attr(ir_node *node)
{
	assert(has_farith_attr(node));
	return (arm_farith_attr_t*)get_irn_generic_attr(node);
}

const arm_farith_attr_t *get_arm_farith_attr_const(const ir_node *node)
{
	assert(has_farith_attr(node));
	return (const arm_farith_attr_t*)get_irn_generic_attr_const(node);
}

arm_CondJmp_attr_t *get_arm_CondJmp_attr(ir_node *node)
{
	assert(is_arm_Bcc(node));
	return (arm_CondJmp_attr_t*)get_irn_generic_attr(node);
}

const arm_CondJmp_attr_t *get_arm_CondJmp_attr_const(const ir_node *node)
{
	assert(is_arm_Bcc(node));
	return (const arm_CondJmp_attr_t*)get_irn_generic_attr_const(node);
}

arm_SwitchJmp_attr_t *get_arm_SwitchJmp_attr(ir_node *node)
{
	assert(is_arm_SwitchJmp(node));
	return (arm_SwitchJmp_attr_t*)get_irn_generic_attr(node);
}

const arm_SwitchJmp_attr_t *get_arm_SwitchJmp_attr_const(const ir_node *node)
{
	assert(is_arm_SwitchJmp(node));
	return (const arm_SwitchJmp_attr_t*)get_irn_generic_attr_const(node);
}

ir_tarval *get_fConst_value(const ir_node *node)
{
	const arm_fConst_attr_t *attr = get_arm_fConst_attr_const(node);
	return attr->tv;
}

void set_fConst_value(ir_node *node, ir_tarval *tv)
{
	arm_fConst_attr_t *attr = get_arm_fConst_attr(node);
	attr->tv = tv;
}

ir_relation get_arm_CondJmp_relation(const ir_node *node)
{
	const arm_CondJmp_attr_t *attr = get_arm_CondJmp_attr_const(node);
	return attr->relation;
}

void set_arm_CondJmp_relation(ir_node *node, ir_relation relation)
{
	arm_CondJmp_attr_t *attr = get_arm_CondJmp_attr(node);
	attr->relation = relation;
}

/* Set the ARM machine node attributes to default values. */
void init_arm_attributes(ir_node *node)
{
	arm_attr_t *const attr = get_arm_attr(node);
	attr->is_load_store    = false;
}

void init_arm_load_store_attributes(ir_node *res, ir_mode *ls_mode,
                                    ir_entity *entity, int entity_sign,
                                    long offset, bool is_frame_entity)
{
	arm_load_store_attr_t *attr = get_arm_load_store_attr(res);
	attr->load_store_mode    = ls_mode;
	attr->entity             = entity;
	attr->entity_sign        = entity_sign;
	attr->is_frame_entity    = is_frame_entity;
	attr->offset             = offset;
	attr->base.is_load_store = true;
}

void init_arm_shifter_operand(ir_node *res, unsigned shifter_op_input,
                              unsigned immediate_value,
                              arm_shift_modifier_t shift_modifier,
                              unsigned shift_immediate)
{
	arm_shifter_operand_t *attr = get_arm_shifter_operand_attr(res);
	attr->immediate_value  = immediate_value;
	attr->shifter_op_input = shifter_op_input;
	attr->shift_modifier   = shift_modifier;
	attr->shift_immediate  = shift_immediate;
}

void init_arm_cmp_attr(ir_node *res, bool ins_permuted, bool is_unsigned)
{
	arm_cmp_attr_t *attr = get_arm_cmp_attr(res);
	attr->ins_permuted = ins_permuted;
	attr->is_unsigned  = is_unsigned;
}

void init_arm_Address_attributes(ir_node *res, ir_entity *entity, int offset)
{
	arm_Address_attr_t *attr = get_arm_Address_attr(res);
	attr->entity    = entity;
	attr->fp_offset = offset;
}

void init_arm_farith_attributes(ir_node *res, ir_mode *mode)
{
	arm_farith_attr_t *attr = get_arm_farith_attr(res);
	attr->mode = mode;
}

int arm_attrs_equal(const ir_node *a, const ir_node *b)
{
	(void)a;
	(void)b;
	return true;
}

int arm_Address_attrs_equal(const ir_node *a, const ir_node *b)
{
	const arm_Address_attr_t *attr_a = get_arm_Address_attr_const(a);
	const arm_Address_attr_t *attr_b = get_arm_Address_attr_const(b);
	return arm_attrs_equal(a, b)
	    && attr_a->entity == attr_b->entity
	    && attr_a->fp_offset == attr_b->fp_offset;
}

int arm_CondJmp_attrs_equal(const ir_node *a, const ir_node *b)
{
	const arm_CondJmp_attr_t *attr_a = get_arm_CondJmp_attr_const(a);
	const arm_CondJmp_attr_t *attr_b = get_arm_CondJmp_attr_const(b);
	return arm_attrs_equal(a, b) && attr_a->relation == attr_b->relation;
}

int arm_SwitchJmp_attrs_equal(const ir_node *a, const ir_node *b)
{
	const arm_SwitchJmp_attr_t *attr_a = get_arm_SwitchJmp_attr_const(a);
	const arm_SwitchJmp_attr_t *attr_b = get_arm_SwitchJmp_attr_const(b);
	return arm_attrs_equal(a, b) && be_switch_attrs_equal(&attr_a->swtch, &attr_b->swtch);
}

int arm_fConst_attrs_equal(const ir_node *a, const ir_node *b)
{
	const arm_fConst_attr_t *attr_a = get_arm_fConst_attr_const(a);
	const arm_fConst_attr_t *attr_b = get_arm_fConst_attr_const(b);
	return arm_attrs_equal(a, b) && attr_a->tv == attr_b->tv;
}

arm_load_store_attr_t *get_arm_load_store_attr(ir_node *node)
{
	return (arm_load_store_attr_t*) get_irn_generic_attr(node);
}

const arm_load_store_attr_t *get_arm_load_store_attr_const(const ir_node *node)
{
	return (const arm_load_store_attr_t*) get_irn_generic_attr_const(node);
}

arm_shifter_operand_t *get_arm_shifter_operand_attr(ir_node *node)
{
	return (arm_shifter_operand_t*) get_irn_generic_attr(node);
}

const arm_shifter_operand_t *get_arm_shifter_operand_attr_const(
		const ir_node *node)
{
	return (const arm_shifter_operand_t*) get_irn_generic_attr_const(node);
}

arm_cmp_attr_t *get_arm_cmp_attr(ir_node *node)
{
	return (arm_cmp_attr_t*) get_irn_generic_attr(node);
}

const arm_cmp_attr_t *get_arm_cmp_attr_const(const ir_node *node)
{
	return (const arm_cmp_attr_t*) get_irn_generic_attr_const(node);
}

int arm_load_store_attrs_equal(const ir_node *a, const ir_node *b)
{
	const arm_load_store_attr_t *attr_a = get_arm_load_store_attr_const(a);
	const arm_load_store_attr_t *attr_b = get_arm_load_store_attr_const(b);
	return arm_attrs_equal(a, b)
	    && attr_a->load_store_mode == attr_b->load_store_mode
	    && attr_a->entity          == attr_b->entity
	    && attr_a->offset          == attr_b->offset
	    && attr_a->is_frame_entity == attr_b->is_frame_entity
	    && attr_a->entity_sign     == attr_b->entity_sign;
}

int arm_shifter_operands_equal(const ir_node *a, const ir_node *b)
{
	const arm_shifter_operand_t *attr_a = get_arm_shifter_operand_attr_const(a);
	const arm_shifter_operand_t *attr_b = get_arm_shifter_operand_attr_const(b);
	return arm_attrs_equal(a, b)
	    && attr_a->shift_modifier == attr_b->shift_modifier
	    && attr_a->immediate_value == attr_b->immediate_value
	    && attr_a->shift_immediate == attr_b->shift_immediate;
}

int arm_cmp_attrs_equal(const ir_node *a, const ir_node *b)
{
	const arm_cmp_attr_t *attr_a = get_arm_cmp_attr_const(a);
	const arm_cmp_attr_t *attr_b = get_arm_cmp_attr_const(b);
	return arm_shifter_operands_equal(a, b)
	    && attr_a->ins_permuted == attr_b->ins_permuted
	    && attr_a->is_unsigned == attr_b->is_unsigned;
}

int arm_farith_attrs_equal(const ir_node *a, const ir_node *b)
{
	const arm_farith_attr_t *attr_a = get_arm_farith_attr_const(a);
	const arm_farith_attr_t *attr_b = get_arm_farith_attr_const(b);
	return arm_attrs_equal(a, b) && attr_a->mode == attr_b->mode;
}
