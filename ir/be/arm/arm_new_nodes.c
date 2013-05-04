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
#include <stdlib.h>
#include <stdbool.h>

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

#include "arm_nodes_attr.h"
#include "arm_new_nodes.h"
#include "arm_optimize.h"

#include "beabi.h"

static bool arm_has_symconst_attr(const ir_node *node)
{
	return is_arm_SymConst(node) || is_arm_FrameAddr(node) || is_arm_Bl(node);
}

static bool has_load_store_attr(const ir_node *node)
{
	return is_arm_Ldr(node) || is_arm_Str(node) || is_arm_LinkLdrPC(node)
		|| is_arm_Ldf(node) || is_arm_Stf(node);
}

static bool has_shifter_operand(const ir_node *node)
{
	return is_arm_Add(node) || is_arm_And(node) || is_arm_Or(node)
		|| is_arm_Eor(node) || is_arm_Bic(node) || is_arm_Sub(node)
		|| is_arm_Rsb(node) || is_arm_Mov(node) || is_arm_Mvn(node)
		|| is_arm_Cmp(node) || is_arm_Tst(node) || is_arm_LinkMovPC(node);
}

static bool has_cmp_attr(const ir_node *node)
{
	return is_arm_Cmp(node) || is_arm_Tst(node);
}

static bool has_farith_attr(const ir_node *node)
{
	return is_arm_Adf(node) || is_arm_Muf(node) || is_arm_Suf(node)
	    || is_arm_Dvf(node) || is_arm_Mvf(node) || is_arm_FltX(node);
}

/**
 * Dumper interface for dumping arm nodes in vcg.
 * @param F        the output file
 * @param n        the node to dump
 * @param reason   indicates which kind of information should be dumped
 */
static void arm_dump_node(FILE *F, const ir_node *n, dump_reason_t reason)
{
	switch (reason) {
	case dump_node_opcode_txt:
		fprintf(F, "%s", get_irn_opname(n));

		if (arm_has_symconst_attr(n)) {
			const arm_SymConst_attr_t *attr = get_arm_SymConst_attr_const(n);
			if (attr->entity != NULL) {
				fputc(' ', F);
				fputs(get_entity_name(attr->entity), F);
			}
		}
		break;

	case dump_node_mode_txt:
		/* mode isn't relevant in the backend */
		break;

	case dump_node_nodeattr_txt:
		/* TODO: dump shift modifiers */
		break;

	case dump_node_info_txt:
		arch_dump_reqs_and_registers(F, n);

		if (has_load_store_attr(n)) {
			const arm_load_store_attr_t *attr
				= get_arm_load_store_attr_const(n);
			ir_fprintf(F, "load_store_mode = %+F\n", attr->load_store_mode);
			ir_fprintf(F, "entity = %+F\n", attr->entity);
			fprintf(F, "offset = %ld\n", attr->offset);
			fprintf(F, "is_frame_entity = %s\n",
					attr->is_frame_entity ? "yes" : "no");
			fprintf(F, "entity_sign = %s\n",
					attr->entity_sign ? "yes" : "no");
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
		if (arm_has_symconst_attr(n)) {
			const arm_SymConst_attr_t *attr = get_arm_SymConst_attr_const(n);

			fprintf(F, "entity = ");
			if (attr->entity != NULL) {
				fprintf(F, "'%s'", get_entity_name(attr->entity));
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
	assert(is_arm_irn(node) && "need arm node to get attributes");
	return (arm_attr_t*)get_irn_generic_attr(node);
}

const arm_attr_t *get_arm_attr_const(const ir_node *node)
{
	assert(is_arm_irn(node) && "need arm node to get attributes");
	return (const arm_attr_t*)get_irn_generic_attr_const(node);
}

#ifndef NDEBUG
static bool has_symconst_attr(const ir_node *node)
{
	return is_arm_SymConst(node) || is_arm_FrameAddr(node) || is_arm_Bl(node);
}
#endif

arm_SymConst_attr_t *get_arm_SymConst_attr(ir_node *node)
{
	assert(has_symconst_attr(node));
	return (arm_SymConst_attr_t*)get_irn_generic_attr(node);
}

const arm_SymConst_attr_t *get_arm_SymConst_attr_const(const ir_node *node)
{
	assert(has_symconst_attr(node));
	return (const arm_SymConst_attr_t*)get_irn_generic_attr_const(node);
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
	assert(is_arm_B(node));
	return (arm_CondJmp_attr_t*)get_irn_generic_attr(node);
}

const arm_CondJmp_attr_t *get_arm_CondJmp_attr_const(const ir_node *node)
{
	assert(is_arm_B(node));
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

arm_CopyB_attr_t *get_arm_CopyB_attr(ir_node *node)
{
	assert(is_arm_CopyB(node));
	return (arm_CopyB_attr_t*)get_irn_generic_attr(node);
}

const arm_CopyB_attr_t *get_arm_CopyB_attr_const(const ir_node *node)
{
	assert(is_arm_CopyB(node));
	return (const arm_CopyB_attr_t*)get_irn_generic_attr_const(node);
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
static void init_arm_attributes(ir_node *node, arch_irn_flags_t flags,
                         const arch_register_req_t ** in_reqs,
						 int n_res)
{
	ir_graph       *irg  = get_irn_irg(node);
	struct obstack *obst = get_irg_obstack(irg);
	arm_attr_t     *attr = get_arm_attr(node);
	backend_info_t *info;

	arch_set_irn_flags(node, flags);
	arch_set_irn_register_reqs_in(node, in_reqs);
	attr->is_load_store    = false;

	info            = be_get_info(node);
	info->out_infos = NEW_ARR_DZ(reg_out_info_t, obst, n_res);
}

static void init_arm_load_store_attributes(ir_node *res, ir_mode *ls_mode,
                                           ir_entity *entity,
                                           int entity_sign, long offset,
                                           bool is_frame_entity)
{
	arm_load_store_attr_t *attr = get_arm_load_store_attr(res);
	attr->load_store_mode    = ls_mode;
	attr->entity             = entity;
	attr->entity_sign        = entity_sign;
	attr->is_frame_entity    = is_frame_entity;
	attr->offset             = offset;
	attr->base.is_load_store = true;
}

static void init_arm_shifter_operand(ir_node *res, unsigned immediate_value,
                                     arm_shift_modifier_t shift_modifier,
                                     unsigned shift_immediate)
{
	arm_shifter_operand_t *attr = get_arm_shifter_operand_attr(res);
	attr->immediate_value = immediate_value;
	attr->shift_modifier  = shift_modifier;
	attr->shift_immediate = shift_immediate;
}

static void init_arm_cmp_attr(ir_node *res, bool ins_permuted, bool is_unsigned)
{
	arm_cmp_attr_t *attr = get_arm_cmp_attr(res);
	attr->ins_permuted = ins_permuted;
	attr->is_unsigned  = is_unsigned;
}

static void init_arm_SymConst_attributes(ir_node *res, ir_entity *entity,
                                         int symconst_offset)
{
	arm_SymConst_attr_t *attr = get_arm_SymConst_attr(res);
	attr->entity    = entity;
	attr->fp_offset = symconst_offset;
}

static void init_arm_farith_attributes(ir_node *res, ir_mode *mode)
{
	arm_farith_attr_t *attr = get_arm_farith_attr(res);
	attr->mode = mode;
}

static void init_arm_CopyB_attributes(ir_node *res, unsigned size)
{
	arm_CopyB_attr_t *attr = get_arm_CopyB_attr(res);
	attr->size = size;
}

static void init_arm_SwitchJmp_attributes(ir_node *res,
                                          const ir_switch_table *table)
{
	arm_SwitchJmp_attr_t *attr = get_arm_SwitchJmp_attr(res);
	attr->table = table;

	be_foreach_out(res, o) {
		arch_set_irn_register_req_out(res, o, arch_no_register_req);
	}
}

static int cmp_attr_arm(const ir_node *a, const ir_node *b)
{
	(void) a;
	(void) b;
	return 0;
}

static int cmp_attr_arm_SymConst(const ir_node *a, const ir_node *b)
{
	const arm_SymConst_attr_t *attr_a;
	const arm_SymConst_attr_t *attr_b;

	if (cmp_attr_arm(a, b))
		return 1;

	attr_a = get_arm_SymConst_attr_const(a);
	attr_b = get_arm_SymConst_attr_const(b);
	return attr_a->entity != attr_b->entity
		|| attr_a->fp_offset != attr_b->fp_offset;
}

static int cmp_attr_arm_CopyB(const ir_node *a, const ir_node *b)
{
	const arm_CopyB_attr_t *attr_a;
	const arm_CopyB_attr_t *attr_b;

	if (cmp_attr_arm(a, b))
		return 1;

	attr_a = get_arm_CopyB_attr_const(a);
	attr_b = get_arm_CopyB_attr_const(b);
	return attr_a->size != attr_b->size;
}

static int cmp_attr_arm_CondJmp(const ir_node *a, const ir_node *b)
{
	(void) a;
	(void) b;
	/* never identical */
	return 1;
}

static int cmp_attr_arm_SwitchJmp(const ir_node *a, const ir_node *b)
{
	(void) a;
	(void) b;
	/* never identical */
	return 1;
}

static int cmp_attr_arm_fConst(const ir_node *a, const ir_node *b)
{
	const arm_fConst_attr_t *attr_a;
	const arm_fConst_attr_t *attr_b;

	if (cmp_attr_arm(a, b))
		return 1;

	attr_a = get_arm_fConst_attr_const(a);
	attr_b = get_arm_fConst_attr_const(b);

	return attr_a->tv != attr_b->tv;
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

static int cmp_attr_arm_load_store(const ir_node *a, const ir_node *b)
{
	const arm_load_store_attr_t *attr_a;
	const arm_load_store_attr_t *attr_b;

	if (cmp_attr_arm(a, b))
		return 1;

	attr_a = get_arm_load_store_attr_const(a);
	attr_b = get_arm_load_store_attr_const(b);
	if (attr_a->entity != attr_b->entity
			|| attr_a->entity_sign != attr_b->entity_sign
			|| attr_a->offset != attr_b->offset)
		return 1;

	return 0;
}

static int cmp_attr_arm_shifter_operand(const ir_node *a, const ir_node *b)
{
	const arm_shifter_operand_t *attr_a;
	const arm_shifter_operand_t *attr_b;

	if (cmp_attr_arm(a, b))
		return 1;

	attr_a = get_arm_shifter_operand_attr_const(a);
	attr_b = get_arm_shifter_operand_attr_const(b);
	if (attr_a->shift_modifier != attr_b->shift_modifier
			|| attr_a->immediate_value != attr_b->immediate_value
			|| attr_a->shift_immediate != attr_b->shift_immediate)
		return 1;

	return 0;
}

static int cmp_attr_arm_cmp(const ir_node *a, const ir_node *b)
{
	const arm_cmp_attr_t *attr_a;
	const arm_cmp_attr_t *attr_b;

	if (cmp_attr_arm(a, b))
		return 1;

	attr_a = get_arm_cmp_attr_const(a);
	attr_b = get_arm_cmp_attr_const(b);
	if (attr_a->ins_permuted != attr_b->ins_permuted
			|| attr_a->is_unsigned != attr_b->is_unsigned)
		return 1;
	return 0;
}

static int cmp_attr_arm_farith(const ir_node *a, const ir_node *b)
{
	const arm_farith_attr_t *attr_a;
	const arm_farith_attr_t *attr_b;

	if (cmp_attr_arm(a, b))
		return 1;

	attr_a = get_arm_farith_attr_const(a);
	attr_b = get_arm_farith_attr_const(b);
	return attr_a->mode != attr_b->mode;
}

/** copies the ARM attributes of a node. */
static void arm_copy_attr(ir_graph *irg, const ir_node *old_node,
                          ir_node *new_node)
{
	struct obstack   *obst    = get_irg_obstack(irg);
	const arm_attr_t *attr_old = get_arm_attr_const(old_node);
	arm_attr_t       *attr_new = get_arm_attr(new_node);
	backend_info_t   *old_info = be_get_info(old_node);
	backend_info_t   *new_info = be_get_info(new_node);

	/* copy the attributes */
	memcpy(attr_new, attr_old, get_op_attr_size(get_irn_op(old_node)));

	/* copy out flags */
	new_info->flags = old_info->flags;
	new_info->out_infos =
		DUP_ARR_D(reg_out_info_t, obst, old_info->out_infos);
	new_info->in_reqs = old_info->in_reqs;
}


/* Include the generated constructor functions */
#include "gen_arm_new_nodes.c.inl"
