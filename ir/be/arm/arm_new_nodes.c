/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief  This file implements the creation of the architecture specific firm
 *         opcodes and the corresponding node constructors for the arm
 *         assembler irg.
 * @author Oliver Richter, Tobias Gneist
 * @version $Id$
 */
#include "config.h"

#include <stdlib.h>

#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irmode_t.h"
#include "ircons_t.h"
#include "iropt_t.h"
#include "irop.h"
#include "irvrfy_t.h"
#include "irprintf.h"
#include "xmalloc.h"

#include "../bearch.h"

#include "arm_nodes_attr.h"
#include "arm_new_nodes.h"
#include "arm_optimize.h"

#include "../beabi.h"
#include "bearch_arm_t.h"

/**
 * Return the fpa immediate from the encoding.
 */
const char *arm_get_fpa_imm_name(long imm_value)
{
	static const char *fpa_imm[] = {
		"0",
		"1",
		"2",
		"3",
		"4",
		"5",
		"10",
		"0.5"
	};
	return fpa_imm[imm_value];
}


/**
 * Dumper interface for dumping arm nodes in vcg.
 * @param n        the node to dump
 * @param F        the output file
 * @param reason   indicates which kind of information should be dumped
 * @return 0 on success or != 0 on failure
 */
static int arm_dump_node(ir_node *n, FILE *F, dump_reason_t reason)
{
	ir_mode     *mode = NULL;
	//arm_attr_t  *attr = get_arm_attr(n);

	switch (reason) {
		case dump_node_opcode_txt:
			fprintf(F, "%s", get_irn_opname(n));
			break;

		case dump_node_mode_txt:
			mode = get_irn_mode(n);

			if (mode) {
				fprintf(F, "[%s]", get_mode_name(mode));
			}
			else {
				fprintf(F, "[?NOMODE?]");
			}
			break;

		case dump_node_nodeattr_txt:
			/* TODO: dump shift modifiers */
			break;

		case dump_node_info_txt:
			arch_dump_reqs_and_registers(F, n);

			if (is_arm_CopyB(n)) {
				//fprintf(F, "size = %lu\n", get_arm_imm_value(n));
			} else {
				/* TODO */
#if 0
				long v =  get_arm_imm_value(n);
				if (ARM_GET_FPA_IMM(attr)) {
					fprintf(F, "immediate float value = %s\n", arm_get_fpa_imm_name(v));
				} else {
					fprintf(F, "immediate value = %ld (0x%08lx)\n", v, v);
				}
#endif
			}

#if 0
			if (is_arm_CmpBra(n) && get_arm_CondJmp_proj_num(n) >= 0) {
				fprintf(F, "proj_num = (%d)\n", get_arm_CondJmp_proj_num(n));
			}
#endif
			break;
	}

	return 0;
}


/* Returns the attributes of a generic Arm node. */
arm_attr_t *get_arm_attr(ir_node *node)
{
	assert(is_arm_irn(node) && "need arm node to get attributes");
	return get_irn_generic_attr(node);
}

const arm_attr_t *get_arm_attr_const(const ir_node *node)
{
	assert(is_arm_irn(node) && "need arm node to get attributes");
	return get_irn_generic_attr_const(node);
}

/**
 * Returns the attributes of an ARM SymConst node.
 */
arm_SymConst_attr_t *get_arm_SymConst_attr(ir_node *node)
{
	assert(is_arm_SymConst(node) || is_arm_FrameAddr(node));
	return get_irn_generic_attr(node);
}

const arm_SymConst_attr_t *get_arm_SymConst_attr_const(const ir_node *node)
{
	assert(is_arm_SymConst(node) || is_arm_FrameAddr(node));
	return get_irn_generic_attr_const(node);
}

static const arm_fpaConst_attr_t *get_arm_fpaConst_attr_const(const ir_node *node)
{
	const arm_attr_t          *attr     = get_arm_attr_const(node);
	const arm_fpaConst_attr_t *fpa_attr = CONST_CAST_ARM_ATTR(arm_fpaConst_attr_t, attr);

	return fpa_attr;
}

static arm_fpaConst_attr_t *get_arm_fpaConst_attr(ir_node *node)
{
	arm_attr_t          *attr     = get_arm_attr(node);
	arm_fpaConst_attr_t *fpa_attr = CAST_ARM_ATTR(arm_fpaConst_attr_t, attr);

	return fpa_attr;
}

/* Returns the attributes of a CondJmp node. */
arm_CondJmp_attr_t *get_arm_CondJmp_attr(ir_node *node)
{
	assert(is_arm_B(node));
	return get_irn_generic_attr(node);
}

const arm_CondJmp_attr_t *get_arm_CondJmp_attr_const(const ir_node *node)
{
	assert(is_arm_B(node));
	return get_irn_generic_attr_const(node);
}

/* Returns the attributes of a SwitchJmp node. */
arm_SwitchJmp_attr_t *get_arm_SwitchJmp_attr(ir_node *node)
{
	assert(is_arm_SwitchJmp(node));
	return get_irn_generic_attr(node);
}

const arm_SwitchJmp_attr_t *get_arm_SwitchJmp_attr_const(const ir_node *node)
{
	assert(is_arm_SwitchJmp(node));
	return get_irn_generic_attr_const(node);
}

/**
 * Returns the argument register requirements of a arm node.
 */
const arch_register_req_t **get_arm_in_req_all(const ir_node *node)
{
	const arm_attr_t *attr = get_arm_attr_const(node);
	return attr->in_req;
}

/**
 * Returns the argument register requirement at position pos of an arm node.
 */
const arch_register_req_t *get_arm_in_req(const ir_node *node, int pos)
{
	const arm_attr_t *attr = get_arm_attr_const(node);
	return attr->in_req[pos];
}

/**
 * Sets the IN register requirements at position pos.
 */
void set_arm_req_in(ir_node *node, const arch_register_req_t *req, int pos)
{
	arm_attr_t *attr  = get_arm_attr(node);
	attr->in_req[pos] = req;
}

/**
 * Returns the fpaConst value
 */
tarval *get_fpaConst_value(const ir_node *node)
{
	const arm_fpaConst_attr_t *attr = get_arm_fpaConst_attr_const(node);
	return attr->tv;
}

/**
 * Sets the tarval value
 */
void set_fpaConst_value(ir_node *node, tarval *tv)
{
	arm_fpaConst_attr_t *attr = get_arm_fpaConst_attr(node);
	attr->tv = tv;
}

/**
 * Returns the proj num
 */
int get_arm_CondJmp_proj_num(const ir_node *node)
{
	const arm_CondJmp_attr_t *attr = get_arm_CondJmp_attr_const(node);
	return attr->proj_num;
}

/**
 * Sets the proj num
 */
void set_arm_CondJmp_proj_num(ir_node *node, int proj_num)
{
	arm_CondJmp_attr_t *attr = get_arm_CondJmp_attr(node);
	attr->proj_num   = proj_num;
}

/**
 * Returns the number of projs of a SwitchJmp.
 */
int get_arm_SwitchJmp_n_projs(const ir_node *node)
{
	const arm_SwitchJmp_attr_t *attr = get_arm_SwitchJmp_attr_const(node);
	return attr->n_projs;
}

/**
 * Sets the number of projs.
 */
void set_arm_SwitchJmp_n_projs(ir_node *node, int n_projs)
{
	arm_SwitchJmp_attr_t *attr = get_arm_SwitchJmp_attr(node);
	attr->n_projs = n_projs;
}

/**
 * Returns the default_proj_num.
 */
long get_arm_SwitchJmp_default_proj_num(const ir_node *node)
{
	const arm_SwitchJmp_attr_t *attr = get_arm_SwitchJmp_attr_const(node);
	return attr->default_proj_num;
}

/**
 * Sets the default_proj_num.
 */
void set_arm_SwitchJmp_default_proj_num(ir_node *node, long default_proj_num)
{
	arm_SwitchJmp_attr_t *attr = get_arm_SwitchJmp_attr(node);
	attr->default_proj_num = default_proj_num;
}

/* Set the ARM machine node attributes to default values. */
static void init_arm_attributes(ir_node *node, int flags,
                         const arch_register_req_t ** in_reqs,
                         const be_execution_unit_t ***execution_units,
						 int n_res)
{
	ir_graph       *irg  = get_irn_irg(node);
	struct obstack *obst = get_irg_obstack(irg);
	arm_attr_t     *attr = get_arm_attr(node);
	backend_info_t *info;
	(void) execution_units;

	arch_irn_set_flags(node, flags);
	attr->in_req           = in_reqs;
	attr->instr_fl         = 0;

	info            = be_get_info(node);
	info->out_infos = NEW_ARR_D(reg_out_info_t, obst, n_res);
	memset(info->out_infos, 0, n_res * sizeof(info->out_infos[0]));
}

static void init_arm_load_store_attributes(ir_node *res, ir_mode *ls_mode,
                                           ir_entity *entity,
                                           int entity_sign, long offset,
                                           bool is_frame_entity)
{
	arm_load_store_attr_t *attr = get_irn_generic_attr(res);
	attr->load_store_mode    = ls_mode;
	attr->entity             = entity;
	attr->entity_sign        = entity_sign;
	attr->is_frame_entity    = is_frame_entity;
	attr->offset             = offset;
	attr->base.is_load_store = true;
}

static void init_arm_shifter_operand(ir_node *res, unsigned immediate_value,
                                     arm_shift_modifier shift_modifier,
                                     unsigned shift_immediate)
{
	arm_shifter_operand_t *attr = get_irn_generic_attr(res);
	attr->immediate_value = immediate_value;
	attr->shift_modifier  = shift_modifier;
	attr->shift_immediate = shift_immediate;
}

static void init_arm_cmp_attr(ir_node *res, bool ins_permuted, bool is_unsigned)
{
	arm_cmp_attr_t *attr = get_irn_generic_attr(res);
	attr->ins_permuted = ins_permuted;
	attr->is_unsigned  = is_unsigned;
}

static void init_arm_SymConst_attributes(ir_node *res, ir_entity *entity)
{
	arm_SymConst_attr_t *attr = get_irn_generic_attr(res);
	attr->entity    = entity;
	attr->fp_offset = 0;
}

static void init_arm_CopyB_attributes(ir_node *res, unsigned size)
{
	arm_CopyB_attr_t *attr = get_irn_generic_attr(res);
	attr->size = size;
}

static int cmp_attr_arm(ir_node *a, ir_node *b)
{
	arm_attr_t *attr_a = get_irn_generic_attr(a);
	arm_attr_t *attr_b = get_irn_generic_attr(b);
	return attr_a->instr_fl != attr_b->instr_fl;
}

static int cmp_attr_arm_SymConst(ir_node *a, ir_node *b)
{
	const arm_SymConst_attr_t *attr_a;
	const arm_SymConst_attr_t *attr_b;

	if (cmp_attr_arm(a, b))
		return 1;

	attr_a = get_irn_generic_attr_const(a);
	attr_b = get_irn_generic_attr_const(b);
	return attr_a->entity != attr_b->entity
		|| attr_a->fp_offset != attr_b->fp_offset;
}

static int cmp_attr_arm_CopyB(ir_node *a, ir_node *b)
{
	const arm_CopyB_attr_t *attr_a;
	const arm_CopyB_attr_t *attr_b;

	if (cmp_attr_arm(a, b))
		return 1;

	attr_a = get_irn_generic_attr_const(a);
	attr_b = get_irn_generic_attr_const(b);
	return attr_a->size != attr_b->size;
}

static int cmp_attr_arm_CondJmp(ir_node *a, ir_node *b)
{
	(void) a;
	(void) b;
	/* never identical */
	return 1;
}

static int cmp_attr_arm_SwitchJmp(ir_node *a, ir_node *b)
{
	(void) a;
	(void) b;
	/* never identical */
	return 1;
}

static int cmp_attr_arm_fpaConst(ir_node *a, ir_node *b)
{
	const arm_fpaConst_attr_t *attr_a;
	const arm_fpaConst_attr_t *attr_b;

	if (cmp_attr_arm(a, b))
		return 1;

	attr_a = get_arm_fpaConst_attr_const(a);
	attr_b = get_arm_fpaConst_attr_const(b);

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

static int cmp_attr_arm_load_store(ir_node *a, ir_node *b)
{
	const arm_load_store_attr_t *attr_a;
	const arm_load_store_attr_t *attr_b;

	if (cmp_attr_arm(a, b))
		return 1;

	attr_a = get_arm_load_store_attr(a);
	attr_b = get_arm_load_store_attr(b);
	if (attr_a->entity != attr_b->entity
			|| attr_a->entity_sign != attr_b->entity_sign
			|| attr_a->offset != attr_b->offset)
		return 1;

	return 0;
}

static int cmp_attr_arm_shifter_operand(ir_node *a, ir_node *b)
{
	const arm_shifter_operand_t *attr_a;
	const arm_shifter_operand_t *attr_b;

	if (cmp_attr_arm(a, b))
		return 1;

	attr_a = get_arm_shifter_operand_attr(a);
	attr_b = get_arm_shifter_operand_attr(b);
	if (attr_a->shift_modifier != attr_b->shift_modifier
			|| attr_a->immediate_value != attr_b->immediate_value
			|| attr_a->shift_immediate != attr_b->shift_immediate)
		return 1;

	return 0;
}

static int cmp_attr_arm_cmp(ir_node *a, ir_node *b)
{
	const arm_cmp_attr_t *attr_a;
	const arm_cmp_attr_t *attr_b;

	if (cmp_attr_arm(a, b))
		return 1;

	attr_a = get_irn_generic_attr_const(a);
	attr_b = get_irn_generic_attr_const(b);
	if (attr_a->ins_permuted != attr_b->ins_permuted
			|| attr_a->is_unsigned != attr_b->is_unsigned)
		return 1;
	return 0;
}

/** copies the ARM attributes of a node. */
static void arm_copy_attr(const ir_node *old_node, ir_node *new_node)
{
	ir_graph          *irg     = get_irn_irg(new_node);
	struct obstack    *obst    = get_irg_obstack(irg);
	const arm_attr_t *attr_old = get_arm_attr_const(old_node);
	arm_attr_t       *attr_new = get_arm_attr(new_node);
	backend_info_t    *old_info = be_get_info(old_node);
	backend_info_t    *new_info = be_get_info(new_node);

	/* copy the attributes */
	memcpy(attr_new, attr_old, get_op_attr_size(get_irn_op(old_node)));

	/* copy out flags */
	new_info->out_infos =
		DUP_ARR_D(reg_out_info_t, obst, old_info->out_infos);
}



/* Include the generated constructor functions */
#include "gen_arm_new_nodes.c.inl"
