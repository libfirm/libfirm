/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   This file implements the creation of the achitecture specific firm
 *          opcodes and the coresponding node constructors for the sparc
 *          assembler irg.
 * @author  Hannes Rapp, Matthias Braun
 */
#include "sparc_new_nodes_t.h"

#include "bearch.h"
#include "bedump.h"
#include "gen_sparc_regalloc_if.h"
#include "ircons_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "irop_t.h"
#include "iropt_t.h"
#include "irprintf.h"
#include "irprog_t.h"
#include "sparc_bearch_t.h"
#include "sparc_nodes_attr.h"
#include "xmalloc.h"
#include <stdlib.h>

bool sparc_has_load_store_attr(const ir_node *node)
{
	return is_sparc_Ld(node) || is_sparc_St(node) || is_sparc_Ldf(node)
	    || is_sparc_Stf(node) || is_sparc_IJmp(node);
}

#ifndef NDEBUG
static bool has_switch_jmp_attr(const ir_node *node)
{
	return is_sparc_SwitchJmp(node);
}
#endif

static bool has_fp_attr(const ir_node *node)
{
	return is_sparc_fadd(node) || is_sparc_fsub(node)
	    || is_sparc_fmul(node) || is_sparc_fdiv(node)
	    || is_sparc_fftoi(node) || is_sparc_fitof(node)
	    || is_sparc_fneg(node) || is_sparc_fcmp(node);
}

static bool has_fp_conv_attr(const ir_node *node)
{
	return is_sparc_fftof(node);
}

/**
 * Dumper interface for dumping sparc nodes in vcg.
 * @param F        the output file
 * @param n        the node to dump
 * @param reason   indicates which kind of information should be dumped
 */
void sparc_dump_node(FILE *F, const ir_node *n, dump_reason_t reason)
{
	switch (reason) {
	case dump_node_opcode_txt:
		fprintf(F, "%s", get_irn_opname(n));
		break;

	case dump_node_mode_txt:
		break;

	case dump_node_info_txt: {
		const sparc_attr_t *sparc_attr = get_sparc_attr_const(n);
		if (sparc_attr->immediate_value_entity) {
			ir_fprintf(F, "entity: %+F (offset %d)\n",
			           sparc_attr->immediate_value_entity,
			           sparc_attr->immediate_value);
		} else {
			ir_fprintf(F, "immediate value: %d\n", sparc_attr->immediate_value);
		}
		if (sparc_has_load_store_attr(n)) {
			const sparc_load_store_attr_t *attr = get_sparc_load_store_attr_const(n);
			ir_fprintf(F, "load store mode: %+F\n", attr->load_store_mode);
			fprintf(F, "is frame entity: %s\n", be_dump_yesno(attr->is_frame_entity));
		}
		if (is_sparc_cond_branch(n)) {
			const sparc_jmp_cond_attr_t *attr
				= get_sparc_jmp_cond_attr_const(n);
			fprintf(F, "relation: %d (%s)\n", (int)attr->relation,
			        get_relation_string(attr->relation));
			fprintf(F, "unsigned: %s\n", be_dump_yesno(attr->is_unsigned));
		}
		if (has_fp_attr(n)) {
			const sparc_fp_attr_t *attr = get_sparc_fp_attr_const(n);
			ir_fprintf(F, "fp_mode: %+F\n", attr->fp_mode);
		}
		if (has_fp_conv_attr(n)) {
			const sparc_fp_conv_attr_t *attr = get_sparc_fp_conv_attr_const(n);
			ir_fprintf(F, "conv from: %+F\n", attr->src_mode);
			ir_fprintf(F, "conv to: %+F\n", attr->dest_mode);
		}
		break;
	}

	case dump_node_nodeattr_txt:
		break;
	}
}

void sparc_set_attr_imm(ir_node *res, ir_entity *entity,
                        int32_t immediate_value)
{
	sparc_attr_t *attr           = (sparc_attr_t*)get_irn_generic_attr(res);
	attr->immediate_value_entity = entity;
	attr->immediate_value        = immediate_value;
	arch_add_irn_flags(res, (arch_irn_flags_t)sparc_arch_irn_flag_immediate_form);
}

void init_sparc_jmp_cond_attr(ir_node *node, ir_relation relation,
                              bool is_unsigned)
{
	sparc_jmp_cond_attr_t *attr = get_sparc_jmp_cond_attr(node);
	attr->relation    = relation;
	attr->is_unsigned = is_unsigned;
}

sparc_attr_t *get_sparc_attr(ir_node *node)
{
	assert(is_sparc_irn(node));
	return (sparc_attr_t*) get_irn_generic_attr(node);
}

const sparc_attr_t *get_sparc_attr_const(const ir_node *node)
{
	assert(is_sparc_irn(node));
	return (const sparc_attr_t*) get_irn_generic_attr_const(node);
}

sparc_load_store_attr_t *get_sparc_load_store_attr(ir_node *node)
{
	assert(sparc_has_load_store_attr(node));
	return (sparc_load_store_attr_t*) get_irn_generic_attr_const(node);
}

const sparc_load_store_attr_t *get_sparc_load_store_attr_const(const ir_node *node)
{
	assert(sparc_has_load_store_attr(node));
	return (const sparc_load_store_attr_t*) get_irn_generic_attr_const(node);
}

sparc_jmp_cond_attr_t *get_sparc_jmp_cond_attr(ir_node *node)
{
	assert(is_sparc_cond_branch(node));
	return (sparc_jmp_cond_attr_t*) get_irn_generic_attr_const(node);
}

const sparc_jmp_cond_attr_t *get_sparc_jmp_cond_attr_const(const ir_node *node)
{
	assert(is_sparc_cond_branch(node));
	return (const sparc_jmp_cond_attr_t*) get_irn_generic_attr_const(node);
}

sparc_switch_jmp_attr_t *get_sparc_switch_jmp_attr(ir_node *node)
{
	assert(has_switch_jmp_attr(node));
	return (sparc_switch_jmp_attr_t*) get_irn_generic_attr_const(node);
}

const sparc_switch_jmp_attr_t *get_sparc_switch_jmp_attr_const(const ir_node *node)
{
	assert(has_switch_jmp_attr(node));
	return (const sparc_switch_jmp_attr_t*) get_irn_generic_attr_const(node);
}

sparc_fp_attr_t *get_sparc_fp_attr(ir_node *node)
{
	assert(has_fp_attr(node));
	return (sparc_fp_attr_t*) get_irn_generic_attr(node);
}

const sparc_fp_attr_t *get_sparc_fp_attr_const(const ir_node *node)
{
	assert(has_fp_attr(node));
	return (const sparc_fp_attr_t*) get_irn_generic_attr_const(node);
}

sparc_fp_conv_attr_t *get_sparc_fp_conv_attr(ir_node *node)
{
	assert(has_fp_conv_attr(node));
	return (sparc_fp_conv_attr_t*) get_irn_generic_attr(node);
}

const sparc_fp_conv_attr_t *get_sparc_fp_conv_attr_const(const ir_node *node)
{
	assert(has_fp_conv_attr(node));
	return (const sparc_fp_conv_attr_t*) get_irn_generic_attr_const(node);
}

void init_sparc_load_store_attributes(ir_node *res, ir_mode *ls_mode,
                                      ir_entity *entity, int32_t offset,
                                      bool is_frame_entity, bool is_reg_reg)
{
	sparc_load_store_attr_t *attr     = get_sparc_load_store_attr(res);
	attr->base.immediate_value_entity = entity;
	attr->base.immediate_value        = offset;
	attr->load_store_mode             = ls_mode;
	attr->is_frame_entity             = is_frame_entity;
	attr->is_reg_reg                  = is_reg_reg;
}

void init_sparc_fp_attributes(ir_node *res, ir_mode *fp_mode)
{
	sparc_fp_attr_t *attr = get_sparc_fp_attr(res);
	attr->fp_mode = fp_mode;
}

void init_sparc_fp_conv_attributes(ir_node *res, ir_mode *src_mode,
                                   ir_mode *dest_mode)
{
	sparc_fp_conv_attr_t *attr = get_sparc_fp_conv_attr(res);
	attr->src_mode = src_mode;
	attr->dest_mode = dest_mode;
}

int sparc_attrs_equal(const ir_node *a, const ir_node *b)
{
	const sparc_attr_t *attr_a = get_sparc_attr_const(a);
	const sparc_attr_t *attr_b = get_sparc_attr_const(b);
	return attr_a->immediate_value == attr_b->immediate_value
	    && attr_a->immediate_value_entity == attr_b->immediate_value_entity;
}

int sparc_load_store_attrs_equal(const ir_node *a, const ir_node *b)
{
	const sparc_load_store_attr_t *attr_a = get_sparc_load_store_attr_const(a);
	const sparc_load_store_attr_t *attr_b = get_sparc_load_store_attr_const(b);
	return sparc_attrs_equal(a, b)
	    && attr_a->is_frame_entity == attr_b->is_frame_entity
	    && attr_a->load_store_mode == attr_b->load_store_mode;
}

int sparc_jmp_cond_attrs_equal(const ir_node *a, const ir_node *b)
{
	const sparc_jmp_cond_attr_t *attr_a = get_sparc_jmp_cond_attr_const(a);
	const sparc_jmp_cond_attr_t *attr_b = get_sparc_jmp_cond_attr_const(b);
	return sparc_attrs_equal(a, b)
	    && attr_a->relation == attr_b->relation
	    && attr_a->is_unsigned == attr_b->is_unsigned;
}

int sparc_fp_attrs_equal(const ir_node *a, const ir_node *b)
{
	const sparc_fp_attr_t *attr_a = get_sparc_fp_attr_const(a);
	const sparc_fp_attr_t *attr_b = get_sparc_fp_attr_const(b);
	return sparc_attrs_equal(a, b) && attr_a->fp_mode == attr_b->fp_mode;
}

int sparc_fp_conv_attrs_equal(const ir_node *a, const ir_node *b)
{
	const sparc_fp_conv_attr_t *attr_a = get_sparc_fp_conv_attr_const(a);
	const sparc_fp_conv_attr_t *attr_b = get_sparc_fp_conv_attr_const(b);
	return sparc_attrs_equal(a, b)
	    && attr_a->src_mode == attr_b->src_mode
	    && attr_a->dest_mode == attr_b->dest_mode;
}

int sparc_switch_jmp_attrs_equal(const ir_node *a, const ir_node *b)
{
	const sparc_switch_jmp_attr_t *attr_a = get_sparc_switch_jmp_attr_const(a);
	const sparc_switch_jmp_attr_t *attr_b = get_sparc_switch_jmp_attr_const(b);
	return sparc_attrs_equal(a, b)
	    && be_switch_attrs_equal(&attr_a->swtch, &attr_b->swtch);
}
