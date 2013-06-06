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

void set_amd64_ls_mode(ir_node *node, ir_mode *mode)
{
  amd64_attr_t *attr = get_amd64_attr(node);
  attr->ls_mode = mode;
}

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

		/* TODO: dump some attributes which should show up */
		/* in node name in dump (e.g. consts or the like)  */

		break;

	case dump_node_info_txt:
		arch_dump_reqs_and_registers(F, n);
		break;
	}
}

const amd64_attr_t *get_amd64_attr_const(const ir_node *node)
{
	assert(is_amd64_irn(node) && "need amd64 node to get attributes");
	return (const amd64_attr_t *)get_irn_generic_attr_const(node);
}

amd64_attr_t *get_amd64_attr(ir_node *node)
{
	assert(is_amd64_irn(node) && "need amd64 node to get attributes");
	return (amd64_attr_t *)get_irn_generic_attr(node);
}

const amd64_SymConst_attr_t *get_amd64_SymConst_attr_const(const ir_node *node)
{
	const amd64_SymConst_attr_t *attr
		= (const amd64_SymConst_attr_t*)get_irn_generic_attr_const(node);
	return attr;
}

amd64_SymConst_attr_t *get_amd64_SymConst_attr(ir_node *node)
{
	amd64_SymConst_attr_t *attr
		= (amd64_SymConst_attr_t*)get_irn_generic_attr(node);
	return attr;
}

const amd64_switch_jmp_attr_t *get_amd64_switch_jmp_attr_const(const ir_node *node)
{
	const amd64_switch_jmp_attr_t *attr
		= (const amd64_switch_jmp_attr_t*)get_irn_generic_attr_const(node);
	return attr;
}

amd64_switch_jmp_attr_t *get_amd64_switch_jmp_attr(ir_node *node)
{
	amd64_switch_jmp_attr_t *attr
		= (amd64_switch_jmp_attr_t*)get_irn_generic_attr(node);
	return attr;
}

/**
 * Initializes the nodes attributes.
 */
static void init_amd64_attributes(ir_node *node, arch_irn_flags_t flags,
                              const arch_register_req_t **in_reqs,
                              int n_res)
{
	ir_graph        *irg  = get_irn_irg(node);
	struct obstack  *obst = get_irg_obstack(irg);
	amd64_attr_t *attr    = get_amd64_attr(node);

	backend_info_t  *info;

	arch_set_irn_flags(node, flags);
	arch_set_irn_register_reqs_in(node, in_reqs);

	info            = be_get_info(node);
	info->out_infos = NEW_ARR_DZ(reg_out_info_t, obst, n_res);

	attr->data.ins_permuted = 0;
	attr->data.cmp_unsigned = 0;
	attr->ext.relation      = ir_relation_false;
}

/**
 * Initialize SymConst attributes.
 */
static void init_amd64_SymConst_attributes(ir_node *node, ir_entity *entity)
{
	amd64_SymConst_attr_t *attr = get_amd64_SymConst_attr(node);
	attr->entity    = entity;
	attr->fp_offset = 0;
}

/**
 * Initialize SwitchJmp attributes.
 */
static void init_amd64_switch_attributes(ir_node *node, const ir_switch_table *table, ir_entity *table_entity)
{
	unsigned n_outs = arch_get_irn_n_outs(node);
	unsigned o;

	amd64_switch_jmp_attr_t *attr = get_amd64_switch_jmp_attr(node);
	attr->table        = table;
	attr->table_entity = table_entity;

	for (o = 0; o < n_outs; o++) {
		arch_set_irn_register_req_out(node, o, arch_no_register_req);
	}
}

/** Compare node attributes for SymConst. */
static int cmp_amd64_attr_SymConst(const ir_node *a, const ir_node *b)
{
	const amd64_SymConst_attr_t *attr_a = get_amd64_SymConst_attr_const(a);
	const amd64_SymConst_attr_t *attr_b = get_amd64_SymConst_attr_const(b);

	if (attr_a->entity != attr_b->entity
	    || attr_a->fp_offset != attr_b->fp_offset)
		return 1;

	return 0;
}

static int cmp_imm(const amd64_imm_t *const imm0, const amd64_imm_t *const imm1)
{
	return imm0->offset != imm1->offset || imm0->sc_sign != imm1->sc_sign
	    || imm0->symconst != imm1->symconst;
}

/** Compare common amd64 node attributes. */
static int cmp_amd64_attr(const ir_node *a, const ir_node *b)
{
	const amd64_attr_t *attr_a = get_amd64_attr_const(a);
	const amd64_attr_t *attr_b = get_amd64_attr_const(b);
	return cmp_imm(&attr_a->imm, &attr_b->imm);
}

/** copies the AMD64 attributes of a node. */
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
