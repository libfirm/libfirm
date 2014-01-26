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

const amd64_movimm_attr_t *get_amd64_movimm_attr_const(const ir_node *node)
{
	assert(is_amd64_irn(node));
	return (const amd64_movimm_attr_t*)get_irn_generic_attr_const(node);
}

amd64_movimm_attr_t *get_amd64_movimm_attr(ir_node *node)
{
	assert(is_amd64_irn(node));
	return (amd64_movimm_attr_t*)get_irn_generic_attr(node);
}

const amd64_cc_attr_t *get_amd64_cc_attr_const(const ir_node *node)
{
	assert(is_amd64_irn(node));
	return (const amd64_cc_attr_t*)get_irn_generic_attr_const(node);
}

amd64_cc_attr_t *get_amd64_cc_attr(ir_node *node)
{
	assert(is_amd64_irn(node));
	return (amd64_cc_attr_t*)get_irn_generic_attr(node);
}

/**
 * Initializes the nodes attributes.
 */
static void init_amd64_attributes(ir_node *node, arch_irn_flags_t flags,
                                  const arch_register_req_t **in_reqs,
                                  int n_res)
{
	arch_set_irn_flags(node, flags);
	arch_set_irn_register_reqs_in(node, in_reqs);

	ir_graph       *irg  = get_irn_irg(node);
	struct obstack *obst = get_irg_obstack(irg);
	backend_info_t *info = be_get_info(node);
	info->out_infos = NEW_ARR_DZ(reg_out_info_t, obst, n_res);
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

static void init_amd64_cc_attributes(ir_node *node, x86_condition_code_t cc)
{
	amd64_cc_attr_t *attr = get_amd64_cc_attr(node);
	attr->cc = cc;
}

static void init_amd64_movimm_attributes(ir_node *node, ir_entity *entity,
                                         int64_t offset)
{
	amd64_movimm_attr_t *attr = get_amd64_movimm_attr(node);
	attr->entity = entity;
	attr->offset = offset;
}

static int cmp_am(const amd64_am_info_t *const am0,
                  const amd64_am_info_t *const am1)
{
	return am0->offset != am1->offset || am0->entity != am1->entity
	    || am0->base_input != am1->base_input
	    || am0->index_input != am1->index_input
	    || am0->log_scale != am1->log_scale
	    || am0->segment != am1->segment;
}

/** Compare common amd64 node attributes. */
static int cmp_amd64_attr(const ir_node *a, const ir_node *b)
{
	const amd64_attr_t *attr_a = get_amd64_attr_const(a);
	const amd64_attr_t *attr_b = get_amd64_attr_const(b);
	return cmp_am(&attr_a->am, &attr_b->am)
	    || attr_a->data.insn_mode != attr_b->data.insn_mode
	    || attr_a->data.needs_frame_ent != attr_b->data.needs_frame_ent
	    || attr_a->ls_mode != attr_b->ls_mode;
}

static int cmp_amd64_movimm_attr(const ir_node *const a,
                                 const ir_node *const b)
{
	if (cmp_amd64_attr(a, b))
		return true;
	const amd64_movimm_attr_t *const attr_a = get_amd64_movimm_attr_const(a);
	const amd64_movimm_attr_t *const attr_b = get_amd64_movimm_attr_const(b);
	return attr_a->entity != attr_b->entity
	    || attr_a->offset != attr_b->offset;
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
