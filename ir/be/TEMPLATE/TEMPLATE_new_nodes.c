/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   This file implements the creation of the achitecture specific firm
 *          opcodes and the coresponding node constructors for the TEMPLATE
 *          assembler irg.
 */
#include <stdlib.h>

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
#include "bedump.h"

#include "TEMPLATE_nodes_attr.h"
#include "TEMPLATE_new_nodes.h"
#include "gen_TEMPLATE_regalloc_if.h"

/**
 * Dumper interface for dumping TEMPLATE nodes in vcg.
 * @param F        the output file
 * @param n        the node to dump
 * @param reason   indicates which kind of information should be dumped
 */
static void TEMPLATE_dump_node(FILE *F, const ir_node *n, dump_reason_t reason)
{
	switch (reason) {
	case dump_node_opcode_txt:
		fprintf(F, "%s", get_irn_opname(n));
		break;

	case dump_node_mode_txt:
		fprintf(F, "[%s]", get_mode_name(get_irn_mode(n)));
		break;

	case dump_node_nodeattr_txt:

		/* TODO: dump some attributes which should show up */
		/* in node name in dump (e.g. consts or the like)  */

		break;

	case dump_node_info_txt:
		be_dump_reqs_and_registers(F, n);
		break;
	}
}

const TEMPLATE_attr_t *get_TEMPLATE_attr_const(const ir_node *node)
{
	assert(is_TEMPLATE_irn(node) && "need TEMPLATE node to get attributes");
	return (const TEMPLATE_attr_t *)get_irn_generic_attr_const(node);
}

TEMPLATE_attr_t *get_TEMPLATE_attr(ir_node *node)
{
	assert(is_TEMPLATE_irn(node) && "need TEMPLATE node to get attributes");
	return (TEMPLATE_attr_t *)get_irn_generic_attr(node);
}

static void set_TEMPLATE_value(ir_node *node, ir_tarval *value)
{
	TEMPLATE_attr_t *attr = get_TEMPLATE_attr(node);
	attr->value = value;
}

static void set_TEMPLATE_entity(ir_node *node, ir_entity *entity)
{
	TEMPLATE_attr_t *attr = get_TEMPLATE_attr(node);
	attr->entity = entity;
}

static int TEMPLATE_attrs_equal(const ir_node *a, const ir_node *b)
{
	const TEMPLATE_attr_t *attr_a = get_TEMPLATE_attr_const(a);
	const TEMPLATE_attr_t *attr_b = get_TEMPLATE_attr_const(b);
	return attr_a->value == attr_b->value
	    && attr_a->entity == attr_b->entity;
}

static void TEMPLATE_copy_attr(ir_graph *irg, const ir_node *old_node,
                               ir_node *new_node)
{
	struct obstack *obst    = get_irg_obstack(irg);
	const void     *attr_old = get_irn_generic_attr_const(old_node);
	void           *attr_new = get_irn_generic_attr(new_node);
	backend_info_t *old_info = be_get_info(old_node);
	backend_info_t *new_info = be_get_info(new_node);

	/* copy the attributes */
	memcpy(attr_new, attr_old, get_op_attr_size(get_irn_op(old_node)));

	/* copy out flags */
	new_info->flags = old_info->flags;
	new_info->out_infos =
		DUP_ARR_D(reg_out_info_t, obst, old_info->out_infos);
	new_info->in_reqs = old_info->in_reqs;
}

/* Include the generated constructor functions */
#include "gen_TEMPLATE_new_nodes.c.inl"
