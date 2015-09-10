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
#include "irop_t.h"
#include "irprintf.h"
#include "xmalloc.h"
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

static void set_TEMPLATE_value(ir_node *const node, ir_entity *const entity, ir_tarval *const value)
{
	TEMPLATE_attr_t *attr = get_TEMPLATE_attr(node);
	attr->entity = entity;
	attr->value  = value;
}

static int TEMPLATE_attrs_equal(const ir_node *a, const ir_node *b)
{
	const TEMPLATE_attr_t *attr_a = get_TEMPLATE_attr_const(a);
	const TEMPLATE_attr_t *attr_b = get_TEMPLATE_attr_const(b);
	return attr_a->value == attr_b->value
	    && attr_a->entity == attr_b->entity;
}

/* Include the generated constructor functions */
#include "gen_TEMPLATE_new_nodes.c.inl"
