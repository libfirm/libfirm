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
#include "TEMPLATE_new_nodes_t.h"

#include "TEMPLATE_nodes_attr.h"
#include "bedump.h"
#include "gen_TEMPLATE_regalloc_if.h"
#include "ircons_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "irop_t.h"
#include "iropt_t.h"
#include "irprintf.h"
#include "irprog_t.h"
#include "xmalloc.h"
#include <stdlib.h>

void TEMPLATE_dump_node(FILE *F, const ir_node *n, dump_reason_t reason)
{
	switch (reason) {
	case dump_node_opcode_txt:
		fprintf(F, "%s", get_irn_opname(n));
		break;

	case dump_node_mode_txt:
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

void set_TEMPLATE_value(ir_node *const node, ir_entity *const entity,
                        ir_tarval *const value)
{
	TEMPLATE_attr_t *attr = get_TEMPLATE_attr(node);
	attr->entity = entity;
	attr->value  = value;
}

int TEMPLATE_attrs_equal(const ir_node *a, const ir_node *b)
{
	const TEMPLATE_attr_t *attr_a = get_TEMPLATE_attr_const(a);
	const TEMPLATE_attr_t *attr_b = get_TEMPLATE_attr_const(b);
	return attr_a->value == attr_b->value
	    && attr_a->entity == attr_b->entity;
}
