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
 * @brief   This file implements the creation of the achitecture specific firm
 *          opcodes and the coresponding node constructors for the amd64
 *          assembler irg.
 * @version $Id: amd64_new_nodes.c 26673 2009-10-01 16:43:13Z matze $
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
static void amd64_dump_node(FILE *F, ir_node *n, dump_reason_t reason)
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
	const amd64_attr_t           *attr     = get_amd64_attr_const(node);
	const amd64_SymConst_attr_t  *sym_attr = CONST_CAST_AMD64_ATTR(amd64_SymConst_attr_t, attr);

	return sym_attr;
}

/**
 * Returns the argument register requirements of a amd64 node.
 */
const arch_register_req_t **get_amd64_in_req_all(const ir_node *node)
{
	const amd64_attr_t *attr = get_amd64_attr_const(node);
	return attr->in_req;
}

/**
 * Returns the argument register requirement at position pos of an amd64 node.
 */
const arch_register_req_t *get_amd64_in_req(const ir_node *node, int pos)
{
	const amd64_attr_t *attr = get_amd64_attr_const(node);
	return attr->in_req[pos];
}

/**
 * Sets the IN register requirements at position pos.
 */
void set_amd64_req_in(ir_node *node, const arch_register_req_t *req, int pos)
{
	amd64_attr_t *attr  = get_amd64_attr(node);
	attr->in_req[pos] = req;
}

/**
 * Initializes the nodes attributes.
 */
static void init_amd64_attributes(ir_node *node, arch_irn_flags_t flags,
                              const arch_register_req_t **in_reqs,
                              const be_execution_unit_t ***execution_units,
                              int n_res)
{
	ir_graph        *irg  = get_irn_irg(node);
	struct obstack  *obst = get_irg_obstack(irg);
	amd64_attr_t *attr    = get_amd64_attr(node);

	backend_info_t  *info;
	(void) execution_units;

	arch_irn_set_flags(node, flags);
	attr->in_req  = in_reqs;

	info            = be_get_info(node);
	info->out_infos = NEW_ARR_D(reg_out_info_t, obst, n_res);
	memset(info->out_infos, 0, n_res * sizeof(info->out_infos[0]));

	attr->data.ins_permuted = 0;
	attr->data.cmp_unsigned = 0;
	attr->ext.pnc           = 0;
	attr->ext.imm_value     = 0;
}

/**
 * Initialize SymConst attributes.
 */
static void init_amd64_SymConst_attributes(ir_node *node, ir_entity *entity)
{
	amd64_SymConst_attr_t *attr = get_irn_generic_attr (node);
	attr->entity = entity;
}

/** Compare node attributes for SymConst. */
static int cmp_amd64_attr_SymConst(ir_node *a, ir_node *b)
{
	const amd64_SymConst_attr_t *attr_a = get_amd64_SymConst_attr_const(a);
	const amd64_SymConst_attr_t *attr_b = get_amd64_SymConst_attr_const(b);

	if (attr_a->entity != attr_b->entity)
		return 1;

	return 0;
}

/** Compare common amd64 node attributes. */
static int cmp_amd64_attr(ir_node *a, ir_node *b)
{
	const amd64_attr_t *attr_a = get_amd64_attr_const(a);
	const amd64_attr_t *attr_b = get_amd64_attr_const(b);

	return attr_a->ext.imm_value != attr_b->ext.imm_value;
}

/* Include the generated constructor functions */
#include "gen_amd64_new_nodes.c.inl"
