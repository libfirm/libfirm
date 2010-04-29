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
 * @brief   code selection (transform FIRM into amd64 FIRM)
 * @version $Id: amd64_transform.c 26673 2009-10-01 16:43:13Z matze $
 */
#include "config.h"

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irgmod.h"
#include "iredges.h"
#include "irvrfy.h"
#include "ircons.h"
#include "iropt_t.h"
#include "debug.h"

#include "../benode.h"
#include "../betranshlp.h"
#include "bearch_amd64_t.h"

#include "amd64_nodes_attr.h"
#include "amd64_transform.h"
#include "amd64_new_nodes.h"

#include "gen_amd64_regalloc_if.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/** holds the current code generator during transformation */
static amd64_code_gen_t *env_cg;

/* Some support functions: */

/**
 * Create a DAG constructing a given Const.
 *
 * @param irn  a Firm const
 */
static ir_node *create_const_graph(ir_node *irn, ir_node *block)
{
	tarval  *tv    = get_Const_tarval(irn);
	ir_mode *mode  = get_tarval_mode(tv);
	dbg_info *dbgi = get_irn_dbg_info(irn);
	unsigned value;

	if (mode_is_reference(mode)) {
		/* AMD64 is 64bit, so we can safely convert a reference tarval into Iu */
		assert(get_mode_size_bits(mode) == get_mode_size_bits(mode_Iu));
		tv = tarval_convert_to(tv, mode_Iu);
	}

	value = get_tarval_long(tv);
	printf ("TEST GENERATE %d\n", value);

	return new_bd_amd64_Immediate(dbgi, block, value);
}

/* Op transformers: */

/**
 * Transforms a Const node.
 *
 * @return The transformed ARM node.
 */
static ir_node *gen_Const(ir_node *node) {
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_mode  *mode  = get_irn_mode(node);
	(void) mode;

	ir_node *res = create_const_graph(node, block);
	be_dep_on_frame (res);

	return res;
}

/* Boilerplate code for transformation: */

static void amd64_pretransform_node(void)
{
	amd64_code_gen_t *cg = env_cg;
	(void) cg;
}

static void set_transformer(ir_op *op, be_transform_func amd64_transform_func)
{
	op->ops.generic = (op_func)amd64_transform_func;
}

static void amd64_register_transformers(void)
{
	clear_irp_opcodes_generic_func();

	set_transformer(op_Const,        gen_Const);
}


void amd64_transform_graph(amd64_code_gen_t *cg)
{
	amd64_register_transformers();
	env_cg = cg;
	be_transform_graph(cg->birg, amd64_pretransform_node);
}

void amd64_init_transform(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.amd64.transform");
}
