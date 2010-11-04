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
 * @brief     Some often needed tool-functions
 * @author    Michael Beck
 * @version   $Id$
 */
#include "config.h"

#include "pset.h"

#include <stdlib.h>
#include "irnode_t.h"
#include "irbackedge_t.h"
#include "irtools.h"
#include "irprintf.h"
#include "irpass_t.h"
#include "iropt_t.h"

void firm_clear_link(ir_node *n, void *env)
{
	(void) env;
	set_irn_link(n, NULL);
}

void firm_clear_node_and_phi_links(ir_node *n, void *env)
{
	(void) env;
	set_irn_link(n, NULL);
	if (is_Block(n))
		set_Block_phis(n, NULL);
	else if (is_Phi(n))
		set_Phi_next(n, NULL);
}

void copy_irn_to_irg(ir_node *n, ir_graph *irg)
{
	ir_op *op = get_irn_op(n);
	ir_graph *old_irg;
	ir_node *nn = NULL;

	/* do not copy standard nodes */
	if (op == op_Bad)
		nn = get_irg_bad(irg);
	else if (op == op_NoMem)
		n = get_irg_no_mem(irg);
	else if (op == op_Block) {
		old_irg = get_irn_irg(n);

		if (n == get_irg_start_block(old_irg))
			nn = get_irg_start_block(irg);
		else if (n == get_irg_end_block(old_irg))
			nn = get_irg_end_block(irg);
	}
	else if (op == op_Start)
		nn = get_irg_start(irg);
	else if (op == op_End)
		nn = get_irg_end(irg);
	else if (op == op_Proj) {
		old_irg = get_irn_irg(n);

		if (n == get_irg_initial_exec(old_irg))
			nn = get_irg_initial_exec(irg);
		else if (n == get_irg_frame(old_irg))
			nn = get_irg_frame(irg);
		else if (n == get_irg_initial_mem(old_irg))
			nn = get_irg_initial_mem(irg);
		else if (n == get_irg_args(old_irg))
			nn = get_irg_args(irg);
	}

	if (nn) {
		set_irn_link(n, nn);
		return;
	}

	nn = new_ir_node(get_irn_dbg_info(n),
	                 irg,
	                 NULL,            /* no block yet, will be set later */
	                 op,
	                 get_irn_mode(n),
	                 get_irn_arity(n),
	                 get_irn_in(n) + 1);


	/* Copy the attributes.  These might point to additional data.  If this
	   was allocated on the old obstack the pointers now are dangling.  This
	   frees e.g. the memory of the graph_arr allocated in new_immBlock. */
	copy_node_attr(irg, n, nn);
	set_irn_link(n, nn);

	/* fix the irg for nodes containing a reference to it */
	if (ir_has_irg_ref(nn)) {
		nn->attr.block.irg.irg = irg;
	}
}

ir_node *irn_copy_into_irg(const ir_node *node, ir_graph *irg)
{
	ir_node  *block = NULL;
	ir_op    *op    = get_irn_op(node);
	int       arity = get_irn_arity(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_mode  *mode  = get_irn_mode(node);
	ir_node  *res;
	int       n_deps;
	int       i;

	if (op != op_Block)
		block = get_nodes_block(node);

	if (op->opar == oparity_dynamic) {
		int i;
		res = new_ir_node(dbgi, irg, block, op, mode, -1, NULL);
		for (i = 0; i < arity; ++i) {
			ir_node *in = get_irn_n(node, i);
			add_irn_n(res, in);
		}
	} else {
		ir_node **ins = get_irn_in(node)+1;
		res = new_ir_node(dbgi, irg, block, op, mode, arity, ins);
	}

	/* copy the attributes */
	copy_node_attr(irg, node, res);

	/* duplicate dependency edges */
	n_deps = get_irn_deps(node);
	for (i = 0; i < n_deps; ++i) {
		ir_node *dep = get_irn_dep(node, i);
		add_irn_dep(res, dep);
	}

	return res;
}

ir_node *exact_copy(const ir_node *node)
{
	return irn_copy_into_irg(node, get_irn_irg(node));
}

static ir_node *get_new_node(const ir_node *old_node)
{
	return (ir_node*) get_irn_link(old_node);
}

void irn_rewire_inputs(ir_node *node)
{
	ir_node *new_node;
	int      arity;
	int      i;

	new_node = get_new_node(node);

	if (!is_Block(node)) {
		ir_node *block     = get_nodes_block(node);
		ir_node *new_block = get_new_node(block);
		set_nodes_block(new_node, new_block);
	}

	arity = get_irn_arity(new_node);
	for (i = 0; i < arity; ++i) {
		ir_node *in     = get_irn_n(node, i);
		ir_node *new_in = get_new_node(in);
		set_irn_n(new_node, i, new_in);
	}

	/* Now the new node is complete. We can add it to the hash table for CSE. */
	add_identities(new_node);
}

void firm_pset_dump(pset *set)
{
	void *obj;

	foreach_pset(set, void*, obj) {
		ir_fprintf(stderr, "%+F\n", obj);
	}
}
