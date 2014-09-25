/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief     Some often needed tool-functions
 * @author    Michael Beck
 */
#include <stdlib.h>
#include "irnode_t.h"
#include "irbackedge_t.h"
#include "irtools.h"
#include "irprintf.h"
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

void firm_clear_block_phis(ir_node *node, void *env)
{
	(void) env;
	if (is_Block(node)) {
		set_Block_phis(node, NULL);
	} else if (is_Phi(node)) {
		set_Phi_next(node, NULL);
	}
}

void firm_collect_block_phis(ir_node *node, void *env)
{
	(void) env;
	if (is_Phi(node))
		add_Block_phi(get_nodes_block(node), node);
}

void copy_irn_to_irg(ir_node *n, ir_graph *irg)
{
	/* do not copy standard nodes */
	ir_node *nn = NULL;
	switch (get_irn_opcode(n)) {
	case iro_NoMem:
		nn = get_irg_no_mem(irg);
		break;

	case iro_Block: {
		ir_graph *old_irg = get_irn_irg(n);
		if (n == get_irg_start_block(old_irg))
			nn = get_irg_start_block(irg);
		else if (n == get_irg_end_block(old_irg))
			nn = get_irg_end_block(irg);
		break;
	}

	case iro_Start:
		nn = get_irg_start(irg);
		break;

	case iro_End:
		nn = get_irg_end(irg);
		break;

	case iro_Proj: {
		ir_graph *old_irg = get_irn_irg(n);
		if (n == get_irg_frame(old_irg))
			nn = get_irg_frame(irg);
		else if (n == get_irg_initial_mem(old_irg))
			nn = get_irg_initial_mem(irg);
		else if (n == get_irg_args(old_irg))
			nn = get_irg_args(irg);
		break;
	}
	}

	if (nn) {
		set_irn_link(n, nn);
		return;
	}

	nn = new_ir_node(get_irn_dbg_info(n),
	                 irg,
	                 NULL,            /* no block yet, will be set later */
	                 get_irn_op(n),
	                 get_irn_mode(n),
	                 get_irn_arity(n),
	                 get_irn_in(n) + 1);


	/* Copy the attributes.  These might point to additional data.  If this
	   was allocated on the old obstack the pointers now are dangling.  This
	   frees e.g. the memory of the graph_arr allocated in new_immBlock. */
	copy_node_attr(irg, n, nn);
	set_irn_link(n, nn);
}

ir_node *irn_copy_into_irg(const ir_node *node, ir_graph *irg)
{
	ir_op    *op    = get_irn_op(node);
	ir_node  *block = op != op_Block ? get_nodes_block(node) : NULL;
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_mode  *mode  = get_irn_mode(node);
	ir_node  *res;
	if (op->opar == oparity_dynamic) {
		res = new_ir_node(dbgi, irg, block, op, mode, -1, NULL);
		foreach_irn_in(node, i, in) {
			add_irn_n(res, in);
		}
	} else {
		ir_node **ins   = get_irn_in(node)+1;
		int       arity = get_irn_arity(node);
		res = new_ir_node(dbgi, irg, block, op, mode, arity, ins);
	}

	/* copy the attributes */
	copy_node_attr(irg, node, res);

	/* duplicate dependency edges */
	for (int i = 0, n_deps = get_irn_n_deps(node); i < n_deps; ++i) {
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
	ir_node *new_node = get_new_node(node);
	if (!is_Block(node)) {
		ir_node *block     = get_nodes_block(node);
		ir_node *new_block = get_new_node(block);
		set_nodes_block(new_node, new_block);
	}

	foreach_irn_in(node, i, in) {
		ir_node *new_in = get_new_node(in);
		set_irn_n(new_node, i, new_in);
	}

	for (int i = 0, n_deps = get_irn_n_deps(new_node); i < n_deps; ++i) {
		ir_node *dep     = get_irn_dep(node, i);
		ir_node *new_dep = get_new_node(dep);
		set_irn_dep(new_node, i, new_dep);
	}

	/* Now the new node is complete. We can add it to the hash table for CSE. */
	add_identities(new_node);
}

void firm_pset_dump(pset *set)
{
	foreach_pset(set, void, obj) {
		ir_fprintf(stderr, "%+F\n", obj);
	}
}
