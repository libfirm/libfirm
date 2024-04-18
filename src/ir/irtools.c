/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief     Some often needed tool-functions
 * @author    Michael Beck
 */
#include "irtools.h"

#include "irbackedge_t.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "irprintf.h"
#include <stdlib.h>

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

	/* Now the new node is complete. We can add it to the hash table for CSE. */
	add_identities(new_node);
}
