/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @brief    Remove unreachable code
 * @author   Andreas Zwinkau
 *
 * This is done by elminiating all edges into the unreachable code. So that
 * after that the unreachable code should be dead.
 */
#include "irgopt.h"

#include <stdbool.h>

#include "irnode_t.h"
#include "irgopt.h"
#include "irgmod.h"
#include "irgwalk.h"

static bool is_block_unreachable(ir_node *block)
{
	return get_Block_dom_depth(block) < 0;
}

/**
 * Eliminate block- and phi-inputs pointing to unreachable code
 */
static void unreachable_to_bad(ir_node *node, void *env)
{
	bool *changed = (bool *)env;

	if (is_Block(node)) {
		ir_graph *irg;
		int       arity;
		int       i;
		/* optimization: we do not have to do anything inside the unreachable
		 * code */
		if (is_block_unreachable(node))
			return;

		arity = get_irn_arity(node);
		irg   = get_irn_irg(node);
		for (i = 0; i < arity; ++i) {
			ir_node *pred = get_Block_cfgpred(node, i);
			if (is_Bad(pred) || !is_block_unreachable(get_nodes_block(pred)))
				continue;
			set_irn_n(node, i, new_r_Bad(irg, mode_X));
			*changed = true;
		}
	} else if (is_Phi(node)) {
		ir_node  *block = get_nodes_block(node);
		int       arity;
		int       i;
		ir_graph *irg;
		/* optimization: we do not have to do anything inside the unreachable
		 * code */
		if (is_block_unreachable(block))
			return;

		irg   = get_irn_irg(node);
		arity = get_irn_arity(node);
		for (i = 0; i < arity; ++i) {
			ir_node *block_pred;
			ir_node *phi_pred = get_irn_n(node, i);
			if (is_Bad(phi_pred))
				continue;
			block_pred = get_Block_cfgpred(block, i);
			if (!is_Bad(block_pred) && !is_block_unreachable(get_nodes_block(block_pred)))
				continue;

			set_irn_n(node, i, new_r_Bad(irg, get_irn_mode(node)));
			*changed = true;
		}
	}
}

/**
 * remove kept nodes in unreachable blocks
 */
static void remove_unreachable_keeps(ir_graph *irg)
{
	ir_node  *end       = get_irg_end(irg);
	int       arity     = get_irn_arity(end);
	ir_node **new_in    = XMALLOCN(ir_node*, arity);
	int       new_arity = 0;
	int       i;

	for (i = 0; i < arity; ++i) {
		ir_node *ka    = get_End_keepalive(end, i);
		ir_node *block = is_Block(ka) ? ka : get_nodes_block(ka);
		if (is_block_unreachable(block))
			continue;
		new_in[new_arity++] = ka;
	}
	if (new_arity != arity)
		set_End_keepalives(end, new_arity, new_in);
	free(new_in);
}

void remove_unreachable_code(ir_graph *irg)
{
	bool changed = false;

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);

	irg_walk_graph(irg, unreachable_to_bad, NULL, &changed);
	remove_unreachable_keeps(irg);

	confirm_irg_properties(irg, changed
		? IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES
		| IR_GRAPH_PROPERTY_NO_TUPLES
		| IR_GRAPH_PROPERTY_ONE_RETURN
		| IR_GRAPH_PROPERTY_MANY_RETURNS
		: IR_GRAPH_PROPERTIES_ALL);
	add_irg_properties(irg, IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE);
}
