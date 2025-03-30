/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @brief    Remove unreachable code
 * @author   Andreas Zwinkau
 *
 * This is done by eliminating all edges into the unreachable code. So that
 * after that the unreachable code should be dead.
 */
#include "irgmod.h"
#include "irgopt.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include <stdbool.h>

static bool is_block_unreachable(ir_node *block)
{
	return get_Block_dom_depth(block) < 0;
}

/**
 * Eliminate block- and phi-inputs pointing to unreachable code
 */
static void unreachable_to_bad(ir_node *node, void *env)
{
	bool *changed = (bool*)env;
	if (is_Block(node)) {
		/* optimization: we do not have to do anything inside the unreachable
		 * code */
		if (is_block_unreachable(node))
			return;

		ir_graph *irg = get_irn_irg(node);
		foreach_irn_in(node, i, pred) {
			if (is_Bad(pred) || !is_block_unreachable(get_nodes_block(pred)))
				continue;
			set_irn_n(node, i, new_r_Bad(irg, mode_X));
			*changed = true;
		}
	} else if (is_Phi(node)) {
		/* optimization: we do not have to do anything inside the unreachable
		 * code */
		ir_node  *block = get_nodes_block(node);
		if (is_block_unreachable(block))
			return;

		ir_graph *const irg = get_irn_irg(node);
		foreach_irn_in(node, i, phi_pred) {
			if (is_Bad(phi_pred))
				continue;
			ir_node *const block_pred = get_Block_cfgpred(block, i);
			if (!is_Bad(block_pred)
			    && !is_block_unreachable(get_nodes_block(block_pred)))
				continue;

			set_irn_n(node, i, new_r_Bad(irg, get_irn_mode(node)));
			*changed = true;
		}
	}
}

/**
 * Remove kept nodes in unreachable blocks.
 *
 * @return whether we modified the End node.
 */
static bool remove_unreachable_keeps(ir_graph *irg)
{
	ir_node  *end       = get_irg_end(irg);
	int       arity     = get_End_n_keepalives(end);
	ir_node **new_in    = XMALLOCN(ir_node*, arity);
	int       new_arity = 0;
	for (int i = 0; i < arity; ++i) {
		ir_node *const ka    = get_End_keepalive(end, i);
		ir_node *const block = get_block(ka);
		if (is_block_unreachable(block))
			continue;
		new_in[new_arity++] = ka;
	}

	bool changed = false;
	if (new_arity != arity) {
		set_End_keepalives(end, new_arity, new_in);
		changed = true;
	}
	free(new_in);

	return changed;
}

void remove_unreachable_code(ir_graph *irg)
{
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE
	                         | IR_GRAPH_PROPERTY_NO_TUPLES);

	bool changed = false;
	irg_walk_graph(irg, unreachable_to_bad, NULL, &changed);
	changed |= remove_unreachable_keeps(irg);

	confirm_irg_properties(irg, changed
		? IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES
		| IR_GRAPH_PROPERTY_NO_TUPLES
		| IR_GRAPH_PROPERTY_ONE_RETURN
		| IR_GRAPH_PROPERTY_MANY_RETURNS
		: IR_GRAPH_PROPERTIES_ALL);
	add_irg_properties(irg, IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE);
}
