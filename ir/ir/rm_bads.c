/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @brief    Remove all Bad nodes from ir graph
 * @author   Andreas Zwinkau
 *
 * The idea behind this pass is that Bad nodes may only be present as block
 * or phi inputs (inputs that are now unreachable).
 */
#include <assert.h>

#include "irnode_t.h"

#include "irgopt.h"
#include "irgmod.h"
#include "irgwalk.h"

#include "irtools.h"

/**
 * Return the number of non-Bad predecessors of the given node.
 */
static unsigned count_non_bads(ir_node *node)
{
	unsigned count = 0;
	foreach_irn_in(node, i, pred) {
		if (!is_Bad(pred))
			++count;
	}
	return count;
}

/**
 * Block-walker, remove Bad block predecessors and shorten Phis.
 * Phi links must be up-to-date.
 */
static void block_remove_bads(ir_node *block)
{
	/* 1. Create a new block without Bad inputs */
	ir_graph  *irg     = get_irn_irg(block);
	const int  max     = get_Block_n_cfgpreds(block);
	const int  new_max = count_non_bads(block);
	assert(max >= new_max);
	ir_node  **new_in  = ALLOCAN(ir_node*, new_max);
	int        j       = 0;
	for (int i = 0; i < max; ++i) {
		ir_node *const block_pred = get_Block_cfgpred(block, i);
		if (!is_Bad(block_pred)) {
			new_in[j++] = block_pred;
		}
	}
	assert(j == new_max);

	/* If the end block is unreachable, it might have zero predecessors. */
	if (new_max == 0) {
		ir_node *end_block = get_irg_end_block(irg);
		if (block == end_block) {
			set_irn_in(block, new_max, new_in);
			return;
		}
	}

	ir_node   *new_block    = new_r_Block(irg, new_max, new_in);
	ir_entity *block_entity = get_Block_entity(block);
	set_Block_entity(new_block, block_entity);

	/* 2. Remove inputs on Phis, where the block input is Bad. */
	for (ir_node *phi = get_Block_phis(block), *next; phi != NULL; phi = next) {
		next = get_Phi_next(phi);

		assert(get_irn_arity(phi) == max);

		int j = 0;
		foreach_irn_in(phi, i, pred) {
			ir_node *const block_pred = get_Block_cfgpred(block, i);
			if (!is_Bad(block_pred)) {
				new_in[j++] = pred;
			}
		}
		assert(j == new_max);

		ir_mode *mode    = get_irn_mode(phi);
		ir_node *new_phi = new_r_Phi(new_block, new_max, new_in, mode);
		exchange(phi, new_phi);
	}

	exchange(block, new_block);
}

static void collect(ir_node *node, void *env)
{
	firm_collect_block_phis(node, NULL);
	if (!is_Block(node))
		return;
	ir_node ***blocks_to_process = (ir_node***)env;
	int        arity    = get_Block_n_cfgpreds(node);
	int        non_bads = count_non_bads(node);
	if (arity != non_bads)
		ARR_APP1(ir_node*, *blocks_to_process, node);
}

void remove_bads(ir_graph *irg)
{
	/* build phi list per block */
	ir_node **blocks_to_process = NEW_ARR_F(ir_node*, 0);
	irg_walk_graph(irg, firm_clear_block_phis, collect, &blocks_to_process);

	size_t n_to_process = ARR_LEN(blocks_to_process);
	for (size_t i = 0; i < n_to_process; ++i) {
		ir_node *block = blocks_to_process[i];
		block_remove_bads(block);
	}
	DEL_ARR_F(blocks_to_process);

	if (n_to_process > 0) {
		confirm_irg_properties(irg,
			IR_GRAPH_PROPERTY_CONSISTENT_OUTS
			| IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE
			| IR_GRAPH_PROPERTY_CONSISTENT_POSTDOMINANCE
			| IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO
			| IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE
			| IR_GRAPH_PROPERTY_NO_TUPLES
			| IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES
			| IR_GRAPH_PROPERTY_ONE_RETURN
			| IR_GRAPH_PROPERTY_MANY_RETURNS
			| IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE_FRONTIERS
			| IR_GRAPH_PROPERTY_CONSISTENT_ENTITY_USAGE);
	}
	add_irg_properties(irg, IR_GRAPH_PROPERTY_NO_BADS);
}
