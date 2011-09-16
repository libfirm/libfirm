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
 * @brief    Remove all Bad nodes from ir graph
 * @author   Andreas Zwinkau
 */
#include "config.h"

#include <assert.h>

#include "irnode_t.h"

#include "irgopt.h"
#include "irgmod.h"
#include "irgwalk.h"

#include "irtools.h"

/**
 * Return the number of non-Bad predecessors of the given node.
 */
static int count_non_bads(ir_node *node)
{
	int arity = get_irn_arity(node);
	int count = 0;
	int i;
	for (i = 0; i < arity; ++i) {
		if (!is_Bad(get_irn_n(node, i)))
			++count;
	}
	return count;
}

/**
 * Block-walker, remove Bad block predecessors and shorten Phis.
 * Phi links must be uptodate.
 */
static void block_remove_bads(ir_node *block)
{
	ir_graph  *irg     = get_irn_irg(block);
	const int  max     = get_irn_arity(block);
	const int  new_max = count_non_bads(block);
	ir_node  **new_in  = ALLOCAN(ir_node*, new_max);
	int        i;
	int        j;
	ir_node   *new_block;
	ir_node   *phi;
	ir_node   *next;
	ir_entity *block_entity;
	assert(max >= new_max);

	/* 1. Create a new block without Bad inputs */
	for (i = j = 0; i < max; ++i) {
		ir_node *block_pred = get_irn_n(block, i);
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

	new_block = new_r_Block(irg, new_max, new_in);
	block_entity = get_Block_entity(block);
	if (block_entity)
		set_Block_entity(new_block, block_entity);

	/* 2. Remove inputs on Phis, where the block input is Bad. */
	for (phi = get_Block_phis(block); phi != NULL; phi = next) {
		ir_node *new_phi;

		next = get_Phi_next(phi);
		assert(get_irn_arity(phi) == max);

		for (i = j = 0; i < max; ++i) {
			ir_node *block_pred = get_irn_n(block, i);

			if (!is_Bad(block_pred)) {
				ir_node *pred = get_irn_n(phi, i);
				new_in[j++] = pred;
			}
		}
		assert(j == new_max);

		new_phi = new_r_Phi(new_block, new_max, new_in, get_irn_mode(phi));
		exchange(phi, new_phi);
	}

	exchange(block, new_block);
}

static void collect(ir_node *node, void *env)
{
	firm_collect_block_phis(node, NULL);
	if (is_Block(node)) {
		ir_node ***blocks_to_process = (ir_node***)env;
		int        arity    = get_irn_arity(node);
		int        non_bads = count_non_bads(node);
		if (arity != non_bads)
			ARR_APP1(ir_node*, *blocks_to_process, node);
	}
}

/* Remove Bad nodes from Phi and Block inputs.
 *
 * This does NOT remove unreachable code.
 *
 * Postcondition: No Bad nodes.
 */
int remove_bads(ir_graph *irg)
{
	size_t i;
	size_t n_to_process;
	ir_node **blocks_to_process = NEW_ARR_F(ir_node*, 0);

	/* build phi list per block */
	irg_walk_graph(irg, firm_clear_block_phis, collect, &blocks_to_process);

	n_to_process = ARR_LEN(blocks_to_process);
	for (i = 0; i < n_to_process; ++i) {
		ir_node *block = blocks_to_process[i];
		block_remove_bads(block);
	}
	DEL_ARR_F(blocks_to_process);

	if (n_to_process > 0) {
		edges_deactivate(irg);
		clear_irg_state(irg, IR_GRAPH_STATE_CONSISTENT_OUTS);
		clear_irg_state(irg, IR_GRAPH_STATE_CONSISTENT_DOMINANCE);
		return 1;
	}

	return 0;
}
