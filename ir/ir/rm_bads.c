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
static void block_remove_bads(ir_node *block, void *env)
{
	int *changed = (int *)env;
	int i, j;
	ir_node **new_in, *new_block, *phi;
	const int max = get_irn_arity(block);
	const int new_max = count_non_bads(block);
	assert(max >= new_max);

	if (is_Bad(block) || max == new_max)
		return;

	new_in = ALLOCAN(ir_node*, new_max);
	*changed = 1;

	assert(get_Block_dom_depth(block) >= 0);

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
		ir_node *end_block = get_irg_end_block(get_irn_irg(block));
		if (block == end_block) {
			set_irn_in(block, new_max, new_in);
			return;
		}
	}

	new_block = new_r_Block(get_irn_irg(block), new_max, new_in);

	/* 2. Remove inputs on Phis, where the block input is Bad. */
	phi = get_Block_phis(block);
	if (phi != NULL) {
		do {
			ir_node *next = get_Phi_next(phi);
			if (get_irn_arity(phi) != new_max) {
				ir_node *new_phi;

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
			phi = next;
		} while (phi != NULL);
	}

	exchange(block, new_block);
}

/* Remove Bad nodes from Phi and Block inputs.
 *
 * Postcondition: No Bad nodes.
 */
int remove_bads(ir_graph *irg)
{
	int changed = 0;
	/* build phi list per block */
	irg_walk_graph(irg, firm_clear_block_phis, firm_collect_block_phis, NULL);

	/* actually remove Bads */
	irg_block_walk_graph(irg, NULL, block_remove_bads, (void *)&changed);

	if (changed) {
		edges_deactivate(irg);
	}

	return changed;
}
