/*
 * Copyright (C) 2011 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @brief    Convert all unreachable blocks into Bad nodes
 * @author   Andreas Zwinkau
 */
#include "config.h"

#include <assert.h>
#include <stdbool.h>

#include "irnode_t.h"
#include "irgopt.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irtools.h"

/**
 * Block-walker
 */
static void unreachable_to_bad(ir_node *node, void *env)
{
	bool *changed = (bool *)env;
	ir_graph *irg = get_irn_irg(node);

	if (is_Block(node)) {
		if (get_Block_dom_depth(node) < 0) {
			exchange(node, new_r_Bad(irg, mode_BB));
			*changed = true;
		}
	} else if (is_Bad(node)) {
		/* nothing to do */
	} else {
		ir_node *block = get_nodes_block(node);
		if (is_Bad(block) || get_Block_dom_depth(block) < 0) {
			exchange(node, new_r_Bad(irg, get_irn_mode(node)));
			*changed = true;
		}
	}
}

/*
 * Transforms unreachable blocks and the nodes within into Bad nodes
 */
void remove_unreachable_blocks(ir_graph *irg)
{
	bool changed = false;

	assure_doms(irg);

	irg_walk_graph(irg, unreachable_to_bad, NULL, &changed);

	if (changed) {
		edges_deactivate(irg);
		set_irg_outs_inconsistent(irg);
	}
}
