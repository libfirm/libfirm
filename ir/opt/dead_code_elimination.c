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
 * @brief    Dead node elimination
 * @author   Michael Beck, Goetz Lindenmaier
 *
 * Strictly speaking dead node elimination is unnecessary in firm - everthying
 * which is not used can't be found by any walker.
 * The only drawback is that the nodes still take up memory. This phase fixes
 * this by copying all (reachable) nodes to a new obstack and throwing away
 * the old one.
 */
#include "config.h"

#include "iroptimize.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "iredges_t.h"
#include "irhooks.h"
#include "irtools.h"
#include "irgwalk.h"
#include "cgana.h"
#include "irouts.h"
#include "trouts.h"
#include "iropt_t.h"
#include "irpass.h"
#include "pmap.h"

/**
 * Reroute the inputs of a node from nodes in the old graph to copied nodes in
 * the new graph
 */
static void rewire_inputs(ir_node *node, void *env)
{
	(void) env;
	irn_rewire_inputs(node);
}

static void copy_node_dce(ir_node *node, void *env)
{
	ir_node  *new_node = exact_copy(node);
	(void) env;

	/* preserve the node numbers for easier debugging */
	new_node->node_nr = node->node_nr;

	set_irn_link(node, new_node);
}

/**
 * Copies the graph reachable from the End node to the obstack
 * in irg. Then fixes the fields containing nodes of the graph.
 *
 * @param copy_node_nr  If non-zero, the node number will be copied
 */
static void copy_graph_env(ir_graph *irg)
{
	ir_node *anchor = irg->anchor;
	ir_node *new_anchor;

	/* copy nodes */
	irg_walk_in_or_dep(anchor, copy_node_dce, rewire_inputs, NULL);

	/* fix the anchor */
	new_anchor = (ir_node*)get_irn_link(anchor);
	assert(new_anchor != NULL);
	irg->anchor = new_anchor;
}

/**
 * Copies all reachable nodes to a new obstack.  Removes bad inputs
 * from block nodes and the corresponding inputs from Phi nodes.
 * Merges single exit blocks with single entry blocks and removes
 * 1-input Phis.
 * Adds all new nodes to a new hash table for CSE.  Does not
 * perform CSE, so the hash table might contain common subexpressions.
 */
void dead_node_elimination(ir_graph *irg)
{
	struct obstack *graveyard_obst = NULL;
	struct obstack *rebirth_obst   = NULL;

	edges_deactivate(irg);

	/* inform statistics that we started a dead-node elimination run */
	hook_dead_node_elim(irg, 1);

	assert(get_irg_phase_state(irg) != phase_building);

	/* Handle graph state */
	free_callee_info(irg);
	free_irg_outs(irg);
	free_trouts();
	free_loop_information(irg);
	free_vrp_data(irg);
	clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);

	/* A quiet place, where the old obstack can rest in peace,
	   until it will be cremated. */
	graveyard_obst = irg->obst;

	/* A new obstack, where the reachable nodes will be copied to. */
	rebirth_obst = XMALLOC(struct obstack);
	irg->obst = rebirth_obst;
	obstack_init(irg->obst);
	irg->last_node_idx = 0;

	/* We also need a new value table for CSE */
	new_identities(irg);

	/* Copy the graph from the old to the new obstack */
	copy_graph_env(irg);

	/* Free memory from old unoptimized obstack */
	obstack_free(graveyard_obst, 0);  /* First empty the obstack ... */
	xfree(graveyard_obst);            /* ... then free it.           */

	/* inform statistics that the run is over */
	hook_dead_node_elim(irg, 0);
}

ir_graph_pass_t *dead_node_elimination_pass(const char *name)
{
	return def_graph_pass(name ? name : "dce", dead_node_elimination);
}
