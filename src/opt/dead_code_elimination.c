/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
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
#include "cgana.h"
#include "iredges_t.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irhooks.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "iroptimize.h"
#include "irouts.h"
#include "irtools.h"
#include "pmap.h"
#include "vrp.h"

/**
 * Reroute the inputs of a node from nodes in the old graph to copied nodes in
 * the new graph
 */
static void rewire_inputs(ir_node *node, void *env)
{
	(void)env;
	irn_rewire_inputs(node);
}

static void copy_node_dce(ir_node *node, void *env)
{
	(void)env;
	ir_node *new_node = exact_copy(node);
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
	/* copy nodes */
	ir_node *anchor = irg->anchor;
	irg_walk_in_or_dep(anchor, copy_node_dce, rewire_inputs, NULL);

	/* fix the anchor */
	ir_node *new_anchor = (ir_node*)get_irn_link(anchor);
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
	edges_deactivate(irg);

	/* Handle graph state */
	free_callee_info(irg);
	free_irg_outs(irg);
	free_loop_information(irg);
	free_vrp_data(irg);
	clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);

	/* A quiet place, where the old obstack can rest in peace,
	   until it will be cremated. */
	struct obstack graveyard_obst = irg->obst;

	/* A new obstack, where the reachable nodes will be copied to. */
	obstack_init(&irg->obst);
	irg->last_node_idx = 0;

	/* We also need a new value table for CSE */
	new_identities(irg);

	/* Copy the graph from the old to the new obstack */
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	copy_graph_env(irg);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	/* Free memory from old unoptimized obstack */
	obstack_free(&graveyard_obst, 0);  /* First empty the obstack ... */
}
