/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Algorithms for computing dominance frontiers.
 * @author      Sebastian Hack, Daniel Grund
 * @date        04.05.2005
 */
#include "array.h"
#include "irdom.h"
#include "iredges_t.h"
#include "obst.h"
#include "pmap.h"

/**
 * A wrapper for get_Block_idom.
 * This function returns the block itself, if the block is the start
 * block. Returning NULL would make any != comparison true which
 * suggests, that the start block is dominated by some other node.
 * @param bl The block.
 * @return The immediate dominator of the block.
 */
static inline ir_node *get_idom(ir_node *bl)
{
	ir_node *idom = get_Block_idom(bl);
	return idom == NULL ? bl : idom;
}

/**
 * Compute the dominance frontier for a given block.
 *
 * @param blk   the block where the calculation starts
 *
 * @return the list of all blocks in the dominance frontier of blk
 */
static ir_node **compute_df(ir_node *blk, ir_dom_front_info_t *info)
{
	ir_node **df_list = NEW_ARR_F(ir_node *, 0);

	/* Add local dominance frontiers */
	foreach_block_succ(blk, edge) {
		ir_node *y = get_edge_src_irn(edge);

		if (get_idom(y) != blk) {
			ARR_APP1(ir_node *, df_list, y);
		}
	}

	/*
	 * Go recursively down the dominance tree and add all blocks
	 * into the dominance frontiers of the children, which are not
	 * dominated by the given block.
	 */
	for (ir_node *c = get_Block_dominated_first(blk); c != NULL;
	     c = get_Block_dominated_next(c)) {
		ir_node **df_c_list = compute_df(c, info);

		for (size_t i = ARR_LEN(df_c_list); i-- > 0;) {
			ir_node *w = df_c_list[i];
			if (get_idom(w) != blk)
				ARR_APP1(ir_node *, df_list, w);
		}
	}

	/* now copy the flexible array to the obstack */
	ir_node **const df = DUP_ARR_D(ir_node*, &info->obst, df_list);
	DEL_ARR_F(df_list);

	pmap_insert(info->df_map, blk, df);
	return df;
}

void ir_compute_dominance_frontiers(ir_graph *irg)
{
	ir_dom_front_info_t *info = &irg->domfront;

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES
	                         | IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);
	obstack_init(&info->obst);
	info->df_map = pmap_create();
	compute_df(get_irg_start_block(irg), info);

	add_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE_FRONTIERS);
}

void ir_free_dominance_frontiers(ir_graph *irg)
{
	clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE_FRONTIERS);

	ir_dom_front_info_t *info = &irg->domfront;
	if (info->df_map == NULL)
		return;

	obstack_free(&info->obst, NULL);
	pmap_destroy(info->df_map);
	info->df_map = NULL;
}

/* Get the dominance frontier of a block. */
ir_node **ir_get_dominance_frontier(const ir_node *block)
{
	ir_graph            *irg  = get_irn_irg(block);
	ir_dom_front_info_t *info = &irg->domfront;
	return pmap_get(ir_node*, info->df_map, block);
}
