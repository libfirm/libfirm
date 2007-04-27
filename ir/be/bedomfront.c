/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief       Algorithms for computing normal and iterated dominance frontiers.
 * @author      Sebastian Hack, Daniel Grund
 * @date        04.05.2005
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "obst.h"
#include "pmap.h"
#include "pdeq.h"
#include "irdom.h"
#include "array.h"
#include "irgraph.h"
#include "iredges_t.h"
#include "irnodeset.h"

#include "bedomfront.h"

/**
 * The dominance frontier for a graph.
 */
struct _be_dom_front_info_t {
	pmap *df_map;         /**< A map, mapping every block to a list of its dominance frontier blocks. */
	struct obstack obst;  /**< An obstack holding all the frontier data. */
};

/**
 * A wrapper for get_Block_idom.
 * This function returns the block itself, if the block is the start
 * block. Returning NULL would make any != comparison true which
 * suggests, that the start block is dominated by some other node.
 * @param bl The block.
 * @return The immediate dominator of the block.
 */
static INLINE
ir_node *get_idom(ir_node *bl)
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
static
ir_node **compute_df(ir_node *blk, be_dom_front_info_t *info)
{
	ir_node *c;
	const ir_edge_t *edge;
	ir_node **df_list = NEW_ARR_F(ir_node *, 0);
	ir_node **df;
	int len;

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
	for (c = get_Block_dominated_first(blk); c; c = get_Block_dominated_next(c)) {
		int i;
		ir_node **df_c_list = compute_df(c, info);

		for (i = ARR_LEN(df_c_list) - 1; i >= 0; --i) {
			ir_node *w = df_c_list[i];
			if (get_idom(w) != blk)
				ARR_APP1(ir_node *, df_list, w);
		}
	}

	/* now copy the flexible array to the obstack */
	len = ARR_LEN(df_list);
	df = NEW_ARR_D(ir_node *, &info->obst, len);
	memcpy(df, df_list, len * sizeof(df[0]));
	DEL_ARR_F(df_list);

	pmap_insert(info->df_map, blk, df);
	return df;
}

be_dom_front_info_t *be_compute_dominance_frontiers(ir_graph *irg)
{
	be_dom_front_info_t *info = xmalloc(sizeof(*info));

	edges_assure(irg);
	obstack_init(&info->obst);
	info->df_map = pmap_create();
	assure_doms(irg);
	(void)compute_df(get_irg_start_block(irg), info);

	return info;
}

void be_free_dominance_frontiers(be_dom_front_info_t *info)
{
	obstack_free(&info->obst, NULL);
	pmap_destroy(info->df_map);
	free(info);
}

/* Get the dominance frontier of a block. */
ir_node **be_get_dominance_frontier(const be_dom_front_info_t *info,
                                    ir_node *block)
{
	return pmap_get(info->df_map, block);
}

/**
 * Calculates the iterated dominance frontier of a set of blocks.
 * Also clears the link field of the returned blocks as a side effect
 */
void be_get_iterated_dominance_frontiers(const be_dom_front_info_t *domfronts,
                                         ir_nodeset_t *blocks)
{
	ir_node *block;
	ir_nodeset_iterator_t iter;
	waitq *worklist = new_waitq();

	foreach_ir_nodeset(blocks, block, iter) {
		waitq_put(worklist, block);
	}

	while(! pdeq_empty(worklist)) {
		int     i;
		ir_node *block       = waitq_get(worklist);
		ir_node **domfront   = be_get_dominance_frontier(domfronts, block);
		int     domfront_len = ARR_LEN(domfront);

		for (i = 0; i < domfront_len; ++i) {
			ir_node *y = domfront[i];
			if(!ir_nodeset_insert(blocks, y))
				continue;

			waitq_put(worklist, y);
		}
	}

	del_waitq(worklist);
}
