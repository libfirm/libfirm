
/**
 * IRG modifications for be routines.
 * @date 4.5.2005
 *
 * Copyright (C) 2005 Universitaet Karlsruhe.
 * Released under the GPL.
 */

#ifndef _BEIRGMOD_H
#define _BEIRGMOD_H

#include "pset.h"

/*
 * Forward type declaration.
 */
typedef struct _dom_front_info_t dom_front_info_t;

/**
 * Compute the dominance frontiers for a given graph.
 * @param  irg The graphs.
 * @return A pointer to the dominance frontier information.
 */
dom_front_info_t *be_compute_dominance_frontiers(ir_graph *irg);

/**
 * Get the dominance frontier of a block.
 * @param info 	A pointer to the dominance frontier information.
 * @param block The block whose dominance frontier you want.
 * @return A set containing the all blocks in the dominance frontier of @p block.
 */
pset *be_get_dominance_frontier(dom_front_info_t *info, ir_node *block);

/**
 * Free some dominance frontier information.
 * @param info Some dominance frontier information.
 */
void be_free_dominance_frontiers(dom_front_info_t *info);

/**
 * Introduce several copies for one node.
 *
 * A copy in this context means, that you want to introduce several new
 * abstract values (in Firm: nodes) for which you know, that they
 * represent the same concrete value. This is the case if you
 * - copy
 * - spill and reload
 * - rematerialize
 * a value.
 *
 * This function reroutes all uses of the original value to the copies in the
 * corresponding dominance subtrees and creates Phi functions if neccessary.
 *
 * @param info		Dominance frontier information.
 * @param orig		The node for which you want to introduce copies.
 * @param n			The number of copies ypu introduce.
 * @param copies	An array of nodes which are copies of @p orig.
 */
void be_introduce_copies_ignore(dom_front_info_t *info, ir_node *orig,
    int n, ir_node *copies[], pset *irgore_uses);

void be_introduce_copies(dom_front_info_t *info, ir_node *orig, int n, ir_node *copies[]);

void be_introduce_copies_pset(dom_front_info_t *info, pset *nodes);

#endif
