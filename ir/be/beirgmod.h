
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
 * - re-materialize
 * a value.
 *
 * This function reroutes all uses of the original value to the copies in the
 * corresponding dominance subtrees and creates Phi functions if necessary.
 *
 * @param info		  Dominance frontier information.
 * @param n           Length of nodes array.
 * @param nodes       The nodes which shall represent the same SSA value.
 * @param ignore_uses A set of nodes probably using one of the nodes in @p nodes.
 *                    Their usage will not adjusted. They remain untouched by this function.
 */
void be_ssa_constr_ignore(dom_front_info_t *info, int n, ir_node *nodes[], pset *ignore_uses);

/**
 * Same as be_ssa_constr_ignore() but with empty ignore set.
 */
void be_ssa_constr(dom_front_info_t *info, int n, ir_node *nodes[]);

/**
 * Same as be_ssa_constr_ignore() but with pset instead of array.
 */
void be_ssa_constr_set_ignore(dom_front_info_t *df, pset *nodes, pset *ignore_uses);

/**
 * Same as be_ssa_constr() but with pset instead of array.
 */
void be_ssa_constr_set(dom_front_info_t *info, pset *nodes);

#endif
