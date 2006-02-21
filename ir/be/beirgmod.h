
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
 * @param n_origs     The number of nodes for which the copies are introduced.
 * @param orig_nodes  The nodes for which you want to introduce copies.
 * @param n_copies    The number of copies you introduce.
 * @param copy_nodes  An array of nodes which are copies of @p orig.
 * @param ignore_uses A set containing uses which shall not be rerouted.
 */
void be_ssa_constr_ignore(dom_front_info_t *info, int n_origs, ir_node *orig_nodes[],
						  int n_copies, ir_node *copy_nodes[], pset *ignore_uses);

/**
 * Same as be_ssa_constr_ignore() but with a single original node.
 */
void be_ssa_constr_single_ignore(dom_front_info_t *info, ir_node *orig, int n, ir_node *copies[], pset *ignore_uses);

/**
 * Same as be_ssa_constr_single_ignore() but without ignoring nodes.
 */
void be_ssa_constr_single(dom_front_info_t *info, ir_node *orig, int n, ir_node *copy_nodes[]);

/**
 * Same as be_ssa_constr_ignore() but without ignoring nodes.
 */
void be_ssa_constr(dom_front_info_t *info, int n_orig, ir_node *orig[], int n, ir_node *copy_nodes[]);

/**
 * Same as be_ssa_constr() but with psets.
 */
void be_ssa_constr_sets(dom_front_info_t *info, pset *origs, pset *copies);

#endif
