/**
 * This file contains the following IRG modifications for be routines:
 * - backend dominance information
 * - SSA construction for a set of nodes
 * - insertion of Perm nodes
 * - empty block elimination
 * - a simple dead node elimination (set inputs of unreachable nodes to BAD)
 *
 * Author:      Sebastian Hack, Daniel Grund, Matthias Braun, Christian Wuerdig
 * Date:        04.05.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 * CVS-Id:      $Id$
 */

#ifndef _BEIRGMOD_H
#define _BEIRGMOD_H

#include "firm_types.h"
#include "pset.h"

#include "belive.h"

/*
 * Forward type declaration.
 */
typedef struct _be_dom_front_info_t be_dom_front_info_t;

/**
 * Compute the dominance frontiers for a given graph.
 * @param  irg The graphs.
 * @return A pointer to the dominance frontier information.
 */
be_dom_front_info_t *be_compute_dominance_frontiers(ir_graph *irg);

/**
 * Get the dominance frontier of a block.
 * @param info 	A pointer to the dominance frontier information.
 * @param block The block whose dominance frontier you want.
 * @return A list containing the all blocks in the dominance frontier of @p block.
 */
ir_node **be_get_dominance_frontier(be_dom_front_info_t *info, ir_node *block);

/**
 * Free some dominance frontier information.
 * @param info Some dominance frontier information.
 */
void be_free_dominance_frontiers(be_dom_front_info_t *info);

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
 * @param lv          Liveness information to be updated. If NULL, liveness updating is simply ignored.
 * @param n           Length of nodes array.
 * @param nodes       The nodes which shall represent the same SSA value.
 * @param phis        A set to which all inserted Phis are added.
 * @param ignore_uses A set of nodes probably using one of the nodes in @p nodes.
 *                    Their usage will not adjusted. They remain untouched by this function.
 */
void be_ssa_constr_phis_ignore(be_dom_front_info_t *info, be_lv_t *lv, int n, ir_node *nodes[], pset *phis, pset *ignore_uses);

/**
 * Same as be_ssa_constr_phis_ignore() but without the ignore set.
 */
void be_ssa_constr_phis(be_dom_front_info_t *info, be_lv_t *lv, int n, ir_node *nodes[], pset *phis);

/**
 * Same as be_ssa_constr_phis_ignore() but without the Phi set.
 */
void be_ssa_constr_ignore(be_dom_front_info_t *info, be_lv_t *lv, int n, ir_node *nodes[], pset *ignore_uses);

/**
 * Same as be_ssa_constr_ignore() but with empty ignore set.
 */
void be_ssa_constr(be_dom_front_info_t *info, be_lv_t *lv, int n, ir_node *nodes[]);

/**
 * Same as be_ssa_constr_ignore() but with pset instead of array.
 */
void be_ssa_constr_set_ignore(be_dom_front_info_t *df, be_lv_t *lv, pset *nodes, pset *ignore_uses);

/**
 * Same as be_ssa_constr() but with pset instead of array.
 */
void be_ssa_constr_set(be_dom_front_info_t *info, be_lv_t *lv, pset *nodes);

/**
 * Same as be_ssa_constr_phis_ignore() but with set instead of array.
 */
void be_ssa_constr_set_phis_ignore(be_dom_front_info_t *info, be_lv_t *lv, pset *nodes, pset *phis, pset *ignore);

/**
 * Same as be_ssa_constr_phis_ignore() but without ignore set.
 */
void be_ssa_constr_set_phis(be_dom_front_info_t *info, be_lv_t *lv, pset *nodes, pset *phis);

/**
 * Insert a Perm which permutes all (non-ignore) live values of a given register class
 * after a certain instruction.
 * @param arch_env  The architecture environment.
 * @param lv        Liveness Information.
 * @param cls       The register class.
 * @param dom_front Dominance frontier information.
 * @param irn       The node to insert the Perm after.
 * @return          The Perm or NULL if nothing was live before @p irn.
 */
ir_node *insert_Perm_after(const arch_env_t *arch_env,
						   be_lv_t *lv,
						   const arch_register_class_t *cls,
						   be_dom_front_info_t *dom_front,
						   ir_node *irn);

struct _be_chordal_env_t;

void extreme_liverange_splitting(struct _be_chordal_env_t *cenv);

/**
 * Removes basic blocks that only contain a jump instruction
 * (this will potentially create critical edges).
 *
 * @param irg  the graph that will be changed
 *
 * @return non-zero if the graph was changed, zero else
 */
int be_remove_empty_blocks(ir_graph *irg);

#endif /* _BEIRGMOD_H */
