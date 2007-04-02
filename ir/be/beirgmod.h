/**
 * @file
 * @brief
 * This file contains the following IRG modifications for be routines:
 * - insertion of Perm nodes
 * - empty block elimination
 * - a simple dead node elimination (set inputs of unreachable nodes to BAD)
 *
 * @author      Sebastian Hack, Daniel Grund, Matthias Braun, Christian Wuerdig
 * @date        04.05.2005
 * @version     $Id$
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _FIRM_BE_IRGMOD_H_
#define _FIRM_BE_IRGMOD_H_

#include "irnode.h"
#include "beirg.h"

#if 0
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
 * corresponding dominance subtrees and creates Phi functions where necessary.
 *
 * @note The visited flag and link fields are used.
 *
 * @param info		  Dominance frontier information.
 * @param lv          Liveness information to be updated. If NULL, no liveness
 *                    updating is performed.
 * @param value       The value that has been duplicated.
 * @param copies_len  the length of the copies array
 * @param copie       an array holding all copies of the value
 * @param phis        An ARR_F where all newly created phis will be inserted,
 *                    may be NULL
 * @param ignore_uses A set of nodes probably using one of the nodes in
 *                    @p nodes. Their usage will not adjusted. They remain
 *                    untouched by this function. May be NULL.
 */
ir_node **be_ssa_construction(const be_dom_front_info_t *info, be_lv_t *lv,
                            ir_node *value, int copies_len, ir_node **copies,
                            const ir_nodeset_t *ignore_uses, int need_new_phis);

/** @deprecated */
void be_ssa_constr_set_ignore(const be_dom_front_info_t *info, be_lv_t *lv,
                              pset *nodes, pset *ignores);
#endif

/**
 * Insert a Perm which permutes all (non-ignore) live values of a given register class
 * after a certain instruction.
 * @param lv        Liveness Information.
 * @param irn       The node to insert the Perm after.
 * @return          The Perm or NULL if nothing was live before @p irn.
 */
ir_node *insert_Perm_after(be_irg_t *birg, const arch_register_class_t *cls,
						   ir_node *irn);

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
