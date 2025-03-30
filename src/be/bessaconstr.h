/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       SSA construction for a set of nodes
 * @author      Sebastian Hack, Daniel Grund, Matthias Braun, Christian Wuerdig
 * @date        30.03.2007
 *
 * The problem: Given a value and a set of "copies" that are known to
 * represent the same abstract value, rewire all usages of the original value
 * to their closest copy while introducing phis as necessary.
 *
 * Algorithm: Mark all blocks in the iterated dominance frontiers of the value
 * and its copies. Link the copies ordered by dominance to the blocks.  Then
 * we search for each use all definitions in the current block, if none is
 * found, then we search one in the immediate dominator. If we are in a block
 * of the dominance frontier, create a phi and do the same search for all
 * phi arguments.
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
 */
#ifndef FIRM_BE_BESSACONSTR_H
#define FIRM_BE_BESSACONSTR_H

#include <stdbool.h>
#include "firm_types.h"
#include "belive.h"
#include "pdeq.h"
#include "irnodemap.h"
#include "obst.h"

typedef struct be_ssa_construction_env_t {
	ir_graph                    *irg;
	const arch_register_req_t   *phi_req;
	deq_t                        worklist;
	ir_node                    **new_phis;
	bool                         iterated_domfront_calculated;
	ir_nodemap                   infos;
	struct obstack               obst;
} be_ssa_construction_env_t;

/**
 * Initializes an SSA construction environment.
 *
 * @param env    an empty SSA construction environment
 * @param irg    the graph
 */
void be_ssa_construction_init(be_ssa_construction_env_t *env, ir_graph *irg);

void be_ssa_construction_add_copy(be_ssa_construction_env_t *env,
                                  ir_node *value);

void be_ssa_construction_add_copies(be_ssa_construction_env_t *env,
                                    ir_node **copies, size_t copies_len);

/**
 * Reconstructs the SSA form for all users of node @p node
 */
void be_ssa_construction_fix_users(be_ssa_construction_env_t *env,
                                   ir_node *node);

void be_ssa_construction_fix_users_array(be_ssa_construction_env_t *env,
                                         ir_node **nodes, size_t nodes_len);

/**
 * Recompute the liveness of the inserted phis.
 * @note Remember that you have to call update_liveness on the copies yourself
 */
void be_ssa_construction_update_liveness_phis(be_ssa_construction_env_t *env,
                                             be_lv_t *lv);

ir_node **be_ssa_construction_get_new_phis(be_ssa_construction_env_t *env);

/**
 * Destroys an SSA construction environment.
 */
void be_ssa_construction_destroy(be_ssa_construction_env_t *env);

#endif
