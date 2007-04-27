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
 * @brief     Introduce several copies for one node.
 * @author    Sebastian Hack, Daniel Grund, Matthias Braun, Christian Wuerdig
 * @date      30.03.2007
 * @version   $Id$
 * Copyright: (c) Universitaet Karlsruhe
 * Licence:   This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.

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
#ifndef FIRM_BE_SSACONSTR_H
#define FIRM_BE_SSACONSTR_H

#include <stdlib.h>
#include "bedomfront.h"
#include "irnode.h"
#include "irnodeset.h"
#include "belive.h"
#include "beirg.h"
#include "pdeq.h"

typedef struct be_ssa_construction_env_t {
	ir_graph                   *irg;
	const be_dom_front_info_t  *domfronts;
	ir_mode                    *mode;
	waitq                      *worklist;
	const ir_nodeset_t         *ignore_uses;
	ir_node                   **new_phis;
	int                         iterated_domfront_calculated;
} be_ssa_construction_env_t;

/**
 * Initializes an ssa construction environment.
 */
void be_ssa_construction_init(be_ssa_construction_env_t *env, be_irg_t *birg);

void be_ssa_construction_add_copy(be_ssa_construction_env_t *env,
                                  ir_node *value);

void be_ssa_construction_add_copies(be_ssa_construction_env_t *env,
                                    ir_node **copies, size_t copies_len);

void be_ssa_construction_set_ignore_uses(be_ssa_construction_env_t *env,
                                         const ir_nodeset_t *ignore_uses);

/**
 * Reconstructs the ssa form for all users of node @p node
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
 * Destroys an ssa construction environment.
 */
void be_ssa_construction_destroy(be_ssa_construction_env_t *env);

#endif
