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
 * @brief       Interblock liveness analysis.
 * @author      Sebastian Hack
 * @date        06.12.2004
 * @version     $Id$
 */
#ifndef FIRM_BE_BELIVE_H
#define FIRM_BE_BELIVE_H

#include <stdio.h>

#include "firm_types.h"
#include "pset.h"

#include "irlivechk.h"
#include "bearch.h"
#include "irnodeset.h"

typedef enum {
	be_lv_state_in  = 1,
	be_lv_state_end = 2,
	be_lv_state_out = 4,
} be_lv_state_t;

typedef struct _be_lv_t be_lv_t;

typedef struct _be_lv_info_t be_lv_info_t;

/**
 * Compute the inter block liveness for a graph.
 * @param irg The graph.
 */
be_lv_t *be_liveness(ir_graph *irg);

/**
 * Check the given liveness information against a freshly computed one.
 */
void be_liveness_check(be_lv_t *lv);

/**
 * Free the liveness information.
 */
void be_liveness_free(be_lv_t *lv);

/**
 * Recompute the complete liveness information.
 */
void be_liveness_recompute(be_lv_t *lv);

/**
 * Update the liveness information for a single node.
 * It is irrelevant if there is liveness information present for the node.
 * The liveness information for the node is firstly deleted and then recomputed.
 * If the node is fresh and never recorded inf the liveness information before,
 * it is more efficient to call be_liveness_introduce().
 */
void be_liveness_update(be_lv_t *lv, ir_node *irn);

/**
 * Remove a node from the liveness information.
 */
void be_liveness_remove(be_lv_t *lv, ir_node *irn);

/**
 * Introduce a new node to the liveness information.
 * The new irn is not deleted from any block's liveness information, so it must be fresh!
 * @param lv The liveness info.
 * @param irn The node.
 */
void be_liveness_introduce(be_lv_t *lv, ir_node *irn);

/**
 * Add all nodes which are missing in the current liveness data.
 * The liveness data of the already existing nodes (in the liveness data) is not touched.
 * @param The liveness info.
 */
void be_liveness_add_missing(be_lv_t *lv);

/**
 * Dump the liveness information for a graph.
 * @param f The output.
 * @param irg The graph.
 */
void be_liveness_dump(const be_lv_t *lv, FILE *f);

/**
 * Dump the liveness information for a graph.
 * @param irg The graph.
 * @param cls_name A string used as substring in the filename.
 */
void be_liveness_dumpto(const be_lv_t *lv, const char *cls_name);

/**
 * Check, if a node is live in at a block.
 * @param block The block.
 * @param irn The node to check for.
 * @return 1, if @p irn is live at the entrance of @p block, 0 if not.
 */
int (be_is_live_in)(const be_lv_t *lv, const ir_node *block, const ir_node *irn);

/**
 * Check, if a node is live out at a block.
 * @param block The block.
 * @param irn The node to check for.
 * @return 1, if @p irn is live at the exit of @p block, 0 if not.
 */
int (be_is_live_out)(const be_lv_t *lv, const ir_node *block, const ir_node *irn);

/**
 * Check, if a node is live at the end of a block.
 * @param block The block.
 * @param irn The node to check for.
 * @return 1, if @p irn is live at the end of the block, 0 if not.
 */
int (be_is_live_end)(const be_lv_t *lv, const ir_node *block, const ir_node *irn);

/**
 * Check, if the SSA dominance property is fulfilled.
 * @param irg The graph.
 * @return   1 if dominance property is fulfilled, 0 otherwise
 */
int be_check_dominance(ir_graph *irg);

/**
 * The liveness transfer function.
 * Updates a live set over a single step from a given node to its predecessor.
 * Everything defined at the node is removed from the set, the uses of the node get inserted.
 * @param arch_env The architecture environment.
 * @param cls      The register class to consider.
 * @param irn      The node at which liveness should be computed.
 * @param live     The set of nodes live before @p irn. This set gets modified by updating it to
 *                 the nodes live after irn.
 * @return live.
 */
pset *be_liveness_transfer(const arch_env_t *arch_env, const arch_register_class_t *cls, ir_node *irn, pset *live);

void be_liveness_transfer_ir_nodeset(const arch_env_t *arch_env, const arch_register_class_t *cls, ir_node *node, ir_nodeset_t *nodeset);

/**
 * Put all node live at the end of a block into a set.
 * @param arch_env The architecture environment.
 * @param cls      The register class to consider.
 * @param bl       The block.
 * @param live     The set to put them into.
 * @return live.
 */
pset *be_liveness_end_of_block(const be_lv_t *lv, const arch_env_t *arch_env, const arch_register_class_t *cls, const ir_node *bl, pset *live);

void be_liveness_end_of_block_ir_nodeset(const be_lv_t *lv, const arch_env_t *arch_env, const arch_register_class_t *cls, const ir_node *bl, ir_nodeset_t *nodeset);

/**
 * Compute a set of nodes which are live at another node.
 * BEWARE: This is the liveness immediately after the node,
 *         so the node itself is alive but it's operands maybe not.
 * @param arch_env The architecture environment.
 * @param cls      The register class to consider.
 * @param pos      The node.
 * @param live     The set to put them into.
 * @return live.
 */
pset *be_liveness_nodes_live_at(const be_lv_t *lv, const arch_env_t *arch_env, const arch_register_class_t *cls, const ir_node *pos, pset *live);

/**
 * Compute a set of nodes which are live at another node.
 * BEWARE: This is the liveness immediately before the node,
 *         so the node itself is not alive but it's operands are.
 * @param arch_env The architecture environment.
 * @param cls      The register class to consider.
 * @param pos      The node.
 * @param live     The set to put them into.
 * @return live.
 */
pset *be_liveness_nodes_live_at_input(const be_lv_t *lv, const arch_env_t *arch_env, const arch_register_class_t *cls, const ir_node *pos, pset *live);

/**
 * Make sure the live sets are computed.
 * @param lv The liveness infirmation.
 */
void be_liveness_assure_sets(be_lv_t *lv);

/**
 * Make sure all information needed for liveness checks is available.
 * @param lv The liveness information.
 */
void be_liveness_assure_chk(be_lv_t *lv);

/**
 * Invalidate the liveness information.
 * You must call this if you modify the program and do not
 * update the liveness with the be_liveness_{update,remove,introduce}
 * functions.
 * @param lv The liveness info.
 */
void be_liveness_invalidate(be_lv_t *lv);

#endif /* FIRM_BE_BELIVE_H */
