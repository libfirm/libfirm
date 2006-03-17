/**
 * Interblock liveness analysis.
 * @author Sebastian Hack
 * @date 6.12.2004
 */

#ifndef _BELIVE_H
#define _BELIVE_H

#include "firm_types.h"
#include "pset.h"
#include "bearch_t.h"

#include <stdio.h>

/**
 * Compute the inter block liveness for a graph.
 * @param irg The graph.
 */
void be_liveness(ir_graph *irg);

/**
 * Dump the liveness information for a graph.
 * @param f The output.
 * @param irg The graph.
 */
void be_liveness_dump(ir_graph *irg, FILE *f);

/**
 * Dump the liveness information for a graph.
 * @param irg The graph.
 * @param cls_name A string used as substring in the filename.
 */
void be_liveness_dumpto(ir_graph *irg, const char *cls_name);

/**
 * Check, if a node is live in at a block.
 * @param block The block.
 * @param irn The node to check for.
 * @return 1, if @p irn is live at the entrance of @p block, 0 if not.
 */
int (is_live_in)(const ir_node *block, const ir_node *irn);

/**
 * Check, if a node is live out at a block.
 * @param block The block.
 * @param irn The node to check for.
 * @return 1, if @p irn is live at the exit of @p block, 0 if not.
 */
int (is_live_out)(const ir_node *block, const ir_node *irn);

/**
 * Check, if a node is live at the end of a block.
 * @param block The block.
 * @param irn The node to check for.
 * @return 1, if @p irn is live at the end of the block, 0 if not.
 */
int (is_live_end)(const ir_node *block, const ir_node *irn);

/**
 * Check, if the SSA dominance property is fulfilled.
 * @param irg The graph.
 */
void be_check_dominance(ir_graph *irg);

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

/**
 * Put all node live at the end of a block into a set.
 * @param arch_env The architecture environment.
 * @param cls      The register class to consider.
 * @param bl       The block.
 * @param live     The set to put them into.
 * @return live.
 */
pset *be_liveness_end_of_block(const arch_env_t *arch_env, const arch_register_class_t *cls, const ir_node *bl, pset *live);

/**
 * Compute a set of nodes which are live at another node.
 * @param arch_env The architecture environment.
 * @param cls      The register class to consider.
 * @param pos      The node.
 * @param live     The set to put them into.
 * @return live.
 */
pset *be_liveness_nodes_live_at(const arch_env_t *arch_env, const arch_register_class_t *cls, const ir_node *pos, pset *live);

#endif /* _BELIVE_H */
