/**
 * Interblock liveness analysis.
 * @author Sebastian Hack
 * @date 6.12.2004
 */

#ifndef _BELIVE_H
#define _BELIVE_H

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
void be_liveness_dump(FILE *f, ir_graph *irg);

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

#endif
