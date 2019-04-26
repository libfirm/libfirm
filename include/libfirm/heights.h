/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Compute heights of nodes inside basic blocks
 * @author  Sebastian Hack
 * @date    19.04.2006
 */
#ifndef FIRM_ANA_HEIGHTS_H
#define FIRM_ANA_HEIGHTS_H

#include "firm_types.h"

#include "begin.h"

/**
 * @ingroup irana
 * @defgroup ir_heights  Node Heights
 *
 * The height is a measure for the longest data dependency path from a node to
 * the end of a basic block. This is useful for scheduling heuristics and can
 * also be used to speedup reachability queries.
 *
 * @{
 */

/**
 * Returns the height of a node inside a basic block.
 * The height of the node is the maximal number of edges between a sink node in
 * that block and the node itself (plus 1).
 * @param h    The heights object.
 * @param irn  The node.
 * @return     The height of the node.
 */
FIRM_API unsigned get_irn_height(const ir_heights_t *h, const ir_node *irn);

/**
 * Checks if the node @p tgt is reachable according to data dependence edges
 * from the node @p src. Both nodes must be in the same block.
 * @param h   The heights object.
 * @param src The source node of a possible path.
 * @param tgt The target node of a possible path.
 * @return    1, if src is data dependent on tgt, 0 if not.
 */
FIRM_API int heights_reachable_in_block(ir_heights_t *h, const ir_node *src,
                                        const ir_node *tgt);

/**
 * Recomputes the height information for a certain block.
 * This can be used to recompute the height information of a block.
 * @param h     The heights object.
 * @param block The block
 * @return The maximum over all heights in the block.
 */
FIRM_API unsigned heights_recompute_block(ir_heights_t *h, ir_node *block);

/**
 * Creates a new heights object. This also computes the heights for each block
 * in the graph.
 * @param irg The graph.
 */
FIRM_API ir_heights_t *heights_new(ir_graph *irg);

/**
 * Frees a heights object.
 * @param h The heights object.
 */
FIRM_API void heights_free(ir_heights_t *h);

/** @} */

#include "end.h"

#endif
