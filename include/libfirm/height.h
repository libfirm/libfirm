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
 * @brief   Compute heights of nodes inside basic blocks
 * @author  Sebastian Hack
 * @date    19.04.2006
 * @version $Id$
 */
#ifndef FIRM_ANA_HEIGHTS_H
#define FIRM_ANA_HEIGHTS_H

typedef struct _heights_t heights_t;

/**
 * Get the height of a node inside a basic block.
 * The height of the node is the maximal number of edges between a sink node in that block and the node itself (plus 1).
 * @param h    The heights object.
 * @param irn  The node.
 * @return     The height of the node.
 */
unsigned get_irn_height(heights_t *h, const ir_node *irn);

/**
 * Check, if a certain node is reachable according to data dependence edges from another node.
 * @param h The heights object.
 * @param n The first node.
 * @param m The other node.
 * @return  1, if n is data dependent on m, 0 if not.
 */
int heights_reachable_in_block(heights_t *h, const ir_node *n, const ir_node *m);

/**
 * Recompute the height information.
 * This can be used to recompute the height information if the graph has changed since the last computation.
 * @param h The heights object.
 */
void heights_recompute(heights_t *h);

/**
 * Recompute the height information for a certain block.
 * This can be used to recompute the height information of a block.
 * @param h     The heights object.
 * @param block The block
 * @return The maximum over all heights in the block.
 */
unsigned heights_recompute_block(heights_t *h, ir_node *block);

/**
 * Make a new heights object.
 * This also computes the heights for each block in the graph.
 * @param irg The graph.
 */
heights_t *heights_new(ir_graph *irg);

/**
 * Free a heights object.
 * @param h The heights object.
 */
void heights_free(heights_t *h);


#endif /* FIRM_ANA_HEIGHTS_H */
