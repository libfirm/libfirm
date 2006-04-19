/*
 * Project:     libFIRM
 * File name:   ir/ana/height.h
 * Purpose:     Compute heights of nodes inside basic blocks
 * Author:      Sebastian Hack
 * Modified by:
 * Created:     19.04.2006
 * CVS-ID:      $Id$
 * Copyright:   (c) 2006 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef _FIRM_HEIGHTS_H
#define _FIRM_HEIGHTS_H

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


#endif /* _FIRM_HEIGHTS_H */
