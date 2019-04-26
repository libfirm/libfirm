/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Compute and access out edges (also called def-use edges).
 * @author   Goetz Lindenmaier, Michael Beck
 * @date     1.2002
 */
#ifndef FIRM_ANA_IROUTS_H
#define FIRM_ANA_IROUTS_H

#include "firm_types.h"

#include "begin.h"

/**
 * @ingroup irana
 * @defgroup irout Reverse Edges
 * Out-Edges are the reverse of the edges in a firm graph (also called def-use
 * edges)
 * @{
 */

/** Returns the number of successors of the node: */
FIRM_API unsigned get_irn_n_outs(const ir_node *node);

/** Returns the User of a node from the Def-Use edge at position pos. */
FIRM_API ir_node *get_irn_out(const ir_node *def, unsigned pos);

/**
 * Returns the User and its input position from the Def-Use edge of def
 * at position pos.
 */
FIRM_API ir_node *get_irn_out_ex(const ir_node *def, unsigned pos, int *in_pos);

/** Returns the number of control flow successors, ignore keep-alives. */
FIRM_API unsigned get_Block_n_cfg_outs(const ir_node *node);

/** Returns the number of control flow successors, honor keep-alives. */
FIRM_API unsigned get_Block_n_cfg_outs_ka(const ir_node *node);

/** Access predecessor n, ignore keep-alives. */
FIRM_API ir_node *get_Block_cfg_out(const ir_node *node, unsigned pos);

/** Access predecessor n, ignore keep-alives also return its input position. */
FIRM_API ir_node *get_Block_cfg_out_ex(const ir_node *node, unsigned pos,
                                       int *in_pos);

/** Access predecessor n, honor keep-alives. */
FIRM_API ir_node *get_Block_cfg_out_ka(const ir_node *node, unsigned pos);

/**
 * Walks over the graph starting at node.  Walks also if graph is in state
 * "outs_inconsistent".
 */
FIRM_API void irg_out_walk(ir_node *node, irg_walk_func *pre,
                           irg_walk_func *post, void *env);

/**
 * Walks only over Block nodes in the graph.  Has its own visited
 * flag, so that it can be interleaved with the other walker.
 * node must be either op_Block or mode_X.
 */
FIRM_API void irg_out_block_walk(ir_node *node, irg_walk_func *pre,
                                 irg_walk_func *post, void *env);

/**
 * Computes the out edges.  Sets a flag in irg to "outs_consistent".  If the
 * graph is changed this flag must be set to "outs_inconsistent".  Computes
 * out edges from block to floating nodes even if graph is in state
 * "op_pin_state_floats".
 */
FIRM_API void compute_irg_outs(ir_graph *irg);

/** Recomputes out edges if necessary */
FIRM_API void assure_irg_outs(ir_graph *irg);

/** Frees memory occupied by out edges data structures */
FIRM_API void free_irg_outs(ir_graph *irg);

/** @} */

#include "end.h"

#endif
