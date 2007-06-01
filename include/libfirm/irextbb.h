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
 * @brief   Extended basis block support.
 * @author  Michael Beck
 * @date    5.2005
 * @version $Id$
 */
#ifndef FIRM_ANA_IREXTBB_H
#define FIRM_ANA_IREXTBB_H

#include "firm_types.h"
#include "execfreq.h"

/** Flags for extended basic block state. */
typedef enum {
  ir_extblk_info_none    = 0,  /**< No extended basic block information is constructed. Default. */
  ir_extblk_info_valid   = 1,  /**< Extended basic block information is valid. */
  ir_extblk_info_invalid = 2   /**< Extended basic block information is constructed but invalid. */
} irg_extblk_info_state;

/* type of callback function for ir_graph walk */
#ifndef _EXTBB_WALK_FUNC_TYPEDEF_
#define _EXTBB_WALK_FUNC_TYPEDEF_
/**
 * The type of a walk function.  Does not use the link field.
 *
 * @param blk  - the extended basic block that is just visited
 * @param env  - an environment pointer passed by the walk functions
 */
typedef void extbb_walk_func(ir_extblk *blk, void *env);
#endif

/**
 * Checks whether a pointer points to a extended basic block.
 * Intern version for libFirm.
 */
int is_ir_extbb(const void *thing);

/**
 * Compute the extended basic blocks for a graph.
 */
void compute_extbb(ir_graph *irg);

/**
 * Compute the extended basic blocks for a graph based on execution frequencies.
 */
void compute_extbb_execfreqs(ir_graph *irg, ir_exec_freq *execfreqs);

/**
 * free all extended block info.
 */
void free_extbb(ir_graph *irg);

/**
 * Return the extended block of a node.
 *
 * @param node  the node
 */
ir_extblk *get_nodes_extbb(ir_node *node);

/**
 * Gets the visited counter of an extended block.
 *
 * @param blk  the extended basic block
 */
unsigned long get_extbb_visited(const ir_extblk *blk);

/**
 * Sets the visited counter of an extended block.
 *
 * @param blk  the extended basic block
 */
void set_extbb_visited(ir_extblk *blk, unsigned long visited);

/**
 * Mark an extended block as visited in a graph.
 * Uses the block visit flag.
 *
 * @param blk  the extended basic block
 */
void mark_extbb_visited(ir_extblk *blk);

/**
 * Returns non-zero if an extended was visited.
 * Uses the block visit flag.
 *
 * @param blk  the extended basic block
 */
int extbb_visited(const ir_extblk *blk);

/**
 * Returns non-zero if an extended block was NOT visited.
 * Uses the block visit flag.
 *
 * @param blk  the extended basic block
 */
int extbb_not_visited(const ir_extblk *blk);

/**
 * Returns the link field of an extended block.
 *
 * @param blk  the extended basic block
 */
void *get_extbb_link(const ir_extblk *blk);

/**
 * Sets the link field of an extended block.
 *
 * @param blk  the extended basic block
 * @param link the new link value
 */
void set_extbb_link(ir_extblk *blk, void *link);

/**
 * Return the number of basic blocks of an extended block.
 *
 * @param blk  the extended basic block
 */
int get_extbb_n_blocks(const ir_extblk *blk);

/**
 * Return the i'th basic block of an extended block.
 *
 * @param blk  the extended basic block
 * @param pos  the position
 */
ir_node *get_extbb_block(ir_extblk *blk, int pos);

/**
 * Return the leader basic block of an extended block.
 *
 * @param blk  the extended basic block
 */
ir_node *get_extbb_leader(ir_extblk *blk);

/**
 * Return the node number of an extended block.
 * Its the block number of the leader block
 *
 * @param blk  the extended basic block
 */
long get_extbb_node_nr(ir_extblk *blk);

/**
 * Walks only over Extended Basic Block nodes in the graph.
 *
 * @param blk  - the start extended block node
 * @param pre  - walker function, executed before the predecessor of a node are visited
 * @param post - walker function, executed after the predecessor of a node are visited
 * @param env  - environment, passed to pre and post
 *
 * This function Walks only over Block nodes in the graph. Has it's own visited
 * flag, so that it can be interleaved with the other walker.
 * If a none block is passed, starts at the block this node belongs to.
 * If end is passed also visits kept alive blocks. Does not use the link field.
 */
void irg_extblock_walk(ir_extblk *blk, extbb_walk_func *pre, extbb_walk_func *post, void *env);

/**
 * Walks only over reachable Extended Basic Block nodes in the graph.
 * Ensures, that the extended block containing the End node is visited last
 * and the block containing Start visited first (in post order).
 *
 * @param irg  - the irg graph
 * @param pre  - walker function, executed before the predecessor of a block are visited
 * @param post - walker function, executed after the predecessor of a block are visited
 * @param env  - environment, passed to pre and post
 */
void irg_extblock_walk_graph(ir_graph *irg, extbb_walk_func *pre, extbb_walk_func *post, void *env);

#endif
