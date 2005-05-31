/*
 * Project:     libFIRM
 * File name:   ir/ana/irextbb.h
 * Purpose:     Extended basis block support.
 * Author:      Michael Beck
 * Modified by:
 * Created:     5.2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file irextbb.h
 *
 *  Computes extended basic blocks.
 *
 *  @author Michael Beck
 */
#ifndef _IREXTBB_H_
#define _IREXTBB_H_

#include "irgraph.h"

typedef struct _ir_extblk ir_extblk;

/** Flags for extended basic block state. */
typedef enum {
  ir_extblk_info_none    = 0,  /**< No extended basic block information is constructed. Default. */
  ir_extblk_info_valid   = 1,  /**< Extended basic block information is valid. */
  ir_extblk_info_invalid = 2,  /**< Extended basic block information is constructed but invalid. */
} irg_extblk_info_state;

/**
 * Compute the extended basic blocks for a graph
 */
void compute_extbb(ir_graph *irg);

/**
 * free all extended block info
 */
void free_extbb(ir_graph *irg);

/**
 * Return the extended block of a node.
 */
ir_extblk *get_nodes_extbb(ir_node *node);

/**
 * Gets the visited counter of an extended block.
 */
unsigned long get_extbb_visited(const ir_extblk *blk);

/**
 * Sets the visited counter of an extended block.
 */
void set_extbb_visited(ir_extblk *blk, unsigned long visited);

/**
 * Mark an extended block as visited in a graph.
 */
void mark_extbb_visited(ir_extblk *blk);

/**
 * Returns non-zero if an extended was visited.
 */
int extbb_visited(const ir_extblk *blk);

/**
 * Returns non-zero if an extended block was NOT visited.
 */
int extbb_not_visited(const ir_extblk *blk);

/**
 * Returns the link field of an extended block.
 */
void *get_extbb_link(const ir_extblk *blk);

/**
 * Sets the link field of an extended block.
 */
void set_extbb_link(ir_extblk *blk, void *link);

/**
 * Return the number of basis blocks of an extended block
 */
int get_extbb_n_blocks(const ir_extblk *blk);

/**
 * Return the i'th basis block of an extended block
 */
ir_node *get_extbb_block(ir_extblk *blk, int pos);


#endif /* _IREXTBB_H_ */
