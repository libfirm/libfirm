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
 * @brief   Extended basis block support -- private header
 * @author  Michael Beck
 * @date    5.2005
 * @version $Id$
 */
#ifndef FIRM_ANA_IREXTBB_T_H
#define FIRM_ANA_IREXTBB_T_H

#include "irgraph_t.h"
#include "irextbb.h"
#include "irtools.h"

/**
 * An extended block.
 */
struct _ir_extblk {
  firm_kind kind;        /**< k_ir_extblk */
  unsigned long visited; /**< visited flag */
  ir_node  **blks;       /**< blocks belonging to this extended block */
  void *link;            /**< private link field */
};

/**
 * Checks whether a pointer points to a extended basic block.
 * Intern version for libFirm.
 */
static INLINE int
_is_ir_extbb (const void *thing) {
  return (get_kind(thing) == k_ir_extblk);
}

/**
 * Gets the visited counter of an extended block.
 * Internal version for libFirm.
 */
static INLINE unsigned long
_get_extbb_visited(const ir_extblk *blk) {
  assert(blk);
  return blk->visited;
}

/**
 * Sets the visited counter of an extended block.
 * Internal version for libFirm.
 */
static INLINE void
_set_extbb_visited(ir_extblk *blk, unsigned long visited) {
  assert(blk);
  blk->visited = visited;
}

/**
 * Mark an extended block as visited in a graph.
 * Internal version for libFirm.
 */
static INLINE void
_mark_extbb_visited(ir_extblk *blk) {
  assert(blk);
  blk->visited = current_ir_graph->block_visited;
}

/**
 * Returns non-zero if an extended was visited.
 * Internal version for libFirm.
 */
static INLINE int
_extbb_visited(const ir_extblk *blk) {
  assert(blk);
  return blk->visited >= current_ir_graph->block_visited;
}

/**
 * Returns non-zero if an extended block was NOT visited.
 * Internal version for libFirm.
 */
static INLINE int
_extbb_not_visited(const ir_extblk *blk) {
  assert(blk);
  return blk->visited < current_ir_graph->block_visited;
}

/**
 * Returns the link field of an extended block.
 * Internal version for libFirm.
 */
static INLINE void *
_get_extbb_link(const ir_extblk *blk) {
  assert(blk);
  return blk->link;
}

/**
 * Sets the link field of an extended block.
 * Internal version for libFirm.
 */
static INLINE void
_set_extbb_link(ir_extblk *blk, void *link) {
  assert(blk);
  blk->link = link;
}

/**
 * Return the number of basis blocks of an extended block
 */
static INLINE int
_get_extbb_n_blocks(const ir_extblk *blk) {
  assert(blk);
  return ARR_LEN(blk->blks);
}

/**
 * Return the i'th basis block of an extended block
 */
static INLINE ir_node *
_get_extbb_block(ir_extblk *blk, int pos)
{
  assert(blk && 0 <= pos && pos < _get_extbb_n_blocks(blk));
  return blk->blks[pos];
}

/**
 * Return the leader basis block of an extended block
 */
static INLINE ir_node *
_get_extbb_leader(ir_extblk *blk)
{
  return blk->blks[0];
}

#define is_ir_extbb(thing)        _is_ir_extbb(thing)
#define get_extbb_visited(blk)    _get_extbb_visited(blk)
#define set_extbb_visited(blk, v) _set_extbb_visited(blk, v)
#define mark_extbb_visited(blk)   _mark_extbb_visited(blk)
#define extbb_visited(blk)        _extbb_visited(blk)
#define extbb_not_visited(blk)    _extbb_not_visited(blk)
#define get_extbb_link(blk)       _get_extbb_link(blk)
#define set_extbb_link(blk, link) _set_extbb_link(blk, link)
#define get_extbb_n_blocks(blk)   _get_extbb_n_blocks(blk)
#define get_extbb_leader(blk)     _get_extbb_leader(blk)

#endif
