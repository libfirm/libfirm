/*
 * Project:     libFIRM
 * File name:   ir/ana/irextbb_t.h
 * Purpose:     Extended basis block support.
 * Author:      Michael Beck
 * Modified by:
 * Created:     5.2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file irextbb_t.h
 *
 *  Computes extended basic blocks.
 *
 *  @author Michael Beck
 */
#ifndef _IREXTBB_T_H_
#define _IREXTBB_T_H_

#include "firm_config.h"
#include "irgraph_t.h"
#include "irextbb.h"

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
  blk->visited = current_ir_graph->visited;
}

/**
 * Returns non-zero if an extended was visited.
 * Internal version for libFirm.
 */
static INLINE int
_extbb_visited(const ir_extblk *blk) {
  assert(blk);
  return blk->visited >= current_ir_graph->visited;
}

/**
 * Returns non-zero if an extended block was NOT visited.
 * Internal version for libFirm.
 */
static INLINE int
_extbb_not_visited(const ir_extblk *blk) {
  assert(blk);
  return blk->visited < current_ir_graph->visited;
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

#define get_extbb_visited(blk)		_get_extbb_visited(blk)
#define set_extbb_visited(blk, v)	_set_extbb_visited(blk, v)
#define mark_extbb_visited(blk)	  _mark_extbb_visited(blk)
#define extbb_visited(blk)        _extbb_visited(blk)
#define extbb_not_visited(blk)    _extbb_not_visited(blk)
#define get_extbb_link(blk)       _get_extbb_link(blk)
#define set_extbb_link(blk, link) _set_extbb_link(blk, link)
#define get_extbb_n_blocks(blk)   _get_extbb_n_blocks(blk)

#endif /* _IREXTBB_H_ */
