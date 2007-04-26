/*
 * Copyrigth (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief    Loop datastructure and access functions -- private stuff.
 * @author   Goetz Lindenmaier
 * @date     7.2002
 * @version  $Id$
 */
#ifndef FIRM_ANA_IRLOOP_T_H
#define FIRM_ANA_IRLOOP_T_H

#include "firm_common.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irloop.h"

/**
 * Possible loop flags, can be or'ed.
 */
typedef enum loop_flags {
  loop_is_count_loop = 0x00000001,  /**< if set it's a counting loop */
  loop_downto_loop   = 0x00000002,  /**< if set, it's a downto loop, else an upto loop */
  loop_is_endless    = 0x00000004,  /**< if set, this is an endless loop */
  loop_is_dead       = 0x00000008,  /**< if set, it's a dead loop ie will never be entered */
  loop_wrap_around   = 0x00000010,  /**< this loop is NOT endless, because of wrap around */
  loop_end_false     = 0x00000020,  /**< this loop end can't be computed "from compute_loop_info.c" */
  do_loop            = 0x00000040,  /**< this is a do loop */
  once               = 0x00000080,  /**< this is a do loop, with a false condition.It itarate once */
} loop_flags_t;

/** The loops datastructure. */
struct ir_loop {
  firm_kind kind;		    /**< A type tag, set to k_ir_loop. */

  struct ir_loop *outer_loop;       /**< The outer loop */
  loop_element   *children;         /**< Mixed array: Contains sons and loop_nodes */
  int depth;                        /**< Nesting depth */
  int n_sons;                       /**< Number of ir_nodes in array "children" */
  int n_nodes;                      /**< Number of loop_nodes in array "children" */
  unsigned flags;                   /**< a set of loop_flags_t */
  tarval  *loop_iter_start;         /**< counting loop: the start value */
  tarval  *loop_iter_end;           /**< counting loop: the last value reached */
  tarval  *loop_iter_increment;     /**< counting loop: the increment */
  ir_node *loop_iter_variable;      /**< The iteration variable of counting loop.*/

  /*
  struct state_entry *mem_phis;
  struct state_entry *states;

  struct obset **oval;
  struct loop_node *link;
  */
#ifdef DEBUG_libfirm
  long loop_nr;            /**< a unique node number for each loop node to make output
			      readable. */
  void *link;              /**< GL @@@ For debugging the analyses. */
#endif
};


/** Add a son loop to a father loop. */
void add_loop_son(ir_loop *loop, ir_loop *son);

/** Add a node to a loop. */
void add_loop_node(ir_loop *loop, ir_node *n);

/** Sets the loop a node belonging to. */
void set_irn_loop(ir_node *n, ir_loop *loop);

/* -------- INLINE functions -------- */

static INLINE int
_is_ir_loop(const void *thing) {
  return (get_kind(thing) == k_ir_loop);
}

static INLINE void
_set_irg_loop(ir_graph *irg, ir_loop *loop) {
  assert(irg);
  irg->loop = loop;
}

static INLINE ir_loop *
_get_irg_loop(ir_graph *irg) {
  assert(irg);
  return irg->loop;
}

static INLINE ir_loop *
_get_loop_outer_loop(const ir_loop *loop) {
  assert(_is_ir_loop(loop));
  return loop->outer_loop;
}

static INLINE int
_get_loop_depth(const ir_loop *loop) {
  assert(_is_ir_loop(loop));
  return loop->depth;
}

static INLINE int
_get_loop_n_sons(const ir_loop *loop) {
  assert(_is_ir_loop(loop));
  return loop->n_sons;
}

/* Uses temporary information to get the loop */
static INLINE ir_loop *
_get_irn_loop(const ir_node *n) {
  return n->loop;
}

#define is_ir_loop(thing)         _is_ir_loop(thing)
#define set_irg_loop(irg, loop)   _set_irg_loop(irg, loop)
#define get_irg_loop(irg)         _get_irg_loop(irg)
#define get_loop_outer_loop(loop) _get_loop_outer_loop(loop)
#define get_loop_depth(loop)      _get_loop_depth(loop)
#define get_loop_n_sons(loop)     _get_loop_n_sons(loop)
#define get_irn_loop(n)           _get_irn_loop(n)

#endif
