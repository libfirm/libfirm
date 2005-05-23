/*
 * Project:     libFIRM
 * File name:   ir/ana/irloop_t.h
 * Purpose:     Loop datastructure and access functions -- private stuff.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     7.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file irloop_t.h
 * Loop datastructure and access functions -- private stuff.
 *
 * @author Goetz Lindenmaier
 */

#include "firm_common.h"
#include "irgraph_t.h"
#include "irloop.h"

#ifndef _IRLOOP_T_H_
#define _IRLOOP_T_H_

/** The loops datastructure. */
struct ir_loop {
  firm_kind kind;		    /**< A type tag, set to k_ir_loop. */

  struct ir_loop *outer_loop;       /**< The outer loop */
  loop_element   *children;         /**< Mixed array: Contains sons and loop_nodes */
  int depth;                        /**< Nesting depth */
  int n_sons;                       /**< Number of ir_nodes in array "children" */
  int n_nodes;                      /**< Number of loop_nodes in array "children" */

  /*
  struct state_entry *mem_phis;
  struct state_entry *states;

  struct obset **oval;
  struct loop_node *link;
  */
#ifdef DEBUG_libfirm
  int loop_nr;             /**< a unique node number for each loop node to make output
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

#define is_ir_loop(thing)         _is_ir_loop(thing)
#define set_irg_loop(irg, loop)   _set_irg_loop(irg, loop)
#define get_irg_loop(irg)         _get_irg_loop(irg)

#endif /* _IRLOOP_T_H_ */
