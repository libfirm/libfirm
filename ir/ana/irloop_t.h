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
*
* @author Goetz Lindenmaier
*/

/* $Id$ */

#include "firm_common.h"
#include "irloop.h"

#ifndef _IRLOOP_T_H_
#define _IRLOOP_T_H_

/** The loops datastructure. */
struct ir_loop {
  firm_kind kind;		    /**< A type tag, set to k_ir_loop. */

  struct ir_loop *outer_loop;       /**< The outer loop */
  loop_element   *children;         /**< Mixed array: Contains sons and loop_nodes */
/*  struct ir_loop **sons; */           /**< Inner loops */
/*  struct ir_node **nodes; */          /**< Nodes in loop. */
  int depth;                        /**< Nesting depth */
  int n_sons;                       /**< Number of ir_nodes in array "children" */
  int n_nodes;                      /**< Number of loop_nodes in array "childern" */

  /*
  struct state_entry *mem_phis;
  struct state_entry *states;

  struct obset **oval;
  struct loop_node *link;
  */
#ifdef DEBUG_libfirm
  int loop_nr;             /**< a unique node number for each loop node to make output
			      readable. */
#endif

};

static INLINE void
add_loop_son(ir_loop *loop, ir_loop *son);

static INLINE void
add_loop_node(ir_loop *loop, ir_node *n);

#endif /* _IRLOOP_T_H_ */
