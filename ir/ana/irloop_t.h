/* Copyright (C) 2002 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Goetz Lindenmaier
**
**  irloops_t.h:
*/

/* $Id$ */

#include "common.h"
#include "irloop.h"

#ifndef _IRLOOP_T_H_
#define _IRLOOP_T_H_

struct ir_loop {
  firm_kind kind;		    /* A type tag, set to k_ir_loop. */

  struct ir_loop *outer_loop;       /* The outer loop */
  struct ir_loop **sons;            /* Inner loops */
  struct ir_node **nodes;           /* Nodes in loop. */
  int depth;                        /* Nesting depth */
  /*
  struct state_entry *mem_phis;
  struct state_entry *states;

  struct obset **oval;
  struct loop_node *link;
  */
};

static INLINE void
add_loop_son(ir_loop *loop, ir_loop *son);

static INLINE void
add_loop_node(ir_loop *loop, ir_node *n);

#endif /* _IRLOOP_T_H_ */
