/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Christian Schaefer
**
** irflag --- optimization flags
*/

/* $Id$ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "irflag.h"
#include "common.h"


/* 0 - don't do this optimization
   1 - lets see, if there is a better graph */
int optimized = 1;                  /* Turn off all optimizations */

int opt_cse = 1;                    /* Hash the nodes */
int opt_global_cse = 0;             /* Don't use block predecessor for comparison */
/* @@@ 0 solage code placement fehlt */
int opt_constant_folding = 1;       /* Evaluate operations */
int opt_unreachable_code = 1;       /* Bad node propagation */
int opt_control_flow = 1;           /* control flow optimizations. */
int opt_dead_node_elimination = 1;  /* Reclaim memory */
int opt_reassociation = 1;          /* Reassociate nodes */
int opt_inline = 1;                 /* Do inlining transformation */

/* set the flags with set_flagname, get the flag with get_flagname */
INLINE void
set_opt_cse (int value)
{
  opt_cse = value;
}

INLINE int
get_opt_cse (void)
{
  return opt_cse;
}

void set_opt_global_cse (int value)
{
  opt_global_cse = value;
}

int  get_opt_global_cse (void)
{
  return opt_global_cse;
}

INLINE void
set_opt_constant_folding (int value)
{
  opt_constant_folding=value;
}

INLINE int
get_opt_constant_folding (void)
{
  return opt_constant_folding;
}

INLINE void
set_opt_unreachable_code(int value)
{
  opt_unreachable_code = value;
}

INLINE int
get_opt_unreachable_code(void)
{
  return opt_unreachable_code;
}

INLINE void set_opt_control_flow(int value) {
  opt_control_flow = value;
}

INLINE int  get_opt_control_flow(void) {
  return opt_control_flow;
}

INLINE void
set_opt_reassociation(int value)
{
  opt_reassociation = value;
}

INLINE int
get_opt_reassociation(void)
{
  return opt_reassociation;
}

INLINE void
set_opt_dead_node_elimination (int value)
{
  opt_dead_node_elimination = value;
}

INLINE int
get_opt_dead_node_elimination (void)
{
  return opt_dead_node_elimination;
}

INLINE void
set_optimize (int value)
{
  optimized = value;
}

INLINE int
get_optimize (void)
{
  return optimized;
}


INLINE void set_opt_inline (int value) {
  opt_inline = value;
}

INLINE int  get_opt_inline (void) {
  return opt_inline;
}
