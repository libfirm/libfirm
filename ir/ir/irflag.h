/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Christian Schaefer
**
** irflag --- optimization flags
*/

/* $Id$ */

#ifndef _IRFLAG_H_
#define _IRFLAG_H_

/* If optimize == 0 no optimizations are performed.
   Default: optimize == 1. */
void set_optimize (int value);
int  get_optimize (void);

/* If opt_constant_folding == 1 perform
    - constant expression evaluation (2 + 5 ==> 7, 3 < 2 ==> false)
    - algebraic simplification  (a * 0 ==> 0, a or a ==> a)
    - simplification of tests   ( !(a < b) ==> (a >= b))
    - merging single exit with single entry blocks
    - unreachable code elimination
    - refining the memory representation
    - remove store after load
   Default: opt_constant_folding == 1. */
void set_opt_constant_folding (int value);
int  get_opt_constant_folding (void);

/* If opt_cse == 1 perform constant subexpression elimination.
   Default: opt_cse == 1. */
void set_opt_cse (int value);
int  get_opt_cse (void);

/* If opt_global_cse == 1 and opt_cse == 1 perform intra procedure
   constant subexpression elimination for floating nodes.  Intra
   procedure cse gets the graph into state "floating".  It is necessary
   to run pre/code motion to get the graph back into state "pinned".
   Default: opt_global_cse == 1. */
void set_opt_global_cse (int value);
int  get_opt_global_cse (void);

/* If opt_unreachable_code == 1 replace nodes (except Block,
   Phi and Tuple) with a Bad predecessor by the Bad node.
   Default: opt_unreachable_code == 1. */
void set_opt_unreachable_code(int value);
int  get_opt_unreachable_code(void);

/* Performs Straightening, if simplifications and loop simplifications. */
void set_opt_control_flow(int value);
int  get_opt_control_flow(void);

/* If opt_reassociation == 1 reassociation is performed.
   Default: opt_reassociation == 1. */
void set_opt_reassociation(int value);
int  get_opt_reassociation(void);

/* If opt_dead_node_elimination == 1 deallocate all dead nodes
   by copying the firm graph.
   Default: opt_dead_node_elimination == 0.  @@@ as buggy, else 1. */
void set_opt_dead_node_elimination (int value);
int  get_opt_dead_node_elimination (void);

/* If opt_inline == 1 the inlining transformation is performed. */
void set_opt_inline (int value);
int  get_opt_inline (void);

#endif
