/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Christian Schaefer
**
** irflag --- optimization flags
*/

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

/* If opt_cse == 1 perfor constant subexpression elimination
   (and, if implemented, global code motion.  @@@ Right now
   cse only within blocks.
   Default: opt_cse == 1. */
void set_opt_cse (int value);
int  get_opt_cse (void);

/* If opt_dead_node_elimination == 1 deallocate all dead nodes
   by copying the firm graph.
   Default: opt_dead_node_elimination == 0.  @@@ as buggy, else 1. */
void set_opt_dead_node_elimination (int value);
int  get_opt_dead_node_elimination (void);

#endif
