/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
** irgmod.h: ir graph modification
*/

# include "irnode.h"


/* Turns a node into a "useless" Tuple.  The Tuple just forms a tuple
   from several inputs.  The tuples predecessors have to be
   set by hand.
   This is useful if a node returning a tuple is removed, but the Projs
   extracting values from the tuple are not available. */
void turn_into_tuple (ir_node *node, int arity);

/* Exchanges two nodes by conserving edges leaving old (i.e., pointers)
   pointing to old. */
inline void exchange (ir_node *old, ir_node *new);
