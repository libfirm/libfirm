/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
** irgmod.h: ir graph modification
*/

# ifndef _IRGMOD_H_
# define _IRGMOD_H_

# include "irnode.h"

/* Turns a node into a "useless" Tuple.  The Tuple node just forms a tuple
   from several inputs.  The predecessors of the tuple have to be
   set by hand.
   This is useful if a node returning a tuple is removed, but the Projs
   extracting values from the tuple are not available. */
void turn_into_tuple (ir_node *node, int arity);

/* Exchanges two nodes by conserving edges leaving old (i.e., pointers
   pointing to old).  Turns the old node into an Id. Requires that
   current_ir_graph is set properly. */
inline void exchange (ir_node *old, ir_node *new);

#endif /* ifndef _IRGMOD_H_ */
