
/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
** irgmod: ir graph modification
*/

# include "irnode_t.h"
# include "irgraph_t.h"
# include "irgmod.h"
# include "array.h"

/* Turns a node into a "useless" Tuple.  The Tuple just forms a tuple
   from several inputs.
   This is useful if a node returning a tuple is removed, but the Projs
   extracting values from the tuple are not available. */
void
turn_into_tuple (ir_node *node, int arity)
{
  assert(node);
  set_irn_op(node, op_Tuple);
  if (get_irn_arity(node) == arity) {
    /* keep old array */
  } else {
    /* Allocate new array, don't free old in_array, it's on the obstack. */
    ir_node *block = get_nodes_Block(node);
    node->in = NEW_ARR_D (ir_node *, current_ir_graph->obst, arity+1);
    set_nodes_Block(node, block);
  }
}

/* Insert irnode `new' in place of irnode `old'
   Since `new' may be bigger than `old' replace `old'
   by an op_Id which is smaller than everything */
inline void
exchange (ir_node *old, ir_node *new)
{
  ir_node *block = old->in[0];

  old->op = op_Id;
  old->in = NEW_ARR_D (ir_node *, current_ir_graph->obst, 2);
  old->in[0] = block;
  old->in[1] = new;
}
