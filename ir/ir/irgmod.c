/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
** irgmod: ir graph modification
*/

# include "irgmod.h"
# include "array.h"

/*  ir_node * */
/*  arg_access (ir_mode *mode, long proj) */
/*  { */
/*    return new_r_Proj (current_ir_graph, current_ir_graph->start,  */
/*  		     current_ir_graph->args, mode, proj); */
/*  } */

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
    /* allocate new array, remove old one. */
    /* !!!??? free old in_array */
    node->in = NEW_ARR_D (ir_node *, current_ir_graph->obst, arity+1);
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
