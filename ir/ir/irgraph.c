/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
**
*/

# include "ircons.h"
# include "irgraph.h"
# include "irprog.h"
# include "iropt.h"
# include "array.h"
# include "irgmod.h"

ir_graph *current_ir_graph;

unsigned long ir_visited = 0;
unsigned long block_visited = 0;


/* Allocates a list of nodes:
    - The start block containing a start node and Proj nodes for it's four
      results (X, M, P, Tuple).
    - The end block containing an end node. This block is not matured after
      new_ir_graph as predecessors need to be added to it.
    - The current block, which is empty and also not matured.
   Further it allocates several datastructures needed for graph construction
   and optimization.
*/
ir_graph *
new_ir_graph (entity *ent, int params)
{
  ir_graph *res;
  ir_node *first_block;
  ir_node *projX;

  res = (ir_graph *) malloc (sizeof (ir_graph));
  current_ir_graph = res;
  add_irp_irg(res);          /* remember this graph global. */

  /** Internal information for graph construction either held in the graph or
  *** initialized for each graph. **/
  res->params = params + 1;  /* number of local variables that are never
                                dereferenced in this graph plus one for
				the store. This is not the number of parameters
                                to the procedure!  */
#if USE_EXPICIT_PHI_IN_STACK
  res->Phi_in_stack = new_Phi_in_stack();  /* A stack needed for automatic Phi
                                generation */
#endif
  res->obst      = (struct obstack *) xmalloc (sizeof (struct obstack));
  obstack_init (res->obst);
  res->value_table = new_identities (); /* Symbol table for local variables
					   of this procedure */

  /** Type inforamtion for the procedure of the graph **/
  res->ent = ent;

  /** Nodes needed in every graph **/
  res->end_block = new_Block ();
  res->end       = new_End ();

  res->start_block = new_Block ();
  res->start     = new_Start ();
  res->bad       = new_ir_node (res, res->start_block, op_Bad, mode_T, 0, NULL);

  /* Proj results of start node */
  projX        = new_Proj (res->start, mode_X, pns_initial_exec);
  set_store (new_Proj (res->start, mode_M, pns_global_store));
  res->frame   = new_Proj (res->start, mode_p, pns_frame_base);
  res->globals = new_Proj (res->start, mode_p, pns_globals);
  res->args    = new_Proj (res->start, mode_T, pns_args);

  add_in_edge(res->start_block, projX);
  // The code generation needs it. leave it in now.
  // Use of this edge is matter of discussion, unresolved. Also possible:
  // add_in_edge(res->start_block, res->start_block), but invalid typed.

  mature_block (res->current_block);

  /** Make a block to start with **/
  first_block = new_Block ();
  add_in_edge (first_block, projX);

  return res;
}

/* access routines for all ir_graph attributes */

ir_node *
get_start_block_of_irgraph (ir_graph *irg)
{
  return irg->start_block;
}

void
set_start_block_of_irgraph (ir_graph *irg, ir_node *node)
{
  irg->start_block = node;
}

ir_node *
get_start_of_irgraph (ir_graph *irg)
{
  return irg->start;
}

void
set_start_of_irgraph(ir_graph *irg, ir_node *node)
{
  irg->start = node;
}

ir_node *
get_end_block_of_irgraph (ir_graph *irg)
{
  return irg->end_block;
}

void
set_end_block_of_irgraph (ir_graph *irg, ir_node *node)
{
  irg->end_block = node;
}

ir_node *
get_end_of_irgraph (ir_graph *irg)
{
  return irg->end;
}

void
set_end_of_irgraph (ir_graph *irg, ir_node *node)
{
  irg->end = node;
}

ir_node *
get_cstore_of_irgraph (ir_graph *irg)
{
  return irg->cstore;
}

void
set_cstore_of_irgraph (ir_graph *irg, ir_node *node)
{
  irg->cstore = node;
}

ir_node *
get_frame_of_irgraph (ir_graph *irg)
{
  return irg->frame;
}

void
set_frame_of_irgraph(ir_graph *irg, ir_node *node)
{
  irg->frame = node;
}


ir_node *
get_irg_globals (ir_graph *irg)
{
  return irg->globals;
}

void
set_irg_globals (ir_graph *irg, ir_node *node)
{
  irg->globals = node;
}



ir_node *
get_args_of_irgraph (ir_graph *irg)
{
  return irg->args;
}

void
set_args_of_irgraph(ir_graph *irg, ir_node *node)
{
  irg->args = node;
}

ir_node *
get_bad_of_irgraph (ir_graph *irg)
{
  return irg->bad;
}

void
set_bad_of_irgraph(ir_graph *irg, ir_node *node)
{
  irg->bad = node;
}

ir_node *
get_current_block_of_irgraph (ir_graph *irg)
{
  return irg->current_block;
}

void
set_current_block_of_irgraph(ir_graph *irg, ir_node *node)
{
  irg->current_block = node;
}

entity *
get_ent_of_irgraph(ir_graph *irg)
{
  return irg->ent;
}

void
set_ent_of_irgraph(ir_graph *irg, entity *ent)
{
  irg->ent = ent;
}

int
get_params_of_irgraph(ir_graph *irg)
{
  return irg->params;
}

void
set_params_of_irgraph(ir_graph *irg, int params)
{
  irg->params = params;
}
