/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
**
*/

/* $Id$ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include "ircons.h"
# include "irgraph_t.h"
# include "irprog.h"
# include "iropt_t.h"
# include "array.h"
# include "irgmod.h"

ir_graph *current_ir_graph;

#if USE_EXPICIT_PHI_IN_STACK
/* really defined in ircons.c */
typedef struct Phi_in_stack Phi_in_stack;
Phi_in_stack *new_Phi_in_stack();
void free_Phi_in_stack(Phi_in_stack *s);
#endif

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
new_ir_graph (entity *ent, int n_loc)
{
  ir_graph *res;
  ir_node *first_block;
  ir_node *projX;

  res = (ir_graph *) malloc (sizeof (ir_graph));
  current_ir_graph = res;
  add_irp_irg(res);          /* remember this graph global. */

  /** Internal information for graph construction either held in the graph or
  *** initialized for each graph. **/
#if PRECISE_EXC_CONTEXT
  res->n_loc = n_loc + 1 + 1; /* number of local variables that are never
                                 dereferenced in this graph plus one for
	    		         the store plus one for links to fragile
				 operations.  n_loc is not the number of
				 parameters to the procedure!  */
#else
  res->n_loc = n_loc + 1;  /* number of local variables that are never
                              dereferenced in this graph plus one for
	    		      the store. This is not the number of parameters
                              to the procedure!  */
#endif

  res->visited = 0;     /* visited flag, for the ir walker */
  res->block_visited=0; /* visited flag, for the 'block'-walker */

#if USE_EXPICIT_PHI_IN_STACK
  res->Phi_in_stack = new_Phi_in_stack();  /* A stack needed for automatic Phi
                                generation */
#endif
  res->obst      = (struct obstack *) xmalloc (sizeof (struct obstack));
  obstack_init (res->obst);
  res->value_table = new_identities (); /* value table for global value
					   numbering for optimizing use in
					   iropt.c */

  /** Type inforamtion for the procedure of the graph **/
  res->ent = ent;
  set_entity_irg(ent, res);

  /** Nodes needed in every graph **/
  res->end_block = new_immBlock ();
  res->end       = new_End ();

  res->start_block = new_immBlock ();
  res->start   = new_Start ();
  res->bad     = new_ir_node (res, res->start_block, op_Bad, mode_T, 0, NULL);

  /* Proj results of start node */
  projX        = new_Proj (res->start, mode_X, pns_initial_exec);
  set_store (new_Proj (res->start, mode_M, pns_global_store));
  res->frame   = new_Proj (res->start, mode_p, pns_frame_base);
  res->globals = new_Proj (res->start, mode_p, pns_globals);
  res->args    = new_Proj (res->start, mode_T, pns_args);

  add_in_edge(res->start_block, projX);
  /*
   * The code generation needs it. leave it in now.
   * Use of this edge is matter of discussion, unresolved. Also possible:
   * add_in_edge(res->start_block, res->start_block), but invalid typed.
   */
  mature_block (res->current_block);

  /** Make a block to start with **/
  first_block = new_immBlock ();
  add_in_edge (first_block, projX);

  return res;
}

/* Frees the passed irgraph.
   Deallocates all nodes in this graph and the ir_graph structure.
   Sets the field irgraph in the corresponding entity to NULL.
   Does not remove the irgraph from the list in irprog (requires
   inefficient search, call remove_irp_irg by hand).
   Does not free types, entities or modes that are used only by this
   graph, nor the entity standing for this graph. */
void free_ir_graph (ir_graph *irg) {
  set_entity_irg(irg->ent, NULL);
  free(irg->obst);
#if USE_EXPICIT_PHI_IN_STACK
  free_Phi_in_stack(irg->Phi_in_stack);
#endif
  free(irg);
}

/* access routines for all ir_graph attributes:
   templates:
   {attr type} get_irg_{attribute name} (ir_graph *irg);
   void set_irg_{attr name} (ir_graph *irg, {attr type} {attr}); */

ir_node *
get_irg_start_block (ir_graph *irg)
{
  return irg->start_block;
}

void
set_irg_start_block (ir_graph *irg, ir_node *node)
{
  irg->start_block = node;
}

ir_node *
get_irg_start (ir_graph *irg)
{
  return irg->start;
}

void
set_irg_start(ir_graph *irg, ir_node *node)
{
  irg->start = node;
}

ir_node *
get_irg_end_block (ir_graph *irg)
{
  return irg->end_block;
}

void
set_irg_end_block (ir_graph *irg, ir_node *node)
{
  irg->end_block = node;
}

ir_node *
get_irg_end (ir_graph *irg)
{
  return irg->end;
}

void
set_irg_end (ir_graph *irg, ir_node *node)
{
  irg->end = node;
}

ir_node *
get_irg_cstore (ir_graph *irg)
{
  return irg->cstore;
}

void
set_irg_cstore (ir_graph *irg, ir_node *node)
{
  irg->cstore = node;
}

ir_node *
get_irg_frame (ir_graph *irg)
{
  return irg->frame;
}

void
set_irg_frame (ir_graph *irg, ir_node *node)
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
get_irg_args (ir_graph *irg)
{
  return irg->args;
}

void
set_irg_args (ir_graph *irg, ir_node *node)
{
  irg->args = node;
}

ir_node *
get_irg_bad (ir_graph *irg)
{
  return irg->bad;
}

void
set_irg_bad (ir_graph *irg, ir_node *node)
{
  irg->bad = node;
}

ir_node *
get_irg_current_block (ir_graph *irg)
{
  return irg->current_block;
}

void
set_irg_current_block (ir_graph *irg, ir_node *node)
{
  irg->current_block = node;
}

entity *
get_irg_ent (ir_graph *irg)
{
  assert(irg && irg->ent);
  return irg->ent;
}

void
set_irg_ent (ir_graph *irg, entity *ent)
{
  irg->ent = ent;
}

int
get_irg_n_loc (ir_graph *irg)
{
  return irg->n_loc;
}

void
set_irg_n_loc (ir_graph *irg, int n_loc)
{
  irg->n_loc = n_loc;
}

unsigned long
get_irg_visited (ir_graph *irg)
{
  return irg->visited;
}

void
set_irg_visited (ir_graph *irg, unsigned long visited)
{
  irg->visited = visited;
}

void
inc_irg_visited (ir_graph *irg)
{
  irg->visited = irg->visited++;
}

unsigned long
get_irg_block_visited (ir_graph *irg)
{
  return irg->block_visited;
}

void
set_irg_block_visited (ir_graph *irg, unsigned long visited)
{
  irg->block_visited = visited;
}

void
inc_irg_block_visited (ir_graph *irg)
{
  irg->block_visited = irg->block_visited++;
}
