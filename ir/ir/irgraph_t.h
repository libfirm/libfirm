/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
** irgraph.h: ir graph construction
*/

/* $Id$ */

# ifndef _IRGRAPH_T_H_
# define _IRGRAPH_T_H_
# include "obst.h"
# include "pset.h"
# include "irgraph.h"

/* ir_graph holds all information for a procedure */
struct ir_graph {
  struct entity  *ent;               /* The entity of this procedure, i.e.,
					the type of the procedure and the
					class it belongs to. */
  struct type    *frame_type;        /* A class type representing the stack frame.
					Can include "inner" methods. */
  struct ir_node *start_block;       /* block the start node will belong to */
  struct ir_node *start;	     /* start node of this ir_graph */
  struct ir_node *end_block;         /* block the end node will belong to */
  struct ir_node *end;		     /* end node of this ir_graph */
  struct ir_node *cstore;	     /* constant store -- no more needed!! */
  struct ir_node *frame;             /* method's frame */
  struct ir_node *globals;           /* pointer to the data segment containing all
				        globals as well as global procedures. */
  struct ir_node *args;              /* methods arguments */
  struct ir_node *bad;		     /* bad node of this ir_graph, the one and
                                        only in this graph */
  struct obstack *obst;		     /* obstack where all of the ir_nodes live */
#if USE_EXPICIT_PHI_IN_STACK
  struct Phi_in_stack *Phi_in_stack; /* needed for automatic Phi construction */
#endif
  struct ir_node *current_block;     /* block for newly gen_*()-erated
					ir_nodes */
  int n_loc;                         /* number of local variable in this
					procedure including procedure parameters. */
  pset *value_table;                 /* value table for global value numbering
					for optimizing use in iropt.c */
  unsigned long visited;             /* this flag is an identifier for
					ir walk. it will be incremented,
					every time, someone walk through
					the graph */
  unsigned long block_visited;       /* same as visited, for a
					complete block */
};
# endif /* _IRGRAPH_T_H_ */
