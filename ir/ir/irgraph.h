/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
** irgraph.h: ir graph construction
*/

# ifndef _IRGRAPH_H_
# define _IRGRAPH_H_

# include "obst.h"
# include "tv.h"
# include "pset.h"
/* @@@ we need at most a subset */
# include "entity.h"


#ifndef _IR_NODE_TYPEDEF_
#define _IR_NODE_TYPEDEF_
/* to resolve recursion between irnode.h and irgraph.h */
typedef struct ir_node ir_node;
#endif

/* ir_graph holds all information for a procedure */
typedef struct {
  struct entity  *ent;             /* The entity of this procedure, i.e., the
                                      type of the procedure and the class it
                                      belongs to. */
  struct ir_node *start_block;     /* block the start node will belong to */
  struct ir_node *start;	   /* start node of this ir_graph */
  struct ir_node *end_block;       /* block the end node will belong to */
  struct ir_node *end;		   /* end node of this ir_graph */
  struct ir_node *cstore;	   /* constant store */
  struct ir_node *frame;	   /* method's frame */
  struct ir_node *globals;         /* pointer to the data segment containing all globals as
				      well as global procedures. */
  struct ir_node *args;            /* methods arguments */
  struct ir_node *bad;		   /* bad node of this ir_graph, the one and
                                      only in this graph */
  struct obstack *obst;		   /* obstack where all of the ir_nodes live */
#if USE_EXPICIT_PHI_IN_STACK
  struct Phi_in_stack *Phi_in_stack; /* needed for automatic Phi construction */
#endif
  struct ir_node *current_block;   /* block for newly gen_*()-erated ir_nodes */
  int params;                      /* number of local variable in this procedure */
  // should be n_loc or so, params is ambiguous.
  pset *value_table;               /* value table for global value numbering / cse
				      for optimizing use in iropt.c */
} ir_graph;


/* Global variable holding the current_ir_graph.  This global variable
   is used by the ir construction interface in ircons and by the
   optimizations. */
extern ir_graph *current_ir_graph;

/* create a new ir graph */
ir_graph *new_ir_graph (entity *ent, int params);

extern unsigned long ir_visited;
extern unsigned long block_visited;

/* access routines for all ir_graph attributes */

ir_node *get_start_block_of_irgraph (ir_graph *irg);
void     set_start_block_of_irgraph (ir_graph *irg, ir_node *node);

ir_node *get_start_of_irgraph (ir_graph *irg);
void     set_start_of_irgraph(ir_graph *irg, ir_node *node);

ir_node *get_end_block_of_irgraph (ir_graph *irg);
void     set_end_block_of_irgraph (ir_graph *irg, ir_node *node);

ir_node *get_end_of_irgraph (ir_graph *irg);
void     set_end_of_irgraph (ir_graph *irg, ir_node *node);

ir_node *get_cstore_of_irgraph (ir_graph *irg);
void     set_cstore_of_irgraph (ir_graph *irg, ir_node *node);

ir_node *get_frame_of_irgraph (ir_graph *irg);
void     set_frame_of_irgraph (ir_graph *irg, ir_node *node);


ir_node *get_irg_globals (ir_graph *irg);
void     set_irg_globals (ir_graph *irg, ir_node *node);


ir_node *get_args_of_irgraph (ir_graph *irg);
void     set_args_of_irgraph (ir_graph *irg, ir_node *node);

ir_node *get_bad_of_irgraph (ir_graph *irg);
void     set_bad_of_irgraph (ir_graph *irg, ir_node *node);

/* not implemented yet
struct obstack *get_obst_of_irgraph (ir_graph *irg);
void set_obst_of_irgraph (ir_graph *irg, struct obstack *obst);
*/

ir_node *get_current_block_of_irgraph (ir_graph *irg);
void     set_current_block_of_irgraph (ir_graph *irg, ir_node *node);

entity  *get_ent_of_irgraph (ir_graph *irg);
void     set_ent_of_irgraph (ir_graph *irg, entity *ent);

int      get_params_of_irgraph (ir_graph *irg);
void     set_params_of_irgraph (ir_graph *irg, int params);

# endif /* _IRGRAPH_H_ */
