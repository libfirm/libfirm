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
  struct entity  *ent;               /* The entity of this procedure, i.e.,
					the type of the procedure and the
					class it belongs to. */
  struct ir_node *start_block;       /* block the start node will belong to */
  struct ir_node *start;	     /* start node of this ir_graph */
  struct ir_node *end_block;         /* block the end node will belong to */
  struct ir_node *end;		     /* end node of this ir_graph */
  struct ir_node *cstore;	     /* constant store */
  struct ir_node *frame;             /* method's frame */
  struct ir_node *globals;           /* pointer to the data segment containing all globals as
				      well as global procedures. */
  struct ir_node *args;              /* methods arguments */
  struct ir_node *bad;		     /* bad node of this ir_graph, the one and
                                        only in this graph */
  struct obstack *obst;		     /* obstack where all of the ir_nodes live */
#if USE_EXPICIT_PHI_IN_STACK
  struct Phi_in_stack *Phi_in_stack; /* needed for automatic Phi
					construction */
#endif
  struct ir_node *current_block;     /* block for newly gen_*()-erated
					ir_nodes */
  int params;                        /* number of local variable in this
					procedure; should be n_loc or so,
					params is ambiguous. */
  pset *value_table;                 /* value table for global value
					numbering for optimizing use in
					iropt.c */
  unsigned long visited;             /* this flag is an identifier for
					ir walk. it will be incremented,
					every time, someone walk through
					the graph */
  unsigned long block_visited;       /* same as visited, for a
					complete block */
} ir_graph;


/* Global variable holding the current_ir_graph.  This global variable
   is used by the ir construction interface in ircons and by the
   optimizations. */
extern ir_graph *current_ir_graph;

/* create a new ir graph.  Automatically sets the field irg of
   entity to the new ir graph. */
ir_graph *new_ir_graph (entity *ent, int params);


/* access routines for all ir_graph attributes */
ir_node *get_irg_start_block (ir_graph *irg);
void set_irg_start_block (ir_graph *irg, ir_node *node);

ir_node *get_irg_start (ir_graph *irg);
void set_irg_start (ir_graph *irg, ir_node *node);

ir_node *get_start_of_irgraph (ir_graph *irg);
void     set_start_of_irgraph(ir_graph *irg, ir_node *node);

ir_node *get_irg_end_block (ir_graph *irg);
void set_irg_end_block (ir_graph *irg, ir_node *node);

ir_node *get_irg_end (ir_graph *irg);
void set_irg_end (ir_graph *irg, ir_node *node);

ir_node *get_irg_cstore (ir_graph *irg);
void set_irg_cstore (ir_graph *irg, ir_node *node);

ir_node *get_irg_frame (ir_graph *irg);
void set_irg_frame (ir_graph *irg, ir_node *node);

ir_node *get_irg_globals (ir_graph *irg);
void     set_irg_globals (ir_graph *irg, ir_node *node);

ir_node *get_irg_args (ir_graph *irg);
void set_irg_args (ir_graph *irg, ir_node *node);

/* Use new_Bad() instead!! */
ir_node *get_irg_bad (ir_graph *irg);
void set_irg_bad (ir_graph *irg, ir_node *node);

/* not implemented yet
struct obstack *get_obst_of_irgraph (ir_graph *irg);
void set_obst_of_irgraph (ir_graph *irg, struct obstack *obst);
*/

ir_node *get_irg_current_block (ir_graph *irg);
void set_irg_current_block (ir_graph *irg, ir_node *node);

entity *get_irg_ent (ir_graph *irg);
void set_irg_ent (ir_graph *irg, entity *ent);

int get_irg_params (ir_graph *irg);
void set_irg_params (ir_graph *irg, int params);

unsigned long get_irg_visited (ir_graph *irg);
void set_irg_visited(ir_graph *irg, unsigned long i);

/* increments visited by one */
void inc_irg_visited(ir_graph *irg);

unsigned long get_irg_block_visited (ir_graph *irg);
void set_irg_block_visited(ir_graph *irg, unsigned long i);

/* increments block_visited by one */
void inc_irg_block_visited(ir_graph *irg);

int      get_params_of_irgraph (ir_graph *irg);
void     set_params_of_irgraph (ir_graph *irg, int params);

# endif /* _IRGRAPH_H_ */
