/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
** irgraph.h: ir graph construction
*/

# ifndef _IRGRAPH_H_
# define _IRGRAPH_H_
# include "tv.h"

/* to resolve recursion between irnode.h and irgraph.h */
#ifndef _IR_NODE_TYPEDEF_
#define _IR_NODE_TYPEDEF_
typedef struct ir_node ir_node;
#endif

/* to resolve recursion between entity.h and irgraph.h */
#ifndef _IR_GRAPH_TYPEDEF_
#define _IR_GRAPH_TYPEDEF_
typedef struct ir_graph ir_graph;
#endif

/* Global variable holding the current_ir_graph.  This global variable
   is used by the ir construction interface in ircons and by the
   optimizations. */
extern ir_graph *current_ir_graph;

/* create a new ir graph.  Automatically sets the field irg of
   entity to the new ir graph. */
ir_graph *new_ir_graph (entity *ent, int params);


/* access routines for all ir_graph attributes */
ir_node *get_irg_start_block (ir_graph *irg);
void     set_irg_start_block (ir_graph *irg, ir_node *node);

ir_node *get_irg_start (ir_graph *irg);
void     set_irg_start (ir_graph *irg, ir_node *node);

ir_node *get_irg_end_block (ir_graph *irg);
void     set_irg_end_block (ir_graph *irg, ir_node *node);

ir_node *get_irg_end (ir_graph *irg);
void     set_irg_end (ir_graph *irg, ir_node *node);

ir_node *get_irg_cstore (ir_graph *irg);
void     set_irg_cstore (ir_graph *irg, ir_node *node);

ir_node *get_irg_frame (ir_graph *irg);
void     set_irg_frame (ir_graph *irg, ir_node *node);

ir_node *get_irg_globals (ir_graph *irg);
void     set_irg_globals (ir_graph *irg, ir_node *node);

ir_node *get_irg_args (ir_graph *irg);
void     set_irg_args (ir_graph *irg, ir_node *node);

/* Use new_Bad() instead!! */
ir_node *get_irg_bad (ir_graph *irg);
void     set_irg_bad (ir_graph *irg, ir_node *node);

ir_node *get_irg_current_block (ir_graph *irg);
void     set_irg_current_block (ir_graph *irg, ir_node *node);

entity  *get_irg_ent (ir_graph *irg);
void     set_irg_ent (ir_graph *irg, entity *ent);

int      get_irg_params (ir_graph *irg);
void     set_irg_params (ir_graph *irg, int params);

/* increments visited by one */
void     inc_irg_visited(ir_graph *irg);
unsigned long get_irg_visited (ir_graph *irg);
void     set_irg_visited(ir_graph *irg, unsigned long i);

/* increments block_visited by one */
void     inc_irg_block_visited(ir_graph *irg);
unsigned long get_irg_block_visited (ir_graph *irg);
void     set_irg_block_visited(ir_graph *irg, unsigned long i);

# endif /* _IRGRAPH_H_ */
