/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
* All rights reserved.
*/

/**
 * @file irgraph.h
 *
 * ir graph construction.
 *
 * @author Martin Trapp, Christian Schaefer
 */

/* $Id$ */

#include "irop.h"

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

/**
 *
 * NAME  Datastructure that holds central information about a procedure
 *
 *    ir_graph *new_ir_graph (entity *ent, int params);
 *    -------------------------------------------------
 *
 *    This constructor generates the basic infrastructure needed to
 *    represent a procedure in FIRM.
 *
 *    The parameters of new_ir_graph are:
 *
 *      *ent             A pointer to an entity representing the procedure.
 *
 *      params           An integer giving the number of local variables in the
 *                       procedure.
 *
 *    It allocates an ir_graph and sets current_ir_graph to point to this
 *    graph.  Further it allocates the following nodes needed for every
 *    procedure:
 *
 *    * The start block containing a start node and Proj nodes for it's
 *      five results (X, M, P, P, T).
 *    * The end block containing an end node. This block is not matured
 *      after executing new_ir_graph as predecessors need to be added to it.
 *      (Maturing a block means fixing it's number of predecessors.)
 *    * The current block, which is empty and also not matured.
 *
 *    Further it enters the global store into the datastructure of the start
 *    block that contanis all valid values in this block (set_store()).  This
 *    datastructure is used to build the Phi nodes and removed after
 *    completion of the graph.  There is no path from end to start in the
 *    graph after calling ir_graph.
 *   pinned    set to "pinned" if no global cse was performed on the graph.
 *             set to "floats" if global cse was performed (and during construction:
 *             did actually change something).  Code placement is necessary.
 */

/* Global variable holding the current_ir_graph.  This global variable
   is used by the ir construction interface in ircons and by the
   optimizations. */
extern ir_graph *current_ir_graph;
ir_graph *get_current_ir_graph(void);
void set_current_ir_graph(ir_graph *graph);

/** This flag indicate the current view. The behaviour of some methods
 * (get_irn_*, set_irn_*) is influenced by this flag. */
extern bool interprocedural_view;
bool get_interprocedural_view(void);
void set_interprocedural_view(bool state);

/** Create a new ir graph to built ir for a procedure.
   ent is the entity representing this procedure, i.e., the type of the
   entity must be of a method type.  The constructor automatically sets the
   field irg of the entity as well as current_ir_graph to the new ir graph.
   n_loc is the number of local variables in this procedure including
   the procedure parameters.
   The state of the ir graph is:  phase_building, pinned, no_outs. */
ir_graph *new_ir_graph (entity *ent, int n_loc);

/** Frees the passed irgraph.
   Deallocates all nodes in this graph and the ir_graph structure.
   Sets the field irgraph in the corresponding entity to NULL.
   Does not remove the irgraph from the list in irprog (requires
   inefficient search, call remove_irp_irg by hand).
   Does not free types, entities or modes that are used only by this
   graph, nor the entity standing for this graph. */
void free_ir_graph (ir_graph *irg);

/* --- access routines for all ir_graph attributes --- */
entity  *get_irg_ent (ir_graph *irg);
void     set_irg_ent (ir_graph *irg, entity *ent);

type    *get_irg_frame_type (ir_graph *irg);
void     set_irg_frame_type (ir_graph *irg, type *ftp);
/* To test for a frame type */
int      is_frame_type(type *ftp);

ir_node *get_irg_start_block (ir_graph *irg);
void     set_irg_start_block (ir_graph *irg, ir_node *node);

ir_node *get_irg_start (ir_graph *irg);
void     set_irg_start (ir_graph *irg, ir_node *node);

ir_node *get_irg_end_block (ir_graph *irg);
void     set_irg_end_block (ir_graph *irg, ir_node *node);

ir_node *get_irg_end (ir_graph *irg);
void     set_irg_end (ir_graph *irg, ir_node *node);

/* @@@ oblivious, no more supported. */
ir_node *get_irg_cstore (ir_graph *irg);
void     set_irg_cstore (ir_graph *irg, ir_node *node);
/* end oblivious */

ir_node *get_irg_frame (ir_graph *irg);
void     set_irg_frame (ir_graph *irg, ir_node *node);

ir_node *get_irg_globals (ir_graph *irg);
void     set_irg_globals (ir_graph *irg, ir_node *node);

ir_node *get_irg_args (ir_graph *irg);
void     set_irg_args (ir_graph *irg, ir_node *node);

ir_node *get_irg_current_block (ir_graph *irg);
void     set_irg_current_block (ir_graph *irg, ir_node *node);

/* Use new_Bad() instead!! */
ir_node *get_irg_bad (ir_graph *irg);
void     set_irg_bad (ir_graph *irg, ir_node *node);

/* Use new_Unknown() instead!! */
ir_node *get_irg_unknown (ir_graph *irg);
void     set_irg_unknown (ir_graph *irg, ir_node *node);

int      get_irg_n_locs (ir_graph *irg);

/********************************************************************************/
/* States of an ir_graph.                                                       */
/********************************************************************************/

/*
   information associated with the graph.  Optimizations invalidate these
   states.  */

/** state: phase values: phase_building, phase_high, phase_low.
   The irg is in phase_building during construction of the irgraph.  It is in
   phase_high after construction.  All nodes are allowed.  To get the irgraph
   into phase_low all Sel nodes must be removed and replaced by explicit
   address computations.  SymConst size and typetag nodes must be removed (@@@
   really?).  Initialization of memory allocated by Alloc must be explicit.
   @@@ More conditions? */
typedef enum {
  phase_building,
  phase_high,
  phase_low
} irg_phase_state;

irg_phase_state get_irg_phase_state (ir_graph *irg);
void set_irg_phase_low(ir_graph *irg);

/* state: pinned
   The graph is "pinned" if all nodes are associated with a basic block.
   It is in state "floats" if nodes are in arbitrary blocks.  In state
   "floats" the block predecessor is set in all nodes, but this can be an
   invalid block, i.e., the block is not a dominator of all the uses of
   the node.
   The enum op_pinned is defined in irop.h. */
op_pinned get_irg_pinned (ir_graph *irg);

/** state: outs_state
   Outs are the back edges or def-use edges.
   Values:  no_outs, outs_consistent, outs_inconsistent
   no_outs: outs are not computed, no memory is allocated.
   outs_consistent:  outs are computed and correct,
   outs_inconsistent: outs have been computed, memory is still allocated,
   but the graph has been changed since. */
typedef enum {
  no_outs,
  outs_consistent,
  outs_inconsistent
} irg_outs_state;
irg_outs_state get_irg_outs_state(ir_graph *irg);
void set_irg_outs_inconsistent(ir_graph *irg);

/** state: dom_state
   Signals the state of the dominator infomation.
   Values:  no_dom, dom_consistent, dom_inconsistent
   no_dom: doms are not computed, no memory is allocated.  The access routines
   may not be used.
   dom_consistent:  dominator information is computed and correct,
   dom_inconsistent: dominator information is computed, memory is still allocated,
   but the graph has been changed since. Using the access routines is possible,
   obtained information may be incorrect. */
typedef enum {
  no_dom,
  dom_consistent,
  dom_inconsistent
} irg_dom_state;
irg_dom_state get_irg_dom_state(ir_graph *irg);
void set_irg_dom_inconsistent(ir_graph *irg);

/* state: loopinfo_state
   Loop information describes the loops within the control and
   data flow of the procedure.
tpedef enum {   @@@ make unrecognizable for jni script!!!
  no_loopinfo,
  loopinfo_consistent,
  loopinfo_inconsistent
} irg_loopinfo_state;
irg_loopinfo_state get_irg_loopinfo_state(ir_graph *irg);
void set_irg_loopinfo_inconsistent(ir_graph *irg);
*/

/* A void * field to link arbritary information to the node. */
void set_irg_link (ir_graph *irg, void *thing);
void *get_irg_link (ir_graph *irg);

/* increments visited by one */
void     inc_irg_visited(ir_graph *irg);
unsigned long get_irg_visited (ir_graph *irg);
void     set_irg_visited(ir_graph *irg, unsigned long i);
unsigned long get_max_irg_visited(void);
void set_max_irg_visited(int val);
unsigned long inc_max_irg_visited(void);

/* increments block_visited by one */
void     inc_irg_block_visited(ir_graph *irg);
unsigned long get_irg_block_visited (ir_graph *irg);
void     set_irg_block_visited(ir_graph *irg, unsigned long i);

# endif /* _IRGRAPH_H_ */
