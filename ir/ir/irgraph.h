/*
 * Project:     libFIRM
 * File name:   ir/ir/irgraph.c
 * Purpose:     Entry point to the representation of procedure code.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file irgraph.h
 *
 * ir graph construction.
 *
 * @author Martin Trapp, Christian Schaefer
 */


#include "irop.h"

# ifndef _IRGRAPH_H_
# define _IRGRAPH_H_
# include "tv.h"
# include "entity.h"

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
 *    op_pin_state_pinned    set to "op_pin_state_pinned" if no global cse was performed on the graph.
 *             set to "op_pin_state_floats" if global cse was performed (and during construction:
 *             did actually change something).  Code placement is necessary.
 */

/** Global variable holding the current ir graph.
 *
 *  This global variable is used by the ir construction
 *  interface in ircons and by the optimizations.
 */
extern ir_graph *current_ir_graph;

ir_graph *get_current_ir_graph(void);
void      set_current_ir_graph(ir_graph *graph);

/** This flag indicate the current view. The behaviour of some methods
 * (get_irn_*, set_irn_*) is influenced by this flag. */
int get_interprocedural_view(void);
void set_interprocedural_view(int state);

/** Create a new ir graph to build ir for a procedure.
 *
 *  ent is the entity representing this procedure, i.e., the type of the
 *  entity must be of a method type.  The constructor automatically sets the
 *  field irg of the entity as well as current_ir_graph to the new ir graph.
 *  n_loc is the number of local variables in this procedure including
 *  the procedure parameters.
 *  The constructor adds the new irgraph to the list in ir_prog.
 *  The state of the ir graph is:  phase_building, op_pin_state_pinned, outs_none.
 *
 *  @see new_pseudo_ir_graph() */
ir_graph *new_ir_graph (entity *ent, int n_loc);

/** Frees the passed irgraph.
 * Deallocates all nodes in this graph and the ir_graph structure.
 * Sets the field irgraph in the corresponding entity to NULL.
 * Does not remove the irgraph from the list in irprog (requires
 * inefficient search, call remove_irp_irg by hand).
 * Does not free types, entities or modes that are used only by this
 * graph, nor the entity standing for this graph.
 */
void free_ir_graph (ir_graph *irg);

/* --- access routines for all ir_graph attributes --- */

/**
 *   Checks whether a pointer points to a ir graph.
 *
 *   @param thing     an arbitrary pointer
 *
 *   @return
 *       true if the thing is a ir graph, else false
 */
int      is_ir_graph(const void *thing);

/* #define get_irg_entity get_irg_ent */
/* #define set_irg_entity set_irg_ent */
entity  *get_irg_entity (const ir_graph *irg);
void     set_irg_entity (ir_graph *irg, entity *ent);

type    *get_irg_frame_type (const ir_graph *irg);
void     set_irg_frame_type (ir_graph *irg, type *ftp);
/* To test for a frame type. O(#irgs) if ftp is class type.  */
int      is_frame_type (const type *ftp);

ir_node *get_irg_start_block (const ir_graph *irg);
void     set_irg_start_block (ir_graph *irg, ir_node *node);

ir_node *get_irg_start (const ir_graph *irg);
void     set_irg_start (ir_graph *irg, ir_node *node);

ir_node *get_irg_end_block (const ir_graph *irg);
void     set_irg_end_block (ir_graph *irg, ir_node *node);

ir_node *get_irg_end (const ir_graph *irg);
void     set_irg_end (ir_graph *irg, ir_node *node);

/* The fields end_reg and end_except contain the end nodes of the
   interprocedural view.  If the view is not constructed they contain
   the nomal end node. */
ir_node *get_irg_end_reg (const ir_graph *irg);
void     set_irg_end_reg (ir_graph *irg, ir_node *node);

ir_node *get_irg_end_except (const ir_graph *irg);
void     set_irg_end_except (ir_graph *irg, ir_node *node);


/* @@@ oblivious, no more supported. */
ir_node *get_irg_cstore (const ir_graph *irg);
void     set_irg_cstore (ir_graph *irg, ir_node *node);
/* end oblivious */

/** Returns the node that represents the frame pointer. */
ir_node *get_irg_frame (const ir_graph *irg);
/** Sets the node that represents the frame pointer. */
void     set_irg_frame (ir_graph *irg, ir_node *node);

/** Returns the node that represents the global pointer. */
ir_node *get_irg_globals (const ir_graph *irg);
/** Sets the node that represents the global pointer. */
void     set_irg_globals (ir_graph *irg, ir_node *node);

/** Returns the node that represents the initial memory. */
ir_node *get_irg_initial_mem (const ir_graph *irg);
/** Sets the node that represents the initial memory. */
void     set_irg_initial_mem (ir_graph *irg, ir_node *node);

/** Returns the node that represents the argument pointer. */
ir_node *get_irg_args (const ir_graph *irg);
/** Sets the node that represents the argument pointer. */
void     set_irg_args (ir_graph *irg, ir_node *node);

/** Returns the current block of a graph. */
ir_node *get_irg_current_block (const ir_graph *irg);
/** Sets the current block of a graph. */
void     set_irg_current_block (ir_graph *irg, ir_node *node);

/** Returns the Bad node.  Use new_Bad() instead!! */
ir_node *get_irg_bad (const ir_graph *irg);
void     set_irg_bad (ir_graph *irg, ir_node *node);

/** Returns the NoMem node.  Use new_NoMem() instead!! */
ir_node *get_irg_no_mem (const ir_graph *irg);
void     set_irg_no_mem (ir_graph *irg, ir_node *node);

/** Returns the number of value numbers of a graph. */
int      get_irg_n_locs (ir_graph *irg);

/** Returns the graph number. */
long     get_irg_graph_nr(ir_graph *irg);

/********************************************************************************/
/* States of an ir_graph.                                                       */
/********************************************************************************/

/*
   information associated with the graph.  Optimizations invalidate these
   states.  */

/** The states of an ir graph.
 *
 * state phase values: phase_building, phase_high, phase_low.
 *
 * The graph is in phase_building during construction of the irgraph.
 * The construction is finished by a call to finalize_cons().
 *
 * Finalize_cons() sets the state to phase_high.  All Firm nodes are
 * allowed.
 *
 * To get the irgraph into phase_low all Sel nodes must be removed and
 * replaced by explicit address computations.  SymConst size and
 * typetag nodes must be removed (@@@ really?).  Initialization of
 * memory allocated by Alloc must be explicit.  @@@ More conditions?
 *
 */
typedef enum {
  phase_building,
  phase_high,
  phase_low
} irg_phase_state;

irg_phase_state get_irg_phase_state (const ir_graph *irg);
void set_irg_phase_low(ir_graph *irg);

/** state: op_pin_state_pinned
   The graph is "op_pin_state_pinned" if all nodes are associated with a basic block.
   It is in state "op_pin_state_floats" if nodes are in arbitrary blocks.  In state
   "op_pin_state_floats" the block predecessor is set in all nodes, but this can be an
   invalid block, i.e., the block is not a dominator of all the uses of
   the node.
   The enum op_pin_state is defined in irop.h. */
op_pin_state get_irg_pinned (const ir_graph *irg);

/** state: outs_state
   Outs are the back edges or def-use edges.
   Values:  outs_none, outs_consistent, outs_inconsistent
   outs_none: outs are not computed, no memory is allocated.
   outs_consistent:  outs are computed and correct,
   outs_inconsistent: outs have been computed, memory is still allocated,
   but the graph has been changed since. */
typedef enum {
  outs_none,
  outs_consistent,
  outs_inconsistent
} irg_outs_state;
irg_outs_state get_irg_outs_state(const ir_graph *irg);
void set_irg_outs_inconsistent(ir_graph *irg);

/** state: dom_state
   Signals the state of the dominator infomation.
   Values:  dom_none, dom_consistent, dom_inconsistent
   dom_none: doms are not computed, no memory is allocated.  The access routines
   may not be used.
   dom_consistent:  dominator information is computed and correct,
   dom_inconsistent: dominator information is computed, memory is still allocated,
   but the graph has been changed since. Using the access routines is possible,
   obtained information may be incorrect. */
typedef enum {
  dom_none,             /**< doms are not computed, no memory is allocated */
  dom_consistent,       /**< dominator information is computed and correct */
  dom_inconsistent      /**<  dominator information is computed but the graph has been changed since */
} irg_dom_state;
irg_dom_state get_irg_dom_state(const ir_graph *irg);
void set_irg_dom_inconsistent(ir_graph *irg);

/** state: loopinfo_state
   Loop information describes the loops within the control and
   data flow of the procedure.  */
typedef enum {
  loopinfo_none             = 0,       /**< No loop information is constructed. Default. */
  loopinfo_constructed      = 1,       /**< Some kind of loop information is constructed. */
  loopinfo_valid            = 2,       /**< Loop information is valid. */
  loopinfo_cf               = 4,       /**< Loop information constructed for control flow only. */
  loopinfo_inter            = 8,       /**< Loop information for interprocedural view. */

  loopinfo_for_firmjni      = 16,      /**< A hack for firmjni:  all enums must differ as they
					  are used in a switch. */

  /** IntRAprocedural loop information constructed and valid. */
  loopinfo_consistent         = loopinfo_constructed | loopinfo_valid,
  /** IntRAprocedural loop information constructed and invalid. */
  loopinfo_inconsistent       = loopinfo_constructed | loopinfo_for_firmjni,

  /** IntERprocedural loop information constructed and valid. */
  loopinfo_ip_consistent      = loopinfo_constructed | loopinfo_inter | loopinfo_valid,
  /** IntERprocedural loop information constructed and invalid. */
  loopinfo_ip_inconsistent    = loopinfo_constructed | loopinfo_inter,

  /** IntRAprocedural control loop information constructed and valid. */
  loopinfo_cf_consistent      = loopinfo_constructed | loopinfo_cf | loopinfo_valid,
  /** IntRAprocedural control loop information constructed and invalid. */
  loopinfo_cf_inconsistent    = loopinfo_constructed | loopinfo_cf,

  /** IntERprocedural control loop information constructed and valid. */
  loopinfo_cf_ip_consistent   = loopinfo_constructed | loopinfo_cf | loopinfo_inter | loopinfo_valid,
  /** IntERprocedural control loop information constructed and invalid. */
  loopinfo_cf_ip_inconsistent = loopinfo_constructed | loopinfo_cf | loopinfo_inter
} irg_loopinfo_state;

irg_loopinfo_state get_irg_loopinfo_state(const ir_graph *irg);
void set_irg_loopinfo_state(ir_graph *irg, irg_loopinfo_state s);
/** Sets the loopinformation state to the appropriate inconsistent state.
   If state is 'none' does not change. */
void set_irg_loopinfo_inconsistent(ir_graph *irg);

/** state: callee_information_state
 *  Call nodes contain a list of possible callees.  This list must be
 *  computed by an analysis.
 *
 *  It's strange that this state is administered on irg basis, as the
 *  information must be computed for the whole program, or not?
 */
typedef enum {
  irg_callee_info_none,
  irg_callee_info_consistent,
  irg_callee_info_inconsistent
} irg_callee_info_state;
irg_callee_info_state get_irg_callee_info_state(const ir_graph *irg);
void                  set_irg_callee_info_state(ir_graph *irg, irg_callee_info_state s);

/** property:
 *  Tells how to handle an ir graph in inlineing.
 */
typedef enum {
  irg_inline_any,         /**< No restriction on inlineing. Default. */
  irg_inline_forbidden,   /**< The graph may not be inlined. */
  irg_inline_recomended,  /**< The graph should be inlined. */
  irg_inline_forced       /**< The graph must be inlined. */
} irg_inline_property;

/** Returns the inline property of a graph. */
irg_inline_property get_irg_inline_property(const ir_graph *irg);
/** Sets the inline property of a graph. */
void set_irg_inline_property(ir_graph *irg, irg_inline_property s);

/** A void * field to link arbritary information to the node. */
void  set_irg_link (ir_graph *irg, void *thing);
void *get_irg_link (const ir_graph *irg);

/* increments visited by one */
void          inc_irg_visited (ir_graph *irg);
unsigned long get_irg_visited (const ir_graph *irg);
void          set_irg_visited (ir_graph *irg, unsigned long i);
unsigned long get_max_irg_visited (void);
void          set_max_irg_visited (int val);
unsigned long inc_max_irg_visited (void);

/* increments block_visited by one */
void          inc_irg_block_visited (ir_graph *irg);
unsigned long get_irg_block_visited (const ir_graph *irg);
void          set_irg_block_visited (ir_graph *irg, unsigned long i);

/** put the proj's into the same block as its predecessors */
void normalize_proj_nodes(ir_graph *irg);

# endif /* _IRGRAPH_H_ */
