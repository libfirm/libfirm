/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief    Entry point to the representation of procedure code.
 * @author   Martin Trapp, Christian Schaefer, Goetz Lindenmaier
 * @version  $Id$
 */
#ifndef FIRM_IR_IRGRAPH_H
#define FIRM_IR_IRGRAPH_H

#include <stddef.h>

#include "firm_types.h"
#include "irop.h"
#include "iropt.h"
#include "irextbb.h"
#include "typerep.h"

/**
 * @page ir_graph   The struct ir_graph
 *
 * This struct contains all information about a procedure.
 * It's allocated directly to memory.
 *
 * The fields of ir_graph:
 *
 * - ent             The entity describing this procedure.
 *
 * The beginning and end of a graph:
 *
 * - start_block     This ir_node is the block that contains the unique
 *                   start node of the procedure.  With it it contains
 *                   the Proj's on starts results.
 *                   Further all Const nodes are placed in the start block.
 * - start           This ir_node is the unique start node of the procedure.
 *
 * - end_block       This ir_node is the block that contains the unique
 *                   end node of the procedure.  This block contains no
 *                   further nodes.
 * - end             This ir_node is the unique end node of the procedure.
 *
 * The following nodes are Projs from the Start node, held in ir_graph for
 * simple access:
 *
 * - frame           The ir_node producing the pointer to the stack frame of
 *                   the procedure as output.  This is the Proj node on the
 *                   third output of the start node.  This output of the start
 *                   node is tagged as pns_frame_base.  In FIRM most local
 *                   variables are modeled as data flow edges.  Static
 *                   allocated arrays can not be represented as data flow
 *                   edges. Therefore FIRM has to represent them in the stack
 *                   frame.
 *
 * - globals         This models a pointer to a space in the memory where
 *                   _all_ global things are held.  Select from this pointer
 *                   with a Sel node the pointer to a global variable /
 *                   procedure / compiler known function... .
 *
 * - tls             This models a pointer to a space in the memory where
 *                   thread local things are held.  Select from this pointer
 *                   with a Sel node the pointer to a thread local variable.
 *
 * - args            The ir_node that produces the arguments of the method as
 *                   it's result.  This is a Proj node on the fourth output of
 *                   the start node.  This output is tagged as pn_Start_T_args.
 *
 * - proj_args       The proj nodes of the args node.
 *
 * - bad             The Bad node is an auxiliary node. It is needed only once,
 *                   so there is this globally reachable node.
 *
 * - no_mem          The NoMem node is an auxiliary node. It is needed only once,
 *                   so there is this globally reachable node.
 *
 * Data structures that are private to a graph:
 *
 * - obst            An obstack that contains all nodes.
 *
 * - current_block   A pointer to the current block.  Any node created with
 *                   one of the node constructors (new_<opcode>) are assigned
 *                   to this block.  It can be set with set_cur_block(block).
 *                   Only needed for ir construction.
 *
 * - params/n_loc    An int giving the number of local variables in this
 *                  procedure.  This is needed for ir construction. Name will
 *                   be changed.
 *
 * - value_table     This hash table (pset) is used for global value numbering
 *                   for optimizing use in iropt.c.
 *
 * - Phi_in_stack;   a stack needed for automatic Phi construction, needed only
 *                   during ir construction.
 *
 * - visited         A int used as flag to traverse the ir_graph.
 *
 * - block_visited    A int used as a flag to traverse block nodes in the graph.
 */

/** Global variable holding the current ir graph.
 *
 *  This global variable is used by the ir construction
 *  interface in ircons and by the optimizations.
 *  Further it is set by all walker functions.
 */
extern ir_graph *current_ir_graph;

ir_graph *get_current_ir_graph(void);
void      set_current_ir_graph(ir_graph *graph);

#ifdef INTERPROCEDURAL_VIEW
/** This flag indicate the current view. The behavior of some methods
 * (get_irn_*, set_irn_*) is influenced by this flag. */
int get_interprocedural_view(void);
void set_interprocedural_view(int state);
#else
#define get_interprocedural_view()  0
#endif

/**
 * Create a new ir graph to build ir for a procedure.
 *
 * @param ent    A pointer to an entity representing the procedure,
 *               i.e., the type of the entity must be of a method type.
 *
 * @param n_loc  The number of local variables in this procedure including
 *               the procedure parameters.
 *
 * This constructor generates the basic infrastructure needed to
 * represent a procedure in FIRM.
 *
 * It allocates an ir_graph and sets the field irg of the entity ent
 * as well as current_ir_graph to point to this graph.
 * Further it allocates the following nodes needed for every
 * procedure:
 *
 * - The start block containing a start node and Proj nodes for it's
 *   seven results (X, M, P, P, P, T, P).
 * - The end block containing an end node. This block is not matured
 *   after executing new_ir_graph() as predecessors need to be added to it.
 *   (Maturing a block means fixing it's number of predecessors.)
 * - The current block, which is empty and also not matured.
 *
 * Further it enters the global store into the data structure of the start
 * block that contains all valid values in this block (set_store()).  This
 * data structure is used to build the Phi nodes and removed after
 * completion of the graph.  There is no path from end to start in the
 * graph after calling ir_graph.
 *
 * The op_pin_state of the graph is set to "op_pin_state_pinned"
 * if no global cse was performed on the graph.
 * It is set to "op_pin_state_floats" if global cse was performed
 * (and during construction: did actually change something).
 * Code placement is necessary.
 *
 * @see new_pseudo_ir_graph()
 */
ir_graph *new_ir_graph (ir_entity *ent, int n_loc);

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
ir_entity *get_irg_entity (const ir_graph *irg);
void       set_irg_entity (ir_graph *irg, ir_entity *ent);

ir_type *get_irg_frame_type (ir_graph *irg);
void     set_irg_frame_type (ir_graph *irg, ir_type *ftp);

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
   the normal end node. */
ir_node *get_irg_end_reg (const ir_graph *irg);
void     set_irg_end_reg (ir_graph *irg, ir_node *node);

ir_node *get_irg_end_except (const ir_graph *irg);
void     set_irg_end_except (ir_graph *irg, ir_node *node);

/** Returns the node that represents the frame pointer. */
ir_node *get_irg_frame (const ir_graph *irg);
/** Sets the node that represents the frame pointer. */
void     set_irg_frame (ir_graph *irg, ir_node *node);

/** Returns the node that represents the global pointer. */
ir_node *get_irg_globals (const ir_graph *irg);
/** Sets the node that represents the global pointer. */
void     set_irg_globals (ir_graph *irg, ir_node *node);

/** Returns the node that represents the tls pointer. */
ir_node *get_irg_tls (const ir_graph *irg);
/** Sets the node that represents the tls pointer. */
void     set_irg_tls (ir_graph *irg, ir_node *node);

/** Returns the node that represents the initial memory. */
ir_node *get_irg_initial_mem (const ir_graph *irg);
/** Sets the node that represents the initial memory. */
void     set_irg_initial_mem (ir_graph *irg, ir_node *node);

/** Returns the node that represents the argument pointer. */
ir_node *get_irg_args (const ir_graph *irg);
/** Sets the node that represents the argument pointer. */
void     set_irg_args (ir_graph *irg, ir_node *node);

/** Returns the node that represents the value parameter base pointer. */
ir_node *get_irg_value_param_base (const ir_graph *irg);
/** Sets the node that represents the value parameter base pointer. */
void     set_irg_value_param_base (ir_graph *irg, ir_node *node);

/** Returns an array of the nodes of the argument pointer. */
ir_node **get_irg_proj_args (const ir_graph *irg);
/** Sets the array of the nodes of the argument pointer. */
void     set_irg_proj_args (ir_graph *irg, ir_node **nodes);

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

#ifdef DEBUG_libfirm
/** Returns the graph number. */
long     get_irg_graph_nr(ir_graph *irg);
#endif

/**
 * Returns the graph number. This is a unique number for the graph and is
 * smaller than get_irp_last_idx()
 * Note: you cannot use this number for get_irp_irg
 */
int get_irg_idx(const ir_graph *irg);

/********************************************************************************/
/* States of an ir_graph.                                                       */
/********************************************************************************/

/*
   information associated with the graph.  Optimizations invalidate these
   states.  */

/** The states of an ir graph.
 *
 * state phase values: phase_building, phase_high, phase_low, phase_backend.
 *
 * The graph is in phase_building during construction of the irgraph.
 * The construction is finished by a call to finalize_cons().
 *
 * Finalize_cons() sets the state to phase_high.  All standard Firm nodes are
 * allowed.
 *
 * To get the irgraph into phase_low all Sel nodes must be removed and
 * replaced by explicit address computations.  SymConst size and
 * type tag nodes must be removed (@@@ really?).  Initialization of
 * memory allocated by Alloc must be explicit.  @@@ More conditions?
 *
 * phase_backend is set if architecture specific machine nodes are inserted
 * (and probably most standard Firm are removed).
 */
typedef enum {
	phase_building,  /**< The graph is still being constructed. */
	phase_high,      /**< The construction of the graph is finish, high level nodes may be present. */
	phase_low,       /**< High level nodes are removed. */
	phase_backend    /**< The graph is taken by the backend.  Machine specific nodes may be present. */
} irg_phase_state;

/** returns the phase_state of an IR graph. */
irg_phase_state get_irg_phase_state (const ir_graph *irg);

/** sets the phase state of an IR graph. */
void set_irg_phase_state(ir_graph *irg, irg_phase_state state);

#define set_irg_phase_low(irg)	set_irg_phase_state(irg, phase_low)

/** state: op_pin_state_pinned
   The graph is "op_pin_state_pinned" if all nodes are associated with a basic block.
   It is in state "op_pin_state_floats" if nodes are in arbitrary blocks.  In state
   "op_pin_state_floats" the block predecessor is set in all nodes, but this can be an
   invalid block, i.e., the block is not a dominator of all the uses of
   the node.
   The enum op_pin_state is defined in irop.h. */
op_pin_state get_irg_pinned (const ir_graph *irg);

/** state: outs_state
 *  Outs are the back edges or def-use edges of ir nodes.
 *  Values:  outs_none, outs_consistent, outs_inconsistent */
typedef enum {
	outs_none,         /**< Outs are not computed, no memory is allocated. */
	outs_consistent,   /**< Outs are computed and correct. */
	outs_inconsistent  /**< Outs have been computed, memory is still allocated,
	                        but the graph has been changed since. */
} irg_outs_state;
irg_outs_state get_irg_outs_state(const ir_graph *irg);
void           set_irg_outs_inconsistent(ir_graph *irg);

/** state:  extended basic block state. */
typedef enum {
	extblk_none    = 0,  /**< No extended basic block information is constructed. Default. */
	extblk_valid   = 1,  /**< Extended basic block information is valid. */
	extblk_invalid = 2   /**< Extended basic block information is constructed but invalid. */
} irg_extblk_state;
irg_extblk_state get_irg_extblk_state(const ir_graph *irg);
void             set_irg_extblk_inconsistent(ir_graph *irg);

/** state: dom_state
 * Signals the state of the dominator / post dominator information.
 */
typedef enum {
	dom_none,             /**< dominator are not computed, no memory is allocated */
	dom_consistent,       /**< dominator information is computed and correct */
	dom_inconsistent      /**< dominator information is computed but the graph has been changed since */
} irg_dom_state;

/** returns the dominator state of an IR graph. */
irg_dom_state get_irg_dom_state(const ir_graph *irg);

/** returns the post dominator state of an IR graph. */
irg_dom_state get_irg_postdom_state(const ir_graph *irg);

/** sets the dominator and post dominator state of an IR graph to inconsistent. */
void set_irg_doms_inconsistent(ir_graph *irg);

/** state: loopinfo_state
 *  Loop information describes the loops within the control and
 *  data flow of the procedure.
 */
typedef enum {
	loopinfo_none             = 0,       /**< No loop information is constructed. Default. */
	loopinfo_constructed      = 1,       /**< Some kind of loop information is constructed. */
	loopinfo_valid            = 2,       /**< Loop information is valid. */
	loopinfo_cf               = 4,       /**< Loop information constructed for control flow only. */
	loopinfo_inter            = 8,       /**< Loop information for interprocedural view. */

	loopinfo_for_firmjni      = 16,      /**< A hack for firmjni:  all enums must differ as they
	                                          are used in a switch. */

	/** IntRAprocedural loop information constructed and valid. */
	loopinfo_consistent         = loopinfo_constructed | loopinfo_for_firmjni | loopinfo_valid,
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

/** Return the current loop information state. */
irg_loopinfo_state get_irg_loopinfo_state(const ir_graph *irg);

/** Sets the current loop information state. */
void set_irg_loopinfo_state(ir_graph *irg, irg_loopinfo_state s);

/** Sets the loop information state to the appropriate inconsistent state.
 *  If state is 'none' does not change. */
void set_irg_loopinfo_inconsistent(ir_graph *irg);
/** Sets the loop information state to the appropriate inconsistent state.
 *  If state is 'none' does not change.
 *  Does this for all irgs. */
void set_irp_loopinfo_inconsistent(void);

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

/** returns the callee_info_state of an IR graph. */
irg_callee_info_state get_irg_callee_info_state(const ir_graph *irg);

/** sets the callee_info_state of an IR graph. */
void                  set_irg_callee_info_state(ir_graph *irg, irg_callee_info_state s);

/** property:
 *  Tells how to handle an ir graph in inlineing.
 */
typedef enum {
	irg_inline_any,            /**< No restriction on inlineing. Default. */
	irg_inline_forbidden,      /**< The graph may not be inlined. */
	irg_inline_recomended,     /**< The graph should be inlined. */
	irg_inline_forced,         /**< The graph must be inlined. */
	irg_inline_forced_no_body  /**< The graph must be inlined. No body is allowed
	                                to be emitted. */
} irg_inline_property;

/** Returns the inline property of a graph. */
irg_inline_property get_irg_inline_property(const ir_graph *irg);
/** Sets the inline property of a graph. */
void set_irg_inline_property(ir_graph *irg, irg_inline_property s);

/**
 * Returns the mask of the additional graph properties.
 * The properties are automatically inherited from the method type
 * if they were not set using set_irg_additional_properties() or
 * set_irg_additional_property().
 *
 * @return a bitset of mtp_additional_property values
 */
unsigned get_irg_additional_properties(const ir_graph *irg);

/** Sets the mask of the additional graph properties. */
void set_irg_additional_properties(ir_graph *irg, unsigned property_mask);

/** Sets one additional graph property. */
void set_irg_additional_property(ir_graph *irg, mtp_additional_property flag);

/** A void * field to link arbitrary information to the node. */
void  set_irg_link (ir_graph *irg, void *thing);
void *get_irg_link (const ir_graph *irg);

/** Increments visited flag by one.
 *  @see also: get_irn_visited() get_irg_block_visited(). */
void          inc_irg_visited (ir_graph *irg);
unsigned long get_irg_visited (const ir_graph *irg);
void          set_irg_visited (ir_graph *irg, unsigned long i);
/** An interprocedural flag valid for all irgs.
 *  @see also: get_irn_visited() get_irg_block_visited(). */
unsigned long get_max_irg_visited (void);
void          set_max_irg_visited (int val);
unsigned long inc_max_irg_visited (void);

/** Increments block_visited by one.
 *  @see also: get_irn_visited() get_irg_block_visited(). */
void          inc_irg_block_visited (ir_graph *irg);
unsigned long get_irg_block_visited (const ir_graph *irg);
void          set_irg_block_visited (ir_graph *irg, unsigned long i);

/**
 * Debug helpers: You can indicate whether you are currently using visited or
 * block_visited flags. If NDEBUG is not defined, then the compiler will abort
 * if 2 parties try to use the flags.
 */
#ifndef NDEBUG
void     set_using_block_visited(ir_graph *irg);
void     clear_using_block_visited(ir_graph *irg);
int      using_block_visited(const ir_graph *irg);
void     set_using_visited(ir_graph *irg);
void     clear_using_visited(ir_graph *irg);
int      using_visited(const ir_graph *irg);
void     set_using_irn_link(ir_graph *irg);
void     clear_using_irn_link(ir_graph *irg);
int      using_irn_link(const ir_graph *irg);
#else
static INLINE void set_using_block_visited(ir_graph *irg) { (void) irg; }
static INLINE void clear_using_block_visited(ir_graph *irg) { (void) irg; }
static INLINE int using_block_visited(const ir_graph *irg) { (void) irg; return 0; }
static INLINE void set_using_visited(ir_graph *irg) { (void) irg; }
static INLINE void clear_using_visited(ir_graph *irg) { (void) irg; }
static INLINE int using_visited(const ir_graph *irg) { (void) irg; return 0; }
static INLINE void set_using_irn_link(ir_graph *irg) { (void) irg; }
static INLINE void clear_using_irn_link(ir_graph *irg) { (void) irg; }
static INLINE int using_irn_link(const ir_graph *irg) { (void) irg; return 0; }
#endif

/** Normalization: Move Proj nodes into the same block as its predecessors */
void normalize_proj_nodes(ir_graph *irg);

/** set a description for local value n */
void set_irg_loc_description(ir_graph *irg, int n, void *description);

/** get the description for local value n */
void *get_irg_loc_description(ir_graph *irg, int n);

/** Returns a estimated node count of the irg. This count is updated
 * after every irg_walk_graph().
 */
unsigned get_irg_estimated_node_cnt(const ir_graph *irg);

/** Returns the last irn index for this graph. */
unsigned get_irg_last_idx(const ir_graph *irg);

/** Returns the floating point model of this graph. */
unsigned get_irg_fp_model(const ir_graph *irg);

/** Sets a floating point model for this graph. */
void set_irg_fp_model(ir_graph *irg, unsigned model);

/**
 * Access custom graph data.
 * The data must have been registered with
 * register_additional_graph_data() before.
 * @param graph The graph to get the data from.
 * @param type The type of the data you registered.
 * @param off The value returned by register_additional_graph_data().
 * @return A pointer of type @p type.
 */
#define get_irg_data(graph,type,off) \
	(assert(off > 0 && "Invalid graph data offset"), (type *) ((char *) (graph) - (off)))

/**
 * Get the pointer to the node some custom data belongs to.
 * @param data The pointer to the custom data.
 * @param off The number as returned by register_additional_graph_data().
 * @return A pointer to the ir node the custom data belongs to.
 */
#define get_irg_data_base(data,off) \
	(assert(off > 0 && "Invalid graph data offset"), (ir_graph *) ((char *) (data) + (off)))

/**
 * Request additional data to be allocated with an ir graph.
 * @param size The size of the additional data required.
 * @return A positive number, if the operation was successful, which
 * must be passed to the access macro get_irg_data(), 0 if the
 * registration failed.
 */
size_t register_additional_graph_data(size_t size);

#endif
