/*
 * Project:     libFIRM
 * File name:   ir/ir/irgraph.c
 * Purpose:     Entry point to the representation of procedure code -- internal header.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file irgraph_t.h
 *
 * ir graph construction.
 *
 * @author Martin Trapp, Christian Schaefer
 */


#ifndef _IRGRAPH_T_H_
#define _IRGRAPH_T_H_

#include "irgraph.h"

#include "firm_common_t.h"
#include "irtypeinfo.h"
#include "irprog.h"

#include "irloop.h"

#include "obst.h"
#include "pset.h"
#include "type_t.h"

#define FRAME_TP_SUFFIX "frame_tp"

/** ir_graph holds all information for a procedure */
struct ir_graph {
  firm_kind         kind;            /**<  always set to k_ir_graph*/
  /* --  Basics of the representation -- */
  struct entity  *ent;               /**< The entity of this procedure, i.e.,
                    the type of the procedure and the
                    class it belongs to. */
  struct type    *frame_type;    /**< A class type representing the stack frame.
				    Can include "inner" methods. */
  struct ir_node *start_block;   /**< block the start node will belong to */
  struct ir_node *start;         /**< start node of this ir_graph */
  struct ir_node *end_block;     /**< block the end node will belong to */
  struct ir_node *end;           /**< end node of this ir_graph */
  struct ir_node *end_reg;       /**< end node of this ir_graph */
  struct ir_node *end_except;    /**< end node of this ir_graph */
  struct ir_node *cstore;        /**< constant store -- no more needed!! */
  struct ir_node *frame;         /**< method's frame */
  struct ir_node *globals;       /**< pointer to the data segment containing all
				    globals as well as global procedures. */
  struct ir_node *initial_mem;   /**< initial memory of this graph */
  struct ir_node *args;          /**< methods arguments */
  struct ir_node *bad;           /**< bad node of this ir_graph, the one and
				    only in this graph */
  /* GL removed: we need unknown with mode for analyses. */
  /*   struct ir_node *unknown;*/           /**< unknown node of this ir_graph */
  struct obstack *obst;          /**< obstack where all of the ir_nodes live */
  struct ir_node *current_block;     /**< block for newly gen_*()-erated
                    ir_nodes */

  /* -- Fields indicating different states of irgraph -- */
  irg_phase_state phase_state;       /**< compiler phase */
  op_pin_state op_pin_state_pinned;  /**< Flag for status of nodes */
  irg_outs_state outs_state;         /**< Out edges. */
  irg_dom_state dom_state;           /**< Dominator information */
  irg_typeinfo_state typeinfo_state;       /**< Validity of type information */
  irg_callee_info_state callee_info_state; /**< Validity of callee information */
  irg_inline_property inline_property;     /**< How to handle inlineing. */
  irg_loopinfo_state loopinfo_state;       /**< state of loop information */

  /* -- Fields for construction -- */
#if USE_EXPLICIT_PHI_IN_STACK
  struct Phi_in_stack *Phi_in_stack; /**< needed for automatic Phi construction */
#endif
  int n_loc;                         /**< number of local variable in this
					procedure including procedure parameters. */

  /* -- Fields for optimizations / analysis information -- */
  pset *value_table;                 /**< hash table for global value numbering (cse)
					for optimizing use in iropt.c */
  struct ir_node **outs;             /**< Space for the out arrays. */

#ifdef DEBUG_libfirm
  int             n_outs;            /* < Size wasted for outs */
#endif /* defined DEBUG_libfirm */
  struct ir_loop *loop;              /**< The outermost loop */
  void *link;                        /**< A void* field to link any information to
                    the node. */

  ir_graph **callers;                /**< For callgraph analyses. */
  int       *caller_isbe;            /**< For callgraph analyses: set if backedge. */
  ir_graph **callees;                /**< For callgraph analyses. */
  int       *callee_isbe;            /**< For callgraph analyses: set if backedge. */
  int        callgraph_loop_depth;
  int        callgraph_recursion_depth;
  ir_loop   *l;

  /* -- Fields for Walking the graph -- */
  unsigned long visited;             /**< this flag is an identifier for
					ir walk. it will be incremented
					every time someone walks through
					the graph */
  unsigned long block_visited;       /**< same as visited, for a complete block */
#ifdef DEBUG_libfirm
  int graph_nr;             /**< a unique graph number for each graph to make output
			       readable. */
#endif
};

/**
 * Initializes the graph construction module
 */
void init_irgraph(void);

/** Make a rudimentary ir graph for the constant code.
   Must look like a correct irg, spare everything else. */
ir_graph *new_const_code_irg(void);

/**
 * Set the op_pin_state_pinned state of a graph.
 *
 * @param irg     the IR graph
 * @param p       new pin state
 */
INLINE void
set_irg_pinned (ir_graph *irg, op_pin_state p);

/** Returns the obstack associated with the graph. */
struct obstack *get_irg_obstack(const ir_graph *irg);

/**
 * Returns true if the node n is allocated on the storage of graph irg.
 *
 * @param irg   the IR graph
 * @param n the IR node
 */
int node_is_in_irgs_storage(ir_graph *irg, ir_node *n);

/*-------------------------------------------------------------------*/
/* inline functions for graphs                                       */
/*-------------------------------------------------------------------*/

extern int __interprocedural_view;

static INLINE int
__get_interprocedural_view(void) {
  return __interprocedural_view;
}

static INLINE int
__is_ir_graph(const void *thing) {
  return (get_kind(thing) == k_ir_graph);
}

/** Returns the start block of a graph. */
static INLINE ir_node *
__get_irg_start_block(const ir_graph *irg) {
  return irg->start_block;
}

static INLINE void
__set_irg_start_block(ir_graph *irg, ir_node *node) {
  irg->start_block = node;
}

static INLINE ir_node *
__get_irg_start(const ir_graph *irg) {
  return irg->start;
}

static INLINE void
__set_irg_start(ir_graph *irg, ir_node *node) {
  irg->start = node;
}

static INLINE ir_node *
__get_irg_end_block(const ir_graph *irg) {
  return irg->end_block;
}

static INLINE void
__set_irg_end_block(ir_graph *irg, ir_node *node) {
  irg->end_block = node;
}

static INLINE ir_node *
__get_irg_end(const ir_graph *irg) {
  return irg->end;
}

static INLINE void
__set_irg_end(ir_graph *irg, ir_node *node) {
  irg->end = node;
}

static INLINE ir_node *
__get_irg_end_reg(const ir_graph *irg) {
  return irg->end_reg;
}

static INLINE ir_node *
__get_irg_end_except (const ir_graph *irg) {
  return irg->end_except;
}

static INLINE ir_node *
__get_irg_cstore(const ir_graph *irg) {
  return irg->cstore;
}

static INLINE void
__set_irg_cstore(ir_graph *irg, ir_node *node) {
  irg->cstore = node;
}

static INLINE ir_node *
__get_irg_frame(const ir_graph *irg) {
  return irg->frame;
}

static INLINE void
__set_irg_frame(ir_graph *irg, ir_node *node) {
  irg->frame = node;
}

static INLINE ir_node *
__get_irg_globals(const ir_graph *irg) {
  return irg->globals;
}

static INLINE void
__set_irg_globals(ir_graph *irg, ir_node *node) {
  irg->globals = node;
}

static INLINE ir_node *
__get_irg_initial_mem(const ir_graph *irg) {
  return irg->initial_mem;
}

static INLINE void
__set_irg_initial_mem(ir_graph *irg, ir_node *node) {
  irg->initial_mem = node;
}

static INLINE ir_node *
__get_irg_args(const ir_graph *irg) {
  return irg->args;
}

static INLINE void
__set_irg_args(ir_graph *irg, ir_node *node) {
  irg->args = node;
}

static INLINE ir_node *
__get_irg_bad(const ir_graph *irg) {
  return irg->bad;
}

static INLINE void
__set_irg_bad(ir_graph *irg, ir_node *node) {
  irg->bad = node;
}

static INLINE ir_node *
__get_irg_current_block(const ir_graph *irg) {
  return irg->current_block;
}

static INLINE void
__set_irg_current_block(ir_graph *irg, ir_node *node) {
  irg->current_block = node;
}

static INLINE entity *
__get_irg_entity(const ir_graph *irg) {
  assert(irg && irg->ent);
  return irg->ent;
}

static INLINE void
__set_irg_entity(ir_graph *irg, entity *ent) {
  irg->ent = ent;
}

static INLINE type *
__get_irg_frame_type(const ir_graph *irg) {
  assert(irg && irg->frame_type);
  return irg->frame_type;
}

static INLINE void
__set_irg_frame_type(ir_graph *irg, type *ftp) {
  assert(is_class_type(ftp));
  irg->frame_type = ftp;
}

static INLINE struct obstack *
__get_irg_obstack(const ir_graph *irg) {
  return irg->obst;
}


static INLINE irg_phase_state
__get_irg_phase_state(const ir_graph *irg) {
  return irg->phase_state;
}

static INLINE void
__set_irg_phase_low(ir_graph *irg) {
  irg->phase_state = phase_low;
}

static INLINE op_pin_state
__get_irg_pinned(const ir_graph *irg) {
  return irg->op_pin_state_pinned;
}

static INLINE irg_outs_state
__get_irg_outs_state(const ir_graph *irg) {
  return irg->outs_state;
}

static INLINE void
__set_irg_outs_inconsistent(ir_graph *irg) {
  irg->outs_state = outs_inconsistent;
}

static INLINE irg_dom_state
__get_irg_dom_state(const ir_graph *irg) {
  return irg->dom_state;
}

static INLINE void
__set_irg_dom_inconsistent(ir_graph *irg) {
  irg->dom_state = dom_inconsistent;
}

static INLINE irg_loopinfo_state
__get_irg_loopinfo_state(const ir_graph *irg) {
  return irg->loopinfo_state;
}

static INLINE void
__set_irg_loopinfo_state(ir_graph *irg, irg_loopinfo_state s) {
  irg->loopinfo_state = s;
}

static INLINE void
__set_irg_pinned(ir_graph *irg, op_pin_state p) {
  irg->op_pin_state_pinned = p;
}

static INLINE irg_callee_info_state
__get_irg_callee_info_state(const ir_graph *irg) {
  return irg->callee_info_state;
}

static INLINE void
__set_irg_callee_info_state(ir_graph *irg, irg_callee_info_state s) {
  irg_callee_info_state irp_state = get_irp_callee_info_state();

  irg->callee_info_state = s;

  /* I could compare ... but who knows? */
  if ((irp_state == irg_callee_info_consistent)  ||
      ((irp_state == irg_callee_info_inconsistent) && (s == irg_callee_info_none)))
      set_irp_callee_info_state(s);
}

static INLINE irg_inline_property
__get_irg_inline_property(const ir_graph *irg) {
  return irg->inline_property;
}

static INLINE void
__set_irg_inline_property(ir_graph *irg, irg_inline_property s) {
  irg->inline_property = s;
}

static INLINE void
__set_irg_link(ir_graph *irg, void *thing) {
  irg->link = thing;
}

static INLINE void *
__get_irg_link(const ir_graph *irg) {
  return irg->link;
}

static INLINE unsigned long
__get_irg_visited(const ir_graph *irg) {
  return irg->visited;
}

static INLINE unsigned long
__get_irg_block_visited(const ir_graph *irg) {
  return irg->block_visited;
}

static INLINE void
__set_irg_block_visited(ir_graph *irg, unsigned long visited) {
  irg->block_visited = visited;
}

static INLINE void
__inc_irg_block_visited(ir_graph *irg) {
  ++irg->block_visited;
}

#define get_interprocedural_view()         __get_interprocedural_view()
#define is_ir_graph(thing)                 __is_ir_graph(thing)
#define get_irg_start_block(irg)           __get_irg_start_block(irg)
#define set_irg_start_block(irg, node)     __set_irg_start_block(irg, node)
#define get_irg_start(irg)                 __get_irg_start(irg)
#define set_irg_start(irg, node)           __set_irg_start(irg, node)
#define get_irg_end_block(irg)             __get_irg_end_block(irg)
#define set_irg_end_block(irg, node)       __set_irg_end_block(irg, node)
#define get_irg_end(irg)                   __get_irg_end(irg)
#define set_irg_end(irg, node)             __set_irg_end(irg, node)
#define get_irg_end_reg(irg)               __get_irg_end_reg(irg)
#define get_irg_end_except(irg)            __get_irg_end_except(irg)
#define get_irg_cstore(irg)                __get_irg_cstore(irg)
#define set_irg_cstore(irg, node)          __set_irg_cstore(irg, node)
#define get_irg_frame(irg)                 __get_irg_frame(irg)
#define set_irg_frame(irg, node)           __set_irg_frame(irg, node)
#define get_irg_globals(irg)               __get_irg_globals(irg)
#define set_irg_globals(irg, node)         __set_irg_globals(irg, node)
#define get_irg_initial_mem(irg)           __get_irg_initial_mem(irg)
#define set_irg_initial_mem(irg, node)     __set_irg_initial_mem(irg, node)
#define get_irg_args(irg)                  __get_irg_args(irg)
#define set_irg_args(irg, node)            __set_irg_args(irg, node)
#define get_irg_bad(irg)                   __get_irg_bad(irg)
#define set_irg_bad(irg, node)             __set_irg_bad(irg, node)
#define get_irg_current_block(irg)         __get_irg_current_block(irg)
#define set_irg_current_block(irg, node)   __set_irg_current_block(irg, node)
#define get_irg_entity(irg)                __get_irg_entity(irg)
#define set_irg_entity(irg, ent)           __set_irg_entity(irg, ent)
#define get_irg_frame_type(irg)            __get_irg_frame_type(irg)
#define set_irg_frame_type(irg, ftp)       __set_irg_frame_type(irg, ftp)
#define get_irg_obstack(irg)               __get_irg_obstack(irg)
#define get_irg_phase_state(irg)           __get_irg_phase_state(irg)
#define set_irg_phase_low(irg)             __set_irg_phase_low(irg)
#define get_irg_pinned(irg)                __get_irg_pinned(irg)
#define get_irg_outs_state(irg)            __get_irg_outs_state(irg)
#define set_irg_outs_inconsistent(irg)     __set_irg_outs_inconsistent(irg)
#define get_irg_dom_state(irg)             __get_irg_dom_state(irg)
#define set_irg_dom_inconsistent(irg)      __set_irg_dom_inconsistent(irg)
#define get_irg_loopinfo_state(irg)        __get_irg_loopinfo_state(irg)
#define set_irg_loopinfo_state(irg, s)     __set_irg_loopinfo_state(irg, s)
#define set_irg_pinned(irg, p)             __set_irg_pinned(irg, p)
#define get_irg_callee_info_state(irg)     __get_irg_callee_info_state(irg)
#define set_irg_callee_info_state(irg, s)  __set_irg_callee_info_state(irg, s)
#define get_irg_inline_property(irg)       __get_irg_inline_property(irg)
#define set_irg_inline_property(irg, s)    __set_irg_inline_property(irg, s)
#define set_irg_link(irg, thing)           __set_irg_link(irg, thing)
#define get_irg_link(irg)                  __get_irg_link(irg)
#define get_irg_visited(irg)               __get_irg_visited(irg)
#define get_irg_block_visited(irg)         __get_irg_block_visited(irg)
#define set_irg_block_visited(irg, v)      __set_irg_block_visited(irg, v)
#define inc_irg_block_visited(irg)         __inc_irg_block_visited(irg)

# endif /* _IRGRAPH_T_H_ */
