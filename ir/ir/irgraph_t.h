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


# ifndef _IRGRAPH_T_H_
# define _IRGRAPH_T_H_
# include "obst.h"
# include "pset.h"
# include "irgraph.h"
# include "firm_common_t.h"
# include "irtypeinfo.h"

#define FRAME_TP_SUFFIX "frame_tp"

/** ir_graph holds all information for a procedure */
struct ir_graph {
  firm_kind         kind;            /**<  always set to k_ir_graph*/
  /* --  Basics of the representation -- */
  struct entity  *ent;               /**< The entity of this procedure, i.e.,
					the type of the procedure and the
					class it belongs to. */
  struct type    *frame_type;        /**< A class type representing the stack frame.
					Can include "inner" methods. */
  struct ir_node *start_block;       /**< block the start node will belong to */
  struct ir_node *start;	     /**< start node of this ir_graph */
  struct ir_node *end_block;         /**< block the end node will belong to */
  struct ir_node *end;		     /**< end node of this ir_graph */
  struct ir_node *cstore;	     /**< constant store -- no more needed!! */
  struct ir_node *frame;             /**< method's frame */
  struct ir_node *globals;           /**< pointer to the data segment containing all
				        globals as well as global procedures. */
  struct ir_node *args;              /**< methods arguments */
  struct ir_node *bad;		     /**< bad node of this ir_graph, the one and
                                        only in this graph */
  /* GL removed: we need unknown with mode for analyses. */
  //  struct ir_node *unknown;           /**< unknown node of this ir_graph */
  struct obstack *obst;		     /**< obstack where all of the ir_nodes live */
  struct ir_node *current_block;     /**< block for newly gen_*()-erated
					ir_nodes */

  /* -- Fields indicating different states of irgraph -- */
  irg_phase_state phase_state;       /**< compiler phase */
  op_pinned pinned;                  /**< Flag for status of nodes */
  irg_outs_state outs_state;         /**< Out edges. */
  irg_dom_state dom_state;           /**< Dominator information */
  irg_typeinfo_state typeinfo_state; /**< Validity of type inforamtion */

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
  struct ir_loop *loop;              /**< The outermost loop */
  void *link;                        /**< A void* field to link any information to
					the node. */

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

void init_irgraph(void);

/** Make a rudimentary ir graph for the constant code.
   Must look like a correct irg, spare everything else. */
ir_graph *new_const_code_irg(void);

/**
 * Set the pinned state of a graph.
 *
 * @irg		the IR graph
 * @p		new pin state
 */
INLINE void
set_irg_pinned (ir_graph *irg, op_pinned p);

/** Returns the obstack associated with the graph. */
struct obstack *get_irg_obstack(ir_graph *irg);

/**
 * Returns true if the node n is allocated on the storage of graph irg.
 *
 * @param irg	the IR graph
 * @param n	the IR node
 */
int node_is_in_irgs_storage(ir_graph *irg, ir_node *n);

# endif /* _IRGRAPH_T_H_ */
