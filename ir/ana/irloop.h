/*
 * Project:     libFIRM
 * File name:   ir/ana/irloop_t.h
 * Purpose:     Loop datastructure and access functions.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     7.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
* @file irloop.h
*
*  Computes backedges in the control and data flow.
*
*  @author Goetz Lindenmaier
*
*  Only Block and Phi/Filter nodes can have incoming backedges.
*  Constructs loops data structure: indicates loop nesting.
*/

# ifndef _IRLOOP_H_
# define _IRLOOP_H_

# include "irgraph.h"
# include "irnode.h"


/* @@@ Interprocedural backedges ... ???? */

/*
 * Backedge information.
 *
 * Predecessors of Block, Phi and interprocedural Filter nodes can
 * have  backedges.  If loop information is computed, this
 * information is computed, too.
 * The backedge information can only be used if the graph is not in
 * phase phase_building.
 */

/** Returns true if the predesessor pos is a backedge. */
bool is_backedge (ir_node *n, int pos);
/** Remarks that edge pos is a backedge. */
void set_backedge (ir_node *n, int pos);
/** Remarks that edge pos is not a backedge. */
void set_not_backedge (ir_node *n, int pos);
/** Returns true if n has backedges. */
bool has_backedges (ir_node *n);
/** Sets backedge information to zero. */
void clear_backedges (ir_node *n);

/**
 * The loops datastructure.
 *
 * The loops datastructure represents circles in the intermediate
 * representation.  It does not represent loops in the terms of a
 * source program.
 * Each ir_graph can contain one outermost loop datastructure.
 * loop is the entry point to the nested loops.
 * The loop datastructure contains a field indicating the depth of
 * the loop within the nesting.  Further it contains a list of the
 * loops with nesting depth -1.  Finally it contains a list of all
 * nodes in the loop.
 *
 * @todo We could add a field pointing from a node to the containing loop,
 * this would cost a lot of memory, though.
 */
typedef struct ir_loop ir_loop;

/* Loop elements are loop nodes and ir nodes */
typedef union {
    firm_kind *kind;    /**< is either k_ir_node or k_ir_loop */
    ir_node *node;      /**< Pointer to an ir_node element */
    ir_loop *son;       /**< Pointer to an ir_loop element */
} loop_element;

void     set_irg_loop(ir_graph *irg, ir_loop *l);
ir_loop *get_irg_loop(ir_graph *irg);

/** Returns the loop n is contained in.
   assumes current_ir_graph set properly. */
ir_loop *get_irn_loop(ir_node *n);

/** Returns outer loop, itself if outermost. */
ir_loop *get_loop_outer_loop (ir_loop *loop);
/** Returns nesting depth of this loop */
int      get_loop_depth (ir_loop *loop);

/* Sons are the inner loops contained in this loop. */
/** Returns the number of inner loops */
int      get_loop_n_sons (ir_loop *loop);
ir_loop *get_loop_son (ir_loop *loop, int pos);
/** Returns the number of nodes contained in loop.  */
int      get_loop_n_nodes (ir_loop *loop);
ir_node *get_loop_node (ir_loop *loop, int pos);

/** Returns the number of elements contained in loop.  */
int      get_loop_n_elements (ir_loop *loop);
/** Returns a loop element.  A loop element can be interpreted as a
    kind pointer, an ir_node* or an ir_loop*. */
loop_element get_loop_element (ir_loop *loop, int pos);

/*
 * Constructing and destructing the loop/backedge information.
 */

/** Constructs backedge information for irg in intraprocedural view. */
void construct_backedges(ir_graph *irg);

/** Constructs backedges for all irgs in interprocedural view.  All
   loops in the graph will be marked as such, not only realizeable
   loops and recursions in the program.  E.g., if the same funcion is
   called twice, there is a loop between the first funcion return and
   the second call.  */
void construct_ip_backedges(void);

#endif /* _IRLOOP_H_ */
