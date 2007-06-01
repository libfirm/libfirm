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
 * @brief       Representation and computation of the callgraph.
 * @author      Goetz Lindenmaier
 * @date        21.7.2004
 * @version     $Id$
 * @summary
 *  This file contains the representation of the callgraph.
 *  The nodes of the call graph are ir_graphs.  The edges between
 *  the nodes are calling relations.  I.e., if method a calls method
 *  b at some point, there is an edge between a and b.
 *
 *  Further this file contains an algorithm to construct the call
 *  graph.  The construction of the callgraph uses the callee
 *  information in Call nodes to determine which methods are called.
 *
 *  Finally this file contains an algorithm that computes backedges
 *  in the callgraph, i.e., the algorithm finds possibly recursive calls.
 *  The algorithm computes an upper bound of all recursive calls.
 */
#ifndef FIRM_ANA_CALLGRAPH_H
#define FIRM_ANA_CALLGRAPH_H

#include "firm_types.h"

/** Flag to indicate state of callgraph. */
typedef enum {
	irp_callgraph_none,                   /**< No callgraph allocated. */
	irp_callgraph_consistent,             /**< Callgraph constistent but calltree is inconsistent */
	irp_callgraph_inconsistent,           /**< Callgraph is allocated but inconsistent. */
	irp_callgraph_and_calltree_consistent /**< Both callgraph and calltree are consistent. */
} irp_callgraph_state;

/** Returns the callgraph state of the program representation. */
irp_callgraph_state get_irp_callgraph_state(void);

/** Sets the callgraph state of the program representation. */
void                set_irp_callgraph_state(irp_callgraph_state s);

/** Returns the number of procedures that call the given irg. */
int       get_irg_n_callers(ir_graph *irg);

/** Returns the caller at position pos. */
ir_graph *get_irg_caller(ir_graph *irg, int pos);

/** Returns non-zero if the caller at position pos is "a backedge", i.e. a recursion. */
int       is_irg_caller_backedge(ir_graph *irg, int pos);

/** Returns non-zero if the irg has a backedge caller. */
int       has_irg_caller_backedge(ir_graph *irg);

/** Returns the maximal loop depth of call nodes that call along this edge. */
int       get_irg_caller_loop_depth(ir_graph *irg, int pos);

/** Returns the number of procedures that are called by the given irg. */
int       get_irg_n_callees(ir_graph *irg);

/** Returns the callee at position pos. */
ir_graph *get_irg_callee(ir_graph *irg, int pos);

/** Returns non-zero if the callee at position pos is "a backedge", i.e. a recursion. */
int       is_irg_callee_backedge(ir_graph *irg, int pos);

/** Returns non-zero if the irg has a backedge callee. */
int       has_irg_callee_backedge(ir_graph *irg);

/** Returns the maximal loop depth of call nodes that call along this edge. */
int       get_irg_callee_loop_depth(ir_graph *irg, int pos);

/** Returns the maximal loop depth of all paths from an external visible method to
    this irg. */
int       get_irg_loop_depth(ir_graph *irg);

/** Returns the maximal recursion depth of all paths from an external visible method to
    this irg. */
int       get_irg_recursion_depth(ir_graph *irg);

/** Returns the method execution frequency of a graph. */
double get_irg_method_execution_frequency(ir_graph *irg);

/**
 * Construct the callgraph. Expects callee information, i.e.,
 * irg_callee_info_consistent must be set.  This can be computed with
 * cgana().
 */
void compute_callgraph(void);

/** Destruct the callgraph. */
void free_callgraph(void);


/** A function type for functions passed to the callgraph walker. */
typedef void callgraph_walk_func(ir_graph *g, void *env);

/**
 * Walks over the callgraph.
 *
 * Walks over the callgraph, starting at the irp main graph.
 * Visits ALL graphs in the irp, even if not reached by the main irg, but for
 * those the call order is not guaranteed.
 *
 * Executes pre before visiting the predecessor of a node, post after.
 * The void* env can be used to pass status information between the
 * pre and post functions.
 *
 * @param pre  - walker function, executed before the predecessor of a node are visited
 * @param post - walker function, executed after the predecessor of a node are visited
 * @param env  - environment, passed to pre and post
 */
void callgraph_walk(callgraph_walk_func *pre, callgraph_walk_func *post, void *env);

/**
 * Compute the backedges that represent recursions and a looptree.
 */
void find_callgraph_recursions(void);

/** Compute interprocedural performance estimates.
 *
 *  Computes
 *   - the loop depth of the method.
 *     The loop depth of an edge between two methods is the
 *     maximal loop depth of the Call nodes that call along this edge.
 *     The loop depth of the method is the loop depth of the most expensive
 *     path from main().
 *   - The recursion depth.  The maximal number of recursions passed
 *     on all paths reaching this method.
 *   - The execution frequency.  As loop depth, but the edge weight is the sum
 *     of the execution frequencies of all Calls along the edge.
 *
 * Expects the main irg is set, see set_irp_main_irg();
 **/
void compute_performance_estimates(void);

/** Computes the interprocedural loop nesting information.
 *
 * Computes two numbers for each irg:  the depth it is called in 'normal'
 * loops and the depth of recursions it is in.
 *
 * Computes callee info and the callgraph if
 * this information is not available.
 *
 * Expects the main irg is set, see set_irp_main_irg();
 */
void analyse_loop_nesting_depth(void);

/** The state of loop nesting depth. */
typedef enum {
	loop_nesting_depth_none,         /**< Loop nesting depths are not computed, no memory is
	                                      allocated, access fails. */
	loop_nesting_depth_consistent,   /**< Loop nesting depth information is computed and correct. */
	loop_nesting_depth_inconsistent  /**< Loop nesting depth is computed but the graphs have been
	                                      changed since. */
} loop_nesting_depth_state;

/** Returns the nesting depth state of the program representation. */
loop_nesting_depth_state get_irp_loop_nesting_depth_state(void);

/** Sets the nesting depth state of the program representation. */
void                     set_irp_loop_nesting_depth_state(loop_nesting_depth_state s);

/** Marks the nesting depth state of the program representation as inconsistent. */
void                     set_irp_loop_nesting_depth_state_inconsistent(void);


#endif /* _CALLGRAPH_H_ */
