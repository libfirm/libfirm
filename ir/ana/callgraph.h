/*
 * Project:     libFIRM
 * File name:   ir/ana/callgraph.h
 * Purpose:     Representation and computation of the callgraph.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     21.7.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef _CALLGRAPH_H_
#define _CALLGRAPH_H_

/**
 * @file callgraph.h
 *
 *  This file contains the representation of the callgraph.
 *  The nodes of the call graph are ir_graphs.  The edges between
 *  ghe nodes are calling relations.  I.e., if method a calls method
 *  b at some point, there is an edge between a and b.
 *
 *  Further this file contains an algorithm to construct the call
 *  graph.  The construction of the callgraph uses the callee
 *  information in Call nodes to determine which methods are called.
 *
 *  Finally this file contains an algorithm that computes backedges
 *  in the callgraph, i.e., the algorithm finds possibly recursive calls.
 *  The algorithm computes an upper bound of all recursive calls.
 *
 */

#include "irgraph.h"

/** Flag to indicate state of callgraph. */
typedef enum {
  irp_callgraph_none,
  irp_callgraph_consistent,   /* calltree is inconsistent */
  irp_callgraph_inconsistent,
  irp_callgraph_and_calltree_consistent
} irp_callgraph_state;
irp_callgraph_state get_irp_callgraph_state(void);
void                set_irp_callgraph_state(irp_callgraph_state s);

/** The functions that call irg. */
int       get_irg_n_callers(ir_graph *irg);
ir_graph *get_irg_caller(ir_graph *irg, int pos);

int       is_irg_caller_backedge(ir_graph *irg, int pos);
int       has_irg_caller_backedge(ir_graph *irg);

/** maximal loop depth of call nodes that call along this edge. */
int       get_irg_caller_loop_depth(ir_graph *irg, int pos);

/** The functions called by irg. */
int       get_irg_n_callees(ir_graph *irg);
ir_graph *get_irg_callee(ir_graph *irg, int pos);

int       is_irg_callee_backedge(ir_graph *irg, int pos);
int       has_irg_callee_backedge(ir_graph *irg);

/** maximal loop depth of call nodes that call along this edge. */
int       get_irg_callee_loop_depth(ir_graph *irg, int pos);

/** Maximal loop depth of all paths from an external visible method to
    this irg. */
int       get_irg_loop_depth(ir_graph *irg);
/** Maximal recursion depth of all paths from an external visible method to
    this irg. */
int       get_irg_recursion_depth(ir_graph *irg);


/** Construct and destruct the callgraph. */
void compute_callgraph(void);
void free_callgraph(void);


/** A function type for fuctions passed to the callgraph walker. */
typedef void callgraph_walk_func(ir_graph *g, void *env);

void callgraph_walk(callgraph_walk_func *pre, callgraph_walk_func *post, void *env);

/** Compute the backedges that represent recursions. */
void find_callgraph_recursions(void);


/** Computes the loop nesting information.
 *
 * Computes callee info and the callgraph if
 * this information is not available.
 */
void analyse_loop_nesting_depth(void);


#endif /* _CALLGRAPH_H_ */
