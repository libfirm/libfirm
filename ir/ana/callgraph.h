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
*  This file contains defines the representation of the callgraph.
*  The nodes of the call graph are ir_graphs.  The edges between
*  The nodes are calling relation.  I.e., if method a calls method
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

/** The functions that call irg. */
int       get_irg_n_callers(ir_graph *irg);
ir_graph *get_irg_caller(ir_graph *irg, int pos);
/* int       is_irg_caller_backedge(ir_graph *irg, int pos);  not implemented */

/** The functions called by irg. */
int       get_irg_n_callees(ir_graph *irg);
ir_graph *get_irg_callee(ir_graph *irg, int pos);
int       is_irg_callee_backedge(ir_graph *irg, int pos);


/** Construct and destruct the callgraph. */
void compute_callgraph(void);
void free_callgraph(void);

/** Compute the backedges that represent recursions. */
void find_callgraph_recursions(void);


#endif /* _CALLGRAPH_H_ */
