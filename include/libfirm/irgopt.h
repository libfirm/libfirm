/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Optimizations for a whole ir graph, i.e., a procedure.
 * @author  Christian Schaefer, Goetz Lindenmaier, Sebastian Felis
 */
#ifndef FIRM_IR_IRGOPT_H
#define FIRM_IR_IRGOPT_H

#include "firm_types.h"

#include "begin.h"

/**
 * @ingroup iroptimize
 * @defgroup irgopt  Graph Transformations
 * @{
 */

/** Applies local optimizations (see iropt.h) to all nodes reachable from node
 * @p n.
 *
 * @param n The node to be optimized.
 */
FIRM_API void local_optimize_node(ir_node *n);

/** Applies local optimizations to a single node.
 *
 * Node constructors call this function, so you should not need to call this
 * manually.
 */
FIRM_API ir_node *optimize_node(ir_node *n);

/** Applies local optimizations (see iropt.h) to all nodes in the graph.
 *
 * @param irg  The graph to be optimized.
 *
 * After applying local_optimize_graph() to a IR-graph, Bad nodes
 * only occur as predecessor of Block and Phi nodes.
 */
FIRM_API void local_optimize_graph(ir_graph *irg);

/** Applies local optimizations (see iropt.h) to all nodes in the graph.
 *
 * After applying optimize_graph_df() to a IR-graph, Bad nodes
 * only occur as predecessor of Block and Phi nodes.
 *
 * This version uses fixpoint iteration.
 *
 * @param irg  The graph to be optimized.
 */
FIRM_API void optimize_graph_df(ir_graph *irg);

/**
 * Perform local optimizations on nodes on const code irg
 */
FIRM_API void local_opts_const_code(void);

/**
 * Eliminates (obviously) unreachable code
 */
FIRM_API void remove_unreachable_code(ir_graph *irg);

/**
 * Removes all Bad nodes from a graph.
 *
 * @param irg  The graph to be optimized.
 */
FIRM_API void remove_bads(ir_graph *irg);

/**
 * Removes all Tuple nodes from a graph.
 *
 * @param irg  The graph to be optimized.
 */
FIRM_API void remove_tuples(ir_graph *irg);

/** Places an empty basic block on critical control flow edges thereby
 * removing them.
 *
 * A critical control flow edge is an edge from a block with several
 * control exits to a block with several control entries (See Muchnic
 * p. 407). Exception edges are always ignored.
 *
 * @param irg  IR Graph
 */
FIRM_API void remove_critical_cf_edges(ir_graph *irg);

/** Places an empty basic block on critical control flow edges thereby
 * removing them.
 *
 * A critical control flow edge is an edge from a block with several
 * control exits to a block with several control entries (See Muchnic
 * p. 407).
 *
 * @param irg                     IR Graph
 * @param ignore_exception_edges  if non-zero, exception edges will be ignored
 */
FIRM_API void remove_critical_cf_edges_ex(ir_graph *irg,
                                          int ignore_exception_edges);

/** @} */

#include "end.h"

#endif
