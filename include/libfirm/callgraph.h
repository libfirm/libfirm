/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Representation and computation of the callgraph.
 * @author      Goetz Lindenmaier
 * @date        21.7.2004
 * @brief       callgraph analysis
 */
#ifndef FIRM_ANA_CALLGRAPH_H
#define FIRM_ANA_CALLGRAPH_H

#include <stddef.h>
#include "firm_types.h"

#include "begin.h"

/**
 * @ingroup irana
 * @defgroup callgraph Callgraph
 *
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
 * @{
 */

/** Flag to indicate state of callgraph. */
typedef enum {
	irp_callgraph_none,                   /**< No callgraph allocated. */
	irp_callgraph_consistent,             /**< Callgraph constistent but calltree is inconsistent */
	irp_callgraph_inconsistent,           /**< Callgraph is allocated but inconsistent. */
	irp_callgraph_and_calltree_consistent /**< Both callgraph and calltree are consistent. */
} irp_callgraph_state;

/** Returns the callgraph state of the program representation. */
FIRM_API irp_callgraph_state get_irp_callgraph_state(void);

/** Sets the callgraph state of the program representation. */
FIRM_API void set_irp_callgraph_state(irp_callgraph_state s);

/** Returns the number of procedures that call the given irg. */
FIRM_API size_t get_irg_n_callers(const ir_graph *irg);

/** Returns the caller at position pos. */
FIRM_API ir_graph *get_irg_caller(const ir_graph *irg, size_t pos);

/** Returns non-zero if the caller at position pos is "a backedge", i.e. a recursion. */
FIRM_API int is_irg_caller_backedge(const ir_graph *irg, size_t pos);

/** Returns non-zero if the irg has a backedge caller. */
FIRM_API int has_irg_caller_backedge(const ir_graph *irg);

/** Returns the maximal loop depth of call nodes that call along this edge. */
FIRM_API size_t get_irg_caller_loop_depth(const ir_graph *irg, size_t pos);

/** Returns the number of procedures that are called by the given irg. */
FIRM_API size_t get_irg_n_callees(const ir_graph *irg);

/** Returns the callee at position pos. */
FIRM_API ir_graph *get_irg_callee(const ir_graph *irg, size_t pos);

/** Returns non-zero if the callee at position pos is "a backedge", i.e. a recursion. */
FIRM_API int is_irg_callee_backedge(const ir_graph *irg, size_t pos);

/** Returns non-zero if the irg has a backedge callee. */
FIRM_API int has_irg_callee_backedge(const ir_graph *irg);

/** Returns the maximal loop depth of call nodes that call along this edge. */
FIRM_API size_t get_irg_callee_loop_depth(const ir_graph *irg, size_t pos);

/** Returns the method execution frequency of a graph. */
FIRM_API double get_irg_method_execution_frequency(const ir_graph *irg);

/**
 * Construct the callgraph. Expects callee information, i.e.,
 * irg_callee_info_consistent must be set.  This can be computed with
 * cgana().
 */
FIRM_API void compute_callgraph(void);

/** Destruct the callgraph. */
FIRM_API void free_callgraph(void);


/** A function type for functions passed to the callgraph walker. */
typedef void callgraph_walk_func(ir_graph *g, void *env);

/**
 * Walks over the callgraph.
 *
 * Walks over the callgraph, starting at the irp main graph.
 * Visits ALL graphs in the irp, even if not reached by the main irg, but for
 * those the call order is not guaranteed.
 *
 * Executes pre before visiting the callees of a node, post after.
 * The void* env can be used to pass status information between the
 * pre and post functions.
 *
 * @param pre  - walker function, executed before the predecessor of a node are visited
 * @param post - walker function, executed after the predecessor of a node are visited
 * @param env  - environment, passed to pre and post
 */
FIRM_API void callgraph_walk(callgraph_walk_func *pre,
                             callgraph_walk_func *post, void *env);

/**
 * Compute the backedges that represent recursions and a looptree.
 */
FIRM_API void find_callgraph_recursions(void);

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
FIRM_API void analyse_loop_nesting_depth(void);

/** The state of loop nesting depth. */
typedef enum {
	loop_nesting_depth_none,         /**< Loop nesting depths are not computed, no memory is
	                                      allocated, access fails. */
	loop_nesting_depth_consistent,   /**< Loop nesting depth information is computed and correct. */
	loop_nesting_depth_inconsistent  /**< Loop nesting depth is computed but the graphs have been
	                                      changed since. */
} loop_nesting_depth_state;

/** Returns the nesting depth state of the program representation. */
FIRM_API loop_nesting_depth_state get_irp_loop_nesting_depth_state(void);

/** Sets the nesting depth state of the program representation. */
FIRM_API void set_irp_loop_nesting_depth_state(loop_nesting_depth_state s);

/** Marks the nesting depth state of the program representation as inconsistent. */
FIRM_API void set_irp_loop_nesting_depth_state_inconsistent(void);

/** @} */

#include "end.h"

#endif
