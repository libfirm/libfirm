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
 * @brief       Copy minimization driver.
 * @author      Daniel Grund
 * @date        11.04.2005
 * @version     $Id$
 *
 * Main file for the optimization reducing the copies needed for:
 * - Phi coalescing
 * - Register-constrained nodes
 * - Two-address code instructions
 */
#ifndef FIRM_BE_BECOPYOPT_H
#define FIRM_BE_BECOPYOPT_H

#include <stdio.h>

#include "firm_types.h"
#include "bechordal.h"

/**
 * Flags for dumping the IFG.
 */
enum {
	CO_IFG_DUMP_COLORS = 1, /**< Dump the graph colored. */
	CO_IFG_DUMP_LABELS = 2, /**< Dump node/edge labels. */
	CO_IFG_DUMP_SHAPE  = 4, /**< Give constrained nodes special shapes. */
	CO_IFG_DUMP_CONSTR = 8  /**< Dump the node constraints in the label. */
};

/**
 * Algorithms.
 */
enum {
	CO_ALGO_NONE,
	CO_ALGO_HEUR,
	CO_ALGO_HEUR2,
	CO_ALGO_HEUR3,
	CO_ALGO_HEUR4,
	CO_ALGO_ILP,
	CO_ALGO_LAST
};

/** The driver for copy minimization. */
void co_driver(be_chordal_env_t *cenv);

typedef struct _copy_opt_t copy_opt_t;

typedef int(*cost_fct_t)(const copy_opt_t *, ir_node *, ir_node *, int);

/** A coalescing algorithm. */
typedef int (co_algo_t)(copy_opt_t *);

/**
 * Generate the problem. Collect all information and optimizable nodes.
 */
copy_opt_t *new_copy_opt(be_chordal_env_t *chordal_env, cost_fct_t get_costs);

/**
 * Free the space used...
 */
void free_copy_opt(copy_opt_t *co);

/**
 * Checks if a node is optimizable, viz. has something to do with coalescing
 * @param arch The architecture environment
 * @param irn  The irn to check
 */
int co_is_optimizable_root(const copy_opt_t *co, ir_node *irn);

/**
 * Computes the costs of a copy according to loop depth
 * @param co   The copy opt object.
 * @param pos	the argument position of arg in the root arguments
 * @return Must be >= 0 in all cases.
 */
int co_get_costs_loop_depth(const copy_opt_t *co, ir_node *root, ir_node* arg, int pos);

/**
 * Computes the costs of a copy according to execution frequency
 * @param co   The copy opt object.
 * @param pos	the argument position of arg in the root arguments
 * @return Must be >= 0 in all cases.
 */
int co_get_costs_exec_freq(const copy_opt_t *co, ir_node *root, ir_node* arg, int pos);

/**
 * All costs equal 1. Using this will reduce the _number_ of copies.
 * @param co   The copy opt object.
 * @return Must be >= 0 in all cases.
 */
int co_get_costs_all_one(const copy_opt_t *co, ir_node *root, ir_node* arg, int pos);

/**
 * Statistics over a copy optimization module.
 */
typedef struct {
	ulong64 aff_edges;            /**< number of affinity edges. */
	ulong64 aff_nodes;            /**< number of nodes with incident affinity edges. */
	ulong64 aff_int;              /**< number of affinity edges whose nodes also interfere. */
	ulong64 inevit_costs;         /**< costs which cannot be evited (due to interfering affinities). */
	ulong64 max_costs;            /**< all costs of the affinities. */
	ulong64 costs;                /**< The costs of the current coloring. */
	ulong64 unsatisfied_edges;    /**< The number of unequally colored affinity edges. */
} co_complete_stats_t;

/**
 * Collect statistics of a copy optimization module.
 * @param co    The copy optimization environment.
 * @param stat  Where to put the stats.
 * @note  This requires the graph info to be computed.
 */
void co_complete_stats(const copy_opt_t *co, co_complete_stats_t *stat);


/**
 * Build internal optimization units structure
 */
void co_build_ou_structure(copy_opt_t *co);

/**
 * Frees the space used by the opt unit representation.
 * Does NOT free the whole copyopt structure
 */
void co_free_ou_structure(copy_opt_t *co);

/**
 * Solves the problem using a heuristic approach
 * Uses the OU data structure
 */
int co_solve_heuristic(copy_opt_t *co);

/**
 * Apply Park/Moon coalescing to the graph.
 * @param co The copy optimization data structure.
 */
void co_solve_park_moon(copy_opt_t *co);

/**
 * Solves the copy minimization problem using another heuristic approach.
 * Uses the OU and the GRAPH data structure.
 */
int co_solve_heuristic_new(copy_opt_t *co);

/**
 * Solves the copy minimization problem using another heuristic approach implemented in Java.
 * This function needs a JVM which is started to call the Java module.
 * Uses the GRAPH data structure.
 */
int co_solve_heuristic_java(copy_opt_t *co);

/**
 * This is the pure C implementation of co_solve_heuristic_java().
 */
int co_solve_heuristic_mst(copy_opt_t *co);

/**
 * Returns the maximal costs possible, i.e. the costs if all
 * pairs would be assigned different registers.
 * Uses the OU data structure
 */
int co_get_max_copy_costs(const copy_opt_t *co);

/**
 * Returns the inevitable costs, i.e. the costs of
 * all copy pairs which interfere.
 * Uses the OU data structure
 */
int co_get_inevit_copy_costs(const copy_opt_t *co);

/**
 * Returns the current costs the copies are causing.
 * The result includes inevitable costs and the costs
 * of the copies regarding the current register allocation
 * Uses the OU data structure
 */
int co_get_copy_costs(const copy_opt_t *co);

/**
 * Returns a lower bound for the costs of copies in this ou.
 * The result includes inevitable costs and the costs of a
 * minimal costs caused by the nodes of the ou.
 * Uses the OU data structure
 */
int co_get_lower_bound(const copy_opt_t *co);

/**
 * Dump the interference graph according to the Appel/George coalescing contest file format.
 * See: http://www.cs.princeton.edu/~appel/coalesce/format.html
 * @note Requires graph structure.
 * @param co The copy opt object.
 * @param f  A file to dump to.
 */
void co_dump_appel_graph(const copy_opt_t *co, FILE *f);

/**
 * Dumps the IFG of the program splitting after each instruction in the Appel format.
 * @param co The copy opt object.
 * @param f  The file to dump to.
 */
void co_dump_appel_graph_cliques(const copy_opt_t *co, FILE *f);
/**
 * Dump the interference graph with the affinity edges and the coloring.
 * @param co    The copy opt structure.
 * @param f     The file to dump to.
 * @param flags The dump flags (see enum above).
 */
void co_dump_ifg_dot(const copy_opt_t *co, FILE *f, unsigned flags);

/**
 * Constructs another internal representation of the affinity edges
 */
void co_build_graph_structure(copy_opt_t *co);

/**
 * Frees the space used by the graph representation.
 * Does NOT free the whole copyopt structure
 */
void co_free_graph_structure(copy_opt_t *co);

/**
 * Solves the problem using mixed integer programming
 * @returns 1 iff solution state was optimal
 * NYI
 */
int co_solve_ilp1(copy_opt_t *co, double time_limit);

/**
 * Solves the problem using mixed integer programming
 * @returns 1 iff solution state was optimal
 * Uses the OU and the GRAPH data structure
 * Dependency of the OU structure can be removed
 */
int co_solve_ilp2(copy_opt_t *co);

/**
 * Checks if a node is optimizable, viz has something to do with coalescing.
 * Uses the GRAPH data structure
 */
int co_gs_is_optimizable(copy_opt_t *co, ir_node *irn);

#endif /* FIRM_BE_BECOPYOPT_H */
