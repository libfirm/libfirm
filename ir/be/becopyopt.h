/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 *
 * Main file for the optimization reducing the copies needed for:
 * - Phi coalescing
 * - Register-constrained nodes
 * - Two-address code instructions
 */
#ifndef FIRM_BE_BECOPYOPT_H
#define FIRM_BE_BECOPYOPT_H

#include <stdbool.h>

#include "firm_types.h"
#include "bechordal.h"

typedef int(*cost_fct_t)(const ir_node *node, int input);

typedef struct {
	int (*copyopt)(copy_opt_t *co); /**< function ptr to run copyopt */
	bool can_improve_existing;
} co_algo_info;

/**
 * Register a new copy optimization algorithm.
 *
 * @param name     the name of the copy optimazation algorithm,
 *                 used to select it
 * @param copyopt  a copy optimazation entry
 */
void be_register_copyopt(const char *name, co_algo_info *copyopt);

/** The driver for copy minimization. */
void co_driver(be_chordal_env_t *cenv);

/**
 * Generate the problem. Collect all information and optimizable nodes.
 */
copy_opt_t *new_copy_opt(be_chordal_env_t *chordal_env, cost_fct_t get_costs);

/**
 * Free the space used...
 */
void free_copy_opt(copy_opt_t *co);

/**
 * Statistics over a copy optimization module.
 */
typedef struct {
	unsigned long long aff_edges;            /**< number of affinity edges. */
	unsigned long long aff_nodes;            /**< number of nodes with incident affinity edges. */
	unsigned long long aff_int;              /**< number of affinity edges whose nodes also interfere. */
	unsigned long long inevit_costs;         /**< costs which cannot be evited (due to interfering affinities). */
	unsigned long long max_costs;            /**< all costs of the affinities. */
	unsigned long long costs;                /**< The costs of the current coloring. */
	unsigned long long unsatisfied_edges;    /**< The number of unequally colored affinity edges. */
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
 * Constructs another internal representation of the affinity edges
 */
void co_build_graph_structure(copy_opt_t *co);

/**
 * Frees the space used by the graph representation.
 * Does NOT free the whole copyopt structure
 */
void co_free_graph_structure(copy_opt_t *co);

/**
 * Checks if a node is optimizable, viz has something to do with coalescing.
 * Uses the GRAPH data structure
 */
int co_gs_is_optimizable(copy_opt_t *co, ir_node *irn);

#endif
