/**
 * Author:      Daniel Grund
 * Date:		11.04.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 * Main file for the optimization reducing the copies needed for:
 * - Phi coalescing
 * - Register-constrained nodes
 * - Two-address code instructions
 */

#ifndef _BECOPYOPT_H
#define _BECOPYOPT_H

#include "firm_types.h"
#include "bechordal.h"

/**
 * Has to be called during the firm init phase
 */
void be_copy_opt_init(void);

typedef int(*cost_fct_t)(ir_node*, ir_node*, int);

typedef struct _copy_opt_t copy_opt_t;

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
 * Checks if the irn is a non-interfering argument of a node which 'is_optimizable'
 */
int co_is_optimizable_arg(const copy_opt_t *co, ir_node *irn);

/**
 * Computes the costs of a copy according to loop depth
 * @param pos:	the argument position of arg in the root arguments
 * @return Must be >= 0 in all cases.
 */
int co_get_costs_loop_depth(ir_node *root, ir_node* arg, int pos);

/**
 * All costs equal 1. Using this will reduce the _number_ of copies.
 * @return Must be >= 0 in all cases.
 */
int co_get_costs_all_one(ir_node *root, ir_node* arg, int pos);




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
 * Solves the problem using mixed integer programming
 * @returns 1 iff solution state was optimal
 * NYI
 */
int co_solve_ilp1(copy_opt_t *co, double time_limit);

/**
 * Solves the problem using mixed integer programming
 * @returns 1 iff solution state was optimal
 * Uses the OU and the GRAPH data structure
 */
int co_solve_ilp2(copy_opt_t *co, double time_limit);

/**
 * Checks if a node is optimizable, viz. has somthing to do with coalescing.
 * Uses the GRAPH data structure
 */
int co_gs_is_optimizable(copy_opt_t *co, ir_node *irn);

#endif /* _BECOPYOPT_H */
