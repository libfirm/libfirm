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

#include "irnode.h"
#include "bechordal.h"


typedef int(*cost_fct_t)(ir_node*, ir_node*, int);

typedef struct _copy_opt_t copy_opt_t;

/**
 * Has to be called during the firm init phase
 */
void be_copy_opt_init(void);

/**
 * Generate the problem. Collect all information and optimizable nodes.
 */
copy_opt_t *new_copy_opt(be_chordal_env_t *chordal_env, cost_fct_t get_costs);

/**
 * Free the space used...
 */
void free_copy_opt(copy_opt_t *co);

/**
 * Computes the costs of a copy according to loop depth
 * @param pos:	the argument position of arg in the root arguments
 * @return Must be >= 0 in all cases.
 */
int co_get_costs_loop_depth(ir_node *root, ir_node* arg, int pos);

/**
 * All costs equal 1. Using this will reduce the number of copies.
 * @return Must be >= 0 in all cases.
 */
int co_get_costs_all_one(ir_node *root, ir_node* arg, int pos);

/**
 * Solves the problem using a heuristic approach
 */
int co_solve_heuristic(copy_opt_t *co);

/**
 * Solves the problem using mixed integer programming
 * @returns 1 iff solution state was optimal
 */
int co_solve_ilp1(copy_opt_t *co, double time_limit);

/**
 * Solves the problem using mixed integer programming
 * @returns 1 iff solution state was optimal
 */
int co_solve_ilp2(copy_opt_t *co, double time_limit);

/**
 * Compares different solutions of the same problem
 */
void co_compare_solvers(be_chordal_env_t *chordal_env);

#endif
