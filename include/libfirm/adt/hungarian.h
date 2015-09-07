/********************************************************************
 ********************************************************************
 **
 ** libhungarian by Cyrill Stachniss, 2004
 **
 ** Added to libFirm by Christian Wuerdig, 2006
 ** Added several options for not-perfect matchings.
 **
 ** Solving the Minimum Assignment Problem using the
 ** Hungarian Method.
 **
 ** ** This file may be freely copied and distributed! **
 **
 ** Parts of the used code was originally provided by the
 ** "Stanford GraphGase", but I made changes to this code.
 ** As asked by  the copyright node of the "Stanford GraphGase",
 ** I hereby proclaim that this file are *NOT* part of the
 ** "Stanford GraphGase" distribution!
 **
 ** This file is distributed in the hope that it will be useful,
 ** but WITHOUT ANY WARRANTY; without even the implied
 ** warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 ** PURPOSE.
 **
 ********************************************************************
 ********************************************************************/

/**
 * @file
 * @brief   Solving the Minimum Assignment Problem using the Hungarian Method.
 */
#ifndef FIRM_ADT_HUNGARIAN_H
#define FIRM_ADT_HUNGARIAN_H

#include "../begin.h"

/**
 * @ingroup algorithms
 * @defgroup hungarian Hungarian Algorithm
 * Solves bipartite matching problems (minimize/maximize cost function)
 * @{
 */

/** type of matching */
typedef enum match_type_t {
	HUNGARIAN_MATCH_NORMAL,   /**< ever nodes matches another node */
	HUNGARIAN_MATCH_PERFECT   /**< matchings of nodes having no edge getting
	                               removed */
} match_type_t;

/** mode of matching (the optimization goal) */
typedef enum hungarian_mode_t {
	HUNGARIAN_MODE_MINIMIZE_COST, /**< minimize cost */
	HUNGARIAN_MODE_MAXIMIZE_UTIL  /**< maximize cost */
} hungarian_mode_t;

/**
 * Internal representation of a bipartite matching problem
 */
typedef struct hungarian_problem_t hungarian_problem_t;

/**
 * This method initialize the hungarian_problem structure and init
 * the cost matrix (missing lines or columns are filled with 0).
 *
 * @param num_rows   Number of rows in the given matrix
 * @param num_cols   Number of cols in the given matrix
 * @param match_type The type of matching
 * @return The problem object.
 */
FIRM_API hungarian_problem_t *hungarian_new(unsigned num_rows,
                                            unsigned num_cols,
                                            match_type_t match_type);

/**
 * Adds an edge from left to right with some costs.
 */
FIRM_API void hungarian_add(hungarian_problem_t *p, unsigned left,
                            unsigned right, unsigned cost);

/**
 * Removes the edge from left to right.
 */
FIRM_API void hungarian_remove(hungarian_problem_t *p, unsigned left,
                               unsigned right);

/**
 * Prepares the cost matrix dependent on the given mode.
 *
 * @param p     The hungarian object
 * @param mode  specify whether to minimize or maximize the costs
 */
FIRM_API void hungarian_prepare_cost_matrix(hungarian_problem_t *p,
                                            hungarian_mode_t mode);

/**
 * Destroys the hungarian object.
 */
FIRM_API void hungarian_free(hungarian_problem_t *p);

/**
 * This method computes the optimal assignment.
 * @param p              The hungarian object
 * @param assignment     The final assignment
 * @param final_cost     The final costs
 * @param cost_threshold Matchings with costs >= this limit will be removed (if limit > 0)
 * @return 0 on success, negative number otherwise
 */
FIRM_API int hungarian_solve(hungarian_problem_t *p, unsigned *assignment,
                             unsigned *final_cost, unsigned cost_threshold);

/**
 * Print the cost matrix.
 * @param p          The hungarian object
 * @param cost_width The minimum field width of the costs
 */
FIRM_API void hungarian_print_cost_matrix(hungarian_problem_t const *p,
                                          int cost_width);

/** @} */

#include "../end.h"

#endif
