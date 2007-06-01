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
 ** "Stanford GraphGase" distrubition!
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
 * @version $Id$
 */
#ifndef FIRM_ADT_HUNGARIAN_H
#define FIRM_ADT_HUNGARIAN_H

#define HUNGARIAN_MODE_MINIMIZE_COST 0
#define HUNGARIAN_MODE_MAXIMIZE_UTIL 1

#define HUNGARIAN_MATCH_NORMAL  0
#define HUNGARIAN_MATCH_PERFECT 1

typedef struct _hungarian_problem_t hungarian_problem_t;

/**
 * This method initialize the hungarian_problem structure and init
 * the cost matrix (missing lines or columns are filled with 0).
 *
 * @param rows       Number of rows in the given matrix
 * @param cols       Number of cols in the given matrix
 * @param width      Element width for matrix dumping
 * @param match_type The type of matching:
 *                   HUNGARIAN_MATCH_PERFECT - every nodes matches another node
 *                   HUNGARIAN_MATCH_NORMAL  - matchings of nodes having no edge getting removed
 * @return The problem object.
 */
hungarian_problem_t *hungarian_new(int rows, int cols, int width, int match_type);

/**
 * Adds an edge from left to right with some costs.
 */
void hungarian_add(hungarian_problem_t *p, int left, int right, int cost);

/**
* Removes the edge from left to right.
*/
void hungarian_remv(hungarian_problem_t *p, int left, int right);

/**
 * Prepares the cost matrix, dependend on the given mode.
 *
 * @param p     The hungarian object
 * @param mode  HUNGARIAN_MODE_MAXIMIZE_UTIL or HUNGARIAN_MODE_MINIMIZE_COST (default)
 */
void hungarian_prepare_cost_matrix(hungarian_problem_t *p, int mode);

/**
 * Destroys the hungarian object.
 */
void hungarian_free(hungarian_problem_t *p);

/**
 * This method computes the optimal assignment.
 * @param p              The hungarian object
 * @param assignment     The final assignment
 * @param final_cost     The final costs
 * @param cost_threshold Matchings with costs >= this limit will be removed (if limit > 0)
 * @return 0 on success, negative number otherwise
 */
int hungarian_solve(hungarian_problem_t *p, int *assignment, int *final_cost, int cost_threshold);

/**
 * Print the cost matrix.
 * @param p The hungarian object
 */
void hungarian_print_costmatrix(hungarian_problem_t *p);

#endif /* _HUNGARIAN_H_ */
