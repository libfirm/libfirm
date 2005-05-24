/**
 * Author:      Daniel Grund
 * Date:		16.05.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include <stdio.h>

typedef enum _opt_t {minimize, maximize} opt_t;
typedef enum _cst_t {objective=0, equal=1, less=2, greater=3} cst_t;
typedef enum _var_t {invalid=0, rhs=1, real=2, binary=3} var_t;
typedef enum _sol_state_t {unknown=0, no_solution_file=1, infeasible=2, unbounded=3, feasible=4, optimal=5} sol_state_t;
typedef struct _lpp_t lpp_t;

#define ERR_NAME_NOT_ALLOWED -2

/**
 * Creates a new problem. Optimization type is minimize or maximize.
 * Implicit row with name "obj" is inserted.
 * Implicit col with name "rhs" is inserted.
 */
lpp_t *new_lpp(const char *name, opt_t opt_type);

void free_lpp(lpp_t *lpp);

/**
 * Adds a constraint to a problem. If a constraint with the same name already
 * exists nothing is altered, and the index of the existing entry is returned.
 * @param cst_name The name of the constraint (1st char only alpha-numeric!). If NULL, a default name will be used.
 * @param cst_type The type of constraint: objective, equality, less-or-equal, greater-or-equal
 * @param rhs The right hand side value to set for this constraint.
 * @return The (new or existing) index of the constraint
 */
int lpp_add_cst(lpp_t *lpp, const char *cst_name, cst_t cst_type, double rhs);

/**
 * Returns the internal index of a constraint.
 * @param cst_name The name of the constraint
 * @return The internal index of constraint @p cst_name or -1 if it does not exist.
 */
int lpp_get_cst_idx(lpp_t *lpp, char *cst_name);

/**
 * Returns the name of a constraint.
 * @param index The internal index of a constraint.
 */
const char *lpp_get_cst_name(lpp_t *lpp, int index);

/**
 * Adds a variable to a problem. If a variable with the same name already
 * exists nothing is altered, and the index of the existing entry is returned.
 * @param var_name The name of the constraint (1st char only alpha-numeric!). If NULL, a default name will be used.
 * @param var_type The type of variable: real, binary
 * @param obj The objactive value coefficient for this variable.
 * @return The (new or existing) index of the variable
 *
 * NOTE: common integer or semi-continous vars are not (yet) implemented
 */
int lpp_add_var(lpp_t *lpp, const char *var_name, var_t var_type, double obj);

/**
 * Returns the internal index of a variable.
 * @param cst_name The name of the variable
 * @return The internal index of variable @p var_name or -1 if it does not exist.
 */
int lpp_get_var_idx(lpp_t *lpp, char *var_name);

/**
 * Returns the name of a variable.
 * @param index The internal index of a variable.
 */
const char *lpp_get_var_name(lpp_t *lpp, int index);

/**
 * Sets the factor of the variable @p var_name in constraint @p cst_name to @p value.
 * Use "obj" as constraint name to set factors in the objective function.
 * Use "rhs" as variable name to set the right hand side values.
 * @return -1 if constraint or variable name does not exist.
 * 			0 otherwise
 */
int lpp_set_factor(lpp_t *lpp, char *cst_name, char *var_name, double value);

/**
 * Same as lpp_set_factor but uses the internal indices instead of names.
 * "obj" and "rhs" both have index 0.
 * @return -1 if an index was invalid
 * 			0 otherwise
 */
int lpp_set_factor_fast(lpp_t *lpp, int cst_idx, int var_idx, double value);

/**
 * Set a starting value for a var.
 * @param var_idx The index of the variable to set the value for.
 * @param value The value to set.
 */
void lpp_set_start_value(lpp_t *lpp, int var_idx, double value);

/**
 * Solve the problem.
 * @return -1 if an error ocurred, 0 otherwise
 */
int lpp_solve(lpp_t *lpp, int use_start_values);

/**
 * @return The solution values of the variables from index begin to index end.
 */
sol_state_t lpp_get_solution(lpp_t *lpp, double *values, int begin, int end);
