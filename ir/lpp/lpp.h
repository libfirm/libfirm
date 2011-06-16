/**
 * Author:      Daniel Grund
 * Date:        16.05.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 * CVS-Id:      $Id: lpp.h 16112 2007-10-07 15:50:49Z mallon $
 *
 * Interface for specifying an milp. Does not define a solution method.
 */
#ifndef _LPP_H
#define _LPP_H

#include <stdio.h>
#include <obstack.h>

#include "set.h"

#include "sp_matrix.h"

typedef enum _lpp_opt_t {
  lpp_minimize,
  lpp_maximize
} lpp_opt_t;

typedef enum _lpp_cst_t {
  lpp_objective = 0,
  lpp_equal     = 1,
  lpp_less      = 2,
  lpp_greater   = 3
} lpp_cst_t;

typedef enum _lpp_var_t {
  lpp_invalid   = 0,
  lpp_rhs       = 1,
  lpp_continous = 2,
  lpp_binary    = 3
} lpp_var_t;

typedef enum _lpp_sol_state_t {
  lpp_unknown    = 0,
  lpp_infeasible = 1,
  lpp_inforunb   = 2,
  lpp_unbounded  = 3,
  lpp_feasible   = 4,
  lpp_optimal    = 5
} lpp_sol_state_t;

typedef enum _lpp_value_kind_t {
  lpp_none           = 0,
  lpp_value_start    = 1,
  lpp_value_solution = 2,
} lpp_value_kind_t;

typedef enum _lpp_emphasis_t {
  lpp_balanced          = 0,
  lpp_feasability       = 1,
  lpp_optimality        = 2,
  lpp_bestbound         = 3,
  lpp_hiddenfeasibility = 4
} lpp_emphasis_t;

typedef struct _name_t {
	const char *name;           /**< the name of the var/constraint supplied by user */
	int nr;                     /**< the col/row number in the matrix */
	lpp_value_kind_t value_kind;
	double value;
	union _type {
		lpp_var_t var_type;
		lpp_cst_t cst_type;
	} type;
} lpp_name_t;

typedef struct _lpp_t {
	/* The problem data */
	const char     *name;            /**< A textual name for this problem */
	FILE           *log;             /**< The log file. */
	lpp_opt_t      opt_type;         /**< Optimization direction */
	struct obstack obst;             /**< Obstack for variable names */
	sp_matrix_t    *m;               /**< The matrix holding objective, constraints and rhs */

	/* Cst/Var to Nr mapping */
	set *cst2nr;                     /**< Holds name_t's for constraints */
	set *var2nr;                     /**< Holds name_t's for variables */

	/* Nr to Cst/Var mapping */
	int        cst_size, var_size;   /**< Size of the csts/vars-arrays below */
	int        cst_next, var_next;   /**< Next free position in arrays below */
	lpp_name_t **csts;               /**< Pointers to the elements in the cst2nr set */
	lpp_name_t **vars;               /**< Pointers to the elements in the var2nr set */
	double     objval;               /**< OUT: Value of the objective function. */
	double     best_bound;           /**< OUT: best bound to the integer solution. */
	double     grow_factor;          /**< The factor by which the vars and constraints are enlarged */

	/* Solving options */
	int    set_bound;                /**< IN: Boolean flag to set a bound for the objective function. */
	double bound;                    /**< IN: The bound. Only valid if set_bound == 1. */
	double time_limit_secs;          /**< IN: Time limit to obey while solving (0.0 means no time limit) */

	/* Solution stuff */
	lpp_sol_state_t sol_state;       /**< State of the solution */
	double          sol_time;        /**< Time in seconds */
	unsigned        iterations;      /**< Number of iterations CPLEX needed to solve the ILP (whatever this means) */

	char           *error;
	unsigned       next_name_number; /**< for internal use only */
	lpp_emphasis_t emphasis;         /**< On what should CPLEX concentrate (feasibility, bestbound, ...) */

	/* some statistic stuff */
	unsigned       send_time;        /**< in case of solve_net: send time in usec */
	unsigned       recv_time;        /**< in case of solve_net: recv time in usec */
	unsigned       n_elems;          /**< number of elements stored in the matrix */
	unsigned       matrix_mem;       /**< memory used by matrix elements (in bytes) */
	double         density;          /**< density of the matrix (percentage) */
} lpp_t;

#define ERR_NAME_NOT_ALLOWED -2

/**
 * Creates a new problem. Optimization type is minimize or maximize.
 * Implicit row with name "obj" is inserted.
 * Implicit col with name "rhs" is inserted.
 */
lpp_t *new_lpp(const char *name, lpp_opt_t opt_type);

/**
 * Creates a new problem. Optimization type is minimize or maximize.
 * Implicit row with name "obj" is inserted.
 * Implicit col with name "rhs" is inserted.
 * @param estimated_vars   The estimated number of variables for the problem
 * @param estimated_csts   The estimated number of constraints for the problem
 * @param grow_factor      By which factor should the problem grow, if there are
 *                         more variables or constraints than estimated.
 */
lpp_t *new_lpp_userdef(const char *name, lpp_opt_t opt_type,
					   int estimated_vars, int estimated_csts, double grow_factor);

/**
 * Frees the matrix embedded in the LPP.
 */
void free_lpp_matrix(lpp_t *lpp);

/**
 * Frees all memory allocated for LPP data structure.
 */
void free_lpp(lpp_t *lpp);

/**
 * @return The constant term in the objective function
 */
double lpp_get_fix_costs(lpp_t *lpp);

/**
 * Sets the constant term in the objective function to @p value
 */
void lpp_set_fix_costs(lpp_t *lpp, double value);

/**
 * Adds a constraint to a problem. If a constraint with the same name already
 * exists nothing is altered, and the index of the existing entry is returned.
 * @param cst_name The name of the constraint (1st char only alpha-numeric!). If NULL, a default name will be used.
 * @param cst_type The type of constraint: objective, equality, less-or-equal, greater-or-equal
 * @param rhs The right hand side value to set for this constraint.
 * @return The (new or existing) index of the constraint
 */
int lpp_add_cst(lpp_t *lpp, const char *cst_name, lpp_cst_t cst_type, double rhs);

/**
 * Adds a constraint to a problem. If a constraint with the same name already
 * exists it dies a horribly cruel death
 * @param cst_name The name of the constraint (1st char only alpha-numeric!). If NULL, a default name will be used.
 * @param cst_type The type of constraint: objective, equality, less-or-equal, greater-or-equal
 * @param rhs The right hand side value to set for this constraint.
 * @return The (new or existing) index of the constraint
 */
int lpp_add_cst_uniq(lpp_t *lpp, const char *cst_name, lpp_cst_t cst_type, double rhs);

/**
 * Returns the internal index of a constraint.
 * @param cst_name The name of the constraint
 * @return The internal index of constraint @p cst_name or -1 if it does not exist.
 */
int lpp_get_cst_idx(lpp_t *lpp, const char *cst_name);

/**
 * Returns the name of a constraint.
 * @param index The internal index of a constraint.
 * @param buf A buffer to hold the name of the constraint
 * @param buf_size Size of the buffer
 */
void lpp_get_cst_name(lpp_t *lpp, int index, char *buf, size_t buf_size);

/**
 * Adds a variable to a problem. If a variable with the same name already
 * exists nothing is altered, and the index of the existing entry is returned.
 * @param var_name The name of the constraint (1st char only alpha-numeric!). If NULL, a default name will be used.
 * @param var_type The type of variable: real, binary
 * @param obj The objective value coefficient for this variable.
 * @return The (new or existing) index of the variable
 *
 * NOTE: common integer or semi-continuous vars are not (yet) implemented
 */
int lpp_add_var(lpp_t *lpp, const char *var_name, lpp_var_t var_type, double obj);

/**
 * Same as lpp_add_var() but the user can supply a default value.
 */
int lpp_add_var_default(lpp_t *lpp, const char *var_name, lpp_var_t var_type, double obj, double startval);

/**
 * Returns the internal index of a variable.
 * @param cst_name The name of the variable
 * @return The internal index of variable @p var_name or -1 if it does not exist.
 */
int lpp_get_var_idx(lpp_t *lpp, const char *var_name);

/**
 * Returns the name of a variable.
 * @param index The internal index of a variable.
 * @param buf A buffer to hold the name of the variable
 * @param buf_size Size of the buffer
 */
void lpp_get_var_name(lpp_t *lpp, int index, char *buf, size_t buf_size);

/**
 * Sets the factor of the variable @p var_name in constraint @p cst_name to @p value.
 * @return -1 if constraint or variable name does not exist.
 *          0 otherwise
 */
int lpp_set_factor(lpp_t *lpp, const char *cst_name, const char *var_name, double value);

/**
 * Same as lpp_set_factor but uses the internal indices instead of names.
 * @return -1 if an index was invalid
 *          0 otherwise
 */
int lpp_set_factor_fast(lpp_t *lpp, int cst_idx, int var_idx, double value);

int lpp_set_factor_fast_bulk(lpp_t *lpp, int cst_idx, int *var_idx, int num_vars, double value);

/**
 * Set a starting value for a var.
 * @param var_idx The index of the variable to set the value for.
 * @param value The value to set.
 */
void lpp_set_start_value(lpp_t *lpp, int var_idx, double value);

/**
 * @return The solution values of the variables from index begin to index end.
 */
lpp_sol_state_t lpp_get_solution(lpp_t *lpp, double *values, int begin, int end);

/**
 * Dumps the lpp into a file with name @p filename in MPS-format
 */
void lpp_dump(lpp_t *lpp, const char *filename);

/**
 * Set the log file, where the solver should write to.
 * @param lpp The problem.
 * @param log The logfile. NULL for no logging.
 */
void lpp_set_log(lpp_t *lpp, FILE *log);

/**
 * Check the start values and list conflicting constraints.
 */
void lpp_check_startvals(lpp_t *lpp);

/**
 * Dump problem into a text file.
 * @param lpp The problem.
 * @param f   The file.
 */
void lpp_dump_plain(lpp_t *lpp, FILE *f);

#define lpp_get_iter_cnt(lpp)         ((lpp)->iterations)
#define lpp_get_sol_time(lpp)         ((lpp)->sol_time)
#define lpp_get_sol_state(lpp)        ((lpp)->sol_state)
#define lpp_get_var_count(lpp)        ((lpp)->var_next-1)
#define lpp_get_cst_count(lpp)        ((lpp)->cst_next-1)
#define lpp_get_sol_state(lpp)        ((lpp)->sol_state)
#define lpp_get_var_sol(lpp,idx)      ((lpp)->vars[idx]->value)
#define lpp_is_sol_valid(lpp)         (lpp_get_sol_state(lpp) >= lpp_feasible)

#define lpp_set_time_limit(lpp,secs)  ((lpp)->time_limit_secs = secs)

/**
 * Set a bound for the objective function.
 * @param lpp The problem.
 * @param bound A bound for the objective function.
 *              If the problem is a minimization problem, the bound
 *              is a lower bound. If it is a maximization problem,
 *              the bound is an upper bound.
 */
#define lpp_set_bound(lpp,bnd)        ((lpp)->set_bound = 1, (lpp)->bound = (bnd))

/**
 * Clear a set bound.
 * @param lpp The problem.
 */
#define lpp_unset_bound(lpp)          ((lpp)->set_bound = 0)

/**
 * Solve an ILP.
 * @param lpp    The problem.
 * @param host   The host to solve on.
 * @param solver The solver to use.
 */
void lpp_solve(lpp_t *lpp, const char* host, const char* solver);

#endif /* _LPP_H */
