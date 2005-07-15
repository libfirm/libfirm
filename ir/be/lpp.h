/**
 * Author:      Daniel Grund
 * Date:		16.05.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 * Interface for specifying an milp. Does not define a solution method.
 */
#ifndef _LPP_H
#define _LPP_H

#include <stdio.h>
#include "set.h"
#include "sp_matrix.h"

typedef enum _opt_t {minimize, maximize} opt_t;
typedef enum _cst_t {objective=0, equal=1, less=2, greater=3} cst_t;
typedef enum _var_t {invalid=0, rhs=1, continous=2, binary=3} var_t;
typedef enum _sol_state_t {unknown=0, infeasible=1, inforunb=2, unbounded=3, feasible=4, optimal=5} sol_state_t;
typedef enum _value_kind_t {none=0, value_start, value_solution} value_kind_t;

typedef struct _name_t name_t;
struct _name_t {
	char *name;					/**< the name of the var/constraint supplied by user */
	int nr;						/**< the col/row number in the matrix */
	value_kind_t value_kind;
	double value;
	union _type {
		var_t var_type;
		cst_t cst_type;
	} type;
};

typedef struct _lpp_t {
	/* The problem data */
	char *name;						/**< A textual name for this problem */
	opt_t opt_type;					/**< Optimization direction */
	sp_matrix_t *m;					/**< The matrix holding objective, constraints and rhs */

	/* Cst/Var to Nr mapping */
	set *cst2nr;					/**< Holds name_t's for constraints */
	set *var2nr;					/**< Holds name_t's for variables */

	/* Nr to Cst/Var mapping */
	int cst_size, var_size;			/**< Size of the csts/vars-arrays below */
	int cst_next, var_next;			/**< Next free position in arrays below */
	name_t **csts;					/**< Pointers to the elements in the cst2nr set */
	name_t **vars;					/**< Pointers to the elements in the var2nr set */

	/* Solution stuff */
	sol_state_t sol_state;
	double sol_time;					/**< Time in seconds */
	unsigned iterations;

	char *error;
	unsigned next_name_number;
} lpp_t;

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
int lpp_add_cst(lpp_t *lpp, char *cst_name, cst_t cst_type, double rhs);

/**
 * Returns the internal index of a constraint.
 * @param cst_name The name of the constraint
 * @return The internal index of constraint @p cst_name or -1 if it does not exist.
 */
int lpp_get_cst_idx(lpp_t *lpp, char *cst_name);

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
 * @param obj The objactive value coefficient for this variable.
 * @return The (new or existing) index of the variable
 *
 * NOTE: common integer or semi-continous vars are not (yet) implemented
 */
int lpp_add_var(lpp_t *lpp, char *var_name, var_t var_type, double obj);

/**
 * Returns the internal index of a variable.
 * @param cst_name The name of the variable
 * @return The internal index of variable @p var_name or -1 if it does not exist.
 */
int lpp_get_var_idx(lpp_t *lpp, char *var_name);

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
 * 			0 otherwise
 */
int lpp_set_factor(lpp_t *lpp, char *cst_name, char *var_name, double value);

/**
 * Same as lpp_set_factor but uses the internal indices instead of names.
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
 * @return The solution values of the variables from index begin to index end.
 */
sol_state_t lpp_get_solution(lpp_t *lpp, double *values, int begin, int end);

/**
 * Dumps the lpp into a file with name @p filename in MPS-format
 */
void lpp_dump(lpp_t *lpp, const char *filename);

#define lpp_get_iter_cnt(lpp) ((lpp)->iterations)
#define lpp_get_sol_time(lpp) ((lpp)->sol_time)

#endif
