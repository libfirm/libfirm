/**
 * Author:      Daniel Grund
 * Date:		16.05.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include <stdio.h>

typedef enum _opt_t {minimize, maximize} opt_t;
typedef enum _cst_t {objective=0, equal=1, less=2, greater=3} cst_t;
typedef enum _var_t {invalid=0, real=1, binary=2} var_t;

/**
 * Fixed-column mps where spaces are allowed in identifiers :-0
 * Free mps where whitespace is a seperator :-)
 * LP format
 */
typedef enum _style_t {s_mps_fixed, s_mps_free, s_lp} style_t;

typedef struct _lpp_t lpp_t;

/**
 * Creates a new problem. Optimization type is minimize or maximize
 */
lpp_t *new_lp(char *name, opt_t opt_type);

void free_lp(lpp_t *lpp);

/**
 * Adds a constraint to a problem. If a constraint with the same name already exists result is undefined.
 * @param cst_name The name of the constraint.
 * @param cst_type The type of constraint: objective, equality, less-or-equal, greater-or-equal
 */
void lpp_add_constr(lpp_t *lpp, const char *cst_name, cst_t cst_type);

/**
 * Adds a variable to a problem. If a variable with the same name already exists result is undefined.
 * @param var_name The name of the variable.
 * @param var_type The type of the var: real or binary
 * NOTE: common integer or semi-continous vars are not (yet) implemented
 */
void lpp_add_var(lpp_t *lpp, const char *var_name, var_t var_type);

/**
 * Sets the factor of the variable @p var_name in constraint @p cst_name to @p value.
 */
void lpp_set_factor(lpp_t *lpp, const char *cst_name, const char *var_name, int value);

/**
 * Sets the value for the right hand side of constraint @p cst_name to @p value
 */
void lpp_set_rhs(lpp_t *lpp, char *cst_name, int value);

/**
 * Sets, for constraint @p cst_name, all factors of all variables
 * given in @p var_names to the values given in @p values.
 */
void lpp_set_all_constr(lpp_t *lpp, char *cst_name, char **var_names, int *values);

/**
 * Writes out the problem in the format specified be @p style.
 */
void lpp_dump(lpp_t *lpp, FILE *out, style_t style);
