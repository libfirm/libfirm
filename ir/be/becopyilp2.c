/**
 * Author:      Daniel Grund
 * Date:		28.02.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 * ILP formalization using G=(V, E, Q):
 *  - 1 class of variables: equal color vars
 *  - Path constraints
 *  - Clique path constraints
 *
 *
 *	\min \sum_{ (i,j) \in Q }  w_ij y_ij
 *
 *		y_ij				=  1			(i,j) \in E
 *
 *		\sum_c y_nc			=  |C| - 1		n \in N, c \in C
 *
 *		y_nc				=  1			n \in N, c \not\in C(n)
 *
 *		\sum_{e \in p} y_e	>= 1			p \in P		path constraints
 *
 *		\sum_{e \in cp} y_e	>= |cp| - 1		cp \in CP	clique-path constraints
 *
 *		y_ij \in N,   w_ij \in R^+
 */

#include "becopyilp_t.h"

#define DEBUG_LVL 1

typedef struct _my_env_t {
	int foo;
} my_env_t;


static void ilp2_build(ilp_env_t *ienv) {
	ienv->lp = new_lpp(ienv->co->name, lpp_minimize);

}

static void ilp2_apply(ilp_env_t *ienv) {

}

int co_solve_ilp2(copy_opt_t *co, double time_limit) {
	lpp_sol_state_t sol_state;
	ilp_env_t *ienv;
	my_env_t my;
	firm_dbg_module_t *dbg = firm_dbg_register("ir.be.coilp2");

	firm_dbg_set_mask(dbg, DEBUG_LVL);

	// my.bla = TODO

	ienv = new_ilp_env(co, dbg, ilp2_build, ilp2_apply, &my);

	sol_state = ilp_go(ienv, time_limit);

	free_ilp_env(ienv);

	return sol_state == lpp_optimal;
}
