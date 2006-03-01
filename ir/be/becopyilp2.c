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

int co_solve_ilp2(copy_opt_t *co, double time_limit) {
	int res = 1;

	return res;
}
