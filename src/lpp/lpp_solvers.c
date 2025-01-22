/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @author  Sebastian Hack
 */
#include "lpp_solvers.h"

#include "lpp_cplex.h"
#include "lpp_gurobi.h"
#include "util.h"

typedef struct lpp_solver_t {
	lpp_solver_func_t *solver;
	char        const *name;
	int                n_instances;
} lpp_solver_t;

static lpp_solver_t const lpp_solvers[] = {
#ifdef WITH_CPLEX
	{ lpp_solve_cplex,   "cplex",   1 },
#endif
#ifdef WITH_GUROBI
	{ lpp_solve_gurobi,  "gurobi",  1 },
#endif
	{ NULL,              NULL,      0 }
};

lpp_solver_func_t *lpp_find_solver(char const *const name)
{
	if (name[0] == '\0')
		return lpp_solvers[0].solver;

	for (lpp_solver_t const *i = lpp_solvers; i->solver; ++i) {
		if (streq(i->name, name))
			return i->solver;
	}

	return NULL;
}
