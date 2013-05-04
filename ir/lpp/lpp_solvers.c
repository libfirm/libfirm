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

lpp_solver_t lpp_solvers[] = {
#ifdef WITH_CPLEX
	{ lpp_solve_cplex,   "cplex",   1 },
#endif
#ifdef WITH_GUROBI
	{ lpp_solve_gurobi,  "gurobi",  1 },
#endif
	{ NULL,              NULL,      0 }
};

lpp_solver_func_t *lpp_find_solver(const char *name)
{
	int i;

	if (name[0] == '\0')
		return lpp_solvers[0].solver;

	for(i = 0; lpp_solvers[i].solver != NULL; i++)
		if(strcmp(lpp_solvers[i].name, name) == 0)
			return lpp_solvers[i].solver;

	return NULL;
}
