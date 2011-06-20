/**
 * @file   lpp_solvers.c
 * @date   16.06.2011
 * @author Sebastian Hack
 *
 * Copyright (C) 2011 Saarland University
 * Released under the LGPL
 */
#include "config.h"

#include <unistd.h>

#include "lpp_cplex.h"
#include "lpp_solvers.h"
#include "lpp_gurobi.h"

static void dummy_solver(lpp_t *lpp)
{
	int i;

	for(i = 0; i < lpp->var_next; ++i) {
		lpp->vars[i]->value = i;
		lpp->vars[i]->value_kind = lpp_value_solution;
	}

	if(lpp->log)
		fprintf(lpp->log, "dummy solver exiting now.\n");

	sleep(1);
	lpp->sol_time = 0.0;
	lpp->iterations = 0;
	lpp->sol_state = lpp_optimal;
}

static void segv_solver(lpp_t *lpp)
{
	int i;

	for(i = 0; i < lpp->var_next; ++i) {
		lpp->vars[i]->value = i;
		lpp->vars[i]->value_kind = lpp_value_solution;
	}

	if(lpp->log)
		fprintf(lpp->log, "segv dummy solver exiting now.\n");

	sleep(1);
	*((int *) 0) = 1;
}

lpp_solver_t lpp_solvers[] = {
#ifdef WITH_CPLEX
	{ lpp_solve_cplex,   "cplex",   1 },
#endif
#ifdef WITH_GUROBI
	{ lpp_solve_gurobi,  "gurobi",  1 },
#endif
	{ dummy_solver,      "dummy",   2 },
	{ segv_solver,       "segv",    2 },
	{ NULL,              NULL,      0 }
};

lpp_solver_func_t *lpp_find_solver(const char *name)
{
	int i;

	for(i = 0; lpp_solvers[i].solver != NULL; i++)
		if(strcmp(lpp_solvers[i].name, name) == 0)
			return lpp_solvers[i].solver;

	return NULL;
}
