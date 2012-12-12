/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @author  Sebastian Hack
 */
#ifndef LPP_LPP_SOLVER_H
#define LPP_LPP_SOLVER_H

#include "lpp.h"

typedef void (lpp_solver_func_t)(lpp_t *lpp);

typedef struct {
	lpp_solver_func_t *solver;
	const char        *name;
	int                n_instances;
} lpp_solver_t;

extern lpp_solver_t lpp_solvers[];

/**
 * Find a solver for a given name.
 */
lpp_solver_func_t *lpp_find_solver(const char *name);

#endif
