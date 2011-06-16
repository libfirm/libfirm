/**
 * @file   lpp_solvers.h
 * @date   16.06.2011
 * @author Sebastian Hack
 *
 * Copyright (C) 2011 Saarland University
 * Released under the LGPL
 */

#ifndef LPP_SOLVER_H
#define LPP_SOLVER_H

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

#endif /* LPP_SOLVER_H */
