/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @author  Sebastian Hack
 */
#ifndef LPP_LPP_SOLVERS_H
#define LPP_LPP_SOLVERS_H

#include "lpp.h"

typedef void lpp_solver_func_t(lpp_t *lpp);

/**
 * Find a solver for a given name.
 */
lpp_solver_func_t *lpp_find_solver(char const *name);

#endif
