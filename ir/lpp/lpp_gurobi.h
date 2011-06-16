/**
 * Author:      Matthias Braun
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef LPP_GUROBI_H
#define LPP_GUROBI_H

#include "lpp.h"

#ifdef WITH_GUROBI
void lpp_solve_gurobi(lpp_t *lpp);
#endif

#endif
