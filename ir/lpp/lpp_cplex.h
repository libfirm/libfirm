/**
 * Author:      Daniel Grund
 * Date:        02.06.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef LPP_CPLEX_H
#define LPP_CPLEX_H

#include "lpp.h"

#ifdef WITH_CPLEX
void lpp_solve_cplex(lpp_t *lpp);
#endif

#endif
