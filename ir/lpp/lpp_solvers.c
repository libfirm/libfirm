/*
 * Copyright (C) 2005-2011 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @author  Sebastian Hack
 */
#include "config.h"

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
