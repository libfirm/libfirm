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
 * @author  Matthias Braun
 */
#include "config.h"

#ifdef WITH_GUROBI
#include "lpp_gurobi.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "obst.h"

#include <gurobi_c.h>

#include "error.h"
#include "sp_matrix.h"

static char gurobi_cst_encoding[4] = { 0, GRB_EQUAL, GRB_LESS_EQUAL, GRB_GREATER_EQUAL };
static char gurobi_var_encoding[4] = { 0, 0, GRB_CONTINUOUS, GRB_BINARY };

typedef struct _gurobi_t {
	lpp_t *lpp;
	GRBenv *env;
	GRBenv *modelenv;
	GRBmodel *model;
} gurobi_t;

static void check_gurobi_error(gurobi_t *grb, int error)
{
	if (error != 0) {
		panic("gurobi error: %s", GRBgeterrormsg(grb->env));
	}
}

static gurobi_t *new_gurobi(lpp_t *lpp)
{
	int error;

	gurobi_t *grb = XMALLOCZ(gurobi_t);
	grb->lpp = lpp;
	/* /tmp/firm_gurobi.log is a hack (see below) */
	error = GRBloadenv(&grb->env, "/tmp/firm_gurobi.log");
	check_gurobi_error(grb, error);
	/* Matze: do not set the FILE* for logging output. Because:
	 *  a) the function is deprecated
	 *  b) gurobi closes the FILE handle when it is done, which leads to
	 *     very unexpected effects when you pass stdout or stderr as logging
	 *     output.
	 * The only thing gurobi sanely supports is giving a string with a filename
	 * :-( ...so we use /tmp/firm_gurobi.log as a temporary measure...
	 */
#if 0
	error = GRBsetlogfile(grb->env, lpp->log);
	check_gurobi_error(grb, error);
#else
	if (lpp->log != stdout && lpp->log != stderr) {
		error = GRBsetintparam(grb->env, GRB_INT_PAR_OUTPUTFLAG, 0);
		check_gurobi_error(grb, error);
	}
#endif

	return grb;
}

static void free_gurobi(gurobi_t *grb)
{
	GRBfreemodel(grb->model);
	GRBfreeenv(grb->env);
	free(grb);
}

/**
 * Build CPLEX data structure from LPP matrix.
 * @note: The LPP matrix is freed after this step, to save memory.
 */
static void gurobi_construct(gurobi_t *grb)
{
	const matrix_elem_t *elem;
	int                  i, o;
	//int                  sv_cnt;
	//int                 *indices;
	//double              *startv;
	int                  numcols, numrows, numentries;
	int                  objsen, *matbeg, *matcnt, *matind;
	double               *obj, *rhs, *matval, *lb;
	char                 *sense, *vartype;
	char                 **colname, **rowname;
	struct obstack       obst;
	lpp_t                *lpp = grb->lpp;
	int                  error;

	numcols    = lpp->var_next-1;
	numrows    = lpp->cst_next-1;
	numentries = matrix_get_entries(lpp->m);
	objsen     = lpp->opt_type == lpp_minimize ? 1 : -1;
	obstack_init(&obst);

	obj     = obstack_alloc(&obst, numcols * sizeof(*obj));
	lb      = obstack_alloc(&obst, numcols * sizeof(*lb));
	colname = obstack_alloc(&obst, numcols * sizeof(*colname));
	rowname = obstack_alloc(&obst, numrows * sizeof(*rowname));
	vartype = obstack_alloc(&obst, numcols * sizeof(*vartype));
	//indices = obstack_alloc(&obst, numcols * sizeof(*indices));
	//startv  = obstack_alloc(&obst, numcols * sizeof(*startv));
	matbeg  = obstack_alloc(&obst, numcols * sizeof(*matbeg));
	matcnt  = obstack_alloc(&obst, numcols * sizeof(*matcnt));
	matind  = obstack_alloc(&obst, numentries * sizeof(*matind));
	matval  = obstack_alloc(&obst, numentries * sizeof(*matval));
	rhs     = obstack_alloc(&obst, numrows * sizeof(*rhs));
	sense   = obstack_alloc(&obst, numrows * sizeof(*sense));

	o      = 0;
	//sv_cnt = 0;
	/* fill the CPLEX matrix*/
	for (i = 0; i < numcols; ++i) {
		lpp_name_t *curr_var = lpp->vars[1+i];

		obj[i] = matrix_get(lpp->m, 0, 1+i);
		lb[i]  = 0.0;

		colname[i] = (char*) curr_var->name;
		vartype[i] = gurobi_var_encoding[curr_var->type.var_type];

#if 0
		if (curr_var->value_kind == lpp_value_start) {
			panic("start values not supported in gurobi yet");
			indices[sv_cnt]  = i;
			startv[sv_cnt++] = curr_var->value;
		}
#endif

		matbeg[i] = o;
		matcnt[i] = 0;
		matrix_foreach_in_col(lpp->m, 1 + i, elem) {
			if (elem->row == 0)
				continue;
			matind[o] = elem->row-1;
			matval[o] = elem->val;
			matcnt[i]++;
			o++;
		}
	}

	/* get constraint stuff (right hand side, type, name) */
	for (i = 0; i < numrows; ++i) {
		lpp_name_t *curr_cst = lpp->csts[1 + i];

		rhs[i]     = matrix_get(lpp->m, 1 + i, 0);
		sense[i]   = gurobi_cst_encoding[curr_cst->type.cst_type];
		rowname[i] = (char*) curr_cst->name;
	}

	error = GRBloadmodel(grb->env, &grb->model, lpp->name, numcols, numrows,
	                     objsen, 0, obj, sense, rhs, matbeg, matcnt, matind,
	                     matval, lb, NULL, vartype, colname, rowname);
	check_gurobi_error(grb, error);
	grb->modelenv = GRBgetenv(grb->model);

	obstack_free(&obst, NULL);
	lpp_free_matrix(lpp);
}

static void gurobi_solve(gurobi_t *grb)
{
	lpp_t *lpp = grb->lpp;
	int i;
	int optimstatus;
	int error;
	int numcols = lpp->var_next-1;
	double *values;
	double  iterations;

	/* Set the time limit appropriately */
	if(lpp->time_limit_secs > 0.0) {
		error = GRBsetdblparam(grb->modelenv, GRB_DBL_PAR_TIMELIMIT, lpp->time_limit_secs);
		check_gurobi_error(grb, error);
	}

#if 0
	/*
	 * If a bound of the objective function is supplied,
	 * set it accordingly, dependign on minimization or maximization.
	 */
	if(lpp->set_bound) {
		fprintf(stderr, "Warning: gurobi bound not implemented yet\n");
	}
#endif

	/* solve */
	error = GRBoptimize(grb->model);
	check_gurobi_error(grb, error);

	/* get solution status */
	error = GRBgetintattr(grb->model, GRB_INT_ATTR_STATUS, &optimstatus);
	check_gurobi_error(grb, error);

	switch (optimstatus) {
	case GRB_OPTIMAL:           lpp->sol_state = lpp_optimal; break;
	case GRB_INFEASIBLE:        lpp->sol_state = lpp_infeasible; break;
	case GRB_INF_OR_UNBD:       lpp->sol_state = lpp_inforunb; break;
	case GRB_UNBOUNDED:         lpp->sol_state = lpp_unbounded; break;
	/* TODO: is this correct? */
	default:                    lpp->sol_state = lpp_feasible; break;
	}

	if (lpp->sol_state >= lpp_feasible) {
		/* get variable solution values */
		values = alloca(numcols * sizeof(*values));
		error = GRBgetdblattrarray(grb->model, GRB_DBL_ATTR_X, 0, numcols,
		                           values);
		check_gurobi_error(grb, error);
		for(i=0; i<numcols; ++i) {
			lpp->vars[1+i]->value      = values[i];
			lpp->vars[1+i]->value_kind = lpp_value_solution;
		}

		/* Get the value of the objective function. */
		error = GRBgetdblattr(grb->model, GRB_DBL_ATTR_OBJVAL, &lpp->objval);
		check_gurobi_error(grb, error);
		error = GRBgetdblattr(grb->model , GRB_DBL_ATTR_OBJBOUND,
		                      &lpp->best_bound);
		if (error != 0) {
			lpp->best_bound = FP_NAN;
		}
	}

	/* get some statistics */
	error = GRBgetdblattr(grb->model, GRB_DBL_ATTR_ITERCOUNT, &iterations);
	check_gurobi_error(grb, error);
	lpp->iterations = (unsigned) iterations;

	error = GRBgetdblattr(grb->model, GRB_DBL_ATTR_RUNTIME, &lpp->sol_time);
	check_gurobi_error(grb, error);
}

void lpp_solve_gurobi(lpp_t *lpp)
{
	gurobi_t *grb = new_gurobi(lpp);
	gurobi_construct(grb);
	gurobi_solve(grb);
	free_gurobi(grb);
}

#endif
