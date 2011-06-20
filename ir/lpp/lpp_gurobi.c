/**
 * Author:      Matthias Braun
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#include "config.h"

#ifdef WITH_GUROBI
#include "lpp_gurobi.h"

#include <stdio.h>
#include <stdlib.h>

#include "obst.h"

#include <gurobi_c.h>

#include "error.h"
#include "sp_matrix.h"

static char gurobi_cst_encoding[4] = { 0, GRB_EQUAL, GRB_LESS_EQUAL, GRB_GREATER_EQUAL };
static char gurobi_var_encoding[4] = { 0, 0, GRB_CONTINUOUS, GRB_BINARY };

typedef struct _gurobi_t {
	lpp_t *lpp;
	GRBenv *env;
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
	error = GRBloadenv(&grb->env, NULL);
	check_gurobi_error(grb, error);
	error = GRBsetlogfile(grb->env, lpp->log);
	check_gurobi_error(grb, error);

	return grb;
}

static void free_gurobi(gurobi_t *grb)
{
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

	obstack_free(&obst, NULL);
	free_lpp_matrix(lpp);
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

	/* set performance parameters */
	// CPXsetintparam(grb->env, CPX_PARAM_MIPSTART, CPX_ON);
	//CPXsetintparam(grb->env, CPX_PARAM_MIPORDTYPE, CPX_MIPORDER_COST);
	/* output every search tree node */
	// CPXsetintparam(grb->env, CPX_PARAM_MIPINTERVAL, 1);

	/* experimental switches */
	// CPXsetintparam(grb->env, CPX_PARAM_VARSEL, CPX_VARSEL_STRONG);
	// CPXsetdblparam(grb->env, CPX_PARAM_BTTOL, 1.0);
	// CPXsetintparam(grb->env, CPX_PARAM_BRDIR, CPX_BRDIR_UP);

	/* Set the time limit appropriately */
	if(lpp->time_limit_secs > 0.0) {
		error = GRBsetdblparam(grb->env, GRB_DBL_PAR_TIMELIMIT, lpp->time_limit_secs);
		check_gurobi_error(grb, error);
	}

	/*
	 * If we have enough time, we instruct cplex to imply some
	 * of its higher order magic to pursue the best solution
	 */
	if(lpp->emphasis) {
		/* not implemented */
	}

	/*
	 * If a bound of the objective function is supplied,
	 * set it accordingly, dependign on minimization or maximization.
	 */
	if(lpp->set_bound) {
		//panic("bound not implemented yet");
		fprintf(stderr, "Warning: gurobi bound not implemented yet\n");
	}

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

	/* get variable solution values */
	values = alloca(numcols * sizeof(*values));
	error = GRBgetdblattrarray(grb->model, GRB_DBL_ATTR_X, 0, numcols, values);
	check_gurobi_error(grb, error);
	for(i=0; i<numcols; ++i) {
		lpp->vars[1+i]->value      = values[i];
		lpp->vars[1+i]->value_kind = lpp_value_solution;
	}

	/* Get the value of the objective function. */
	error = GRBgetdblattr(grb->model, GRB_DBL_ATTR_OBJVAL, &lpp->objval);
	check_gurobi_error(grb, error);
	error = GRBgetdblattr(grb->model , GRB_DBL_ATTR_OBJBOUND, &lpp->best_bound);
	check_gurobi_error(grb, error);

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
