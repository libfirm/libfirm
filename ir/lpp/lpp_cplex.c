/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @author  Daniel Grund
 */
#include "lpp_cplex.h"
#include "panic.h"

#ifdef WITH_CPLEX

#include "obst.h"
#include "sp_matrix.h"
#include "stat_timing.h"
#include <ilcplex/cplex.h>
#include <stdio.h>
#include <stdlib.h>

static char cpx_cst_encoding[4] = "?ELG";
static char cpx_var_encoding[4] = "??CB";

typedef struct _cpx_t {
	lpp_t *lpp;
	CPXENVptr env;
	CPXLPptr prob;
	int status;
	char buf[1024];
} cpx_t;

static void chk_cpx_err(cpx_t *cpx)
{
	if (cpx->status) {
		if (CPXgeterrorstring(cpx->env, cpx->status, cpx->buf))
			printf("%s", cpx->buf);
		else
			printf("Unknown CPLEX error\n");
	}
}

static cpx_t *new_cpx(lpp_t *lpp)
{
	cpx_t *cpx = XMALLOCZ(cpx_t);
	cpx->lpp = lpp;
	cpx->env = CPXopenCPLEX(&cpx->status);
	chk_cpx_err(cpx);
	cpx->prob = CPXcreateprob(cpx->env, &cpx->status, lpp->name);
	chk_cpx_err(cpx);
	CPXchgobjsen(cpx->env, cpx->prob, (lpp->opt_type == lpp_minimize)?1:-1);
	chk_cpx_err(cpx);
	if (lpp->log && CPXsetlogfile(cpx->env, lpp->log))
		lpp->log = NULL;
	return cpx;
}

static void free_cpx(cpx_t *cpx)
{
	CPXfreeprob(cpx->env, &cpx->prob);
	CPXcloseCPLEX(&cpx->env);
	free(cpx);
}

/**
 * Build CPLEX data structure from LPP matrix.
 * @note: The LPP matrix is freed after this step, to save memory.
 */
static void cpx_construct(cpx_t *cpx)
{
	int            i, o, sv_cnt;
	int            numcols, numrows, numentries;
	int            objsen, *matbeg, *matcnt, *matind, *indices;
	double        *obj, *rhs, *matval, *lb, *ub, *startv;
	char          *sense, *vartype;
	char         **colname, **rowname;
	struct obstack obst;
	lpp_t         *lpp = cpx->lpp;

	numcols    = lpp->var_next-1;
	numrows    = lpp->cst_next-1;
	numentries = matrix_get_entries(lpp->m);
	objsen     = lpp->opt_type == lpp_minimize ? 1 : -1;
	obstack_init(&obst);

	obj     = obstack_alloc(&obst, numcols * sizeof(*obj));
	lb      = obstack_alloc(&obst, numcols * sizeof(*lb));
	ub      = obstack_alloc(&obst, numcols * sizeof(*ub));
	colname = obstack_alloc(&obst, numcols * sizeof(*colname));
	rowname = obstack_alloc(&obst, numrows * sizeof(*rowname));
	vartype = obstack_alloc(&obst, numcols * sizeof(*vartype));
	indices = obstack_alloc(&obst, numcols * sizeof(*indices));
	startv  = obstack_alloc(&obst, numcols * sizeof(*startv));
	matbeg  = obstack_alloc(&obst, numcols * sizeof(*matbeg));
	matcnt  = obstack_alloc(&obst, numcols * sizeof(*matcnt));
	matind  = obstack_alloc(&obst, numentries * sizeof(*matind));
	matval  = obstack_alloc(&obst, numentries * sizeof(*matval));
	rhs     = obstack_alloc(&obst, numrows * sizeof(*rhs));
	sense   = obstack_alloc(&obst, numrows * sizeof(*sense));

	o      = 0;
	sv_cnt = 0;
	/* fill the CPLEX matrix*/
	for (i = 0; i < numcols; ++i) {
		lpp_name_t *curr_var = lpp->vars[1+i];

		obj[i] = matrix_get(lpp->m, 0, 1+i);
		lb[i]  = 0.0;
		ub[i]  = CPX_INFBOUND;

		colname[i] = (char*) curr_var->name;
		vartype[i] = cpx_var_encoding[curr_var->type.var_type];

		if (curr_var->value_kind == lpp_value_start) {
			indices[sv_cnt]  = i;
			startv[sv_cnt++] = curr_var->value;
		}

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
		sense[i]   = cpx_cst_encoding[curr_cst->type.cst_type];
		rowname[i] = (char*) curr_cst->name;
	}

	cpx->status = CPXcopylpwnames(cpx->env, cpx->prob,
	                              numcols, numrows, objsen,
	                              obj, rhs, sense,
	                              matbeg, matcnt, matind, matval,
	                              lb, ub, NULL,
	                              colname, rowname);
	chk_cpx_err(cpx);

	cpx->status = CPXcopyctype(cpx->env, cpx->prob, vartype);
	chk_cpx_err(cpx);
	cpx->status = CPXcopymipstart(cpx->env, cpx->prob, sv_cnt, indices, startv);
	chk_cpx_err(cpx);

	obstack_free(&obst, NULL);
	lpp_free_matrix(lpp);
}

static void cpx_solve(cpx_t *cpx)
{
	int i, CPX_state, numcols;
	double *values;
	timing_ticks_t tvb;
	timing_ticks_t tva;

	lpp_t *lpp = cpx->lpp;
	numcols = CPXgetnumcols(cpx->env, cpx->prob);
	chk_cpx_err(cpx);

	/* set performance parameters */
	// CPXsetintparam(cpx->env, CPX_PARAM_MIPSTART, CPX_ON);
	CPXsetintparam(cpx->env, CPX_PARAM_MIPORDTYPE, CPX_MIPORDER_COST);
	/* output every search tree node */
	// CPXsetintparam(cpx->env, CPX_PARAM_MIPINTERVAL, 1);

	/* experimental switches */
	// CPXsetintparam(cpx->env, CPX_PARAM_VARSEL, CPX_VARSEL_STRONG);
	// CPXsetdblparam(cpx->env, CPX_PARAM_BTTOL, 1.0);
	// CPXsetintparam(cpx->env, CPX_PARAM_BRDIR, CPX_BRDIR_UP);


	/* Set the time limit appropriately */
	if(lpp->time_limit_secs > 0.0)
		CPXsetdblparam(cpx->env, CPX_PARAM_TILIM, lpp->time_limit_secs);

	/*
	 * If we have enough time, we instruct cplex to imply some
	 * of its higher order magic to pursue the best solution
	 */
	if(lpp->emphasis) {
	  CPXsetintparam(cpx->env, CPX_PARAM_MIPEMPHASIS, lpp->emphasis);
	}

	/*
	 * If a bound of the objective function is supplied,
	 * set it accordingly, dependign on minimization or maximization.
	 */
	if(lpp->set_bound) {
		CPXsetdblparam(cpx->env, (lpp->opt_type == lpp_minimize
		                          ? CPX_PARAM_OBJLLIM : CPX_PARAM_OBJULIM), lpp->bound);
	}

	/* turn on the fancy messages :) */
	// CPXsetintparam (cpx->env, CPX_PARAM_SCRIND, CPX_ON);

	/* solve */
	timing_ticks(tvb);
	cpx->status = CPXmipopt(cpx->env, cpx->prob);
	timing_ticks(tva);
	chk_cpx_err(cpx);

	/* get solution status */
	CPX_state = CPXgetstat(cpx->env, cpx->prob);
	{
	  char buf[512];
	  CPXgetstatstring(cpx->env, CPX_state, buf);
	  fprintf(stderr, "%s\n", buf);
	}
	switch (CPX_state) {
		case CPXMIP_INFEASIBLE:
		case CPX_STAT_INFEASIBLE:   lpp->sol_state = lpp_infeasible; break;
		case CPXMIP_INForUNBD:
		case CPX_STAT_INForUNBD:    lpp->sol_state = lpp_inforunb; break;
		case CPXMIP_UNBOUNDED:
		case CPX_STAT_UNBOUNDED:    lpp->sol_state = lpp_unbounded; break;
		case CPXMIP_ABORT_FEAS:
		case CPXMIP_FAIL_FEAS:
		case CPXMIP_MEM_LIM_FEAS:
		case CPXMIP_NODE_LIM_FEAS:
		case CPXMIP_TIME_LIM_FEAS:  lpp->sol_state = lpp_feasible; break;
		case CPXMIP_OPTIMAL:
		case CPXMIP_OPTIMAL_TOL:    /* TODO: Is this ok? Read the docu more closely */
		case CPX_STAT_OPTIMAL:      lpp->sol_state = lpp_optimal; break;
		default:                    lpp->sol_state = lpp_unknown;
	}

	/* get variable solution values */
	values = alloca(numcols * sizeof(*values));
	CPXgetmipx(cpx->env, cpx->prob, values, 0, numcols-1);
	chk_cpx_err(cpx);
	for(i=0; i<numcols; ++i) {
		lpp->vars[1+i]->value = values[i];
		lpp->vars[1+i]->value_kind = lpp_value_solution;
	}

	/* Get the value of the objective function. */
	CPXgetmipobjval(cpx->env, cpx->prob, &lpp->objval);
	CPXgetbestobjval(cpx->env, cpx->prob, &lpp->best_bound);

	/* get some statistics */
	timing_ticks_sub(tva, tvb);
	lpp->sol_time = timing_ticks_dbl(tva);
	lpp->iterations = CPXgetmipitcnt(cpx->env, cpx->prob);
}

void lpp_solve_cplex(lpp_t *lpp)
{
	cpx_t *cpx = new_cpx(lpp);
	cpx_construct(cpx);
	cpx_solve(cpx);
	free_cpx(cpx);
}

#else

void lpp_solve_cplex(lpp_t *lpp)
{
	(void)lpp;
	panic("libfirm compiled without cplex support");
}

#endif
