/**
 * Author:      Daniel Grund
 * Date:		02.06.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#undef HAVE_CPLEX

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "lpp_local.h"
#include <stdio.h>

#ifdef HAVE_CPLEX
#include <alloca.h>
#include <sys/time.h>

#include "xmalloc.h"
#include "assert.h"
#include "sp_matrix.h"
#include "ilcplex/cplex.h"

#undef LOGFILE //stdout
#define TIME_LIMIT 30 /* in sec. 0 for none */

static char cpx_cst_encoding[4] = {'?', 'E', 'L', 'G'};
static char cpx_var_encoding[4] = {'?', '?', 'C', 'B'};

typedef struct _cpx_t {
	lpp_t *lpp;
	CPXENVptr env;
	CPXLPptr prob;
	int	status;
	char buf[1024];
} cpx_t;

static INLINE void chk_cpx_err(cpx_t *cpx) {
	if (cpx->status) {
		if (CPXgeterrorstring(cpx->env, cpx->status, cpx->buf))
			printf(cpx->buf);
		else
			printf("Unknown CPLEX error\n");
		assert(0);
	}
}

static cpx_t *new_cpx(lpp_t *lpp) {
	cpx_t *cpx = xcalloc(1, sizeof(*cpx));
	cpx->lpp = lpp;
	cpx->env = CPXopenCPLEX(&cpx->status);
	chk_cpx_err(cpx);
	cpx->prob = CPXcreateprob(cpx->env, &cpx->status, lpp->name);
	chk_cpx_err(cpx);
	CPXchgobjsen(cpx->env, cpx->prob, (lpp->opt_type == minimize)?1:-1);
	chk_cpx_err(cpx);
#ifdef LOGFILE
	if (CPXsetlogfile(cpx->env, LOGFILE))
		assert(0 && "Could not set logfile");
#endif
	return cpx;
}

static void free_cpx(cpx_t *cpx) {
	CPXfreeprob(cpx->env, &cpx->prob);
	CPXcloseCPLEX(&cpx->env);
	free(cpx);
}

static void cpx_construct(cpx_t *cpx) {
	const matrix_elem_t *elem;
	int i, o, sv_cnt, numcols, numrows, numentries, objsen, *matbeg, *matcnt, *matind, *indices;
	double *obj, *rhs, *matval, *lb, *ub, *startv;
	char *sense, *vartype, **colname;
	lpp_t *lpp = cpx->lpp;

	numcols = lpp->var_next-1;
	numrows = lpp->cst_next-1;
	numentries = matrix_get_entries(lpp->m);
	objsen  = lpp->opt_type == minimize ? 1 : -1;

	obj     = alloca(numcols * sizeof(*obj));
	lb      = alloca(numcols * sizeof(*lb));
	ub      = alloca(numcols * sizeof(*ub));
	colname = alloca(numcols * sizeof(*colname));
	vartype = alloca(numcols * sizeof(*vartype));
	indices = alloca(numcols * sizeof(*indices));
	startv  = alloca(numcols * sizeof(*startv));
	matbeg  = alloca(numcols * sizeof(*matbeg));
	matcnt  = alloca(numcols * sizeof(*matcnt));
	matind  = alloca(numentries * sizeof(*matind));
	matval  = alloca(numentries * sizeof(*matval));
	rhs     = alloca(numrows * sizeof(*rhs));
	sense   = alloca(numrows * sizeof(*sense));

	o = 0;
	sv_cnt = 0;
	for(i=0; i<numcols; ++i) {
		name_t *curr_var = lpp->vars[1+i];
		obj[i] = matrix_get(lpp->m, 0, 1+i);
		lb[i] = 0.0;
		ub[i] = CPX_INFBOUND;
		colname[i] = curr_var->name;
		vartype[i] = cpx_var_encoding[curr_var->type.var_type];
		if(curr_var->value_kind == value_start) {
			indices[sv_cnt]  = i;
			startv[sv_cnt++] = curr_var->value;
		}
		matbeg[i] = o;
		matcnt[i] = 0;
		matrix_foreach_in_col(lpp->m, 1+i, elem) {
			if (elem->row == 0)
				continue;
			matind[o] = elem->row-1;
			matval[o] = elem->val;
			matcnt[i]++;
			o++;
		}
	}

	for(i=0; i<numrows; ++i) {
		rhs[i]   = matrix_get(lpp->m, 1+i, 0);
		sense[i] = cpx_cst_encoding[lpp->csts[1+i]->type.cst_type];
	}

	cpx->status = CPXcopylpwnames(cpx->env, cpx->prob,
						numcols, numrows, objsen,
						obj, rhs, sense,
						matbeg, matcnt, matind, matval,
						lb, ub, NULL,
						colname, NULL);
	chk_cpx_err(cpx);
	cpx->status = CPXcopyctype(cpx->env, cpx->prob, vartype);
	chk_cpx_err(cpx);
	cpx->status = CPXcopymipstart(cpx->env, cpx->prob, sv_cnt, indices, startv);
	chk_cpx_err(cpx);
}

static void cpx_solve(cpx_t *cpx) {
	int i, CPX_state, numcols;
	double *values;
	struct timeval tvb, tva;

	lpp_t *lpp = cpx->lpp;
	numcols = CPXgetnumcols(cpx->env, cpx->prob);
	chk_cpx_err(cpx);

	/* set performance parameters */
	CPXsetintparam(cpx->env, CPX_PARAM_MIPSTART, CPX_ON);
	CPXsetintparam(cpx->env, CPX_PARAM_MIPEMPHASIS, CPX_MIPEMPHASIS_BESTBOUND);
	CPXsetintparam(cpx->env, CPX_PARAM_VARSEL, CPX_VARSEL_STRONG);
	if (TIME_LIMIT)
		CPXsetdblparam(cpx->env, CPX_PARAM_TILIM, TIME_LIMIT);

	/* solve */
	gettimeofday(&tvb, NULL);
	cpx->status = CPXmipopt(cpx->env, cpx->prob);
	gettimeofday(&tva, NULL);
	chk_cpx_err(cpx);

	/* get solution status */
	CPX_state = CPXgetstat(cpx->env, cpx->prob);
	switch (CPX_state) {
		case CPXMIP_INFEASIBLE:
		case CPX_STAT_INFEASIBLE:	lpp->sol_state = infeasible; break;
		case CPXMIP_INForUNBD:
		case CPX_STAT_INForUNBD:	lpp->sol_state = inforunb; break;
		case CPXMIP_UNBOUNDED:
		case CPX_STAT_UNBOUNDED:	lpp->sol_state = unbounded; break;
		case CPXMIP_ABORT_FEAS:
		case CPXMIP_FAIL_FEAS:
		case CPXMIP_MEM_LIM_FEAS:
		case CPXMIP_NODE_LIM_FEAS:
		case CPXMIP_TIME_LIM_FEAS:	lpp->sol_state = feasible; break;
		case CPXMIP_OPTIMAL:
		case CPX_STAT_OPTIMAL:		lpp->sol_state = optimal; break;
		default:					lpp->sol_state = unknown;
	}
	assert(lpp->sol_state == optimal || lpp->sol_state == feasible);

	/* get variable solution values */
	values = alloca(numcols * sizeof(*values));
	CPXgetmipx(cpx->env, cpx->prob, values, 0, numcols-1);
	chk_cpx_err(cpx);
	for(i=0; i<numcols; ++i) {
		lpp->vars[1+i]->value = values[i];
		lpp->vars[1+i]->value_kind = value_solution;
	}

	/* get some statistics */
	lpp->sol_time = tva.tv_sec - tvb.tv_sec;
	lpp->iterations = CPXgetmipitcnt(cpx->env, cpx->prob);
}

void lpp_solve_local(lpp_t *lpp) {
	cpx_t *cpx = new_cpx(lpp);
	cpx_construct(cpx);
	cpx_solve(cpx);
	free_cpx(cpx);
}

#else

void lpp_solve_local(lpp_t *lpp) {
	fprintf(stderr, "CPLEX not available!\n");
}
#endif
