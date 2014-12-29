/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @author  Daniel Grund
 */
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "obst.h"
#include "hashptr.h"
#include "debug.h"
#include "set.h"
#include "debug.h"
#include "panic.h"

#include "sp_matrix.h"
#include "mps.h"
#include "lpp_t.h"
#include "lpp_comm.h"
#include "lpp_solvers.h"
#include "lpp_net.h"

#define HASH_NAME_T(n) hash_str((n)->name)

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static inline char *obst_xstrdup(struct obstack *obst, const char *str)
{
	return (char*)obstack_copy0(obst, str, strlen(str));
}

static int cmp_name_t(const void *x, const void *y, size_t size)
{
	const lpp_name_t *n = (const lpp_name_t*)x;
	const lpp_name_t *m = (const lpp_name_t*)y;
	(void)size;
	return strcmp(n->name, m->name);
}

/**
 * Update statistic information about matrix usage.
 */
static void update_stats(lpp_t *lpp)
{
	lpp->n_elems    = matrix_get_entries(lpp->m);
	lpp->matrix_mem = lpp->n_elems * matrix_get_elem_size();
	lpp->density    = (double)lpp->n_elems / (double)(lpp->cst_next * lpp->var_next) * 100.0;
}

lpp_t *lpp_new(const char *name, lpp_opt_t opt_type)
{
	return lpp_new_userdef(name, opt_type, 64, 64, 2.0);
}

lpp_t *lpp_new_userdef(const char *name, lpp_opt_t opt_type,
			           int estimated_vars, int estimated_csts,
			           double grow_factor)
{
	lpp_t *lpp;
	int   idx;

	DEBUG_ONLY(dbg = firm_dbg_register("lpp");)
	lpp = XMALLOCZ(lpp_t);
	obstack_init(&lpp->obst);

	lpp->name        = obst_xstrdup(&lpp->obst, name);
	lpp->opt_type    = opt_type;
	lpp->grow_factor = grow_factor;
	lpp->cst2nr      = new_set(cmp_name_t, estimated_csts);
	lpp->var2nr      = new_set(cmp_name_t, estimated_vars);
	lpp->cst_size    = estimated_csts;
	lpp->var_size    = estimated_vars;
	lpp->csts        = XMALLOCNZ(lpp_name_t *, estimated_csts);
	lpp->vars        = XMALLOCNZ(lpp_name_t *, estimated_vars);
	lpp->m           = new_matrix(estimated_csts, estimated_vars);
	lpp->emphasis    = lpp_balanced;
	idx              = lpp_add_cst(lpp, "obj", lpp_objective, 0);
	(void)idx;
	assert(idx == 0);
	idx              = lpp_add_var(lpp, "rhs", lpp_rhs, 0);
	(void)idx;
	assert(idx == 0);

	return lpp;
}

void lpp_free_matrix(lpp_t *lpp)
{
	del_matrix(lpp->m);
	lpp->m = NULL;
}

void lpp_free(lpp_t *lpp)
{
	obstack_free(&lpp->obst, NULL);

	del_set(lpp->cst2nr);
	del_set(lpp->var2nr);

	/* matrix might have been already deleted */
	if (lpp->m)
		del_matrix(lpp->m);

	free(lpp->csts);
	free(lpp->vars);

	free(lpp);
}

double lpp_get_fix_costs(lpp_t *lpp)
{
	return matrix_get(lpp->m, 0, 0);
}

void lpp_set_fix_costs(lpp_t *lpp, double value)
{
	matrix_set(lpp->m, 0, 0, value);
}

static int name2nr(set *where, const char *name)
{
	lpp_name_t find, *found;
	find.name = name;
	found = set_find(lpp_name_t, where, &find, sizeof(find), HASH_NAME_T(&find));
	return (found ? found->nr : -1);
}

static int cst_nr(const lpp_t *lpp, const char *name)
{
	return name2nr(lpp->cst2nr, name);
}

static int var_nr(const lpp_t *lpp, const char *name)
{
	return name2nr(lpp->var2nr, name);
}

static inline char *get_next_name(lpp_t *lpp)
{
	char *res = OALLOCN(&lpp->obst, char, 12);
	snprintf(res, 12, "_%u", lpp->next_name_number++);
	return res;
}

int lpp_add_cst(lpp_t *lpp, const char *cst_name, lpp_cst_t cst_type, double rhs)
{
	lpp_name_t n, *inner;

	DBG((dbg, LEVEL_2, "%s %d %g\n", cst_name, cst_type, rhs));

	if (cst_name && cst_name[0] == '_')
		return ERR_NAME_NOT_ALLOWED;

	if (cst_name)
		n.name = obst_xstrdup(&lpp->obst, cst_name);
	else
		n.name = get_next_name(lpp);

	n.nr  = -1;
	inner = set_insert(lpp_name_t, lpp->cst2nr, &n, sizeof(n), HASH_NAME_T(&n));
	assert(inner);

	if (inner->nr == -1) {
		inner->value_kind    = lpp_none;
		inner->value         = 0.0;
		inner->nr            = lpp->cst_next;
		inner->type.cst_type = cst_type;

		if (lpp->cst_next == lpp->cst_size) {
			lpp->cst_size = (int)((double)lpp->cst_size * lpp->grow_factor) + 1;
			lpp->csts     = XREALLOC(lpp->csts, lpp_name_t *, lpp->cst_size);
		}

		lpp->csts[lpp->cst_next] = inner;
		lpp->cst_next++;
		matrix_set(lpp->m, inner->nr, 0, rhs);
	}

	update_stats(lpp);
	return inner->nr;
}

int lpp_add_cst_uniq(lpp_t *lpp, const char *cst_name, lpp_cst_t cst_type, double rhs)
{
#ifndef NDEBUG
	if (cst_name) {
		lpp_name_t n;

		n.name = cst_name;
		n.nr   = -1;
		assert(!set_find(lpp_name_t, lpp->cst2nr, &n, sizeof(n), HASH_NAME_T(&n)) &&
		    "constraint already exists");
	}
#endif

	return lpp_add_cst(lpp, cst_name, cst_type, rhs);
}

int lpp_get_cst_idx(lpp_t *lpp, const char *cst_name)
{
	DBG((dbg, LEVEL_2, "%s --> %d\n", cst_name, cst_nr(lpp, cst_name)));
	return cst_nr(lpp, cst_name);
}

void lpp_get_cst_name(lpp_t *lpp, int index, char *buf, size_t buf_size)
{
	DBG((dbg, LEVEL_2, "%d --> %s\n", index, lpp->csts[index]->name));
	strncpy(buf, lpp->csts[index]->name, buf_size);
}

int lpp_add_var_default(lpp_t *lpp, const char *var_name, lpp_var_t var_type, double obj, double startval)
{
	int val;

	val = lpp_add_var(lpp, var_name, var_type, obj);
	lpp_set_start_value(lpp, val, startval);

	return val;
}

int lpp_add_var(lpp_t *lpp, const char *var_name, lpp_var_t var_type, double obj)
{
	lpp_name_t n, *inner;

	DBG((dbg, LEVEL_2, "%s %d %g\n", var_name, var_type, obj));

	assert(var_type != lpp_invalid && "invalid is for internal use only");

	if (var_name && var_name[0] == '_')
		return ERR_NAME_NOT_ALLOWED;

	if (var_name)
		n.name = obst_xstrdup(&lpp->obst, var_name);
	else
		n.name = get_next_name(lpp);

	n.nr  = -1;
	inner = set_insert(lpp_name_t, lpp->var2nr, &n, sizeof(n), HASH_NAME_T(&n));
	assert(inner);

	if (inner->nr == -1) {
		inner->nr            = lpp->var_next;
		inner->value_kind    = lpp_none;
		inner->value         = 0;
		inner->type.var_type = var_type;

		if (lpp->var_next == lpp->var_size) {
			lpp->var_size = (int)((double)lpp->var_size * lpp->grow_factor) + 1;
			lpp->vars     = XREALLOC(lpp->vars, lpp_name_t *, lpp->var_size);
		}

		lpp->vars[lpp->var_next] = inner;
		lpp->var_next++;
		matrix_set(lpp->m, 0, inner->nr, obj);
	}

	update_stats(lpp);
	return inner->nr;
}

int lpp_get_var_idx(lpp_t *lpp, const char *var_name)
{
	DBG((dbg, LEVEL_2, "%s --> %d\n", var_name, var_nr(lpp, var_name)));
	return var_nr(lpp, var_name);
}

void lpp_get_var_name(lpp_t *lpp, int index, char *buf, size_t buf_size)
{
	DBG((dbg, LEVEL_2, "%d --> %s\n", index, lpp->vars[index]->name));
	strncpy(buf, lpp->vars[index]->name, buf_size);
}

int lpp_set_factor(lpp_t *lpp, const char *cst_name, const char *var_name, double value)
{
	int cst, var;

	cst = cst_nr(lpp, cst_name);
	var = var_nr(lpp, var_name);
	assert(cst != -1 && var != -1);
	DBG((dbg, LEVEL_2, "%s[%d] %s[%d] %g\n", cst_name, cst, var_name, var, value));
	matrix_set(lpp->m, cst, var, value);
	update_stats(lpp);
	return 0;
}

int lpp_set_factor_fast(lpp_t *lpp, int cst_idx, int var_idx, double value)
{
	assert(cst_idx >= 0 && var_idx >= 0);
	assert(cst_idx < lpp->cst_next && var_idx < lpp->var_next);
	DBG((dbg, LEVEL_2, "%s[%d] %s[%d] %g\n", lpp->csts[cst_idx]->name, cst_idx, lpp->vars[var_idx]->name, var_idx, value));
	matrix_set(lpp->m, cst_idx, var_idx, value);
	update_stats(lpp);
	return 0;
}

int lpp_set_factor_fast_bulk(lpp_t *lpp, int cst_idx, int *var_idx, int num_vars, double value)
{
	assert(cst_idx >= 0 && cst_idx < lpp->cst_next);
	assert(num_vars < lpp->var_next);
	DBG((dbg, LEVEL_2, "row %s[%d] %d vars %g\n", lpp->csts[cst_idx]->name, cst_idx, num_vars, value));
	matrix_set_row_bulk(lpp->m, cst_idx, var_idx, num_vars, value);
	update_stats(lpp);
	return 0;
}

void lpp_set_start_value(lpp_t *lpp, int var_idx, double value)
{
	assert(var_idx > 0 && var_idx < lpp->var_next);
	DBG((dbg, LEVEL_2, "%d %s %g\n", var_idx, lpp->vars[var_idx]->name, value));
	lpp->vars[var_idx]->value = value;
	lpp->vars[var_idx]->value_kind = lpp_value_start;
}

lpp_sol_state_t lpp_get_solution(lpp_t *lpp, double *values, int begin, int end)
{
	int i;

	if (lpp->sol_state < lpp_feasible)
		return lpp->sol_state;

	/* here we are feasible or optimal */
	for (i = 0; i < end - begin + 1; ++i)
		values[i] = lpp->vars[begin + i]->value;

	return lpp->sol_state;
}

void lpp_check_startvals(lpp_t *lpp)
{
	int cst_idx;

	for (cst_idx = 1; cst_idx < lpp->cst_next; ++cst_idx) {
		double     sum     = 0.0;
		lpp_name_t *cst    = lpp->csts[cst_idx];
		double     cst_val = matrix_get(lpp->m, cst_idx, 0);
		int        var_idx;

		for (var_idx = 1; var_idx < lpp->var_next; ++var_idx) {
				if (lpp->vars[var_idx]->value_kind != lpp_value_start)
					goto next;

				sum += lpp->vars[var_idx]->value *
					matrix_get(lpp->m, cst_idx, var_idx);
		}
		switch (cst->type.cst_type) {
			case lpp_equal:
				if(sum != cst_val) {
					fprintf(stderr, "constraint %s unsatisfied: %g != %g\n", cst->name, sum, cst_val);
				}
				break;
			case lpp_less_equal:
				if(sum > cst_val) {
					fprintf(stderr, "constraint %s unsatisfied: %g >= %g\n", cst->name, sum, cst_val);
				}
				break;
			case lpp_greater_equal:
				if(sum < cst_val) {
					fprintf(stderr, "constraint %s unsatisfied: %g <= %g\n", cst->name, sum, cst_val);
				}
				break;
			default:
				panic("unknown constraint type");
		}
next: ;
	}
}

void lpp_dump(lpp_t *lpp, const char *filename)
{
	FILE *out = fopen(filename, "wt");
	mps_write_mps(lpp, s_mps_fixed, out);
	fclose(out);
}

void lpp_set_log(lpp_t *lpp, FILE *log)
{
	lpp->log = log;
}


static const char *lpp_cst_op_to_str(lpp_cst_t cst)
{
	switch (cst) {
	case lpp_equal:
		return "=";
	case lpp_less_equal:
		return "<=";
	case lpp_greater_equal:
		return ">=";
	default:
		return "";
	}
}

void lpp_dump_plain(lpp_t *lpp, FILE *f)
{
	int i;

	fprintf(f, lpp->opt_type == lpp_minimize ? "Minimize\n" : "Maximize\n");
	for(i = 0; i < lpp->cst_next; ++i) {
		lpp_name_t *cst = lpp->csts[i];


		fprintf(f, "%16s: ", cst->name);
		matrix_foreach_in_row(lpp->m, cst->nr, elm) {
			lpp_name_t *var = lpp->vars[elm->col];
			/* TODO Perhaps better a define LPP_COL_RHS */
			if(elm->col > 0)
				fprintf(f, "%+4.1f %-16s ", elm->val, var->name);
		}

		if (i == 0) {
			fprintf(f, "\nSubject To\n");
			continue;
		}

		fprintf(f, "%3s %+4.1f\n",
				lpp_cst_op_to_str(cst->type.cst_type), matrix_get(lpp->m, cst->nr, 0));
	}

	fprintf(f, "Binary\n");
	for(i = 0; i < lpp->var_next; ++i) {
		lpp_name_t *var = lpp->vars[i];
		if (var->type.var_type == lpp_binary)
			fprintf(f, "%16s\n", var->name);
	}
	fprintf(f, "End\n");
}

/**
 * Serialize a lpp to a file descriptor.
 * @param comm The file descriptor.
 * @param lpp The lpp.
 */
void lpp_serialize(lpp_comm_t *comm, const lpp_t *lpp, int with_names)
{
	int n, i;

	lpp_writel(comm, with_names);
	lpp_writel(comm, lpp->cst_next);
	lpp_writel(comm, lpp->var_next);
	lpp_writel(comm, lpp->opt_type);
	lpp_writes(comm, lpp->name);

	/* write options */
	lpp_writel(comm, lpp->set_bound);
	lpp_writed(comm, lpp->bound);
	lpp_writed(comm, lpp->time_limit_secs);
	lpp_writel(comm, lpp->emphasis);

	for(i = 0; i < lpp->cst_next; ++i) {
		lpp_name_t *name = lpp->csts[i];
		lpp_writel(comm, name->nr);
		lpp_writel(comm, name->value_kind);
		lpp_writel(comm, name->type.cst_type);

		if(with_names)
			lpp_writes(comm, name->name);
	}

	for(i = 0; i < lpp->var_next; ++i) {
		lpp_name_t *name = lpp->vars[i];
		lpp_writel(comm, name->nr);
		lpp_writel(comm, name->value_kind);
		lpp_writel(comm, name->type.var_type);

		if(with_names)
			lpp_writes(comm, name->name);
	}

	n = 0;
	matrix_foreach(lpp->m, elm)
		n++;

	assert(n == matrix_get_entries(lpp->m));
	lpp_writel(comm, n);
	matrix_foreach(lpp->m, elm) {
		lpp_writel(comm, elm->row);
		lpp_writel(comm, elm->col);
		lpp_writed(comm, elm->val);
	}
}

/**
 * Deserialize an lpp from a file descriptor.
 * @param comm The file descriptor.
 * @return The Problem.
 */
lpp_t *lpp_deserialize(lpp_comm_t *comm)
{
	int i, n;
	int with_names;

	lpp_t *lpp = XMALLOCZ(lpp_t);

	/* read general settings */
	with_names    = lpp_readl(comm);
	lpp->cst_next = lpp_readl(comm);
	lpp->var_next = lpp_readl(comm);
	lpp->opt_type = (lpp_opt_t)lpp_readl(comm);
	lpp->name     = lpp_reads(comm);

	/* read options */
	lpp->set_bound       = lpp_readl(comm);
	lpp->bound           = lpp_readd(comm);
	lpp->time_limit_secs = lpp_readd(comm);
	lpp->emphasis        = (lpp_emphasis_t)lpp_readl(comm);

	lpp->cst_size = lpp->cst_next;
	lpp->var_size = lpp->var_next;

	lpp->cst2nr   = new_set(cmp_name_t, lpp->cst_next);
	lpp->var2nr   = new_set(cmp_name_t, lpp->var_next);

	lpp->csts     = XMALLOCNZ(lpp_name_t*, lpp->cst_next);
	lpp->vars     = XMALLOCNZ(lpp_name_t*, lpp->var_next);
	lpp->m        = new_matrix(lpp->cst_next, lpp->var_next);

	for(i = 0; i < lpp->cst_next; ++i) {
		lpp_name_t name, *res;

		name.nr            = lpp_readl(comm);
		name.value_kind    = (lpp_value_kind_t)lpp_readl(comm);
		name.type.cst_type = (lpp_cst_t)lpp_readl(comm);

		if(with_names) {
			name.name = lpp_reads(comm);
		} else {
			char* buf = XMALLOCN(char, 32);
			snprintf(buf, 32, "c%d\n", name.nr);
			name.name = buf;
		}

		res = set_insert(lpp_name_t, lpp->cst2nr, &name, sizeof(name), HASH_NAME_T(&name));
		lpp->csts[name.nr] = res;
	}

	for(i = 0; i < lpp->var_next; ++i) {
		lpp_name_t name, *res;

		name.nr            = lpp_readl(comm);
		name.value_kind    = (lpp_value_kind_t)lpp_readl(comm);
		name.type.var_type = (lpp_var_t)lpp_readl(comm);

		if(with_names) {
			name.name = lpp_reads(comm);
		} else {
			char* buf = XMALLOCN(char, 32);
			snprintf(buf, 32, "v%d\n", name.nr);
			name.name = buf;
		}

		res = set_insert(lpp_name_t, lpp->var2nr, &name, sizeof(name), HASH_NAME_T(&name));
		lpp->vars[name.nr] = res;
	}

	n = lpp_readl(comm);
	for(i = 0; i < n; ++i) {
		matrix_elem_t elm;
		elm.row = lpp_readl(comm);
		elm.col = lpp_readl(comm);
		elm.val = lpp_readd(comm);
		matrix_set(lpp->m, elm.row, elm.col, elm.val);
	}

	return lpp;
}

void lpp_serialize_values(lpp_comm_t *comm, const lpp_t *lpp, lpp_value_kind_t value_kind)
{
	int i, n;

	for(i = 0, n = 0; i < lpp->var_next; ++i)
		n += lpp->vars[i]->value_kind == value_kind;

	/* Write the number of values to expect */
	lpp_writel(comm, n);

	/* send the values */
	for(i = 0, n = lpp->var_next; i < n; ++i) {
		const lpp_name_t *name = lpp->vars[i];
		if(name->value_kind == value_kind) {
			lpp_writel(comm, name->nr);
			lpp_writed(comm, name->value);
		}
	}
}

void lpp_deserialize_values(lpp_comm_t *comm, lpp_t *lpp, lpp_value_kind_t value_kind)
{
	int i, n;

	/* Get the number of values to read */
	n = lpp_readl(comm);

	for(i = 0; i < n; ++i) {
		int nr = lpp_readl(comm);
		lpp_name_t *name = lpp->vars[nr];

		name->value_kind = value_kind;
		name->value = lpp_readd(comm);
	}
}

void lpp_serialize_stats(lpp_comm_t *comm, const lpp_t *lpp)
{
	lpp_writel(comm, lpp->sol_state);
	lpp_writel(comm, lpp->iterations);
	lpp_writed(comm, lpp->sol_time);
	lpp_writed(comm, lpp->objval);
	lpp_writed(comm, lpp->best_bound);
}

void lpp_deserialize_stats(lpp_comm_t *comm, lpp_t *lpp)
{
	lpp->sol_state  = (lpp_sol_state_t)lpp_readl(comm);
	lpp->iterations = lpp_readl(comm);
	lpp->sol_time   = lpp_readd(comm);
	lpp->objval     = lpp_readd(comm);
	lpp->best_bound = lpp_readd(comm);
}

void lpp_solve(lpp_t *lpp, const char* host, const char* solver)
{
	if (host == NULL || strlen(host) == 0) {
		lpp_solver_func_t* f = lpp_find_solver(solver);
		if (f != NULL)
			f(lpp);
	} else {
		lpp_solve_net(lpp, host, solver);
	}
}

