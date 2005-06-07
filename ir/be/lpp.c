/**
 * Author:      Daniel Grund
 * Date:		Fri 13.05.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_IO_H
#include <io.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "xmalloc.h"
#include "debug.h"
#include "assert.h"
#include "hashptr.h"
#include "mps.h"
#include "lpp.h"

#define DEBUG_LVL SET_LEVEL_1
static firm_dbg_module_t *dbg = NULL;

#define HASH_NAME_T(n) HASH_STR((n)->name, strlen((n)->name))

static int cmp_name_t(const void *x, const void *y, size_t size) {
	const name_t *n = x;
	const name_t *m = y;
	return strcmp(n->name, m->name);
}

#define INITIAL_SIZE 64

lpp_t *new_lpp(const char *name, opt_t opt_type) {
	lpp_t *lpp;

	dbg = firm_dbg_register("ir.be.lpp");
	firm_dbg_set_mask(dbg, DEBUG_LVL);

	lpp = xcalloc(1, sizeof(*lpp));
	lpp->name = xstrdup(name);
	lpp->opt_type = opt_type;
	lpp->cst2nr = new_set(cmp_name_t, INITIAL_SIZE);
	lpp->var2nr = new_set(cmp_name_t, INITIAL_SIZE);
	lpp->cst_size = INITIAL_SIZE;
	lpp->var_size = INITIAL_SIZE;
	lpp->csts = xcalloc(INITIAL_SIZE, sizeof(*lpp->csts));
	lpp->vars = xcalloc(INITIAL_SIZE, sizeof(*lpp->vars));
	lpp->m = new_matrix(INITIAL_SIZE, INITIAL_SIZE);
	lpp_add_cst(lpp, "obj", objective, 0);
	lpp_add_var(lpp, "rhs", rhs, 0);
	return lpp;
}

void free_lpp(lpp_t *lpp) {
	int i;
	for(i=0;i<lpp->cst_next;++i)
		free(lpp->csts[i]->name);
	for(i=0;i<lpp->var_next;++i)
		free(lpp->vars[i]->name);
	del_set(lpp->cst2nr);
	del_set(lpp->var2nr);
	del_matrix(lpp->m);
	free(lpp->name);
	free(lpp->csts);
	free(lpp->vars);
	if (lpp->error)
		free(lpp->error);
	free(lpp);
}

static INLINE int name2nr(set *where, char *name) {
	name_t find, *found;
	find.name = name;
	found = set_find(where, &find, sizeof(find), HASH_NAME_T(&find));
	return (found ? found->nr : -1);
}

#define cst_nr(lpp, name) name2nr(lpp->cst2nr, name)
#define var_nr(lpp, name) name2nr(lpp->var2nr, name)

static INLINE char *get_next_name(lpp_t *lpp) {
	char *res = xmalloc(12);
	snprintf(res, 12, "_%d", lpp->next_name_number++);
	return res;
}

int lpp_add_cst(lpp_t *lpp, char *cst_name, cst_t cst_type, double rhs) {
	name_t n, *inner;
	DBG((dbg, LEVEL_2, "%s %d %g\n", cst_name, cst_type, rhs));
	if (cst_name && cst_name[0] == '_')
		return ERR_NAME_NOT_ALLOWED;
	if (cst_name)
		n.name = xstrdup(cst_name);
	else
		n.name = get_next_name(lpp);
	n.nr = -1;
	inner = set_insert(lpp->cst2nr, &n, sizeof(n), HASH_NAME_T(&n));
	assert(inner);

	if (inner->nr == -1) {
		inner->nr = lpp->cst_next;
		inner->type.cst_type = cst_type;
		if (lpp->cst_next == lpp->cst_size) {
			lpp->cst_size *= 2;
			lpp->csts = xrealloc(lpp->csts, lpp->cst_size * sizeof(*lpp->csts));
		}
		lpp->csts[lpp->cst_next] = inner;
		lpp->cst_next++;
		matrix_set(lpp->m, inner->nr, 0, rhs);
	}

	return inner->nr;
}

int lpp_get_cst_idx(lpp_t *lpp, char *cst_name) {
	DBG((dbg, LEVEL_2, "%s --> %d\n", cst_name, cst_nr(lpp, cst_name)));
	return cst_nr(lpp, cst_name);
}

void lpp_get_cst_name(lpp_t *lpp, int index, char *buf, size_t buf_size) {
	DBG((dbg, LEVEL_2, "%d --> %s\n", index, lpp->csts[index]->name));
	strncpy(buf, lpp->csts[index]->name, buf_size);
}

int lpp_add_var(lpp_t *lpp, char *var_name, var_t var_type, double obj) {
	name_t n, *inner;
	DBG((dbg, LEVEL_2, "%s %d %g\n", var_name, var_type, obj));
	assert(var_type != invalid && "invalid is for internal use only");
	if (var_name && var_name[0] == '_')
		return ERR_NAME_NOT_ALLOWED;
	if (var_name)
		n.name = xstrdup(var_name);
	else
		n.name = get_next_name(lpp);
	n.nr = -1;
	inner = set_insert(lpp->var2nr, &n, sizeof(n), HASH_NAME_T(&n));
	assert(inner);

	if (inner->nr == -1) {
		inner->nr = lpp->var_next;
		inner->value_kind = 0;
		inner->value = 0;
		inner->type.var_type = var_type;
		if (lpp->var_next == lpp->var_size) {
			lpp->var_size *= 2;
			lpp->vars = xrealloc(lpp->vars, lpp->var_size * sizeof(*lpp->vars));
		}
		lpp->vars[lpp->var_next] = inner;
		lpp->var_next++;
		matrix_set(lpp->m, 0, inner->nr, obj);
	}

	return inner->nr;
}

int lpp_get_var_idx(lpp_t *lpp, char *var_name) {
	DBG((dbg, LEVEL_2, "%s --> %d\n", var_name, var_nr(lpp, var_name)));
	return var_nr(lpp, var_name);
}

void lpp_get_var_name(lpp_t *lpp, int index, char *buf, size_t buf_size) {
	DBG((dbg, LEVEL_2, "%d --> %s\n", index, lpp->vars[index]->name));
	strncpy(buf, lpp->vars[index]->name, buf_size);
}

int lpp_set_factor(lpp_t *lpp, char *cst_name, char *var_name, double value) {
	int cst, var;

	cst = cst_nr(lpp, cst_name);
	var = var_nr(lpp, var_name);
	assert(cst != -1 && var != -1);
	DBG((dbg, LEVEL_2, "%s[%d] %s[%d] %g\n", cst_name, cst, var_name, var, value));
	matrix_set(lpp->m, cst, var, value);
	return 0;
}

int lpp_set_factor_fast(lpp_t *lpp, int cst_idx, int var_idx, double value) {
	assert(cst_idx >= 0 && var_idx >= 0);
	assert(cst_idx < lpp->cst_next && var_idx < lpp->var_next);
	DBG((dbg, LEVEL_2, "%s[%d] %s[%d] %g\n", lpp->csts[cst_idx]->name, cst_idx, lpp->vars[var_idx]->name, var_idx, value));
	matrix_set(lpp->m, cst_idx, var_idx, value);
	return 0;
}

void lpp_set_start_value(lpp_t *lpp, int var_idx, double value) {
	assert(var_idx > 0 && var_idx < lpp->var_next);
	DBG((dbg, LEVEL_2, "%d %s %g\n", var_idx, lpp->vars[var_idx]->name, value));
	lpp->vars[var_idx]->value = value;
	lpp->vars[var_idx]->value_kind = value_start;
}

sol_state_t lpp_get_solution(lpp_t *lpp, double *values, int begin, int end) {
	int i;
	if (lpp->sol_state < feasible)
		return lpp->sol_state;
	/* here we are feasible or optimal */
	for (i=0; i<end-begin+1; ++i)
		values[i] = lpp->vars[begin+i]->value;
	return lpp->sol_state;
}

void lpp_dump(lpp_t *lpp, const char *filename) {
	FILE *out = fopen(filename, "wt");
	mps_write_mps(lpp, s_mps_fixed, out);
	fclose(out);
}
