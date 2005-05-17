/**
 * Author:      Daniel Grund
 * Date:		Fri 13.05.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include "lpp.h"
#include "xmalloc.h"
#include "assert.h"
#include "hashptr.h"
#include "set.h"
#include "sp_matrix.h"

typedef struct _name_t name_t;

struct _name_t {
	const char *name;		/**< the name of the var/constraint supplied by user */
	int nr;					/**< the col/row number in the matrix */
	union _type {
		var_t var_type;
		cst_t cst_type;
	} type;
};

struct _lpp_t {
	/* The problem data */
	char *name;						/**< A textual name for this problem */
	opt_t opt_type;					/**< Optimization direction */
	sp_matrix_t *m;					/**< The matrix holding objective and constraints */
	int rhs_size;					/**< Size of the rhs-array below */
	int *rhs;						/**< The right hand side vector */

	/* Cst/Var to Nr mapping */
	set *cst2nr;					/**< Holds name_t's for constraints */
	set *var2nr;					/**< Holds name_t's for variables */

	/* Nr to Cst/Var mapping */
	int cst_size, var_size;			/**< Size of the csts/vars-arrays below */
	int cst_next, var_next;			/**< Next free position in arrays below */
	name_t **csts;					/**< Pointers to the elements in the cst2nr set */
	name_t **vars;					/**< Pointers to the elements in the var2nr set */
};

#define HASH_NAME_T(n) HASH_STR((n)->name, strlen((n)->name))

static int cmp_name_t(const void *x, const void *y, size_t size) {
	const name_t *n = x;
	const name_t *m = y;
	return strcmp(n->name, m->name);
}

#define INITIAL_SIZE 64

lpp_t *new_lp(char *name, opt_t opt_type) {
	lpp_t *lpp = xcalloc(1, sizeof(*lpp));
	lpp->name = name;
	lpp->opt_type = opt_type;
	lpp->cst2nr = new_set(cmp_name_t, INITIAL_SIZE);
	lpp->var2nr = new_set(cmp_name_t, INITIAL_SIZE);
	lpp->cst_size = INITIAL_SIZE;
	lpp->csts = xcalloc(INITIAL_SIZE, sizeof(*lpp->csts));
	lpp->var_size = INITIAL_SIZE;
	lpp->vars = xcalloc(INITIAL_SIZE, sizeof(*lpp->vars));
	lpp->m = new_matrix(INITIAL_SIZE, INITIAL_SIZE);
	lpp->rhs_size = INITIAL_SIZE;
	lpp->rhs = xcalloc(INITIAL_SIZE, sizeof(*lpp->rhs));
	return lpp;
}

void free_lp(lpp_t *lpp) {
	del_set(lpp->cst2nr);
	del_set(lpp->var2nr);
	del_matrix(lpp->m);
	free(lpp->csts);
	free(lpp->vars);
	free(lpp->rhs);
	free(lpp);
}

void lpp_add_constr(lpp_t *lpp, const char *cst_name, cst_t cst_type) {
	name_t n, *inner;
	n.name = xstrdup(cst_name);
	n.nr = lpp->cst_next++;
	n.type.cst_type = cst_type;
	inner = set_insert(lpp->cst2nr, &n, sizeof(n), HASH_NAME_T(&n));
	assert(inner);
	if (lpp->cst_next == lpp->cst_size)
		lpp->csts = xrealloc(lpp->csts, 2 * lpp->cst_size * sizeof(*lpp->csts));
	lpp->csts[lpp->cst_next++] = inner;
}

void lpp_add_var(lpp_t *lpp, const char *var_name, var_t var_type) {
	name_t n, *inner;
	assert(var_type != invalid && "invalid is for internal use only");
	n.name = xstrdup(var_name);
	n.nr = lpp->var_next++;
	n.type.var_type = var_type;
	inner = set_insert(lpp->var2nr, &n, sizeof(n), HASH_NAME_T(&n));
	assert(inner);
	if (lpp->var_next == lpp->var_size)
		lpp->vars = xrealloc(lpp->vars, 2 * lpp->var_size * sizeof(*lpp->vars));
	lpp->vars[lpp->var_next++] = inner;
}

static INLINE int name2nr(set *where, const char *name) {
	name_t find, *found;
	find.name = name;
	found = set_find(where, &find, sizeof(find), HASH_NAME_T(&find));
	if (found)
		return found->nr;
	else
		return -1;
}

#define cst_nr(lp, name) name2nr(lpp->cst2nr, name)
#define var_nr(lp, name) name2nr(lpp->var2nr, name)

void lpp_set_factor(lpp_t *lpp, const char *cst_name, const char *var_name, int value) {
	int cst, var;
	cst = cst_nr(lp, cst_name);
	var = var_nr(lp, var_name);
	assert(cst != -1);
	assert(var != -1);
	matrix_set(lpp->m, cst, var, value);
}

static INLINE void adjust_rhs_size(lpp_t *lpp) {
	if (lpp->cst_size > lpp->rhs_size) {
		lpp->rhs_size = lpp->cst_size;
		lpp->rhs = xrealloc(lpp->rhs, lpp->rhs_size);
	}
}

void lpp_set_rhs(lpp_t *lpp, char *cst_name, int value) {
	int cst = cst_nr(lp, cst_name);
	assert(cst != -1);
	adjust_rhs_size(lpp);
	lpp->rhs[cst] = value;
}

void lpp_set_all_constr(lpp_t *lpp, char *cst_name, char **var_names, int *values) {
	assert(0 && "NYI");
}

/******************************************************************************
       MPS-STUFF
******************************************************************************/

static char *mps_cst_encoding[4] = {"N", "E", "L", "G"};
typedef enum _mps_line_t {l_raw,
						  l_ind_name, l_ind_objs, l_ind_rows, l_ind_cols, l_ind_rhs, l_ind_end,
						  l_data_row, l_data_col1, l_data_col2, l_marker} mps_line_t;

static INLINE void mps_write_line(FILE *out, style_t style, mps_line_t line_type, ...) {
	va_list args;
	char *fmt = "";

	assert(style == s_mps_fixed || style == s_mps_free);
	va_start(args, line_type);

	if (style == s_mps_fixed) {
		/* white spaces are important! */
		switch (line_type) {
			case l_raw:			fmt = "%s\n"; break;
			case l_ind_name:	fmt = "NAME          %s\n"; break;
			case l_ind_objs:	fmt = "OBJSENSE\n"; break;
			case l_ind_rows:	fmt = "ROWS\n"; break;
			case l_ind_cols:	fmt = "COLUMNS\n"; break;
			case l_ind_rhs:		fmt = "RHS\n"; break;
			case l_ind_end:		fmt = "ENDATA\n"; break;
			case l_data_row:	fmt = " %-2s %-8s\n"; break; /* Field 1-2 */
			case l_data_col1:	fmt = "    %-8s  %-8s  %12d\n"; break; /* Field 2-4 */
			case l_data_col2:	fmt = "    %-8s  %-8s  %12d   %-8s  %12d\n"; break; /* Field 2-6 */
			case l_marker:		fmt = "    M%-7d  'MARKER'                 '%s'\n"; break; /* Field 2,3,5 */
			default: assert(0);
		}
	} else {
		switch (line_type) {
			case l_raw:			fmt = "%s\n"; break;
			case l_ind_name:	fmt = "NAME %s\n"; break;
			case l_ind_objs:	fmt = "OBJSENSE\n"; break;
			case l_ind_rows:	fmt = "ROWS\n"; break;
			case l_ind_cols:	fmt = "COLUMNS\n"; break;
			case l_ind_rhs:		fmt = "RHS\n"; break;
			case l_ind_end:		fmt = "ENDATA\n"; break;
			case l_data_row:	fmt = " %s\t%s\n"; break;
			case l_data_col1:	fmt = " %s\t%s\t%d\n"; break;
			case l_data_col2:	fmt = " %s\t%s\t%d\t%s\t%d\n"; break;
			case l_marker:		fmt = " M%d\t'MARKER'\t'%s'\n"; break;
			default: assert(0);
		}
	}

	vfprintf(out, fmt, args);
	va_end(args);
}

static INLINE int mps_insert_markers(FILE *out, style_t style, var_t curr, var_t last, int marker_nr) {
	assert(style == s_mps_fixed || style == s_mps_free);
	if (last != curr) {
		/* print end-marker for last */
		if (last == binary)
			mps_write_line(out, style, l_marker, marker_nr++, "INTEND");

		/* print begin-marker for curr */
		if (curr == binary)
			mps_write_line(out, style, l_marker, marker_nr++, "INTORG");
	}
	return marker_nr;
}

static void lpp_dump_mps(lpp_t *lpp, FILE *out, style_t style) {
	int i, count, marker_nr = 0;
	const name_t *curr;
	var_t last_type;
	assert(style == s_mps_fixed || style == s_mps_free);

	/* NAME */
	mps_write_line(out, style, l_ind_name, lpp->name);

	/* OBJSENSE */
	mps_write_line(out, style, l_ind_objs);
	if (lpp->opt_type == maximize)
		mps_write_line(out, style, l_raw, " MAX");

	/* ROWS */
	mps_write_line(out, style, l_ind_rows);
	for(i=0; i<lpp->cst_next; ++i) {
		curr = lpp->csts[i];
		mps_write_line(out, style, l_data_row, mps_cst_encoding[curr->type.cst_type], curr->name);
	}

	/* COLUMNS */
	mps_write_line(out, style, l_ind_cols);
	last_type = invalid;
	for(i=0; i<lpp->var_next; ++i) {
		const matrix_elem_t *elem, *before = NULL;
		curr = lpp->vars[i];

		/* markers */
		marker_nr = mps_insert_markers(out, style, last_type, curr->type.var_type, marker_nr);
		last_type = curr->type.var_type;

		/* participation in constraints */
		count = 0;
		matrix_foreach_in_col(lpp->m, curr->nr, elem) {
			if (count == 0) {
				before = elem;
				count = 1;
			} else {
				mps_write_line(out, style, l_data_col2, curr->name, lpp->csts[before->row]->name, before->val, lpp->csts[elem->row]->name, elem->val);
				count = 0;
			}
		}
		if (count == 1)
			mps_write_line(out, style, l_data_col1, curr->name, lpp->csts[before->row]->name, before->val);
	}
	mps_insert_markers(out, style, last_type, invalid, marker_nr); /* potential end-marker */

	/* RHS */
	mps_write_line(out, style, l_ind_rhs);
	for(i=0; i<lpp->rhs_size/2; ++i)
		mps_write_line(out, style, l_data_col2, "rhs", lpp->csts[2*i]->name, lpp->rhs[2*i], lpp->csts[2*i+1]->name, lpp->rhs[2*i+1]);
	if ((lpp->rhs_size & 1) == 1)
		mps_write_line(out, style, l_data_col1, "rhs", lpp->csts[lpp->rhs_size-1]->name, lpp->rhs[lpp->rhs_size-1]);

	/* ENDATA */
	mps_write_line(out, style, l_ind_end);
}

/******************************************************************************
       LP-STUFF
******************************************************************************/

static void lpp_dump_lp(lpp_t *lpp, FILE *out) {
	assert(0 && "NYI");
}

void lpp_dump(lpp_t *lpp, FILE *out, style_t style) {
	adjust_rhs_size(lpp);
	switch (style) {
		case s_mps_fixed:
		case s_mps_free:	lpp_dump_mps(lpp, out, style); break;
		case s_lp: 			lpp_dump_lp(lpp, out); break;
		default: assert(0);
	}
}
