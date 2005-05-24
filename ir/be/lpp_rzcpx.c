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
#include <sys/types.h>
#include <sys/stat.h>

#include "debug.h"
#include "lpp.h"
#include "xmalloc.h"
#include "assert.h"
#include "hashptr.h"
#include "set.h"
#include "sp_matrix.h"

/* CPLEX-account related stuff */
#undef DELETE_FILES		/**< deletes all dumped files after use. Files on server are always deleted. */
#define SSH_USER_HOST "kb61@sp-smp.rz.uni-karlsruhe.de"
#define SSH_PASSWD_FILE "/ben/daniel/.smppw"
#define EXPECT_FILENAME "runme" /* name of the expect-script */

typedef struct _name_t name_t;
typedef enum _value_kind_t {none=0, start, solution} value_kind_t;

#define DEBUG_LVL SET_LEVEL_1
static firm_dbg_module_t *dbg = NULL;


struct _name_t {
	char *name;					/**< the name of the var/constraint supplied by user */
	int nr;						/**< the col/row number in the matrix */
	value_kind_t value_kind;
	double value;
	union _type {
		var_t var_type;
		cst_t cst_type;
	} type;
};

struct _lpp_t {
	/* The problem data */
	char *name;						/**< A textual name for this problem */
	opt_t opt_type;					/**< Optimization direction */
	sp_matrix_t *m;					/**< The matrix holding objective, constraints and rhs */

	/* Cst/Var to Nr mapping */
	set *cst2nr;					/**< Holds name_t's for constraints */
	set *var2nr;					/**< Holds name_t's for variables */

	/* Nr to Cst/Var mapping */
	int cst_size, var_size;			/**< Size of the csts/vars-arrays below */
	int cst_next, var_next;			/**< Next free position in arrays below */
	name_t **csts;					/**< Pointers to the elements in the cst2nr set */
	name_t **vars;					/**< Pointers to the elements in the var2nr set */

	/* Solution stuff */
	char *error;
	sol_state_t sol_state;
	double sol_time;
	unsigned iterations;

	unsigned next_name_number;
};

#define HASH_NAME_T(n) HASH_STR((n)->name, strlen((n)->name))

static INLINE FILE *ffopen(const char *base, const char *ext, const char *mode) {
	FILE *out;
	char buf[1024];

	snprintf(buf, sizeof(buf), "%s.%s", base, ext);
	if (! (out = fopen(buf, mode))) {
		fprintf(stderr, "Cannot open file %s in mode %s\n", buf, mode);
		return NULL;
	}
	return out;
}

static int cmp_name_t(const void *x, const void *y, size_t size) {
	const name_t *n = x;
	const name_t *m = y;
	return strcmp(n->name, m->name);
}

#define INITIAL_SIZE 64

lpp_t *new_lpp(const char *name, opt_t opt_type) {
	dbg = firm_dbg_register("ir.be.copyoptilp");
	firm_dbg_set_mask(dbg, DEBUG_LVL);

	lpp_t *lpp = xcalloc(1, sizeof(*lpp));
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

int lpp_add_cst(lpp_t *lpp, const char *cst_name, cst_t cst_type, double rhs) {
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

const char *lpp_get_cst_name(lpp_t *lpp, int index) {
	DBG((dbg, LEVEL_2, "%d --> %s\n", index, lpp->csts[index]->name));
	return lpp->csts[index]->name;
}

int lpp_add_var(lpp_t *lpp, const char *var_name, var_t var_type, double obj) {
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

const char *lpp_get_var_name(lpp_t *lpp, int index) {
	DBG((dbg, LEVEL_2, "%d --> %s\n", index, lpp->vars[index]->name));
	return lpp->vars[index]->name;
}

int lpp_set_factor(lpp_t *lpp, char *cst_name, char *var_name, double value) {
	int cst, var;

	cst = cst_nr(lpp, cst_name);
	var = var_nr(lpp, var_name);
	assert(cst != -1 && var != -1);
	if (cst == -1 || var == -1)
		return -1;
	DBG((dbg, LEVEL_2, "%s[%d] %s[%d] %g\n", cst_name, cst, var_name, var, value));
	matrix_set(lpp->m, cst, var, value);
	return 0;
}

int lpp_set_factor_fast(lpp_t *lpp, int cst_idx, int var_idx, double value) {
	assert(cst_idx >= 0 && var_idx >= 0);
	assert(cst_idx < lpp->cst_next && var_idx < lpp->var_next);
	if (cst_idx >= lpp->cst_next || var_idx >= lpp->var_next || cst_idx < 0 || var_idx < 0)
		return -1;
	DBG((dbg, LEVEL_2, "%s[%d] %s[%d] %g\n", lpp->csts[cst_idx]->name, cst_idx, lpp->vars[var_idx]->name, var_idx, value));
	matrix_set(lpp->m, cst_idx, var_idx, value);
	return 0;
}

void lpp_set_start_value(lpp_t *lpp, int var_idx, double value) {
	assert(var_idx >= 0 && var_idx < lpp->var_next);
	DBG((dbg, LEVEL_2, "%d %s %g\n", var_idx, lpp->vars[var_idx]->name, value));
	lpp->vars[var_idx]->value = value;
	lpp->vars[var_idx]->value_kind = start;
}


/******************************************************************************
       MPS-STUFF
******************************************************************************/

/**
 * Fixed-column mps where spaces are allowed in identifiers :-0
 * Free mps where whitespace is a seperator :-)
 * LP format
 */
typedef enum _style_t {s_mps_fixed, s_mps_free} style_t;

static char *mps_cst_encoding[4] = {"N", "E", "L", "G"};
typedef enum _mps_line_t {l_raw,
						  l_ind_name, l_ind_objs, l_ind_rows, l_ind_cols, l_ind_rhs, l_ind_end,
						  l_data_row, l_data_col1, l_data_col2, l_data_mst, l_marker} mps_line_t;

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
			case l_data_col1:	fmt = "    %-8s  %-8s  %12g\n"; break; /* Field 2-4 */
			case l_data_col2:	fmt = "    %-8s  %-8s  %12g   %-8s  %12g\n"; break; /* Field 2-6 */
			case l_data_mst:	fmt = "    %-8s            %12g\n"; break; /* Field 3-4 */
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
			case l_data_col1:	fmt = " %s\t%s\t%g\n"; break;
			case l_data_col2:	fmt = " %s\t%s\t%g\t%s\t%g\n"; break;
			case l_data_mst:	fmt = " %s\t%g\n"; break;
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

static void lpp_write_cmd(lpp_t *lpp) {
	FILE *out = ffopen(lpp->name, "cmd", "wt");
	fprintf(out, "set logfile %s.sol\n", lpp->name);
	fprintf(out, "set mip strategy mipstart 1\n");
	fprintf(out, "set mip emphasis 3\n"); /* moving best bound */
	fprintf(out, "set mip strategy variableselect 3\n"); /* strong branching */
//	fprintf(out, "set mip strategy branch 1\n"); /* branch up first */
	fprintf(out, "read %s.mps\n", lpp->name);
	fprintf(out, "read %s.mst\n", lpp->name);
	fprintf(out, "optimize\n");
	fprintf(out, "display solution variables -\n");
	fprintf(out, "quit\n");
	fclose(out);
}

static void lpp_write_mps(lpp_t *lpp, style_t style) {
	FILE *out;
	int i, count, marker_nr = 0;
	const name_t *curr;
	const matrix_elem_t *elem, *before;
	var_t last_type;
	assert(style == s_mps_fixed || style == s_mps_free);

	out = ffopen(lpp->name, "mps", "wt");
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
	for(i=1; i<lpp->var_next; ++i) { /* column 0 is rhs */
		curr = lpp->vars[i];

		/* markers */
		marker_nr = mps_insert_markers(out, style, curr->type.var_type, last_type, marker_nr);
		last_type = curr->type.var_type;

		/* participation in constraints */
		count = 0;
		matrix_foreach_in_col(lpp->m, curr->nr, elem) {
			if (count == 0) {
				before = elem;
				count = 1;
			} else {
				mps_write_line(out, style, l_data_col2, curr->name, lpp->csts[before->row]->name, (double)before->val, lpp->csts[elem->row]->name, (double)elem->val);
				count = 0;
			}
		}
		if (count == 1)
			mps_write_line(out, style, l_data_col1, curr->name, lpp->csts[before->row]->name, (double)before->val);
	}
	mps_insert_markers(out, style, invalid, last_type, marker_nr); /* potential end-marker */

	/* RHS */
	mps_write_line(out, style, l_ind_rhs);
	count = 0;
	matrix_foreach_in_col(lpp->m, 0, elem) {
		if (count == 0) {
			before = elem;
			count = 1;
		} else {
			mps_write_line(out, style, l_data_col2, "rhs", lpp->csts[before->row]->name, (double)before->val, lpp->csts[elem->row]->name, (double)elem->val);
			count = 0;
		}
	}
	if (count == 1)
		mps_write_line(out, style, l_data_col1, "rhs", lpp->csts[before->row]->name, (double)before->val);

	/* ENDATA */
	mps_write_line(out, style, l_ind_end);
	fclose(out);
}

static void lpp_write_mst(lpp_t *lpp, style_t style) {
	int i;
	FILE *out = ffopen(lpp->name, "mst", "wt");
	mps_write_line(out, style, l_ind_name, "");
	for (i=0; i<lpp->var_next; ++i) {
		const name_t *var = lpp->vars[i];
		if (var->value_kind == start)
			mps_write_line(out, style, l_data_mst, var->name, (double)var->value);
	}
	mps_write_line(out, style, l_ind_end);
	fclose(out);
}

static void lpp_write_exp(lpp_t *lpp) {
	FILE *pwfile, *out;
	char passwd[128];

	pwfile = fopen(SSH_PASSWD_FILE, "rt");
	fgets(passwd, sizeof(passwd), pwfile);
	fclose(pwfile);

	out = ffopen(EXPECT_FILENAME, "exp", "wt");
	fprintf(out, "#! /usr/bin/expect\n");
	fprintf(out, "spawn scp %s.mps %s.mst %s.cmd %s:\n", lpp->name, lpp->name, lpp->name, SSH_USER_HOST); /* copy problem files */
	fprintf(out, "expect \"word:\"\nsend \"%s\\n\"\ninteract\n", passwd);

	fprintf(out, "spawn ssh %s \"./cplex90 < %s.cmd\"\n", SSH_USER_HOST, lpp->name); /* solve */
	fprintf(out, "expect \"word:\"\nsend \"%s\\n\"\ninteract\n", passwd);

	fprintf(out, "spawn scp %s:%s.sol .\n", SSH_USER_HOST, lpp->name); /*copy back solution */
	fprintf(out, "expect \"word:\"\nsend \"%s\\n\"\ninteract\n", passwd);

	fprintf(out, "spawn ssh %s ./dell\n", SSH_USER_HOST); /* clean files on server */
	fprintf(out, "expect \"word:\"\nsend \"%s\\n\"\ninteract\n", passwd);
	fclose(out);
}

/**
 * Sets the colors of irns according to the values of variables found in the
 * output file of the solver.
 */
static void lpp_read_solution(lpp_t *lpp) {
	FILE *in;
	double sol_time;
	unsigned iter;
	int vars_section = 0;
	char var_name[128];
	double var_value;

	if (!(in = ffopen(lpp->name, "sol", "rt"))) {
		lpp->sol_state = no_solution_file;
		return;
	}
	while (!feof(in)) {
		char buf[1024];
		fgets(buf, sizeof(buf), in);

		/* error and solution state */
		if (!strncmp(buf, "CPLEX Error", 11))
			lpp->error = xstrdup(buf);
		else if (!strncmp(buf, "Warning:", 8))
			lpp->error = xstrdup(buf);
		else if (!strncmp(buf, "Integer optimal solution:", 25))
			lpp->sol_state = optimal;
		else if (!strcmp(buf, "No integer feasible solution exists."))
			lpp->sol_state = infeasible;
		else if (!strcmp(buf, "Error termination, integer feasible:"))
			lpp->sol_state = feasible;
		/* stats */
		else if (sscanf(buf, "Solution time = %lg sec. Iterations = %u", &sol_time, &iter) == 2) {
			lpp->sol_time = sol_time;
			lpp->iterations = iter;
		}
		/* variable values */
		else if(!strcmp(buf, "Variable Name           Solution Value")) {
			int i;
			vars_section = 1;
			for(i=0; i<lpp->var_next; ++i) {
				name_t *var = lpp->vars[i];
				var->value = 0;
				var->value_kind = solution;
			}
		}
		else if(!strncmp(buf, "All other var", 13))
			vars_section = 0;
		else if (vars_section) {
			if (sscanf(buf, "%s %lg", var_name, &var_value) == 2)
				lpp->vars[var_nr(lpp, var_name)]->value = var_value;
			else
				assert(0 && "There should be variables to read in!");
		}
	}
	fclose(in);
	if (lpp->error) {
		printf("\n%s\n", lpp->error);
		assert(0);
	}
}

#ifdef DELETE_FILES
static void lpp_delete_files(lpp_t *lpp) {
	char buf[1024];
	int end = snprintf(buf, sizeof(buf), "%s", lpp->name);

	snprintf(buf+end, sizeof(buf)-end, ".mps");
	remove(buf);
	snprintf(buf+end, sizeof(buf)-end, ".mst");
	remove(buf);
	snprintf(buf+end, sizeof(buf)-end, ".cmd");
	remove(buf);
	snprintf(buf+end, sizeof(buf)-end, ".sol");
	remove(buf);
	remove(EXPECT_FILENAME ".exp");
}
#endif

int lpp_solve(lpp_t *lpp, int use_start_values) {
	lpp_write_cmd(lpp);
	lpp_write_mps(lpp, s_mps_free);
	if (use_start_values)
		lpp_write_mst(lpp, s_mps_free);
	lpp_write_exp(lpp);

	/* call the expect script */
	chmod(EXPECT_FILENAME ".exp", 0700);
	system(EXPECT_FILENAME ".exp");

	lpp_read_solution(lpp);
#ifdef DELETE_FILES
	lpp_delete_files(lpp);
#endif
	return 0;
}

sol_state_t lpp_get_solution(lpp_t *lpp, double *values, int begin, int end) {
	int i;
	if (lpp->sol_state < 4)
		return lpp->sol_state;
	/* here we are feasible or optimal */
	for (i=0; i<end-begin+1; ++i)
		values[i] = lpp->vars[begin+i]->value;
	return lpp->sol_state;
}
