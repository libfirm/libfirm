/**
 * Author:      Daniel Grund
 * Date:		02.06.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include <stdarg.h>
#include <assert.h>
#include "mps.h"

/**
 * These must comply to the enum cst_t in lpp.h
 */
static char *mps_cst_encoding[4] = {"N", "E", "L", "G"};

/**
 * Diffferent line styles which can be used in a mps file
 */
typedef enum _mps_line_t {l_raw,
						  l_ind_name, l_ind_objs, l_ind_rows, l_ind_cols, l_ind_rhs, l_ind_end,
						  l_data_row, l_data_col1, l_data_col2, l_data_mst, l_marker} mps_line_t;

static void mps_write_line(FILE *out, style_t style, mps_line_t line_type, ...) {
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

void mps_write_mps(lpp_t *lpp, style_t style, FILE *out) {
	int i, count, marker_nr = 0;
	const name_t *curr;
	const matrix_elem_t *elem, *before = NULL;
	var_t last_type;
	assert(style == s_mps_fixed || style == s_mps_free);

	/* NAME */
	mps_write_line(out, style, l_ind_name, lpp->name);

	/* OBJSENSE */
	if (lpp->opt_type == maximize) {
		mps_write_line(out, style, l_ind_objs);
		mps_write_line(out, style, l_raw, " MAX");
	}

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
}

void mps_write_mst(lpp_t *lpp, style_t style, FILE *out) {
	int i;
	mps_write_line(out, style, l_ind_name, "");
	for (i=0; i<lpp->var_next; ++i) {
		const name_t *var = lpp->vars[i];
		if (var->value_kind == value_start)
			mps_write_line(out, style, l_data_mst, var->name, (double)var->value);
	}
	mps_write_line(out, style, l_ind_end);
}
