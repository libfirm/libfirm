#include "gaussseidel.h"

#include "util.h"
#include "xmalloc.h"
#include <assert.h>
#include <math.h>
#include <string.h>

typedef struct {
	double   v;
	unsigned col_idx;
} col_val_t;

typedef struct {
	unsigned   c_cols;
	unsigned   n_cols;
	double     diag;
	col_val_t *cols;
} row_col_t;

struct gs_matrix_t {
	unsigned   c_rows;
	unsigned   n_zero_entries; /**< Upper bound on number of 0 entries */
	row_col_t *rows;
};

static void alloc_cols(row_col_t *row, unsigned c_cols)
{
	assert(c_cols > row->c_cols);
	row->c_cols = c_cols;
	row->cols   = XREALLOC(row->cols, col_val_t, c_cols);
}

static void alloc_rows(gs_matrix_t *m, unsigned c_rows, unsigned c_cols,
                       unsigned begin_init)
{
	assert(c_rows > m->c_rows);

	m->c_rows = c_rows;
	m->rows   = XREALLOC(m->rows, row_col_t, c_rows);

	for (unsigned i = begin_init; i < c_rows; ++i) {
		m->rows[i].c_cols = 0;
		m->rows[i].n_cols = 0;
		m->rows[i].diag   = 0.0;
		m->rows[i].cols   = NULL;
		if (c_cols > 0)
			alloc_cols(&m->rows[i], c_cols);
	}
}

gs_matrix_t *gs_new_matrix(unsigned n_init_rows, unsigned n_init_cols)
{
	gs_matrix_t *res = XMALLOCZ(gs_matrix_t);
	alloc_rows(res, n_init_rows, n_init_cols, 0);
	return res;
}

void gs_delete_matrix(gs_matrix_t *m)
{
	for (unsigned i = 0; i < m->c_rows; ++i) {
		if (m->rows[i].c_cols)
			free(m->rows[i].cols);
	}
	if (m->c_rows)
		free(m->rows);
	free(m);
}

unsigned gs_matrix_get_n_entries(const gs_matrix_t *m)
{
	unsigned n_entries = 0;
	for (unsigned i = 0; i < m->c_rows; ++i) {
		n_entries += m->rows[i].n_cols;
		n_entries += (m->rows[i].diag != 0.0) ? 1 : 0;
	}

	return n_entries - m->n_zero_entries;
}

void gs_matrix_set(gs_matrix_t *m, unsigned row, unsigned col, double val)
{
	assert(row < m->c_rows);
	assert(col < m->c_rows);
	row_col_t *the_row = &m->rows[row];

	if (row == col) {
		/* Note that we store the diagonal inverted to turn divisions to mults
		 * in matrix_gauss_seidel(). */
		assert(val != 0.0);
		the_row->diag = 1.0 / val;
		return;
	}

	/* Search for correct column */
	col_val_t *cols = the_row->cols;
	unsigned   min  = 0;
	unsigned   max  = the_row->n_cols;
	unsigned   c    = max/2;
	while (min < max) {
		unsigned idx = cols[c].col_idx;
		if (idx < col)
			min = MAX(c, min+1);
		else if (idx > col)
			max = MIN(c, max-1);
		else
			break;
		c = (min+max)/2;
	}

	/* Have we found the entry? */
	if (c < the_row->n_cols && the_row->cols[c].col_idx == col) {
		the_row->cols[c].v = val;
		if (val == 0.0)
			m->n_zero_entries++;
		return;
	}

	/* We haven't found the entry, so we must create a new one.
	 * Is there enough space? */
	if (the_row->n_cols <= the_row->c_cols)
		alloc_cols(the_row, the_row->c_cols + 16);

	/* Shift right-most entries to the right by one */
	for (unsigned i = the_row->n_cols; i > c; --i)
		the_row->cols[i] = the_row->cols[i-1];

	/* Finally insert the new entry */
	the_row->n_cols++;
	the_row->cols[c].col_idx = col;
	the_row->cols[c].v = val;

	/* Check that the entries are sorted */
	assert(c==0 || the_row->cols[c-1].col_idx < the_row->cols[c].col_idx);
	assert(c>=the_row->n_cols-1
	    || the_row->cols[c].col_idx < the_row->cols[c+1].col_idx);
}

double gs_matrix_get(const gs_matrix_t *m, unsigned row, unsigned col)
{
	assert(row < m->c_rows);
	row_col_t *the_row = &m->rows[row];
	if (row == col)
		return the_row->diag != 0.0 ? 1.0 / the_row->diag : 0.0;

	/* Search for correct column */
	unsigned c;
	for (c = 0; c < the_row->n_cols && the_row->cols[c].col_idx < col; ++c) {
	}

	if (c >= the_row->n_cols || the_row->cols[c].col_idx > col)
		return 0.0;

	assert(the_row->cols[c].col_idx == col);
	return the_row->cols[c].v;
}

/* NOTE: You can slice out miss_rate and weights.
 * This does ONE step of gauss_seidel. Termination must be checked outside!
 * This solves m*x=0. You must add stuff for m*x=b. See wikipedia german and english article. Should be simple.
 * param a is the number of rows in the matrix that should be considered.
 *
 * Note that the diagonal element is stored separately in this matrix implementation.
 * */
double gs_matrix_gauss_seidel(const gs_matrix_t *m, double *x)
{
	double res = 0.0;
	unsigned n = m->c_rows;

	for (unsigned r = 0; r < n; ++r) {
		row_col_t *row  = &m->rows[r];
		col_val_t *cols = row->cols;

		double sum = 0.0;
		for (unsigned c = 0; c < row->n_cols; ++c) {
			unsigned col_idx = cols[c].col_idx;
			sum += cols[c].v * x[col_idx];
		}

		double old = x[r];
		double nw  = - sum * row->diag;
		/* nw = old - overdrive * (old + sum * row->diag); */
		res += fabs(old - nw);
		x[r] = nw;
	}

	return res;
}

void gs_matrix_dump(const gs_matrix_t *m, FILE *out)
{
	unsigned size  = m->c_rows;
	double  *elems = XMALLOCN(double, size);

	/* The rows which have some content */
	for (unsigned r = 0; r < size; ++r) {
		row_col_t *row = &m->rows[r];

		memset(elems, 0, size * sizeof(*elems));

		for (unsigned c = 0; c < row->n_cols; ++c) {
			unsigned col_idx = row->cols[c].col_idx;
			elems[col_idx] = row->cols[c].v;
		}
		elems[r] = row->diag != 0.0 ? 1.0 / row->diag : 0.0;

		for (unsigned i = 0; i < size; ++i) {
			if (elems[i] != 0.0)
				fprintf(out, "%+4.4f ", elems[i]);
			else
				fprintf(out, "        ");
		}
		fprintf(out, "\n");
	}

	free(elems);
}
