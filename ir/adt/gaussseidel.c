#include <assert.h>
#include <math.h>
#include <string.h>
#include "xmalloc.h"
#include "gaussseidel.h"
#include "firm_config.h"
#include "util.h"

#define MAX(x,y)   ((x) > (y) ? (x) : (y))
#define MIN(x,y)   ((x) < (y) ? (x) : (y))

/**
 * The number of newly allocated rows (realloc)
 * when there is no more room. Must be >= 1.
 */
#define ROW_INCREASE_FACTOR 1.2

/**
 * The number of newly allocated cols (realloc)
 * when there is no more room. Must be >= 1.
 */
#define COL_INCREASE 2

typedef struct _col_val_t {
	double v;
	int col_idx;
} col_val_t;

typedef struct _row_col_t {
	int c_cols;
	int n_cols;
	double diag;
	col_val_t *cols;
} row_col_t;

struct _gs_matrix_t {
	int initial_col_increase;
	int c_rows;
	int n_zero_entries;           ///< Upper bound on number of entries equal to 0.0
	row_col_t *rows;
};

static INLINE void alloc_cols(row_col_t *row, int c_cols) {
	assert(c_cols > row->c_cols);
	row->c_cols = c_cols;
	row->cols   = xrealloc(row->cols, c_cols * sizeof(*row->cols));
}

static INLINE void alloc_rows(gs_matrix_t *m, int c_rows, int c_cols, int begin_init) {
	int i;
	assert(c_rows > m->c_rows);

	m->c_rows = c_rows;
	m->rows   = xrealloc(m->rows, c_rows * sizeof(*m->rows));

	for (i = begin_init; i < c_rows; ++i) {
		m->rows[i].c_cols = 0;
		m->rows[i].n_cols = 0;
		m->rows[i].diag   = 0.0;
		m->rows[i].cols   = NULL;
		if (c_cols > 0)
			alloc_cols(&m->rows[i], c_cols);
	}
}

gs_matrix_t *gs_new_matrix(int n_init_rows, int n_init_cols) {
	gs_matrix_t *res = xmalloc(sizeof(*res));
	memset(res, 0, sizeof(*res));
	if (n_init_rows < 16)
		n_init_rows = 16;
	res->initial_col_increase = n_init_cols;
	alloc_rows(res, n_init_rows, n_init_cols, 0);
	return res;
}

void gs_delete_matrix(gs_matrix_t *m) {
	int i;
	for (i = 0; i < m->c_rows; ++i) {
		if (m->rows[i].c_cols)
			xfree(m->rows[i].cols);
	}
	if (m->c_rows)
		xfree(m->rows);
	xfree(m);
}

unsigned gs_matrix_get_n_entries(const gs_matrix_t *m) {
	int i;
	unsigned n_entries = 0;

	for (i = 0; i < m->c_rows; ++i) {
		n_entries += m->rows[i].n_cols;
		n_entries += (m->rows[i].diag != 0.0) ? 1 : 0;
	}

	return n_entries - m->n_zero_entries;
}

int gs_matrix_get_sizeof_allocated_memory(const gs_matrix_t *m) {
	int i, n_col_val_ts = 0;
	for (i = 0; i < m->c_rows; ++i)
		n_col_val_ts += m->rows[i].c_cols;

	return n_col_val_ts * sizeof(col_val_t) + m->c_rows * sizeof(row_col_t) + sizeof(gs_matrix_t);
}

void gs_matrix_assure_row_capacity(gs_matrix_t *m, int row, int min_capacity) {
	row_col_t *the_row = &m->rows[row];
	if (the_row->c_cols < min_capacity)
		alloc_cols(the_row, min_capacity);
}

void gs_matrix_trim_row_capacities(gs_matrix_t *m) {
	int i;
	for (i = 0; i < m->c_rows; ++i) {
		row_col_t *the_row = &m->rows[i];
		if (the_row->c_cols) {
			the_row->c_cols    = the_row->n_cols;
			if (the_row->c_cols)
				the_row->cols = xrealloc(the_row->cols, the_row->c_cols * sizeof(*the_row->cols));
			else
				xfree(the_row->cols);
		}
	}
}

void gs_matrix_delete_zero_entries(gs_matrix_t *m) {
	int i, read_pos;
	for (i = 0; i < m->c_rows; ++i) {
		row_col_t *the_row = &m->rows[i];
		int write_pos = 0;

		for (read_pos = 0; read_pos < the_row->n_cols; ++read_pos)
			if (the_row->cols[read_pos].v != 0.0 && read_pos != write_pos)
				the_row->cols[write_pos++] = the_row->cols[read_pos];

		the_row->n_cols = write_pos;
	}
	m->n_zero_entries = 0;
}

void gs_matrix_set(gs_matrix_t *m, int row, int col, double val) {
	row_col_t *the_row;
	col_val_t *cols;
	int min, max, c, i;

	if (row >= m->c_rows) {
		int new_c_rows = (int)(ROW_INCREASE_FACTOR * row);
		alloc_rows(m, new_c_rows, m->initial_col_increase, m->c_rows);
	}

	the_row = &m->rows[row];

	if (row == col) {
		/* Note that we store the diagonal inverted to turn divisions to mults in
		 * matrix_gauss_seidel(). */
		assert(val != 0.0);
		the_row->diag = 1.0 / val;
		return;
	}

	// Search for correct column
	cols = the_row->cols;
	min  = 0;
	max  = the_row->n_cols;
	c    = (max+min)/2;
	while (min < max) {
		int idx = cols[c].col_idx;
		if (idx < col)
			min = MAX(c, min+1);
		else if (idx > col)
			max = MIN(c, max-1);
		else
			break;
		c = (max+min)/2;
	}

	// Have we found the entry?
	if (c < the_row->n_cols && the_row->cols[c].col_idx == col) {
		the_row->cols[c].v = val;
		if (val == 0.0)
			m->n_zero_entries++;
		return;
	}

	// We haven't found the entry, so we must create a new one.
	// Is there enough space?
	if (the_row->c_cols == the_row->n_cols)
		alloc_cols(the_row, the_row->c_cols + COL_INCREASE);

	// Shift right-most entries to the right by one
	for (i = the_row->n_cols; i > c; --i)
		the_row->cols[i] = the_row->cols[i-1];

	// Finally insert the new entry
	the_row->n_cols++;
	the_row->cols[c].col_idx = col;
	the_row->cols[c].v = val;

	// Check that the entries are sorted
	assert(c==0 || the_row->cols[c-1].col_idx < the_row->cols[c].col_idx);
	assert(c>=the_row->n_cols-1 || the_row->cols[c].col_idx < the_row->cols[c+1].col_idx);
}

double gs_matrix_get(const gs_matrix_t *m, int row, int col) {
	row_col_t *the_row;
	int c;

	if (row >= m->c_rows)
		return 0.0;

	the_row = &m->rows[row];

	if (row == col)
		return the_row->diag != 0.0 ? 1.0 / the_row->diag : 0.0;

	// Search for correct column
	for (c = 0; c < the_row->n_cols && the_row->cols[c].col_idx < col; ++c);

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
double gs_matrix_gauss_seidel(const gs_matrix_t *m, double *x, int n) {
	double res = 0.0;
	int r;

	assert(n <= m->c_rows);

	for (r = 0; r < n; ++r) {
		row_col_t *row  = &m->rows[r];
		col_val_t *cols = row->cols;
		double sum, old, nw;
		int c;

		sum = 0.0;
		for (c = 0; c < row->n_cols; ++c) {
			int col_idx = cols[c].col_idx;
			sum += cols[c].v * x[col_idx];
		}

		old  = x[r];
		nw   = - sum * row->diag;
		// nw   = old - overdrive * (old + sum * row->diag);
		res += fabs(old - nw);
		x[r] = nw;
	}

	return res;
}

void gs_matrix_export(const gs_matrix_t *m, double *nw, int size)
{
	int effective_rows = MIN(size, m->c_rows);
	int c, r;

	memset(nw, 0, size * size * sizeof(*nw));
	for (r=0; r < effective_rows; ++r) {
		row_col_t *row = &m->rows[r];
		int base       = r * size;

		assert(row->diag != 0.0);
		nw[base + r] = 1.0 / row->diag;
		for (c = 0; c < row->n_cols; ++c) {
			int col_idx = row->cols[c].col_idx;
			nw[base + col_idx] = row->cols[c].v;
		}
	}
}

void gs_matrix_dump(const gs_matrix_t *m, int a, int b, FILE *out) {
	int effective_rows = MIN(a, m->c_rows);
	int r, c, i;
	double *elems = xmalloc(b * sizeof(*elems));

	// The rows which have some content
	for (r=0; r < effective_rows; ++r) {
		row_col_t *row = &m->rows[r];

		memset(elems, 0, b * sizeof(*elems));

		for (c = 0; c < row->n_cols; ++c) {
			int col_idx = row->cols[c].col_idx;
			elems[col_idx] = row->cols[c].v;
		}
		elems[r] = row->diag != 0.0 ? 1.0 / row->diag : 0.0;

		for (i = 0; i < b; ++i)
			if (elems[i] != 0.0)
				fprintf(out, "%+4.4f ", elems[i]);
			else
				fprintf(out, "        ");
		fprintf(out, "\n");
	}

	// Append 0-rows to fit height of matrix
	for (r=effective_rows; r < a; ++r) {
		for (c=0; c < b; ++c)
				fprintf(out, "        ");
		fprintf(out, "\n");
	}

	xfree(elems);
}

void gs_matrix_self_test(int d) {
	int i, o;
	gs_matrix_t *m = gs_new_matrix(10, 10);

	for (i=0; i<d; ++i)
		for (o=0; o<d; ++o)
			gs_matrix_set(m, i, o, i*o);

	for (i=0; i<d; ++i)
		for (o=0; o<d; ++o)
			assert(gs_matrix_get(m, i, o) == i*o);
	gs_delete_matrix(m);
}
