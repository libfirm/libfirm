#include <assert.h>
#include <string.h>

#include "pbqp_t.h"
#include "matrix.h"

pbqp_matrix *pbqp_matrix_alloc(pbqp *pbqp, unsigned rows, unsigned cols)
{
	assert(cols> 0);
	assert(rows> 0);

	unsigned length = rows * cols;

	pbqp_matrix *mat = obstack_alloc(&pbqp->obstack, sizeof(*mat) + sizeof(*mat->entries) * length);
	assert(mat);

	mat->cols = cols;
	mat->rows = rows;
	memset(mat->entries, 0, sizeof(*mat->entries) * length);

	return mat;
}

pbqp_matrix *pbqp_matrix_copy(pbqp *pbqp, pbqp_matrix *m)
{
	unsigned     len  = m->rows * m->cols;
	pbqp_matrix *copy = obstack_copy(&pbqp->obstack, m, sizeof(*copy) + sizeof(*copy->entries) * len);
	assert(copy);

	return copy;
}

void pbqp_matrix_add(pbqp_matrix *sum, pbqp_matrix *summand)
{
	int i;
	int len;

	assert(sum);
	assert(summand);
	assert(sum->cols == summand->cols);
	assert(sum->rows == summand->rows);

	len = sum->rows * sum->cols;

	for (i = 0; i < len; ++i) {
		sum->entries[i] += summand->entries[i];
	}
}

void pbqp_matrix_set(pbqp_matrix *mat, unsigned row, unsigned col, num value)
{
	assert(mat);
	assert(col < mat->cols);
	assert(row < mat->rows);

	mat->entries[row * mat->cols + col] = value;
}
