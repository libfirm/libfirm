#include "assert.h"

#include "pbqp_t.h"
#include "matrix.h"

pbqp_matrix *pbqp_matrix_alloc(pbqp *pbqp, unsigned rows, unsigned cols)
{
	pbqp_matrix *mat;
	unsigned index;

	assert(cols> 0);
	assert(rows> 0);

	unsigned length = rows * cols;

	mat = obstack_alloc(&pbqp->obstack, sizeof(*mat) + sizeof(num) * (length - 1));
	assert(mat);

	mat->cols = cols;
	mat->rows = rows;
	for (index = 0; index < length; ++index) {
		mat->entries[index] = 0;
	}

	return mat;
}

pbqp_matrix *pbqp_matrix_copy(pbqp *pbqp, pbqp_matrix *m)
{
	int i;
	int len;
	pbqp_matrix *copy = obstack_alloc(&pbqp->obstack, sizeof(*copy));

	assert(copy);

	len = m->rows * m->cols;

	for (i = 0; i < len; ++i) {
		copy->entries[i] = m->entries[i];
	}

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
