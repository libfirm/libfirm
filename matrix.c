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

pbqp_matrix *pbqp_matrix_copy_and_transpose(pbqp *pbqp, pbqp_matrix *m)
{
	unsigned     i;
	unsigned     j;
	unsigned     cols = m->cols;
	unsigned     rows = m->rows;
	unsigned     len  = rows * cols;
	pbqp_matrix *copy = obstack_alloc(&pbqp->obstack, sizeof(*copy) + sizeof(*copy->entries) * len);
	assert(copy);

	for (i = 0; i < rows; ++i) {
		for (j = 0; j < cols; ++j) {
			copy->entries[j*rows+i] = m->entries[i*cols+j];
		}
	}

	copy->cols = rows;
	copy->rows = cols;

	return copy;
}

void pbqp_matrix_transpose(pbqp *pbqp, pbqp_matrix *mat)
{
	unsigned len;

	assert(mat);
	len = mat->rows * mat->cols;

	pbqp_matrix *tmp = pbqp_matrix_copy_and_transpose(pbqp, mat);

	memcpy(mat, tmp, sizeof(*mat) + sizeof(*mat->entries) * len);

	obstack_free(&pbqp->obstack, tmp);
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
		sum->entries[i] = pbqp_add(sum->entries[i], summand->entries[i]);
	}
}

void pbqp_matrix_set(pbqp_matrix *mat, unsigned row, unsigned col, num value)
{
	assert(mat);
	assert(col < mat->cols);
	assert(row < mat->rows);

	mat->entries[row * mat->cols + col] = value;
}

num pbqp_matrix_get_col_min(pbqp_matrix *matrix, unsigned col_index, vector *flags)
{
	unsigned row_index;
	num min = INF_COSTS;

	assert(matrix);
	assert(flags);
	assert(matrix->rows == flags->len);

	unsigned col_len = matrix->cols;
	unsigned row_len = matrix->rows;

	for (row_index = 0; row_index < row_len; ++row_index) {
		/* Ignore virtual deleted columns. */
		if (flags->entries[row_index].data == INF_COSTS) continue;

		num elem = matrix->entries[row_index * col_len + col_index];

		if (elem < min) {
			min = elem;
		}
	}

	return min;
}

void pbqp_matrix_sub_col_value(pbqp_matrix *matrix, unsigned col_index,
		vector *flags, num value)
{
	unsigned row_index;

	assert(matrix);
	assert(flags);
	assert(matrix->rows == flags->len);

	unsigned col_len = matrix->cols;
	unsigned row_len = matrix->rows;

	for (row_index = 0; row_index < row_len; ++row_index) {
		/* inf - x = inf if x < inf */
		if (matrix->entries[row_index * col_len + col_index] == INF_COSTS && value
				!= INF_COSTS)
			continue;
		matrix->entries[row_index * col_len + col_index] -= value;
	}
}

num pbqp_matrix_get_row_min(pbqp_matrix *matrix, unsigned row_index, vector *flags)
{
	unsigned col_index;
	num min = INF_COSTS;

	assert(matrix);
	assert(flags);
	assert(matrix->cols == flags->len);

	unsigned len = flags->len;

	for (col_index = 0; col_index < len; ++col_index) {
		/* Ignore virtual deleted columns. */
		if (flags->entries[col_index].data == INF_COSTS) continue;

		num elem = matrix->entries[row_index * len + col_index];

		if (elem < min) {
			min = elem;
		}
	}

	return min;
}

void pbqp_matrix_sub_row_value(pbqp_matrix *matrix, unsigned row_index,
		vector *flags, num value)
{
	unsigned col_index;

	assert(matrix);
	assert(flags);
	assert(matrix->cols == flags->len);

	unsigned len = flags->len;

	for (col_index = 0; col_index < len; ++col_index) {
		/* inf - x = inf if x < inf */
		if (matrix->entries[row_index * len + col_index] == INF_COSTS && value
				!= INF_COSTS)
			continue;
		matrix->entries[row_index * len + col_index] -= value;
	}
}

int pbqp_matrix_is_zero(pbqp_matrix *mat, vector *src_vec, vector *tgt_vec)
{
	unsigned col_index;
	unsigned col_len;
	unsigned row_index;
	unsigned row_len;

	assert(mat);
	assert(src_vec);
	assert(tgt_vec);
	assert(mat->cols = tgt_vec->len);
	assert(mat->rows = src_vec->len);

	col_len = mat->cols;
	row_len = mat->rows;

	for (row_index = 0; row_index < row_len; ++row_index) {
		if (src_vec->entries[row_index].data == INF_COSTS) continue;
		for (col_index = 0; col_index < col_len; ++col_index) {
			if (tgt_vec->entries[col_index].data == INF_COSTS) continue;

			if (mat->entries[row_index * col_len + col_index] != 0) {
				return 0;
			}
		}
	}

	return 1;
}

void pbqp_matrix_add_to_all_cols(pbqp_matrix *mat, vector *vec)
{
	unsigned col_index;
	unsigned col_len;
	unsigned row_index;
	unsigned row_len;

	assert(mat);
	assert(vec);
	assert(mat->rows == vec->len);

	col_len = mat->cols;
	row_len = mat->rows;

	for (row_index = 0; row_index < row_len; ++row_index) {
		num value = vec->entries[row_index].data;
		for (col_index = 0; col_index < col_len; ++col_index) {
			mat->entries[row_index * col_len + col_index] = pbqp_add(
					mat->entries[row_index * col_len + col_index], value);
		}
	}
}

void pbqp_matrix_add_to_all_rows(pbqp_matrix *mat, vector *vec)
{
	unsigned col_index;
	unsigned col_len;
	unsigned row_index;
	unsigned row_len;

	assert(mat);
	assert(vec);
	assert(mat->cols == vec->len);

	col_len = mat->cols;
	row_len = mat->rows;

	for (row_index = 0; row_index < row_len; ++row_index) {
		for (col_index = 0; col_index < col_len; ++col_index) {
			num value = vec->entries[col_index].data;

			mat->entries[row_index * col_len + col_index] = pbqp_add(mat->entries[row_index * col_len + col_index], value);
		}
	}
}
