/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   PBQP matrix.
 * @date    02.10.2008
 * @author  Sebastian Buchwald
 */
#include "matrix.h"

#include "pbqp_t.h"
#include "vector.h"
#include <assert.h>
#include <string.h>

pbqp_matrix_t *pbqp_matrix_alloc(pbqp_t *pbqp, unsigned rows, unsigned cols)
{
	assert(cols > 0);
	assert(rows > 0);

	unsigned length = rows * cols;
	pbqp_matrix_t *mat = (pbqp_matrix_t *)obstack_alloc(&pbqp->obstack, sizeof(*mat) + sizeof(*mat->entries) * length);

	mat->cols = cols;
	mat->rows = rows;
	memset(mat->entries, 0, sizeof(*mat->entries) * length);

	return mat;
}

pbqp_matrix_t *pbqp_matrix_copy(pbqp_t *pbqp, pbqp_matrix_t *m)
{
	unsigned       len  = m->rows * m->cols;
	pbqp_matrix_t *copy = (pbqp_matrix_t *)obstack_copy(&pbqp->obstack, m, sizeof(*copy) + sizeof(*copy->entries) * len);
	assert(copy);

	return copy;
}

pbqp_matrix_t *pbqp_matrix_copy_and_transpose(pbqp_t *pbqp, pbqp_matrix_t *m)
{
	unsigned       cols = m->cols;
	unsigned       rows = m->rows;
	unsigned       len  = rows * cols;
	pbqp_matrix_t *copy = (pbqp_matrix_t *)obstack_alloc(&pbqp->obstack, sizeof(*copy) + sizeof(*copy->entries) * len);

	for (unsigned i = 0; i < rows; ++i) {
		for (unsigned j = 0; j < cols; ++j) {
			copy->entries[j * rows + i] = m->entries[i * cols + j];
		}
	}

	copy->cols = rows;
	copy->rows = cols;

	return copy;
}

void pbqp_matrix_transpose(pbqp_t *pbqp, pbqp_matrix_t *mat)
{
	unsigned       len = mat->rows * mat->cols;
	pbqp_matrix_t *tmp = pbqp_matrix_copy_and_transpose(pbqp, mat);

	memcpy(mat, tmp, sizeof(*mat) + sizeof(*mat->entries) * len);

	obstack_free(&pbqp->obstack, tmp);
}

void pbqp_matrix_add(pbqp_matrix_t *sum, pbqp_matrix_t *summand)
{
	assert(sum->cols == summand->cols);
	assert(sum->rows == summand->rows);

	unsigned len = sum->rows * sum->cols;

	for (unsigned i = 0; i < len; ++i) {
		sum->entries[i] = pbqp_add(sum->entries[i], summand->entries[i]);
	}
}

void pbqp_matrix_set_col_value(pbqp_matrix_t *mat, unsigned col, num value)
{
	assert(col < mat->cols);

	unsigned row_len = mat->rows;

	for (unsigned row_index = 0; row_index < row_len; ++row_index) {
		mat->entries[row_index * mat->cols + col] = value;
	}
}

void pbqp_matrix_set_row_value(pbqp_matrix_t *mat, unsigned row, num value)
{
	assert(row < mat->rows);

	unsigned col_len = mat->cols;

	for (unsigned col_index = 0; col_index < col_len; ++col_index) {
		mat->entries[row * mat->cols + col_index] = value;
	}
}

void pbqp_matrix_set(pbqp_matrix_t *mat, unsigned row, unsigned col, num value)
{
	assert(col < mat->cols);
	assert(row < mat->rows);

	mat->entries[row * mat->cols + col] = value;
}

num pbqp_matrix_get_col_min(pbqp_matrix_t *matrix, unsigned col_index, vector_t *flags)
{
	num      min     = INF_COSTS;
	unsigned col_len = matrix->cols;
	unsigned row_len = matrix->rows;

	assert(row_len == flags->len);

	for (unsigned row_index = 0; row_index < row_len; ++row_index) {
		/* Ignore virtual deleted columns. */
		if (flags->entries[row_index].data == INF_COSTS) continue;

		num elem = matrix->entries[row_index * col_len + col_index];

		if (elem < min) {
			min = elem;
		}
	}

	return min;
}

unsigned pbqp_matrix_get_col_min_index(pbqp_matrix_t *matrix, unsigned col_index, vector_t *flags)
{
	unsigned min_index = 0;
	num      min       = INF_COSTS;
	unsigned col_len   = matrix->cols;
	unsigned row_len   = matrix->rows;

	assert(row_len == flags->len);

	for (unsigned row_index = 0; row_index < row_len; ++row_index) {
		/* Ignore virtual deleted columns. */
		if (flags->entries[row_index].data == INF_COSTS) continue;

		num elem = matrix->entries[row_index * col_len + col_index];

		if (elem < min) {
			min       = elem;
			min_index = row_index;
		}
	}

	return min_index;
}

void pbqp_matrix_sub_col_value(pbqp_matrix_t *matrix, unsigned col_index,
                               vector_t *flags, num value)
{
	unsigned col_len = matrix->cols;
	unsigned row_len = matrix->rows;

	assert(row_len == flags->len);

	for (unsigned row_index = 0; row_index < row_len; ++row_index) {
		if (flags->entries[row_index].data == INF_COSTS) {
			matrix->entries[row_index * col_len + col_index] = 0;
			continue;
		}
		/* inf - x = inf if x < inf */
		if (matrix->entries[row_index * col_len + col_index] == INF_COSTS
		    && value != INF_COSTS)
			continue;
		matrix->entries[row_index * col_len + col_index] -= value;
	}
}

num pbqp_matrix_get_row_min(pbqp_matrix_t *matrix, unsigned row_index, vector_t *flags)
{
	num      min = INF_COSTS;
	unsigned len = flags->len;

	assert(matrix->cols == len);

	for (unsigned col_index = 0; col_index < len; ++col_index) {
		/* Ignore virtual deleted columns. */
		if (flags->entries[col_index].data == INF_COSTS) continue;

		num elem = matrix->entries[row_index * len + col_index];

		if (elem < min) {
			min = elem;
		}
	}

	return min;
}

unsigned pbqp_matrix_get_row_min_index(pbqp_matrix_t *matrix, unsigned row_index, vector_t *flags)
{
	unsigned min_index = 0;
	num      min       = INF_COSTS;
	unsigned len       = flags->len;

	assert(matrix->cols == len);

	for (unsigned col_index = 0; col_index < len; ++col_index) {
		/* Ignore virtual deleted columns. */
		if (flags->entries[col_index].data == INF_COSTS) continue;

		num elem = matrix->entries[row_index * len + col_index];

		if (elem < min) {
			min = elem;
			min_index = col_index;
		}
	}

	return min_index;
}

void pbqp_matrix_sub_row_value(pbqp_matrix_t *matrix, unsigned row_index,
                               vector_t *flags, num value)
{
	unsigned col_len = matrix->cols;

	assert(col_len == flags->len);

	for (unsigned col_index = 0; col_index < col_len; ++col_index) {
		if (flags->entries[col_index].data == INF_COSTS) {
			matrix->entries[row_index * col_len + col_index] = 0;
			continue;
		}
		/* inf - x = inf if x < inf */
		if (matrix->entries[row_index * col_len + col_index] == INF_COSTS
		    && value != INF_COSTS)
			continue;
		matrix->entries[row_index * col_len + col_index] -= value;
	}
}

int pbqp_matrix_is_zero(pbqp_matrix_t *mat, vector_t *src_vec, vector_t *tgt_vec)
{
	unsigned col_len = mat->cols;
	unsigned row_len = mat->rows;

	assert(col_len == tgt_vec->len);
	assert(row_len == src_vec->len);

	for (unsigned row_index = 0; row_index < row_len; ++row_index) {
		if (src_vec->entries[row_index].data == INF_COSTS)
			continue;

		for (unsigned col_index = 0; col_index < col_len; ++col_index) {
			if (tgt_vec->entries[col_index].data == INF_COSTS)
				continue;

			if (mat->entries[row_index * col_len + col_index] != 0) {
				return 0;
			}
		}
	}

	return 1;
}

void pbqp_matrix_add_to_all_cols(pbqp_matrix_t *mat, vector_t *vec)
{
	unsigned col_len = mat->cols;
	unsigned row_len = mat->rows;

	assert(row_len == vec->len);

	for (unsigned row_index = 0; row_index < row_len; ++row_index) {
		num value = vec->entries[row_index].data;

		for (unsigned col_index = 0; col_index < col_len; ++col_index) {
			mat->entries[row_index * col_len + col_index] = pbqp_add(mat->entries[row_index * col_len + col_index], value);
		}
	}
}

void pbqp_matrix_add_to_all_rows(pbqp_matrix_t *mat, vector_t *vec)
{
	unsigned col_len = mat->cols;
	unsigned row_len = mat->rows;

	assert(col_len == vec->len);

	for (unsigned row_index = 0; row_index < row_len; ++row_index) {
		for (unsigned col_index = 0; col_index < col_len; ++col_index) {
			num value = vec->entries[col_index].data;

			mat->entries[row_index * col_len + col_index] = pbqp_add(mat->entries[row_index * col_len + col_index], value);
		}
	}
}
