/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   PBQP vector.
 * @date    02.10.2008
 * @author  Sebastian Buchwald
 */
#include "vector.h"

#include "adt/array.h"
#include <string.h>

num pbqp_add(num x, num y)
{
	if (x == INF_COSTS || y == INF_COSTS)
		return INF_COSTS;

	num res = x + y;

#if !KAPS_USE_UNSIGNED
	/* No positive overflow. */
	assert(x < 0 || y < 0 || res >= x);
	assert(x < 0 || y < 0 || res >= y);
#endif

	/* No negative overflow. */
	assert(x > 0 || y > 0 || res <= x);
	assert(x > 0 || y > 0 || res <= y);

	/* Result is not infinity.*/
	assert(res < INF_COSTS);

	return res;
}

vector_t *vector_alloc(pbqp_t *pbqp, unsigned length)
{
	vector_t *vec = (vector_t *)obstack_alloc(&pbqp->obstack, sizeof(*vec) + sizeof(*vec->entries) * length);
	assert(length > 0);

	vec->len = length;
	memset(vec->entries, 0, sizeof(*vec->entries) * length);

	return vec;
}

vector_t *vector_copy(pbqp_t *pbqp, vector_t *v)
{
	unsigned  len  = v->len;
	vector_t *copy = (vector_t *)obstack_copy(&pbqp->obstack, v, sizeof(*copy) + sizeof(*copy->entries) * len);
	assert(copy);

	return copy;
}

void vector_add(vector_t *sum, vector_t *summand)
{
	unsigned len = sum->len;

	assert(len == summand->len);

	for (unsigned i = 0; i < len; ++i) {
		sum->entries[i].data = pbqp_add(sum->entries[i].data, summand->entries[i].data);
	}
}

void vector_set(vector_t *vec, unsigned index, num value)
{
	assert(index < vec->len);
	vec->entries[index].data = value;
}

#if KAPS_ENABLE_VECTOR_NAMES
void vector_set_description(vector_t *vec, unsigned index, const char *name)
{
	assert(index < vec->len);
	vec->entries[index].name = name;
}
#endif

void vector_add_value(vector_t *vec, num value)
{
	unsigned len = vec->len;

	for (unsigned index = 0; index < len; ++index) {
		vec->entries[index].data = pbqp_add(vec->entries[index].data, value);
	}
}

void vector_add_matrix_col(vector_t *vec, pbqp_matrix_t *mat, unsigned col_index)
{
	unsigned len = vec->len;

	assert(len == mat->rows);
	assert(col_index < mat->cols);

	for (unsigned index = 0; index < len; ++index) {
		vec->entries[index].data = pbqp_add(vec->entries[index].data, mat->entries[index * mat->cols + col_index]);
	}
}

void vector_add_matrix_row(vector_t *vec, pbqp_matrix_t *mat, unsigned row_index)
{
	unsigned len = vec->len;

	assert(len == mat->cols);
	assert(row_index < mat->rows);


	for (unsigned index = 0; index < len; ++index) {
		vec->entries[index].data = pbqp_add(vec->entries[index].data, mat->entries[row_index * mat->cols + index]);
	}
}

num vector_get_min(vector_t *vec)
{
	unsigned len = vec->len;
	num      min = INF_COSTS;

	assert(len > 0);

	for (unsigned index = 0; index < len; ++index) {
		num elem = vec->entries[index].data;

		if (elem < min) {
			min = elem;
		}
	}

	return min;
}

unsigned vector_get_min_index(vector_t *vec)
{
	unsigned len       = vec->len;
	unsigned min_index = 0;
	num      min       = INF_COSTS;

	assert(len > 0);

	for (unsigned index = 0; index < len; ++index) {
		num elem = vec->entries[index].data;

		if (elem < min) {
			min = elem;
			min_index = index;
		}
	}

	return min_index;
}
