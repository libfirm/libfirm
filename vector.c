/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   PBQP vector.
 * @date    02.10.2008
 * @author  Sebastian Buchwald
 * @version $Id$
 */
#include "config.h"

#include <string.h>

#include "adt/array.h"

#include "vector.h"

num pbqp_add(num x, num y)
{
	if (x == INF_COSTS || y == INF_COSTS) return INF_COSTS;

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

vector *vector_alloc(pbqp *pbqp, unsigned length)
{
	assert(length > 0);
	vector *vec = obstack_alloc(&pbqp->obstack, sizeof(*vec) + sizeof(*vec->entries) * length);
	assert(vec);

	vec->len = length;
	memset(vec->entries, 0, sizeof(*vec->entries) * length);

	return vec;
}

vector *vector_copy(pbqp *pbqp, vector *v)
{
	unsigned  len  = v->len;
	vector   *copy = obstack_copy(&pbqp->obstack, v, sizeof(*copy) + sizeof(*copy->entries) * len);
	assert(copy);

	return copy;
}

void vector_add(vector *sum, vector *summand)
{
	int i;
	int len;

	assert(sum);
	assert(summand);
	assert(sum->len == summand->len);

	len = sum->len;

	for (i = 0; i < len; ++i) {
		sum->entries[i].data = pbqp_add(sum->entries[i].data,
				summand->entries[i].data);
	}
}

void vector_set(vector *vec, unsigned index, num value)
{
	assert(index < vec->len);
	vec->entries[index].data = value;
}

#if KAPS_ENABLE_VECTOR_NAMES
void vector_set_description(vector *vec, unsigned index, const char *name)
{
	assert(index < vec->len);
	vec->entries[index].name = name;
}
#endif

void vector_add_value(vector *vec, num value)
{
	unsigned index;
	unsigned len;

	assert(vec);

	len = vec->len;

	for (index = 0; index < len; ++index) {
		vec->entries[index].data = pbqp_add(vec->entries[index].data, value);
	}
}

void vector_add_matrix_col(vector *vec, pbqp_matrix *mat, unsigned col_index)
{
	unsigned index;
	unsigned len;

	assert(vec);
	assert(mat);
	assert(vec->len == mat->rows);
	assert(col_index < mat->cols);

	len = vec->len;

	for (index = 0; index < len; ++index) {
		vec->entries[index].data = pbqp_add(vec->entries[index].data, mat->entries[index * mat->cols + col_index]);
	}
}

void vector_add_matrix_row(vector *vec, pbqp_matrix *mat, unsigned row_index)
{
	unsigned index;
	unsigned len;

	assert(vec);
	assert(mat);
	assert(vec->len == mat->cols);
	assert(row_index < mat->rows);

	len = vec->len;

	for (index = 0; index < len; ++index) {
		vec->entries[index].data = pbqp_add(vec->entries[index].data,
				mat->entries[row_index * mat->cols + index]);
	}
}

num vector_get_min(vector *vec)
{
	unsigned index;
	unsigned len;
	num      min = INF_COSTS;

	assert(vec);

	len = vec->len;
	assert(len > 0);

	for (index = 0; index < len; ++index) {
		num elem = vec->entries[index].data;

		if (elem < min) {
			min = elem;
		}
	}

	return min;
}

unsigned vector_get_min_index(vector *vec)
{
	unsigned index;
	unsigned len;
	unsigned min_index = 0;
	num      min       = INF_COSTS;

	assert(vec);

	len = vec->len;
	assert(len > 0);

	for (index = 0; index < len; ++index) {
		num elem = vec->entries[index].data;

		if (elem < min) {
			min = elem;
			min_index = index;
		}
	}

	return min_index;
}
