#include <string.h>

#include "adt/array.h"

#include "pbqp_t.h"
#include "vector.h"

num pbqp_add(num x, num y)
{
	if (x == INF_COSTS || y == INF_COSTS) return INF_COSTS;

	return x + y;
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

#if EXT_GRS_DEBUG
void vector_set_description(vector *vec, unsigned index, char *name)
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
