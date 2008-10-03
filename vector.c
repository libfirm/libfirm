#include <string.h>

#include "adt/array.h"

#include "pbqp_t.h"
#include "vector.h"

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
	vector *copy = obstack_alloc(&pbqp->obstack, sizeof(*copy) + sizeof(*copy->entries) * length);

	assert(copy);

	unsigned len = v->len;

	copy->len = len;
	memcpy(copy->entries, v->entries, sizeof(*copy->entries) * len);

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
		sum->entries[i].data += summand->entries[i].data;
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
