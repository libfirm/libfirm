#include "adt/array.h"

#include "pbqp_t.h"
#include "vector.h"

vector *vector_alloc(pbqp *pbqp, unsigned length)
{
	vector *vec;
	unsigned index;

	assert(length > 0);
	vec = obstack_alloc(&pbqp->obstack, sizeof(*vec) + sizeof(vec_elem) * (length - 1));
	assert(vec);

	vec->len = length;
	for (index = 0; index < length; ++index) {
		vec->entries[index].data = 0;
#if EXT_GRS_DEBUG
		vec->entries[index].name = NULL;
#endif
	}

	return vec;
}

vector *vector_copy(pbqp *pbqp, vector *v)
{
	int i;
	int len;
	vector *copy = obstack_alloc(&pbqp->obstack, sizeof(*copy));

	assert(copy);

	len = v->len;

	for (i = 0; i < len; ++i) {
		copy->entries[i] = v->entries[i];
	}

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
