#include "adt/array.h"

#include "vector.h"

vector *vector_copy(pbqp *pbqp, vector *v)
{
	int i;
	int len;
	vector *copy = obstack_alloc(pbqp->obstack, sizeof(*copy));

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
		sum->entries[i] += summand->entries[i];
	}
}
