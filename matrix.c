#include "assert.h"

#include "pbqp_t.h"
#include "matrix.h"

matrix *matrix_copy(pbqp *pbqp, matrix *m)
{
	int i;
	int len;
	matrix *copy = obstack_alloc(&pbqp->obstack, sizeof(*copy));

	assert(copy);

	len = m->rows * m->cols;

	for (i = 0; i < len; ++i) {
		copy->entries[i] = m->entries[i];
	}

	return copy;
}
