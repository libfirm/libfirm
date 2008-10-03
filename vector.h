#ifndef KAPS_VECTOR_H
#define KAPS_VECTOR_H

#include "vector_t.h"

/* Copy the given vector. */
vector *vector_copy(pbqp *pbqp, vector *v);

/* sum += summand */
void vector_add(vector *sum, vector *summand);

#endif /* KAPS_VECTOR_H */
