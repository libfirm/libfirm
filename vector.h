#ifndef KAPS_VECTOR_H
#define KAPS_VECTOR_H

#include "vector_t.h"

vector *vector_alloc(pbqp *pbqp, unsigned length);

/* Copy the given vector. */
vector *vector_copy(pbqp *pbqp, vector *v);

/* sum += summand */
void vector_add(vector *sum, vector *summand);

void vector_set(vector *vec, unsigned index, num value);

#if EXT_GRS_DEBUG
void vector_set_description(vector *vec, unsigned index, char *name);
#endif

void vector_add_value(vector *vec, num value);

void vector_add_matrix_col(vector *vec, pbqp_matrix *mat, unsigned col_index);
void vector_add_matrix_row(vector *vec, pbqp_matrix *mat, unsigned row_index);

unsigned vector_get_min_index(vector *vec);

#endif /* KAPS_VECTOR_H */
