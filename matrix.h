#ifndef KAPS_MATRIX_H
#define KAPS_MATRIX_H

#include "matrix_t.h"

pbqp_matrix *pbqp_matrix_alloc(pbqp *pbqp, unsigned rows, unsigned cols);

/* Copy the given matrix. */
pbqp_matrix *pbqp_matrix_copy(pbqp *pbqp, pbqp_matrix *m);

/* sum += summand */
void pbqp_matrix_add(pbqp_matrix *sum, pbqp_matrix *summand);

void pbqp_matrix_set(pbqp_matrix *mat, unsigned row, unsigned col, num value);

#endif /* KAPS_MATRIX_H */
