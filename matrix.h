#ifndef KAPS_MATRIX_H
#define KAPS_MATRIX_H

#include "matrix_t.h"

/* Copy the given matrix. */
matrix *matrix_copy(pbqp *pbqp, matrix *m);

/* sum += summand */
void matrix_add(matrix *sum, matrix *summand);

#endif /* KAPS_MATRIX_H */
