#ifndef KAPS_MATRIX_H
#define KAPS_MATRIX_H

#include "matrix_t.h"

pbqp_matrix *pbqp_matrix_alloc(pbqp *pbqp, unsigned rows, unsigned cols);

/* Copy the given matrix. */
pbqp_matrix *pbqp_matrix_copy(pbqp *pbqp, pbqp_matrix *m);

pbqp_matrix *pbqp_matrix_copy_and_transpose(pbqp *pbqp, pbqp_matrix *m);

/* sum += summand */
void pbqp_matrix_add(pbqp_matrix *sum, pbqp_matrix *summand);

void pbqp_matrix_set(pbqp_matrix *mat, unsigned row, unsigned col, num value);

num pbqp_matrix_get_col_min(pbqp_matrix *matrix, unsigned col_index, vector *flags);
num pbqp_matrix_get_row_min(pbqp_matrix *matrix, unsigned row_index, vector *flags);

void pbqp_matrix_sub_col_value(pbqp_matrix *matrix, unsigned col_index,
		vector *flags, num value);
void pbqp_matrix_sub_row_value(pbqp_matrix *matrix, unsigned row_index,
		vector *flags, num value);

int pbqp_matrix_is_zero(pbqp_matrix *mat, vector *src_vec, vector *tgt_vec);

#endif /* KAPS_MATRIX_H */
