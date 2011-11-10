/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   PBQP matrix.
 * @date    02.10.2008
 * @author  Sebastian Buchwald
 */
#ifndef KAPS_MATRIX_H
#define KAPS_MATRIX_H

#include "matrix_t.h"

pbqp_matrix_t *pbqp_matrix_alloc(pbqp_t *pbqp, unsigned rows, unsigned cols);

/* Copy the given matrix. */
pbqp_matrix_t *pbqp_matrix_copy(pbqp_t *pbqp, pbqp_matrix_t *m);

pbqp_matrix_t *pbqp_matrix_copy_and_transpose(pbqp_t *pbqp, pbqp_matrix_t *m);

void pbqp_matrix_transpose(pbqp_t *pbqp, pbqp_matrix_t *mat);

/* sum += summand */
void pbqp_matrix_add(pbqp_matrix_t *sum, pbqp_matrix_t *summand);

void pbqp_matrix_set(pbqp_matrix_t *mat, unsigned row, unsigned col, num value);

num pbqp_matrix_get_col_min(pbqp_matrix_t *matrix, unsigned col_index, vector_t *flags);
num pbqp_matrix_get_row_min(pbqp_matrix_t *matrix, unsigned row_index, vector_t *flags);

unsigned pbqp_matrix_get_col_min_index(pbqp_matrix_t *matrix, unsigned col_index, vector_t *flags);
unsigned pbqp_matrix_get_row_min_index(pbqp_matrix_t *matrix, unsigned row_index, vector_t *flags);

void pbqp_matrix_set_col_value(pbqp_matrix_t *mat, unsigned col, num value);
void pbqp_matrix_set_row_value(pbqp_matrix_t *mat, unsigned row, num value);

void pbqp_matrix_sub_col_value(pbqp_matrix_t *matrix, unsigned col_index,
                               vector_t *flags, num value);
void pbqp_matrix_sub_row_value(pbqp_matrix_t *matrix, unsigned row_index,
                               vector_t *flags, num value);

int pbqp_matrix_is_zero(pbqp_matrix_t *mat, vector_t *src_vec, vector_t *tgt_vec);

void pbqp_matrix_add_to_all_cols(pbqp_matrix_t *mat, vector_t *vec);
void pbqp_matrix_add_to_all_rows(pbqp_matrix_t *mat, vector_t *vec);

#endif /* KAPS_MATRIX_H */
