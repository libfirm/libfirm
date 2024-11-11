/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
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

#endif
