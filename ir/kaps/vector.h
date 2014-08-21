/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   PBQP vector.
 * @date    02.10.2008
 * @author  Sebastian Buchwald
 */
#ifndef KAPS_VECTOR_H
#define KAPS_VECTOR_H

#include "vector_t.h"

num pbqp_add(num x, num y);

vector_t *vector_alloc(pbqp_t *pbqp, unsigned length);

/* Copy the given vector. */
vector_t *vector_copy(pbqp_t *pbqp, vector_t *v);

/* sum += summand */
void vector_add(vector_t *sum, vector_t *summand);

void vector_set(vector_t *vec, unsigned index, num value);

#if KAPS_ENABLE_VECTOR_NAMES
void vector_set_description(vector_t *vec, unsigned index, const char *name);
#endif

void vector_add_value(vector_t *vec, num value);

void vector_add_matrix_col(vector_t *vec, pbqp_matrix_t *mat, unsigned col_index);
void vector_add_matrix_row(vector_t *vec, pbqp_matrix_t *mat, unsigned row_index);

num vector_get_min(vector_t *vec);
unsigned vector_get_min_index(vector_t *vec);

#endif
