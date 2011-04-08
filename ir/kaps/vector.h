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
 * @brief   PBQP vector.
 * @date    02.10.2008
 * @author  Sebastian Buchwald
 * @version $Id$
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

#endif /* KAPS_VECTOR_H */
