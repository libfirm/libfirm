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

vector *vector_alloc(pbqp *pbqp, unsigned length);

/* Copy the given vector. */
vector *vector_copy(pbqp *pbqp, vector *v);

/* sum += summand */
void vector_add(vector *sum, vector *summand);

void vector_set(vector *vec, unsigned index, num value);

#if KAPS_ENABLE_VECTOR_NAMES
void vector_set_description(vector *vec, unsigned index, const char *name);
#endif

void vector_add_value(vector *vec, num value);

void vector_add_matrix_col(vector *vec, pbqp_matrix *mat, unsigned col_index);
void vector_add_matrix_row(vector *vec, pbqp_matrix *mat, unsigned row_index);

num vector_get_min(vector *vec);
unsigned vector_get_min_index(vector *vec);

#endif /* KAPS_VECTOR_H */
