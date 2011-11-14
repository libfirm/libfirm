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
 * @brief   PBQP vector data types.
 * @date    02.10.2008
 * @author  Sebastian Buchwald
 */
#include "config.h"
#ifndef KAPS_VECTOR_T_H
#define KAPS_VECTOR_T_H

#include "pbqp_t.h"

typedef struct vec_elem_t vec_elem_t;

struct vec_elem_t {
	num data;
#if KAPS_ENABLE_VECTOR_NAMES
	const char *name;
#endif
};

typedef struct vector_t vector_t;

struct vector_t {
	unsigned   len;
	vec_elem_t entries[];
};

#endif /* KAPS_VECTOR_T_H */
