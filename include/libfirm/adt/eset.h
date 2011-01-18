/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @brief       a pointer hashset (WARNING: deprecated, use hashset_new.*
 *              instead)
 * @author      Hubert Schmid
 * @date        09.06.2002
 * @version     $Id$
 * @deprecated
 */
#ifndef FIRM_ADT_ESET_H
#define FIRM_ADT_ESET_H

#include "../begin.h"

/**
 * "eset" is a set of addresses. The addresses are used for element
 * compare and hash calculation.
 * The value "NULL" can not be stored, as it is used as internal sentinel.
 */
typedef struct eset eset;

/** Creates a new empty set. */
FIRM_API eset *eset_create(void);

/**
 * Creates a copy of the given set. Does NOT work if NULL is contained in source. */
FIRM_API eset *eset_copy(eset *source);

/** Deletes a set. */
FIRM_API void eset_destroy(eset *s);

/** Returns the number of elements in the set. */
FIRM_API size_t eset_count(eset *s);

/** Inserts an address into the set. */
FIRM_API void eset_insert(eset *s, void *p);

/** Checks, whether an address is element of a set. */
FIRM_API int eset_contains(eset *s, void *p);

/**
 * Starts the iteration over a set and returns the first element or NULL
 * if the set is empty.
 *
 * @note: It is NOT possible to add new elements while iterating through a set.
 */
FIRM_API void *eset_first(eset *s);

/**
 * Continues iteration through a set and returns the next element or NULL if the
 * iteration is finished.
 *
 * @note: It is NOT possible to add new elements while iterating through a set.
 */
FIRM_API void *eset_next(eset *s);

/** Inserts all elements of source into target (union).  Does NOT work if NULL is contained in source. */
FIRM_API void eset_insert_all(eset *target, eset *source);

#define eset_foreach(eset, type, iter) \
	for ((iter) = (type)eset_first((eset)); (iter) != NULL; (iter) = (type)eset_next((eset)))

#include "../end.h"

#endif
