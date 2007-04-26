/*
 * Copyrigth (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief   Iterators for the several collection types used in firm.
 *          Useful for formatted and unified dumping of collections of objects.
 * @author  Sebastian Hack
 * @date    29.11.2004
 * @version $Id$
 */
#ifndef FIRM_ADT_ITERATOR_H
#define FIRM_ADT_ITERATOR_H

#include "fourcc.h"

/**
 * The iterator magic word.
 */
#define ITERATOR_MAGIC FOURCC('I', 'T', 'E', 'R')

/**
 * Check, if some memory object appears to be an iterator.
 * @param ptr Some memory.
 * @return 1, if that memory area appears to be an iterator, 0 if not.
 */
#define is_iterator(ptr) (((const iterator_t *) (ptr))->magic == ITERATOR_MAGIC)

typedef struct _iterator_t {
	unsigned magic;
	void *(*start)(void *collection);
	void *(*next)(void *collection, void *curr);
	void (*finish)(void *collection, void *curr);
} iterator_t;

/**
 * An iterator implementation for linked lists.
 */
extern const iterator_t *list_iterator;

/**
 * An iterator implementation for psets.
 */
extern const iterator_t *pset_iterator;

#endif
