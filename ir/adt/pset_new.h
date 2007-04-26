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
 * @date    17.03.2007
 * @brief   hashset containing pointers
 * @author  Matthias Braun
 * @version $Id$
 *
 * @note This has been named pset_new_new for now until all code has been
 *       changed to use this instead of the old deprecated pset_new functions!
 *       This version performs better than pset in terms of speed and memory
 *       usage and allows multiple iterators over the set
 */
#ifndef FIRM_ADT_PSET_NEW_H
#define FIRM_ADT_PSET_NEW_H

#define HashSet          pset_new_t
#define HashSetIterator  pset_new_iterator_t
#define ValueType        void*
#define DO_REHASH
#include "hashset.h"
#undef DO_REHASH
#undef HashSet
#undef HashSetIterator
#undef ValueType

/**
 * Initializes a pset_new
 *
 * @param pset_new   Pointer to allocated space for the pset_new
 */
void pset_new_init(pset_new_t *pset_new);

/**
 * Initializes a pset_new
 *
 * @param pset_new                Pointer to allocated space for the pset_new
 * @param expected_elements   Number of elements expected in the pset_new (rougly)
 */
void pset_new_init_size(pset_new_t *pset_new, size_t expected_elements);

/**
 * Destroys a pset_new and frees the memory allocated for hashtable. The memory of
 * the pset_new itself is not freed.
 *
 * @param pset_new   Pointer to the pset_new
 */
void pset_new_destroy(pset_new_t *pset_new);

/**
 * Inserts an element into a pset_new.
 *
 * @param pset_new   Pointer to the pset_new
 * @param ptr    Pointer to insert into the pset_new
 * @returns      1 if the pointer was inserted, 0 if it was already there
 */
int pset_new_insert(pset_new_t *pset_new, void *ptr);

/**
 * Removes an element from a pset_new. Does nothing if the pset_new doesn't contain the
 * element.
 *
 * @param pset_new   Pointer to the pset_new
 * @param ptr    Pointer to remove from the pset_new
 */
void pset_new_remove(pset_new_t *pset_new, const void *ptr);

/**
 * Tests whether a pset_new contains a pointer
 *
 * @param pset_new   Pointer to the pset_new
 * @param ptr    The pointer to test
 * @returns      1 @p pset_new contains the @p ptr, 0 otherwise
 */
int pset_new_contains(const pset_new_t *pset_new, const void *ptr);

/**
 * Returns the number of pointers contained in the pset_new
 *
 * @param pset_new   Pointer to the pset_new
 * @returns      Number of pointers contained in the pset_new
 */
size_t pset_new_size(const pset_new_t *pset_new);

/**
 * Initializes a pset_new iterator. Sets the iterator before the first element in
 * the pset_new.
 *
 * @param iterator   Pointer to already allocated iterator memory
 * @param pset_new       Pointer to the pset_new
 */
void pset_new_iterator_init(pset_new_iterator_t *iterator, const pset_new_t *pset_new);

/**
 * Advances the iterator and returns the current element or NULL if all elements
 * in the pset_new have been processed.
 * @attention It is not allowed to use pset_new_insert or pset_new_remove while
 *            iterating over a pset_new; pset_new_remove_iter is allowed.
 *
 * @param iterator  Pointer to the pset_new iterator.
 * @returns         Next element in the pset_new or NULL
 */
void* pset_new_iterator_next(pset_new_iterator_t *iterator);

/**
 * Removes the element that the iterator currently points to from the hashset.
 *
 * @param pset_new      Pointer to the pset_new
 * @param iterator  Pointer to the iterator
 */
void pset_new_remove_iterator(pset_new_t *pset_new, const pset_new_iterator_t *iterator);

/**
 * Convenience macro for iterating over a pset_new.
 */
#define foreach_pset_new(pset_new, ptr, iter)    \
	for(pset_new_iterator_init(&iter, pset_new), \
		ptr = pset_new_iterator_next(&iter);     \
		ptr != NULL; ptr = pset_new_iterator_next(&iter))

#endif
