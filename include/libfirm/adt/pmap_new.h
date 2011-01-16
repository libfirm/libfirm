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
 * @date    17.03.2007
 * @brief   pointer-to-pointer map
 * @author  Olaf Liebe
 * @version $Id$
 *
 * @note Same as pset_new for sets. Adds a more useful iteration concept and
 *       allows deletion of entries.
 */
#ifndef FIRM_ADT_PMAP_NEW_H
#define FIRM_ADT_PMAP_NEW_H

#include "../begin.h"

typedef struct pmap_new_entry_t {
	/* We never return a pointer into the map. So the key pointer in the map
	 * (which used for hashing and comparison), can never be altered by the
	 * user. There is no need to make the pointer const.
	 * Also making the value that is pointed to const (like pmap) doesn't make
	 * sense, since the pointer is used as key and not the target value. */
	void *key;
	void *value;
} pmap_new_entry_t;

#define HashSet          pmap_new_t
#define HashSetIterator  pmap_new_iterator_t
#define ValueType        pmap_new_entry_t
#define DO_REHASH
#include "hashset.h"
#undef DO_REHASH
#undef HashSet
#undef HashSetIterator
#undef ValueType

typedef struct pmap_new_t           pmap_new_t;
typedef struct pmap_new_iterator_t  pmap_new_iterator_t;

/**
 * Returns the number of entries contained in the pmap_new
 *
 * @param    map  Pointer to the pmap_new
 * @returns       Number of entries contained in the pmap_new
 */
FIRM_API size_t pmap_new_size(const pmap_new_t *map);

/**
 * Initializes a pmap_new
 *
 * @param  map  Pointer to allocated space for the pmap_new
 */
FIRM_API void pmap_new_init(pmap_new_t *map);

/**
 * Initializes a pmap_new
 *
 * @param  map                Pointer to allocated space for the pmap_new
 * @param  expected_elements  Number of elements expected in the pmap_new
 */
FIRM_API void pmap_new_init_size(pmap_new_t *map, size_t expected_elements);

/**
 * Destroys a pmap_new and frees the memory allocated for hashtable. The memory
 * of the pmap_new itself is not freed.
 *
 * @param  map  Pointer to the pmap_new
 */
FIRM_API void pmap_new_destroy(pmap_new_t *map);

/**
 * Inserts an entry into a pmap_new. This will overwrite existing entries with
 * the same key.
 *
 * @param  map    Pointer to the pmap_new
 * @param  key    The entries key pointer
 * @param  value  The entries value pointer
 */
FIRM_API void pmap_new_insert(pmap_new_t *map, void *key, void *value);

/**
 * Removes an element from a pmap_new.
 *
 * @param    map  Pointer to the pmap_new
 * @param    key  The key pointer of the element to remove
 * @returns       1 if the entry was removed, 0 if there was no entry with
 *                that key
 */
FIRM_API int pmap_new_remove(pmap_new_t *map, const void *key);

/**
 * Find an entry within the pmap_new.
 *
 * @param    map  Pointer to the pmap_new
 * @param    key  The key pointer of the element to find
 * @returns       the element that was found or a null entry
 *                (use pmap_new_is_null to check for null)
 */
FIRM_API pmap_new_entry_t pmap_new_find(const pmap_new_t *map, const void *key);

/**
 * Get a value pointer from the pmap_new.
 *
 * @param    map  Pointer to the pmap_new
 * @param    key  The key pointer associated with the value to find
 * @returns       the value associated with the key or NULL
 *                (use pmap_new_find if NULL values are stored in the map)
 */
FIRM_API void *pmap_new_get(const pmap_new_t *map, const void *key);

/**
 * Tests whether a pmap_new contains an entry
 *
 * @param    map  Pointer to the pmap_new
 * @param    key  The key pointer of the entry to find
 * @returns       1 if the entry was found, 0 if not
 */
FIRM_API int pmap_new_contains(const pmap_new_t *map, const void *key);

/**
 * Initializes a pmap_new iterator. Sets the iterator before the first element
 * in the pmap_new.
 *
 * @param  it   Pointer to already allocated iterator memory
 * @param  map  Pointer to the pmap_new
 */
FIRM_API void pmap_new_iterator_init(pmap_new_iterator_t *it, const pmap_new_t *map);

/**
 * Advances the iterator and returns the current entry or a null entry if all
 * elements in the pset_new have been processed (use pmap_new_is_null to check
 * for null).
 *
 * @attention It is not allowed to use pmap_new_insert or pmap_new_remove while
 *            iterating over a pmap_new; pmap_new_remove_iter is allowed.
 *
 * @param    it  Pointer to the pmap_new iterator
 * @returns      Next entry in the pmap_new or a null entry
 */
FIRM_API pmap_new_entry_t pmap_new_iterator_next(pmap_new_iterator_t *it);

/**
 * Removes the entry that the iterator currently points to from the hashset.
 *
 * @param  map  Pointer to the pmap_new
 * @param  it   Pointer to the iterator
 */
FIRM_API void pmap_new_remove_iterator(pmap_new_t *map, const pmap_new_iterator_t *it);

/**
 * Tests if the given entry is a null entry.
 *
 * @param  entry  The entry to test
 */
FIRM_API int pmap_new_is_null(const pmap_new_entry_t entry);

/**
 * Convenience macro for iterating over a pmap_new.
 */
#define foreach_pmap_new(pmap_new, entry, iter) \
	for(pmap_new_iterator_init(&iter, pmap_new), \
		entry = pmap_new_iterator_next(&iter); \
		!pmap_new_is_null(entry); \
		entry = pmap_new_iterator_next(&iter))

#include "../end.h"

#endif
