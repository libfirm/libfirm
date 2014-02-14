/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @date    17.03.2007
 * @brief   hashset containing pointers
 * @author  Matthias Braun
 *
 * @note This has been named pset_new_new for now until all code has been
 *       changed to use this instead of the old deprecated pset_new functions!
 *       This version performs better than pset in terms of speed and memory
 *       usage and allows multiple iterators over the set
 */
#ifndef FIRM_ADT_PSET_NEW_H
#define FIRM_ADT_PSET_NEW_H

#include <stdbool.h>

/** @cond PRIVATE */

#define HashSet          pset_new_t
#define HashSetIterator  pset_new_iterator_t
#define ValueType        void*
#define DO_REHASH
#include "hashset.h"
#undef DO_REHASH
#undef HashSet
#undef HashSetIterator
#undef ValueType

/** @endcond */

/** a pointer (hash)set */
typedef struct pset_new_t           pset_new_t;
/** iterator over a pointer set.
 * @see #pset_new_t */
typedef struct pset_new_iterator_t  pset_new_iterator_t;

/**
 * Initializes a pset_new
 *
 * @param pset_new   Pointer to allocated space for the pset_new
 */
void pset_new_init(pset_new_t *pset_new);

/**
 * Initializes a pset_new
 *
 * @param pset_new            Pointer to allocated space for the pset_new
 * @param expected_elements   Number of elements expected in the pset_new (roughly)
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
 * @returns      true if the pointer was inserted, false if it was already there
 */
bool pset_new_insert(pset_new_t *pset_new, void *ptr);

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
 */
bool pset_new_contains(const pset_new_t *pset_new, const void *ptr);

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
#define foreach_pset_new(pset_new, type, ptr, iter) \
	for (pset_new_iterator_init(&iter, pset_new); (ptr = (type)pset_new_iterator_next(&iter));)

#endif
