/*
 * This file is part of libFirm.
 * Copyright (C) 2016 University of Karlsruhe.
 */

/**
 * @file
 * @author    Raphael von der Grün
 * @date      2016
 * @brief     A CV hash map.
 */
#ifndef _FIRM_CVHASHMAP_H_
#define _FIRM_CVHASHMAP_H_

#include "firm_types.h"
#include "cloning_vector_t.h"

typedef struct cv_hashmap_entry_t {
	cloning_vector_t cv;
	void    *data;
} cv_hashmap_entry_t;

#define HashSet          cv_hashmap_t
#define HashSetIterator  cv_hashmap_iterator_t
#define ValueType        cv_hashmap_entry_t
#define DO_REHASH
#include "hashset.h"
#undef DO_REHASH
#undef ValueType
#undef HashSetIterator
#undef HashSet

typedef struct cv_hashmap_t           cv_hashmap_t;
typedef struct cv_hashmap_iterator_t  cv_hashmap_iterator_t;

/**
 * Initializes a cv_hashmap with default size.
 *
 * @param cv_hashmap      Pointer to allocated space for the cv_hashmap
 */
void cv_hashmap_init(cv_hashmap_t *cv_hashmap);

/**
 * Initializes a cv_hashmap
 *
 * @param cv_hashmap         Pointer to allocated space for the cv_hashmap
 * @param expected_elements   Number of elements expected in the cv_hashmap
 *                            (roughly)
 */
void cv_hashmap_init_size(cv_hashmap_t *cv_hashmap,
                              size_t expected_elements);

/**
 * Destroys a cv_hashmap and frees the memory allocated for hashtable. The
 * memory of the cv_hashmap itself is not freed.
 *
 * @param cv_hashmap   Pointer to the cv_hashmap
 */
void cv_hashmap_destroy(cv_hashmap_t *cv_hashmap);

/**
 * Inserts a cv into a cv_hashmap.
 *
 * @param cv_hashmap  Pointer to the cv_hashmap
 * @param cv         cv to insert into the cv_hashmap
 * @param data         data to associate with the cv
 */
void cv_hashmap_insert(cv_hashmap_t *cv_hashmap, cloning_vector_t cv,
                           void *data);

/**
 * Removes a cv from a cv_hashmap. Does nothing if the cv_hashmap doesn't
 * contain the cv.
 *
 * @param cv_hashmap  Pointer to the cv_hashmap
 * @param cv         Node to remove from the cv_hashmap
 */
void cv_hashmap_remove(cv_hashmap_t *cv_hashmap, const cloning_vector_t cv);

/**
 * Tests whether a cv_hashmap contains a specific cv
 *
 * @param cv_hashmap   Pointer to the cv_hashmap
 * @param cv          The pointer to find
 * @returns             the associated data of the cv or NULL
 */
void *cv_hashmap_get(const cv_hashmap_t *cv_hashmap,
                         const cloning_vector_t cv);

#define cv_hashmap_get(type, self, cv) ((type*)cv_hashmap_get((self), (cv)))

/**
 * Returns the number of pointers contained in the cv_hashmap
 *
 * @param cv_hashmap   Pointer to the cv_hashmap
 * @returns             Number of pointers contained in the cv_hashmap
 */
size_t cv_hashmap_size(const cv_hashmap_t *cv_hashmap);

/**
 * Initializes a cv_hashmap iterator. Sets the iterator before the first
 * element in the cv_hashmap.
 *
 * @param iterator   Pointer to already allocated iterator memory
 * @param cv_hashmap       Pointer to the cv_hashmap
 */
void cv_hashmap_iterator_init(cv_hashmap_iterator_t *iterator,
                                  const cv_hashmap_t *cv_hashmap);

/**
 * Advances the iterator and returns the current element or NULL if all elements
 * in the cv_hashmap have been processed.
 * @attention It is not allowed to use cv_hashmap_insert or cv_hashmap_remove
 * while iterating over a cv_hashmap.
 *
 * @param iterator  Pointer to the cv_hashmap iterator.
 * @returns         Next element in the cv_hashmap or NULL
 */
cv_hashmap_entry_t cv_hashmap_iterator_next(
		cv_hashmap_iterator_t *iterator);

/**
 * Removes the element the iterator currently points to
 *
 * @param cv_hashmap  Pointer to the cv_hashmap
 * @param iterator     Pointer to the cv_hashmap iterator.
 */
void cv_hashmap_remove_iterator(cv_hashmap_t *cv_hashmap,
                                    const cv_hashmap_iterator_t *iterator);

#define foreach_ir_cv_hashmap(cv_hashmap, entry, iter) \
	for (cv_hashmap_iterator_init(&iter, cv_hashmap); (entry = cv_hashmap_iterator_next(&iter)).cv;)

#endif
