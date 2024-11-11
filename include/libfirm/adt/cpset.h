/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @date    16.03.2007
 * @brief   a set of pointers with a custom compare function
 * @author  Matthias Braun
 */
#ifndef FIRM_ADT_CPSET_H
#define FIRM_ADT_CPSET_H

/**
 * @ingroup adt
 * @defgroup Pointer Set (custom Compare)
 * A pointer set with user-definable compare function
 * @{
 */

/**
 * The type of a cpset compare function.
 *
 * @param p1   pointer to an element
 * @param p2   pointer to another element
 *
 * @return  1 if the elements are identically, zero else
 */
typedef int (*cpset_cmp_function) (const void *p1, const void *p2);

/**
 * The type of a cpset hash function.
 */
typedef unsigned (*cpset_hash_function) (const void *obj);

/** @cond PRIVATE */

#define HashSet          cpset_t
#define HashSetIterator  cpset_iterator_t
#define HashSetEntry     cpset_hashset_entry_t
#define ValueType        void*
#define ADDITIONAL_DATA  cpset_cmp_function cmp_function; cpset_hash_function hash_function;
#include "hashset.h"
#undef ADDITIONAL_DATA
#undef ValueType
#undef HashSetEntry
#undef HashSetIterator
#undef HashSet

/** @endcond */

/** a pointer set with custom compare function */
typedef struct cpset_t          cpset_t;
/** iterator over a pointer set with custom compare function
 * @see #cpset_t */
typedef struct cpset_iterator_t cpset_iterator_t;

/**
 * Initializes a cpset
 *
 * @param cpset           Pointer to allocated space for the cpset
 * @param hash_function   The hash function to use
 * @param cmp_function    The compare function to use
 */
void cpset_init(cpset_t *cpset, cpset_hash_function hash_function,
                cpset_cmp_function cmp_function);

/**
 * Initializes a cpset
 *
 * @param cpset              Pointer to allocated space for the cpset
 * @param hash_function      The hash function to use
 * @param cmp_function       The compare function to use
 * @param expected_elements  Number of elements expected in the cpset (roughly)
 */
void cpset_init_size(cpset_t *cpset, cpset_hash_function hash_function,
                     cpset_cmp_function cmp_function,
                     size_t expected_elements);

/**
 * Destroys a cpset and frees the memory allocated for hashtable. The memory of
 * the cpset itself is not freed.
 *
 * @param cpset   Pointer to the cpset
 */
void cpset_destroy(cpset_t *cpset);

/**
 * Inserts an element into a cpset.
 *
 * @param cpset   Pointer to the cpset
 * @param obj     Element to insert into the cpset
 * @returns       The element itself or a pointer to an existing element
 */
void* cpset_insert(cpset_t *cpset, void *obj);

/**
 * Removes an element from a cpset. Does nothing if the cpset doesn't contain the
 * element.
 *
 * @param cpset   Pointer to the cpset
 * @param obj     Pointer to remove from the cpset
 */
void cpset_remove(cpset_t *cpset, const void *obj);

/**
 * Tests whether a cpset contains a pointer
 *
 * @param cpset   Pointer to the cpset
 * @param obj     The pointer to find
 * @returns       An equivalent object to @p obj or NULL
 */
void *cpset_find(const cpset_t *cpset, const void *obj);

/**
 * Returns the number of pointers contained in the cpset
 *
 * @param cpset   Pointer to the cpset
 * @returns       Number of pointers contained in the cpset
 */
size_t cpset_size(const cpset_t *cpset);

/**
 * Initializes a cpset iterator. Sets the iterator before the first element in
 * the cpset.
 *
 * @param iterator   Pointer to already allocated iterator memory
 * @param cpset       Pointer to the cpset
 */
void cpset_iterator_init(cpset_iterator_t *iterator, const cpset_t *cpset);

/**
 * Advances the iterator and returns the current element or NULL if all elements
 * in the cpset have been processed.
 * @attention It is not allowed to use cpset_insert or cpset_remove while
 *            iterating over a cpset.
 *
 * @param iterator  Pointer to the cpset iterator.
 * @returns         Next element in the cpset or NULL
 */
void *cpset_iterator_next(cpset_iterator_t *iterator);

/**
 * Removed the element the iterator currently points to
 *
 * @param cpset     Pointer to the cpset
 * @param iterator  Pointer to the cpset iterator.
 */
void cpset_remove_iterator(cpset_t *cpset, const cpset_iterator_t *iterator);

/** @} */

#endif
