/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       hashset: data structure containing objects accessible by their key
 * @author      Markus Armbruster
 */
#ifndef FIRM_ADT_SET_H
#define FIRM_ADT_SET_H

#include <stddef.h>

#include "../begin.h"

/**
 * @ingroup adt
 * @defgroup set Generic Hashset
 * Generic Hashset
 * @note This code has been deprecated. Use hashset for new code.
 * @{
 */

/**
 * The abstract type of a set.
 *
 * This sets stores copies of its elements, so there is no need
 * to store the elements after they were added to a set.
 *
 * @see pset
 */
typedef struct set set;

/** The entry of a set, representing an element in the set and its meta-information */
typedef struct set_entry {
	unsigned hash;  /**< the hash value of the element */
	size_t size;    /**< the size of the element */
	int dptr[1];    /**< the element itself, data copied in must not need more
	                     alignment than this */
} set_entry;

/**
 * The type of a set compare function.
 *
 * @param elt   pointer to an element
 * @param key   pointer to another element
 * @param size  size of the elements
 *
 * @return
 *    0 if the elements are identically, non-zero else
 *
 * @note
 *    Although it is possible to define different meanings of equality
 *    of two elements of a set, they can be only equal if their sizes are
 *    are equal. This is checked before the compare function is called.
 */
typedef int (*set_cmp_fun) (void const *elt, void const *key, size_t size);

/**
 * Creates a new set.
 *
 * @param func    The compare function of this set.
 * @param slots   Initial number of collision chains.  I.e., \#slots
 *                different keys can be hashed without collisions.
 *
 * @returns
 *    created set
 */
FIRM_API set *new_set(set_cmp_fun func, size_t slots);

/**
 * Deletes a set and all elements of it.
 *
 * @param set  the set to delete
 */
FIRM_API void del_set(set *set);

/**
 * Returns the number of elements in a set.
 *
 * @param set   the set
 */
FIRM_API size_t set_count(set const *set);

/**
 * Searches an element in a set.
 *
 * @param set   the set to search in
 * @param key   the element to is searched
 * @param size  the size of key
 * @param hash  the hash value of key
 * @return
 *    The address of the found element in the set or NULL if it was not found.
 */
FIRM_API void *set_find(set *set, void const *key, size_t size, unsigned hash);

/**
 * Inserts an element into a set.
 *
 * @param set   the set to insert in
 * @param key   a pointer to the element to be inserted.  Element is copied!
 * @param size  the size of the element that should be inserted
 * @param hash  the hash-value of the element
 *
 * @return a pointer to the inserted element
 *
 * @note
 *    It is not possible to insert one element more than once. If an element
 *    that should be inserted is already in the set, this functions does
 *    nothing but returning its pointer.
 */
FIRM_API void *set_insert(set *set, void const *key, size_t size, unsigned hash);

/**
 * Inserts an element into a set and returns its set_entry.
 *
 * @param set   the set to insert in
 * @param key   a pointer to the element to be inserted. Element is copied!
 * @param size  the size of the element that should be inserted
 * @param hash  the hash-value of the element
 *
 * @return a pointer to the set_entry of the inserted element
 *
 * @note
 *    It is not possible to insert an element more than once. If an element
 *    that should be inserted is already in the set, this functions does
 *    nothing but returning its set_entry.
 */
FIRM_API set_entry *set_hinsert(set *set, void const *key, size_t size,
                                unsigned hash);

/**
 * Inserts an element into a set, zero-terminate it and returns its set_entry.
 *
 * @param set   the set to insert in
 * @param key   a pointer to the element to be inserted.  Element is copied!
 * @param size  the size of the element that should be inserted
 * @param hash  the hash-value of the element
 *
 * @return a pointer to the set_entry of the inserted element
 *
 * @note
 *    It is not possible to insert on element more than once. If an element
 *    that should be inserted is already in the set, this functions does
 *    nothing but returning its set_entry.
 */
FIRM_API set_entry *set_hinsert0(set *set, void const *key, size_t size,
                                 unsigned hash);

/**
 * Returns the first element of a set.
 *
 * @param set  the set to iterate
 *
 * @return a pointer to the element or NULL if the set is empty
 */
FIRM_API void *set_first(set *set);

/**
 * Returns the first element of a set.
 * This is a wrapper for set_first(set); It allows to express the
 * intended type of the set elements (instead of weakly typed void*).
 *
 * @param set  the set to iterate
 * @param type type of the set elements
 *
 * @return a pointer to the element or NULL if the set is empty
 */
#define set_first(type, set) ((type*)set_first((set)))

/**
 * Returns the next element of a set.
 *
 * @param set  the set to iterate
 *
 * @return a pointer to the next element or NULL if the
 *         iteration is finished
 */
FIRM_API void *set_next(set *set);

/**
 * Returns the next element of a set.
 * This is a wrapper for set_next(set); It allows to express the
 * intended type of the set elements (instead of weakly typed void*).
 *
 * @param set  the set to iterate
 * @param type type of the set elements
 *
 * @return a pointer to the next element or NULL if the
 *         iteration is finished
 */
#define set_next(type, set) ((type*)set_next((set)))

/**
 * Breaks the iteration of a set. Must be called before
 * the next set_first() call if the iteration was NOT
 * finished.
 *
 * @param set  the set
 */
FIRM_API void set_break(set *set);

/**
 * Iterates over an set.
 *
 * @param set    the set
 * @param type   type of iterator variable
 * @param entry  the iterator
 */
#define foreach_set(set, type, entry) for (type *entry = set_first(type, set); entry; entry = set_next(type, set))

/** @cond PRIVATE */

/* implementation specific */
#define new_set(cmp, slots) ((new_set) ((cmp), (slots)))
#define set_find(type, set, key, size, hash) \
  ((type*)_set_search((set), 1 ? (key) : (type*)0 /* type check */, (size), (hash), _set_find))
#define set_insert(type, set, key, size, hash) \
  ((type*)_set_search((set), 1 ? (key) : (type*)0 /* type check */, (size), (hash), _set_insert))
#define set_hinsert(set, key, size, hash) \
  ((set_entry *)_set_search ((set), (key), (size), (hash), _set_hinsert))
#define set_hinsert0(set, key, size, hash) \
  ((set_entry *)_set_search ((set), (key), (size), (hash), _set_hinsert0))

typedef enum { _set_find, _set_insert, _set_hinsert, _set_hinsert0 } _set_action;

FIRM_API void *_set_search(set *set, void const *key, size_t size,
                           unsigned hash, _set_action action);

/** @endcond */

/** @} */

#include "../end.h"

#endif
