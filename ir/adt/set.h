/*
 * Project:     libFIRM
 * File name:   ir/adt/set.h
 * Purpose:     Declarations for set.
 * Author:      Markus Armbruster
 * Modified by:
 * Created:     1999 by getting from fiasco
 * CVS-ID:      $Id$
 * Copyright:   (c) 1995, 1996 Markus Armbruster
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file set.h
 *
 * Declarations for set.
 */

#ifndef _SET_H
#define _SET_H

#include <stddef.h>

/**
 * The abstract type of a set.
 *
 * This sets stores copies of its elements, so there is no need
 * to store the elements after they were added to a set.
 *
 * @see pset
 */
typedef struct set set;

/** The entry of a set, representing an element in the set and it's meta-information */
typedef struct set_entry {
  unsigned hash;    /**< the hash value of the element */
  size_t size;      /**< the size of the element */
  int dptr[1];			/**< the element itself, data copied in must not need more
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
 *    Although it is possible to define different meanings for equality of two
 *    elements of a sets, they can be only equal if there sizes are
 *    equal. This is checked before the compare function is called.
 */
typedef int (*set_cmp_fun) (const void *elt, const void *key, size_t size);

/**
 * Creates a new set.
 *
 * @param func    The compare function of this set.
 * @param slots   Initial number of collision chains.  I.e., #slots
 *                different keys can be hashed without collisions.
 *
 * @returns
 *    created set
 */
set *new_set (set_cmp_fun func, int slots);

/**
 * Deletes a set and all elements of it.
 */
void del_set (set *set);

/**
 * Returns the number of elements in a set.
 *
 * @param set   the set
 */
int set_count (set *set);

/**
 * Searches an element in a set.
 *
 * @param set   the set to search in
 * @param key   the element to is searched
 * @param size  the size of key
 * @param hash  the hash value of key
 *
 * @return
 *    The address of the found element in the set or NULL if it was not found.
 */
void *set_find (set *set, const void *key, size_t size, unsigned hash);

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
 *    It is not possible to insert on element more than once. If an element
 *    that should be inserted is already in the set, this functions does
 *    nothing but returning its pointer.
 */
void *set_insert (set *set, const void *key, size_t size, unsigned hash);

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
set_entry *set_hinsert (set *set, const void *key, size_t size, unsigned hash);

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
set_entry *set_hinsert0 (set *set, const void *key, size_t size, unsigned hash);

/**
 * Returns the first element of a set.
 *
 * @param set  the set to iterate
 *
 * @return a pointer to the element or NULL if the set is empty
 */
void *set_first (set *set);

/**
 * Returns the next element of a set.
 *
 * @param set  the set to iterate
 *
 * @return a pointer to the next element or NULL if the
 *         iteration is finished
 */
void *set_next (set *set);

/**
 * Breaks the iteration of a set. Must be called before
 * the next pset_first() call if the iteration was NOT
 * finished.
 *
 * @param pset  the pset
 */
void set_break (set *set);

/* implementation specific */
#define new_set(cmp, slots) (SET_TRACE (new_set) ((cmp), (slots)))
#define set_find(set, key, size, hash) \
  _set_search ((set), (key), (size), (hash), _set_find)
#define set_insert(set, key, size, hash) \
  _set_search ((set), (key), (size), (hash), _set_insert)
#define set_hinsert(set, key, size, hash) \
  ((set_entry *)_set_search ((set), (key), (size), (hash), _set_hinsert))
#define set_hinsert0(set, key, size, hash) \
  ((set_entry *)_set_search ((set), (key), (size), (hash), _set_hinsert0))

#define SET_VRFY(set) (void)0

#ifdef STATS
/**
 * Prints statistics on a set to stdout.
 *
 * @param set  the set
 */
void set_stats (set *set);
#else
# define set_stats(s) ((void)0)
#endif

#ifdef DEBUG
void set_describe (set *);
#endif


/* Private */

typedef enum { _set_find, _set_insert, _set_hinsert, _set_hinsert0 } _set_action;

void *_set_search (set *, const void *, size_t, unsigned, _set_action);

#if defined(DEBUG) && defined(HAVE_GNU_MALLOC)
extern const char *set_tag;
# ifdef SET_ID
#   define SET_TRACE set_tag = SET_ID,
# else
#   define SET_TRACE set_tag = __FILE__,
# endif
#else /* !(DEBUG && HAVE_GNU_MALLOC) */
#   define SET_TRACE
#endif /* !(DEBUG && HAVE_GNU_MALLOC) */

#endif
