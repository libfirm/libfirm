/*
 * Project:     libFIRM
 * File name:   ir/adt/pset.h
 * Purpose:     Declarations for pset.
 * Author:      Markus Armbruster
 * Modified by:
 * Created:     1999 by getting from fiasco
 * CVS-ID:      $Id$
 * Copyright:   (c) 1995, 1996 Markus Armbruster
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef _PSET_H
#define _PSET_H

#include <stddef.h>

/**
 * The abstract type of a pset (Set of pointers).
 *
 * This kind of sets stores only pointer to elements, the elements itself
 * must be stored somewere else.
 *
 * @see set
 */
typedef struct pset pset;

/** The entry of a pset, representing an element pointer in the set and it's meta-information */
typedef struct {
  unsigned hash;
  void *dptr;
} pset_entry;

/**
 * The type of a set compare function.
 *
 * @param elt   pointer to an element
 * @param key   pointer to another element
 *
 * @return
 *    0 if the elements are identically, non-zero else
 */
typedef int (*pset_cmp_fun) (const void *elt, const void *key);

/**
 * Creates a new pset.
 *
 * @param func    the compare function of this pset
 * @param slots   number of slots
 *
 * @returns
 *    created pset
 */
pset *new_pset (pset_cmp_fun func, int slots);

/**
 * Deletes a pset.
 *
 * @note
 *    This does NOT delete the elements of this pset, just it's pointers!
 */
void del_pset (pset *pset);

/**
 * Searches an element pointer in a pset.
 *
 * @param pset  the pset to search in
 * @param key   the element to is searched
 * @param hash  the hash value of key
 *
 * @return
 *    the pointer of the found element in the pset of NULL if it was not found
 */
void *pset_find (pset *pset, const void *key, unsigned hash);

/**
 * Inserts an element pointer into a pset.
 *
 * @param pset  the pset to insert in
 * @param key   a pointer to the element to be inserted
 * @param hash  the hash-value of the element
 *
 * @return a pointer to the inserted element
 *
 * @note
 *    It is not possible to insert on element more than once. If a element
 *    that should be inserted is already in the set, this functions does
 *    nothing but returning its set_entry.

 */
void *pset_insert (pset *pset, const void *key, unsigned hash);

/**
 * Inserts an element pointer into a pset and returns its pset_entry.
 *
 * @param pset  the pset to insert in
 * @param key   a pointer to the element to be inserted
 * @param hash  the hash-value of the element
 *
 * @return a pointer to the pset_entry of the inserted element
 *
 * @note
 *    It is not possible to insert on element more than once. If a element
 *    that should be inserted is already in the pset, this functions does
 *    nothing but returning its pset_entry.
 */
pset_entry *pset_hinsert (pset *pset, const void *key, unsigned hash);

/**
 * Removes an element from a pset.
 *
 * @param pset  the pset to insert in
 * @param key   a pointer to the element to be inserted
 * @param hash  the hash-value of the element
 *
 * @return
 *    the pointer to the removed element
 *
 * @remark
 *    The current implementation did not allow to remove non-existing elements
 */
void *pset_remove (pset *pset, const void *key, unsigned hash);

/** returns the first element of a pset */
void *pset_first (pset *pset);

/** returns the next element of a pset */
void *pset_next (pset *pset);

/** Breaks the iteration of a set. Must be called before the next pset_first() call */
void pset_break (pset *pset);

#define new_pset(cmp, slots) (PSET_TRACE (new_pset) ((cmp), (slots)))
#define pset_find(pset, key, hash) \
  _pset_search ((pset), (key), (hash), _pset_find)
#define pset_insert(pset, key, hash) \
  _pset_search ((pset), (key), (hash), _pset_insert)
#define pset_hinsert(pset, key, hash) \
  ((pset_entry *)_pset_search ((pset), (key), (hash), _pset_hinsert))

#ifdef STATS
void pset_stats (pset *);
#else
# define pset_stats(s) ((void)0)
#endif

#ifdef DEBUG
void pset_describe (pset *);
#endif

/* @@@ NYI */
#define PSET_VRFY(pset) (void)0


/* Private */

typedef enum { _pset_find, _pset_insert, _pset_hinsert } _pset_action;

void *_pset_search (pset *, const void *, unsigned, _pset_action);

#if defined(DEBUG) && defined(HAVE_GNU_MALLOC)
extern const char *pset_tag;
# ifdef PSET_ID
#   define PSET_TRACE pset_tag = SET_ID,
# else
#   define PSET_TRACE pset_tag = __FILE__,
# endif
#else /* !(DEBUG && HAVE_GNU_MALLOC) */
#   define PSET_TRACE
#endif /* !(DEBUG && HAVE_GNU_MALLOC) */

#endif
