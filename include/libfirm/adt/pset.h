/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief      optimized version of set for sets containing only pointers
 *             (deprecated)
 * @author     Markus Armbruster
 */
#ifndef FIRM_ADT_PSET_H
#define FIRM_ADT_PSET_H

#include <stddef.h>

#include "hashptr.h"

#include "../begin.h"

/**
 * @ingroup adt
 * @defgroup pset Pointer Set
 * (Hash)sets containing pointers.
 * @note This code has been deprecated. Use pset_new or cpset for new code.
 * @{
 */

/**
 * The default comparison function for pointers.
 * @param x A pointer.
 * @param y A pointer.
 * @return 0 if @p x and @p y are equal. Some value != 0 otherwise.
 */
FIRM_API int pset_default_ptr_cmp(void const *x, void const *y);

/**
 * The abstract type of a pset (Set of pointers).
 *
 * This kind of sets stores only pointer to elements, the elements itself
 * must be stored somewhere else.
 *
 * @see set
 */
typedef struct pset pset;

/** Inserts into pointer set with default hash function. */
#define pset_insert_ptr(set,key)  pset_insert(set, key, hash_ptr(key))
/** Inserts into pointer set with default hash function and return entry */
#define pset_hinsert_ptr(set,key) pset_hinsert(set, key, hash_ptr(key))
/** Removes pointer from pointer set with default hash function */
#define pset_remove_ptr(set,key)  pset_remove(set, key, hash_ptr(key))
/** Finds pointer in pointer set with default hash function */
#define pset_find_ptr(set,key)    pset_find(set, key, hash_ptr(key))
/** Creates new pointer set with default compare function */
#define pset_new_ptr(slots)       new_pset(pset_default_ptr_cmp, slots)
/** Creates new pointer set with default compare function and default size */
#define pset_new_ptr_default()    pset_new_ptr(64)

/** The entry of a pset, representing an element pointer in the set and its
 * meta-information */
typedef struct {
  unsigned hash; /**< hash value of element */
  void    *dptr; /**< pointer to element data */
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
typedef int (*pset_cmp_fun) (void const *elt, void const *key);

/**
 * Creates a new pset.
 *
 * @param func    The compare function of this pset.
 * @param slots   Initial number of collision chains.  I.e., \#slots
 *                different keys can be hashed without collisions.
 * @returns created pset
 */
FIRM_API pset *new_pset(pset_cmp_fun func, size_t slots);

/**
 * Deletes a pset.
 *
 * @param pset   the pset
 *
 * @note
 *    This does NOT delete the elements of this pset, just its pointers!
 */
FIRM_API void del_pset(pset *pset);

/**
 * Returns the number of elements in a pset.
 *
 * @param pset   the pset
 */
FIRM_API size_t pset_count(pset const *pset);

/**
 * Searches an element pointer in a pset.
 *
 * @param pset  the pset to search in
 * @param key   the element to search
 * @param hash  the hash value of key
 *
 * @return
 *    the pointer of the found element in the pset or NULL if it was not found
 */
FIRM_API void *pset_find(pset *pset, void const *key, unsigned hash);

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
 *    It is not possible to insert an element more than once. If an element
 *    that should be inserted is already in the set, this functions does
 *    nothing but returning its already existing set_entry.
 */
FIRM_API void *pset_insert(pset *pset, void const *key, unsigned hash);

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
 *    It is not possible to insert an element more than once. If an element
 *    that should be inserted is already in the pset, this functions does
 *    nothing but returning its pset_entry.
 */
FIRM_API pset_entry *pset_hinsert(pset *pset, void const *key, unsigned hash);

/**
 * Removes an element from a pset.
 *
 * @param pset  the pset to delete in
 * @param key   a pointer to the element to be deleted
 * @param hash  the hash-value of the element
 *
 * @return
 *    the pointer to the removed element
 *
 * @remark
 *    The current implementation did not allow to remove non-existing elements.
 *    @@@ so, does it do now?
 *    Further, it is allowed to remove elements during an iteration
 *    including the current one.
 */
FIRM_API void *pset_remove(pset *pset, void const *key, unsigned hash);

/**
 * Returns the first element of a pset.
 *
 * @param pset  the pset to iterate
 *
 * @return a pointer to the element or NULL if the set is empty
 */
FIRM_API void *pset_first(pset *pset);

/**
 * Returns the first element of a pset.
 * This is a wrapper for pset_first(set); It allows to express the
 * intended type of the set elements (instead of weakly typed void*).
 *
 * @param type  destination type of the pointers in the set
 * @param pset  the pset to iterate
 *
 * @return a pointer to the element or NULL if the set is empty
 */
#define pset_first(type, pset) ((type*)pset_first((pset)))

/**
 * Returns the next element of a pset.
 *
 * @param pset  the pset to iterate
 *
 * @return a pointer to the next element or NULL if the
 *         iteration is finished
 */
FIRM_API void *pset_next(pset *pset);

/**
 * Returns the next element of a pset.
 * This is a wrapper for pset_next(set); It allows to express the
 * intended type of the set elements (instead of weakly typed void*).
 *
 * @param type  destination type of the pointers in the set
 * @param pset  the pset to iterate
 *
 * @return a pointer to the next element or NULL if the
 *         iteration is finished
 */
#define pset_next(type, pset) ((type*)pset_next((pset)))

/**
 * Breaks the iteration of a set. Must be called before
 * the next pset_first() call if the iteration was NOT
 * finished.
 *
 * @param pset  the pset
 */
FIRM_API void pset_break(pset *pset);

/**
 * Iterates over a pset.
 *
 * @param pset   the pset
 * @param type   type of iterator variable
 * @param entry  the iterator
 */
#define foreach_pset(pset, type, entry) for (type *entry = pset_first(type, pset); entry; entry = pset_next(type, pset))

/**
 * Inserts all elements of the pointer set src into
 * the set target (union).
 *
 * @param target  the target set, will contain the union
 * @param src     a set, will not be changed
 */
FIRM_API void pset_insert_pset_ptr(pset *target, pset *src);

/** @cond PRIVATE */

#define new_pset(cmp, slots) ((new_pset) ((cmp), (slots)))
#define pset_find(pset, key, hash) \
  _pset_search ((pset), (key), (hash), _pset_find)
#define pset_insert(pset, key, hash) \
  _pset_search ((pset), (key), (hash), _pset_insert)
#define pset_hinsert(pset, key, hash) \
  ((pset_entry *)_pset_search ((pset), (key), (hash), _pset_hinsert))

typedef enum { _pset_find, _pset_insert, _pset_hinsert } _pset_action;

FIRM_API void *_pset_search(pset *set, void const *key, unsigned hash,
                            _pset_action action);

/** @endcond */

/** @} */

#include "../end.h"

#endif
