/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       double ended queue of generic pointers.
 * @author      Christian von Roques
 */
#ifndef FIRM_ADT_PDEQ_H
#define FIRM_ADT_PDEQ_H

#include <stddef.h>

#include "../begin.h"

/**
 * @ingroup adt
 * @defgroup pdeq Double Ended Queue
 * Implementation of a double ended queue data structure for generic pointers
 * @{
 */

/**
 * The type of the pointer compare function.
 *
 * @param elem  The list element.
 * @param key   The user supplied key.
 *
 * @return 0 if the element matches the key, non-zero else.
 */
typedef int (*cmp_fun)(const void *elem, const void *key);

/**
 * The pointer double ended queue (list).
 */
typedef struct pdeq pdeq;

/**
 * Creates a new double ended pointer list.
 *
 * @return A new list.
 */
FIRM_API pdeq *new_pdeq(void);

/**
 * Delete a double ended pointer list.
 *
 * @param dq   The list to be deleted.
 */
FIRM_API void del_pdeq(pdeq *dq);

/**
 * Returns the length of a double ended pointer list.
 *
 * @param dq   The list.
 */
FIRM_API size_t pdeq_len(pdeq const *dq);

/**
 * Checks if a list is empty.
 *
 * @param dq   The list.
 *
 * @return  non-zero if the list is empty.
 */
FIRM_API int pdeq_empty(pdeq const *dq);

/**
 * Returns non-zero if a double ended pointer list
 * contains a pointer x.
 *
 * @param dq  The list.
 * @param x   The pointer to be searched for.
 */
FIRM_API int pdeq_contains(pdeq const *dq, const void *x);

/**
 * Search a key in a double ended pointer list, the search
 * is controlled by a compare function.
 * An element is found, if the compare function returns 0.
 * The search is started from the left site of the list.
 *
 * @param qp   The list.
 * @param cmp  The compare function.
 * @param key  The search key.
 *
 * @return The address of the element entry if the key was found,
 *         NULL else.
 */
FIRM_API void *pdeq_search(pdeq const *qp, cmp_fun cmp, const void *key);

/**
 * Convert the double ended pointer list into a linear array beginning from
 * left, the first element in the linear array will be the left one.
 *
 * @param qp   The list.
 * @param dst  A pointer to a pointer array with must be at least
 *             pdeq_len(dq) * sizeof(void *)
 *
 * @return  dst
 */
FIRM_API void **pdeq_copyl(const pdeq *qp, const void **dst);

/**
 * Convert the double ended pointer list into a linear array beginning from
 * right, the first element in the linear array will be the right one.
 *
 * @param qp   The list.
 * @param dst  A pointer to a pointer array with must be at least
 *             pdeq_len(dq) * sizeof(void *)
 *
 * @return  dst
 */
FIRM_API void **pdeq_copyr(const pdeq *qp, const void **dst);

/**
 * Add a pointer to the left side of a double ended pointer list.
 *
 * @param dq  The list to add a pointer to.
 * @param x   The pointer element to be added
 *
 * @return The list.
 */
FIRM_API pdeq *pdeq_putl(pdeq *dq, const void *x);

/**
 * Add a pointer to the right side of a double ended pointer list.
 *
 * @param dq  The list to add a pointer to.
 * @param x   The pointer element to be added
 *
 * @return The list.
 */
FIRM_API pdeq *pdeq_putr(pdeq *dq, const void *x);

/**
 * Retrieve (and remove) a pointer from the left site of a double ended pointer
 * list.
 *
 * @param dq   The list
 * @return The pointer element.
 * @remark This function will fail if the list is empty.
 */
FIRM_API void *pdeq_getl(pdeq *dq);

/**
 * Retrieve (and remove) a pointer from the right site of a double ended pointer
 * list.
 *
 * @param dq   The list
 * @return The pointer element.
 * @remark This function will fail if the list is empty.
 */
FIRM_API void *pdeq_getr(pdeq *dq);

/** @} */

#include "../end.h"

#endif
