/*
 * Project:     libFIRM
 * File name:   ir/adt/pdeq.h
 * Purpose:     Declarations for pdeq.
 * Author:      Christian von Roques
 * Modified by:
 * Created:     1999 by getting from fiasco
 * CVS-ID:      $Id$
 * Copyright:   (c) 1995, 1996 Christian von Roques
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _PDEQ_H_
#define _PDEQ_H_

/**
 * @file pdeq.h
 *
 * Declarations for double ended queue/list of generic pointers.
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
pdeq *new_pdeq(void);

/**
 * Creates a new double ended pointer list and puts an initial pointer element in.
 *
 * @param x   The pointer element to put in.
 *
 * @return The new list.
 */
pdeq *new_pdeq1(const void *x);

/**
 * Delete a double ended pointer list.
 *
 * @param dq   The list to be deleted.
 */
void del_pdeq(pdeq *dq);

/**
 * Returns the lenght of a double ended pointer list.
 *
 * @param dq   The list.
 */
int pdeq_len(pdeq *dq);

/**
 * Checks if a list is empty.
 *
 * @param dq   The list.
 *
 * @return  non-zero if the list is empty.
 */
int pdeq_empty(pdeq *dq);

/**
 * Returns non-zero if a double ended pointer list
 * contains a pointer x.
 *
 * @param dp  The list.
 * @param x   The pointer to be searched for.
 */
int pdeq_contains(pdeq *dq, const void *x);

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
void *pdeq_search(pdeq *qp, cmp_fun cmp, const void *key);

/**
 * Convert the double ended pointer list into a linear array beginning from
 * left, the first element in the linear array will be the left one.
 *
 * @param dq   The list.
 * @param dst  A pointer to a pointer array with must be at least
 *             pdeq_len(dq) * sizeof(void *)
 *
 * @return  dst
 */
void **pdeq_copyl(pdeq *qp, const void **dst);

/**
 * Convert the double ended pointer list into a linear array beginning from
 * right, the first element in the linear array will be the right one.
 *
 * @param dq   The list.
 * @param dst  A pointer to a pointer array with must be at least
 *             pdeq_len(dq) * sizeof(void *)
 *
 * @return  dst
 */
void **pdeq_copyr(pdeq *qp, const void **dst);

/**
 * Add a pointer to the left site of a double ended pointer list.
 *
 * @param dq  The list to add a pointer to.
 * @param x   The pointer element to be added
 *
 * @return The list.
 */
pdeq *pdeq_putl(pdeq *dq, const void *x);

/**
 * Add a pointer to the right site of a double ended pointer list.
 *
 * @param dq  The list to add a pointer to.
 * @param x   The pointer element to be added
 *
 * @return The list.
 */
pdeq *pdeq_putr(pdeq *dq, const void *x);

/**
 * Retrieve a pointer from the left site of a double ended pointer list.
 *
 * @param dq   The list
 *
 * @return The pointer element.
 *
 * @remark This function will fail if the list is empty.
 */
void *pdeq_getl(pdeq *dq);

/**
 * Retrieve a pointer from the right site of a double ended pointer list.
 *
 * @param dq   The list
 *
 * @return The pointer element.
 *
 * @remark This function will fail if the list is empty.
 */
void *pdeq_getr(pdeq *dq);

#ifdef NDEBUG
#define PDEQ_VRFY(deq) ((void)0)
#else
#define PDEQ_VRFY(deq) _pdeq_vrfy ((deq))
void _pdeq_vrfy(pdeq *dq);
#endif

#endif /* _PDEQ_H_ */
