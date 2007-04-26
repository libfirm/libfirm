/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief       double ended queue of generic pointers.
 * @author      Christian von Roques
 * @version     $Id$
 */
#ifndef FIRM_ADT_PDEQ_H
#define FIRM_ADT_PDEQ_H

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
 * @param dq  The list.
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
 * @param qp   The list.
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
 * @param qp   The list.
 * @param dst  A pointer to a pointer array with must be at least
 *             pdeq_len(dq) * sizeof(void *)
 *
 * @return  dst
 */
void **pdeq_copyr(pdeq *qp, const void **dst);

/**
 * Add a pointer to the left side of a double ended pointer list.
 *
 * @param dq  The list to add a pointer to.
 * @param x   The pointer element to be added
 *
 * @return The list.
 */
pdeq *pdeq_putl(pdeq *dq, const void *x);

/**
 * Add a pointer to the right side of a double ended pointer list.
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

/**
 * The pdeq is often used as a wait queue. A helper
 * type to support this.
 */
typedef pdeq waitq;

/**
 * Creates a new pointer wait queue (fifo).
 *
 * @return A new queue.
 */
#define new_waitq()  new_pdeq()

/**
 * Delete a wait queue (fifo)
 *
 * @param wq   The wait queue.
 */
#define del_waitq(wq) del_pdeq(wq)

/**
 * Retrieve a pointer from the wait queue (fifo).
 *
 * @param wq   The wait queue.
 *
 * @return The pointer element.
 *
 * @remark This function will fail if the queue is empty.
 */
#define waitq_get(wq)  pdeq_getl(wq)

/**
 * Add a pointer to the wait queue (fifo).
 *
 * @param wq  The wait queue
 * @param x   The pointer element to be added
 *
 * @return The wait queue.
 */
#define waitq_put(wq, x) pdeq_putr((wq), (x))

/**
 * Checks if a wait queue is empty.
 *
 * @param wq   The wait queue.
 *
 * @return  non-zero if the queue is empty.
 */
#define waitq_empty(wq) pdeq_empty(wq)

/**
 * The pdeq can be used as a stack. A helper
 * type to support this.
 */
typedef pdeq stack;

/**
 * Creates a new pointer stack (lifo).
 *
 * @return A new stack.
 */
#define new_stack()  new_pdeq()

/**
 * Pop a pointer from the stack (lifo).
 *
 * @param st   The stack.
 *
 * @return The pointer element.
 *
 * @remark This function will fail if the stack is empty.
 */
#define stack_pop(st)  pdeq_getr(st)

/**
 * Push a pointer to the stack (lifo).
 *
 * @param st  The stack.
 * @param x   The pointer element to be added
 *
 * @return The stack.
 */
#define stack_push(st, x) pdeq_putr((st), (x))

/**
 * Checks if a stack is empty.
 *
 * @param st   The stack.
 *
 * @return  non-zero if the stack is empty.
 */
#define stack_empty(st) pdeq_empty(wq)

#endif
