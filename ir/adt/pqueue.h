/**
* @file    pqueue.h
* @date    18.04.2007
* @author  Christian Wuerdig
* @brief   Implementation of a priority queue. This is the ported version of the
*          original Java implementation by Matthias Braun.
* @version $Id$
*/

#ifndef _PQUEUE_H_
#define _PQUEUE_H_

typedef struct _pqueue_t pqueue;

/**
 * Creates a new priority queue.
 * @return A priority queue of initial length 0.
 */
pqueue *new_pqueue(void);

/**
 * Frees all memory allocated by the priority queue.
 * @param q   The priority queue to destroy.
 */
void del_pqueue(pqueue *q);

/**
 * Inserts a new element into a priority queue.
 * @param q      The priority queue the element should be inserted to.
 * @param data   The actual data which should be stored in the queue.
 * @param key    The priority for the data.
 */
void pqueue_put(pqueue *q, void *data, int key);

/**
 * Returns and removes the first element, ie. that one with the highest priority, from the queue.
 * @param q   The priority queue.
 * @return The first element of the queue. Asserts if queue is empty.
 */
void *pqueue_get(pqueue *q);

/**
 * Get the length of the priority queue.
 * @param q   The priority queue.
 * @return The length of the queue.
 */
int pqueue_length(pqueue *q);

/**
 * Returns true if queue is empty.
 * @param q   The priority queue.
 * @return 1 if the queue is empty, 0 otherwise.
 */
int pqueue_empty(pqueue *q);

#endif /* _PQUEUE_H_ */
