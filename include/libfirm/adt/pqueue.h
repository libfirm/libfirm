/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @date    18.04.2007
 * @author  Christian Wuerdig
 * @brief   Implementation of a priority queue. This is the ported version of
            the original Java implementation by Matthias Braun.
 */
#ifndef FIRM_ADT_PQUEUE_H
#define FIRM_ADT_PQUEUE_H

#include <stddef.h>

#include "../begin.h"

/**
 * @ingroup adt
 * @defgroup pqueue  Priority Queue
 * A priority queue.
 * Implementation based on a heap data structure
 * @{
 */

/** priority queue */
typedef struct pqueue_t pqueue_t;

/**
 * Creates a new priority queue.
 * @return A priority queue of initial length 0.
 */
FIRM_API pqueue_t *new_pqueue(void);

/**
 * Frees all memory allocated by the priority queue.
 * @param q   The priority queue to destroy.
 */
FIRM_API void del_pqueue(pqueue_t *q);

/**
 * Inserts a new element into a priority queue.
 * @param q         The priority queue the element should be inserted to.
 * @param data      The actual data which should be stored in the queue.
 * @param priority  The priority for the data.
 */
FIRM_API void pqueue_put(pqueue_t *q, void *data, int priority);

/**
 * Returns and removes the first element, i.e. that one with the highest priority, from the queue.
 * @param q   The priority queue.
 * @return The first element of the queue. Asserts if queue is empty.
 */
FIRM_API void *pqueue_pop_front(pqueue_t *q);

/**
 * Get the length of the priority queue.
 * @param q   The priority queue.
 * @return The length of the queue.
 */
FIRM_API size_t pqueue_length(pqueue_t const *q);

/**
 * Returns true if queue is empty.
 * @param q   The priority queue.
 * @return 1 if the queue is empty, 0 otherwise.
 */
FIRM_API int pqueue_empty(pqueue_t const *q);

/** @} */

#include "../end.h"

#endif
