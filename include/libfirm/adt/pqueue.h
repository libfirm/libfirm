/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @date    18.04.2007
 * @author  Christian Wuerdig
 * @brief   Implementation of a priority queue. This is the ported version of the
 *          original Java implementation by Matthias Braun.
 * @version $Id$
 */
#ifndef FIRM_ADT_PQUEUE_H
#define FIRM_ADT_PQUEUE_H

typedef struct _pqueue_t pqueue_t;

/**
 * Creates a new priority queue.
 * @return A priority queue of initial length 0.
 */
pqueue_t *new_pqueue(void);

/**
 * Frees all memory allocated by the priority queue.
 * @param q   The priority queue to destroy.
 */
void del_pqueue(pqueue_t *q);

/**
 * Inserts a new element into a priority queue.
 * @param q         The priority queue the element should be inserted to.
 * @param data      The actual data which should be stored in the queue.
 * @param priority  The priority for the data.
 */
void pqueue_put(pqueue_t *q, void *data, int priority);

/**
 * Returns and removes the first element, ie. that one with the highest priority, from the queue.
 * @param q   The priority queue.
 * @return The first element of the queue. Asserts if queue is empty.
 */
void *pqueue_pop_front(pqueue_t *q);

/**
 * Get the length of the priority queue.
 * @param q   The priority queue.
 * @return The length of the queue.
 */
int pqueue_length(const pqueue_t *q);

/**
 * Returns true if queue is empty.
 * @param q   The priority queue.
 * @return 1 if the queue is empty, 0 otherwise.
 */
int pqueue_empty(const pqueue_t *q);

#endif
