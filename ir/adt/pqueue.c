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
 * @author  Christian Wuerdig, Matthias Braun
 * @brief   Priority Queue implementation based on the heap datastructure
 * @version $Id$
 */
#include "array.h"
#include "pqueue.h"

/*
 * Implements a heap.
 *
 * Implementation note: It might seem strange that we start indexing at 0
 * but use 2*i and 2*i+1 to find the left and right sucessor of an index.
 * The trick is that for index 0 the left successor is 0 again, and the
 * right successor is 1 in this scheme. For the right successor 1 everything
 * works like usual. We simply took care in the algorithms that they still
 * work with the left child of 0 being 0 again. This was possible without
 * any extra ifs or arithmetic.
 * Thus we can save the wastage of 1 array position you can see in other
 * implementations or the ugly (i+1)*2 - 1 and (i+1)*2 for calculating the
 * left and right child. (At the expense that stuff easily breaks when you make
 * changes and don't think that the left child of 0 is 0 :-/)
 *
 */

typedef struct _pqueue_el_t {
	void *data;
	int  key;
} pqueue_el_t;

struct _pqueue_t {
	pqueue_el_t *elems;
};

/**
 * Enforces the heap characteristics if the queue
 * starting from element at position @p pos.
 */
static void pqueue_heapify(pqueue *q, unsigned pos) {
	unsigned len = ARR_LEN(q->elems);

	while (pos * 2 < len) {
		pqueue_el_t tmp;
		unsigned    exchange = pos;

		if (q->elems[exchange].key < q->elems[pos * 2].key) {
			exchange = pos * 2;
		}

		if ((pos * 2 + 1) < len && q->elems[exchange].key < q->elems[pos * 2 + 1].key) {
			exchange = pos * 2 + 1;
		}

		if (exchange == pos)
			break;

		tmp                = q->elems[pos];
		q->elems[pos]      = q->elems[exchange];
		q->elems[exchange] = tmp;

		pos = exchange;
	}
}

/**
 * Sifts up a newly inserted element at position @p pos.
 */
static void pqueue_sift_up(pqueue *q, unsigned pos) {
	while(q->elems[pos].key > q->elems[pos / 2].key) {
		pqueue_el_t tmp;

		tmp               = q->elems[pos];
		q->elems[pos]     = q->elems[pos / 2];
		q->elems[pos / 2] = tmp;

		pos /= 2;
	}
}

/**
 * Creates a new priority queue.
 * @return A priority queue of initial length 0.
 */
pqueue *new_pqueue(void) {
	pqueue *res = xmalloc(sizeof(*res));
	res->elems = NEW_ARR_F(pqueue_el_t, 0);
	return res;
}

/**
 * Frees all memory allocated by the priority queue.
 * @param q   The priority queue to destroy.
 */
void del_pqueue(pqueue *q) {
	DEL_ARR_F(q->elems);
	free(q);
}

/**
 * Inserts a new element into a priority queue.
 * @param q      The priority queue the element should be inserted to.
 * @param data   The actual data which should be stored in the queue.
 * @param key    The priority for the data.
 */
void pqueue_put(pqueue *q, void *data, int key) {
	pqueue_el_t el;

	el.data = data;
	el.key  = key;

	ARR_APP1(pqueue_el_t, q->elems, el);

	pqueue_sift_up(q, ARR_LEN(q->elems) - 1);
}

/**
 * Returns and removes the first element, ie. that one with the highest priority, from the queue.
 * @param q   The priority queue.
 * @return The first element of the queue. Asserts if queue is empty.
 */
void *pqueue_get(pqueue *q) {
	switch(ARR_LEN(q->elems)) {
		case 0:
			assert(0 && "Attempt to retrieve element from empty priority queue.");
			return NULL;
			break;
		case 1:
			ARR_SHRINKLEN(q->elems, 0);
			return q->elems[0].data;
			break;
		default: {
			void *data = q->elems[0].data;
			int  len   = ARR_LEN(q->elems) - 1;

			q->elems[0] = q->elems[len];
			ARR_SHRINKLEN(q->elems, len);
			pqueue_heapify(q, 0);

			return data;
		}
	}
}

/**
 * Get the length of the priority queue.
 * @param q   The priority queue.
 * @return The length of the queue.
 */
int pqueue_length(pqueue *q) {
	return ARR_LEN(q->elems);
}

/**
 * Returns true if queue is empty.
 * @param q   The priority queue.
 * @return 1 if the queue is empty, 0 otherwise.
 */
int pqueue_empty(pqueue *q) {
	return ARR_LEN(q->elems) == 0;
}
