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
 * @author  Christian Wuerdig, Matthias Braun
 * @brief   Priority Queue implementation based on the heap datastructure
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

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
	int  priority;
} pqueue_el_t;

struct _pqueue_t {
	pqueue_el_t *elems;
};

/**
 * Enforces the heap characteristics if the queue
 * starting from element at position @p pos.
 */
static void pqueue_heapify(pqueue_t *q, unsigned pos) {
	unsigned len = ARR_LEN(q->elems);

	while (pos * 2 < len) {
		pqueue_el_t tmp;
		unsigned    exchange = pos;

		if (q->elems[exchange].priority < q->elems[pos * 2].priority) {
			exchange = pos * 2;
		}

		if ((pos * 2 + 1) < len
				&& q->elems[exchange].priority < q->elems[pos * 2 + 1].priority) {
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
static void pqueue_sift_up(pqueue_t *q, unsigned pos) {
	while(q->elems[pos].priority > q->elems[pos / 2].priority) {
		pqueue_el_t tmp;

		tmp               = q->elems[pos];
		q->elems[pos]     = q->elems[pos / 2];
		q->elems[pos / 2] = tmp;

		pos /= 2;
	}
}

pqueue_t *new_pqueue(void) {
	pqueue_t *res = XMALLOC(pqueue_t);
	res->elems = NEW_ARR_F(pqueue_el_t, 0);
	return res;
}

void del_pqueue(pqueue_t *q) {
	DEL_ARR_F(q->elems);
	free(q);
}

void pqueue_put(pqueue_t *q, void *data, int priority) {
	pqueue_el_t el;

	el.data     = data;
	el.priority = priority;

	ARR_APP1(pqueue_el_t, q->elems, el);

	pqueue_sift_up(q, ARR_LEN(q->elems) - 1);
}

void *pqueue_pop_front(pqueue_t *q) {
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

int pqueue_length(const pqueue_t *q) {
	return ARR_LEN(q->elems);
}

int pqueue_empty(const pqueue_t *q) {
	return ARR_LEN(q->elems) == 0;
}
