/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
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
 * @author  Christian Wuerdig, Matthias Braun
 * @brief   Priority Queue implementation based on the heap data structure
 */
#include "pqueue.h"

#include "array.h"
#include "panic.h"

typedef struct pqueue_el_t {
	void *data;
	int   priority;
} pqueue_el_t;

struct pqueue_t {
	pqueue_el_t *elems;
};

/**
 * Enforces the heap characteristics if the queue
 * starting from element at position @p pos.
 */
static void pqueue_heapify(pqueue_t *q, size_t pos)
{
	size_t len = ARR_LEN(q->elems);
	while (pos * 2 < len) {
		size_t exchange = pos;
		if (q->elems[exchange].priority < q->elems[pos * 2].priority)
			exchange = pos * 2;

		if ((pos * 2 + 1) < len
		    && q->elems[exchange].priority < q->elems[pos * 2 + 1].priority)
			exchange = pos * 2 + 1;

		if (exchange == pos)
			break;

		pqueue_el_t tmp    = q->elems[pos];
		q->elems[pos]      = q->elems[exchange];
		q->elems[exchange] = tmp;

		pos = exchange;
	}
}

/**
 * Sifts up a newly inserted element at position @p pos.
 */
static void pqueue_sift_up(pqueue_t *q, size_t pos)
{
	while (q->elems[pos].priority > q->elems[pos / 2].priority) {
		pqueue_el_t tmp   = q->elems[pos];
		q->elems[pos]     = q->elems[pos / 2];
		q->elems[pos / 2] = tmp;

		pos /= 2;
	}
}

pqueue_t *new_pqueue(void)
{
	pqueue_t *res = XMALLOC(pqueue_t);
	res->elems = NEW_ARR_F(pqueue_el_t, 0);
	return res;
}

void del_pqueue(pqueue_t *q)
{
	DEL_ARR_F(q->elems);
	free(q);
}

void pqueue_put(pqueue_t *q, void *data, int priority)
{
	pqueue_el_t el = {
		.data = data,
		.priority = priority
	};
	ARR_APP1(pqueue_el_t, q->elems, el);

	pqueue_sift_up(q, ARR_LEN(q->elems) - 1);
}

void *pqueue_pop_front(pqueue_t *q)
{
	switch (ARR_LEN(q->elems)) {
	case 0:
		panic("attempt to retrieve element from empty priority queue");
	case 1:
		ARR_SHRINKLEN(q->elems, 0);
		return q->elems[0].data;
	default: {
		void   *data = q->elems[0].data;
		size_t len   = ARR_LEN(q->elems) - 1;

		q->elems[0] = q->elems[len];
		ARR_SHRINKLEN(q->elems, len);
		pqueue_heapify(q, 0);
		return data;
	}
	}
}

size_t pqueue_length(pqueue_t const *q)
{
	return ARR_LEN(q->elems);
}

int pqueue_empty(pqueue_t const *q)
{
	return ARR_LEN(q->elems) == 0;
}
