/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       double ended queue of generic pointers.
 * @author      Christian von Roques
 * @date        1999 by getting from fiasco
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "fourcc.h"
#include "pdeq.h"
#include "util.h"
#include "xmalloc.h"

#define PDEQ_MAGIC1 FOURCC('P','D','E','1')
#define PDEQ_MAGIC2 FOURCC('P','D','E','2')

/** Size of pdeq block cache. */
#define TUNE_NSAVED_PDEQS 16

/** A size handled efficiently by malloc(), at least 1K.  */
#define PREF_MALLOC_SIZE 2048

/**
 * Maximal number of data items in a pdeq chunk.
 */
#define NDATA ((PREF_MALLOC_SIZE - offsetof(pdeq, data)) / sizeof(void *))

#ifdef NDEBUG
# define VRFY(dq) ((void)0)
#else
# define VRFY(dq) assert((dq) && ((dq)->magic == PDEQ_MAGIC1))
#endif

/**
 * A pointer double ended queue.
 * This structure is used as a list chunk either.
 */
struct pdeq {
#ifndef NDEBUG
	unsigned magic;       /**< debug magic */
#endif
	pdeq *l_end, *r_end;  /**< left and right ends of the queue */
	pdeq *l, *r;          /**< left and right neighbor */
	size_t n;             /**< number of elements in the current chunk */
	size_t p;             /**< the read/write pointer */
	const void *data[];   /**< storage for elements */
};


/**
 * cache of unused, pdeq blocks to speed up new_pdeq and del_pdeq.
 */
static pdeq *pdeq_block_cache[TUNE_NSAVED_PDEQS];

/**
 * Number of pdeqs in pdeq_store.
 */
static unsigned pdeqs_cached;

/**
 * Free a pdeq chunk, put in into the cache if possible.
 *
 * @param p   The pdeq chunk.
 */
static inline void free_pdeq_block (pdeq *p)
{
#ifndef NDEBUG
	p->magic = 0xbadf00d1;
#endif
	if (pdeqs_cached < TUNE_NSAVED_PDEQS) {
		pdeq_block_cache[pdeqs_cached++] = p;
	} else {
		free (p);
	}
}

/**
 * Allocate a new pdeq chunk, get it from the cache if possible.
 *
 * @return A new pdeq chunk.
 */
static inline pdeq *alloc_pdeq_block (void)
{
	pdeq *p;
	if (pdeqs_cached > 0) {
		p = pdeq_block_cache[--pdeqs_cached];
	} else {
		p = (pdeq*) xmalloc(PREF_MALLOC_SIZE);
	}
	return p;
}

pdeq *new_pdeq(void)
{
	pdeq *dq;

	dq = alloc_pdeq_block();

#ifndef NDEBUG
	dq->magic = PDEQ_MAGIC1;
#endif
	dq->l_end = dq->r_end = dq;
	dq->l = dq->r = NULL;
	dq->n = dq->p = 0;

	VRFY(dq);
	return dq;
}

void del_pdeq(pdeq *dq)
{
	pdeq *q, *qq;

	VRFY(dq);

	q = dq->l_end; /* left end of chain */
	/* pdeq trunk empty, but !pdeq_empty() ==> trunk not in chain */
	if (dq->n == 0 && dq->l_end != dq ) {
		free_pdeq_block(dq);
	}

	/* Free all blocks in the pdeq chain */
	do {
		qq = q->r;
		free_pdeq_block(q);
	} while ((q = qq));
}

int pdeq_empty(pdeq *dq)
{
	VRFY(dq);
	return dq->l_end->n == 0;
}

size_t pdeq_len(pdeq *dq)
{
	size_t n;
	pdeq *q;

	VRFY(dq);

	n = 0;
	q = dq->l_end;
	do {
		n += q->n;
		q = q->r;
	}  while (q);

	return n;
}

pdeq *pdeq_putr(pdeq *dq, const void *x)
{
	pdeq *rdq;
	size_t n;

	VRFY(dq);

	rdq = dq->r_end;
	if (rdq->n >= NDATA) {  /* tailblock full */
		pdeq *ndq;

		ndq = dq;           /* try to reuse trunk, but ... */
		if (dq->n) {        /* ... if trunk used */
			/* allocate and init new block */
			ndq = alloc_pdeq_block();
#ifndef NDEBUG
			ndq->magic = PDEQ_MAGIC2;
#endif
			ndq->l_end = ndq->r_end = NULL;
		}

		ndq->r = NULL;
		ndq->l = rdq; rdq->r = ndq;
		ndq->n = 0; ndq->p = 0;
		dq->r_end = ndq;
		rdq = ndq;
	}

	n = rdq->n++ + rdq->p;
	if (n >= NDATA) n -= NDATA;

	rdq->data[n] = x;

	VRFY(dq);
	return dq;
}

pdeq *pdeq_putl(pdeq *dq, const void *x)
{
	pdeq *ldq;

	VRFY(dq);

	ldq = dq->l_end;
	if (ldq->n >= NDATA) {  /* headblock full */
		pdeq *ndq;

		ndq = dq;           /* try to reuse trunk, but ... */
		if (dq->n) {        /* ... if trunk used */
			/* allocate and init new block */
			ndq = alloc_pdeq_block();
#ifndef NDEBUG
			ndq->magic = PDEQ_MAGIC2;
#endif
			ndq->l_end = ndq->r_end = NULL;
		}

		ndq->l = NULL;
		ndq->r = ldq; ldq->l = ndq;
		ndq->n = 0; ndq->p = 0;
		dq->l_end = ndq;
		ldq = ndq;
	}

	ldq->n++;
	size_t p = ldq->p;
	if (p == 0)
		p = NDATA;

	ldq->data[--p] = x;
	ldq->p         = p;

	VRFY(dq);
	return dq;
}

void *pdeq_getr(pdeq *dq)
{
	pdeq *rdq;
	const void *x;
	size_t n;

	VRFY(dq);
	assert(dq->l_end->n);

	rdq = dq->r_end;
	n = rdq->p + --rdq->n;
	if (n >= NDATA) n -= NDATA;
	x = rdq->data[n];

	if (rdq->n == 0) {
		if (rdq->l) {
			dq->r_end = rdq->l;
			rdq->l->r = NULL;
			rdq->l = NULL;
		} else {
			dq->r_end = dq->l_end = dq;
		}
		if (dq != rdq) {
			free_pdeq_block(rdq);
		}
	}

	VRFY(dq);
	return (void *)x;
}

void *pdeq_getl(pdeq *dq)
{
	pdeq *ldq;
	const void *x;
	size_t p;

	VRFY(dq);
	assert(dq->l_end->n);

	ldq = dq->l_end;
	p = ldq->p;
	x = ldq->data[p];
	if (++p >= NDATA) p = 0;
	ldq->p = p;

	if (--ldq->n == 0) {
		if (ldq->r) {
			dq->l_end = ldq->r;
			ldq->r->l = NULL;
			ldq->r = NULL;
		} else {
			dq->l_end = dq->r_end = dq;
		}
		if (dq != ldq) {
			free_pdeq_block(ldq);
		}
	}

	VRFY(dq);
	return (void *)x;
}

int pdeq_contains(pdeq *dq, const void *x)
{
	pdeq *q;

	VRFY(dq);

	q = dq->l_end;
	do {
		size_t p, ep;

		p = q->p; ep = p + q->n;

		if (ep > NDATA) {
			do {
				if (q->data[p] == x) return 1;
			} while (++p < NDATA);
			p = 0;
			ep -= NDATA;
		}

		while (p < ep) {
			if (q->data[p++] == x) return 1;
		}

		q = q->r;
	} while (q);

	return 0;
}

void *pdeq_search(pdeq *dq, cmp_fun cmp, const void *key)
{
	pdeq *q;
	size_t p;

	VRFY(dq);

	q = dq->l_end;
	do {
		size_t ep;

		p = q->p; ep = p + q->n;

		if (ep > NDATA) {
			do {
				if (!cmp(q->data[p], key)) return (void *)q->data[p-1];
			} while (++p < NDATA);
			p = 0;
			ep -= NDATA;
		}

		while (p < ep) {
			if (!cmp(q->data[p++], key)) return (void *)q->data[p-1];
		}

		q = q->r;
	} while (q);

	return NULL;
}

void **pdeq_copyl(pdeq *dq, const void **dst)
{
	pdeq *q;
	const void **d = dst;

	VRFY(dq);

	q = dq->l_end;
	while (q) {
		size_t p, n;

		p = q->p; n = q->n;

		if (n + p > NDATA) {
			/* p is always < NDATA */
			size_t nn = NDATA - p;
			MEMCPY(d, &q->data[p], nn); d += nn;
			p = 0; n -= nn;
		}

		MEMCPY(d, &q->data[p], n); d += n;

		q = q->r;
	}

	return (void **)dst;
}

void **pdeq_copyr(pdeq *dq, const void **dst)
{
	pdeq *q;
	const void **d = dst;

	VRFY(dq);

	q = dq->r_end;
	while (q) {
		size_t p, i;

		p = q->p; i = q->n + p - 1;
		if (i >= NDATA) {
			i -= NDATA;
			for (;; --i) {
				*d++ = q->data[i];
				if (i == 0)
					break;
			}
			i = NDATA - 1;
		}

		for (;; --i) {
			*d++ = q->data[i];
			if (i <= p)
				break;
		}

		q = q->l;
	}

	return (void **)dst;
}
