/* Pdeq --- double ended queue of generic pointers.
   Copyright (C) 1995, 1996 Christian von Roques */

/* $Id$ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
# ifdef HAVE_STRING_H
#  include <string.h>
# endif
#include "tune.h"
#include "cookies.h"
#include "debug.h"
#include "malloc.h"
#include "pdeq.h"


#ifndef INLINE
#ifdef USE_GCC_INLINE
#define INLINE inline
#else
#define INLINE
#endif
#endif

/* # of data items in block */
#define NDATA ((int)((PREF_MALLOC_SIZE - offsetof (pdeq, data)) / sizeof (void *)))

#ifdef NDEBUG
# define VRFY(dq) ((void)0)
#else
# define VRFY(dq) \
    (d_(df_vrfy_level,1) ? _pdeq_vrfy ((dq)) : assert ((dq) && ((dq)->cookie == PDEQ_COOKIE1)))
#endif


struct pdeq {
#ifndef NDEBUG
  unsigned cookie;
#endif
  pdeq *l_end, *r_end;		/* left and right ends of the deque */
  pdeq *l, *r;			/*  left and right neighbour */
  int n, p;
  const void *data[1];
};


/* cache of unused, pdeq blocks to speed up new_pdeq and del_pdeq. */
/* +1 for compilers that can't grok empty arrays */
pdeq *pdeq_block_cache[TUNE_NSAVED_PDEQS+1];
unsigned pdeqs_cached;		/* # pdeqs in pdeq_store */


static INLINE void
free_pdeq_block (pdeq *p)
{
#ifndef NDEBUG
  p->cookie = 0xbadf00d1;
#endif
  if (pdeqs_cached < TUNE_NSAVED_PDEQS) {
    if (d_ (df_pdeq, 2)) printf ("[%p ==> pdeq_block_cache] ", p);
    pdeq_block_cache[pdeqs_cached++] = p;
  } else {
    if (d_ (df_pdeq, 2)) printf ("[%p ==> free] ", p);
    xfree (p);
  }
}


static INLINE pdeq *
alloc_pdeq_block (void)
{
  pdeq *p;
  if (TUNE_NSAVED_PDEQS && pdeqs_cached) {
    p = pdeq_block_cache[--pdeqs_cached];
    if (d_ (df_pdeq, 2)) printf ("[pdeq_block_cache ==> %p] ", p);
  } else {
    p = xmalloc (PREF_MALLOC_SIZE);
    if (d_ (df_pdeq, 2)) printf ("[malloc ==> %p] ", p);
  }
  return p;
}


#ifndef NDEBUG
void
_pdeq_vrfy (pdeq *dq)
{
  pdeq *q;

  if (d_ (df_pdeq, 5)) printf ("[pdeq_vrfy %p] ", dq);

  assert (   dq
	  && (dq->cookie == PDEQ_COOKIE1)
	  && (dq->l_end && dq->r_end));
  q = dq->l_end;
  while (q) {
    assert (   ((q == dq) || (q->cookie == PDEQ_COOKIE2))
	    && ((q == dq->l_end) ^ (q->l!=0))
	    && ((q == dq->r_end) ^ (q->r!=0))
	    && (!q->l || (q == q->l->r))
	    && ((q->n>=0) && (q->n<=NDATA))
	    && ((q == dq->l_end) || (q == dq->r_end) || (q->n == NDATA))
	    && ((q->p>=0) && (q->p<NDATA)));
    q = q->r;
  }
}
#endif


pdeq *
new_pdeq (void)
{
  pdeq *dq;

  dq = alloc_pdeq_block();

#ifndef NDEBUG
  dq->cookie = PDEQ_COOKIE1;
#endif
  dq->l_end = dq->r_end = dq;
  dq->l = dq->r = NULL;
  dq->n = dq->p = 0;

  VRFY (dq);
  if (d_ (df_pdeq, 1)) printf ("(new_pdeq ==> %p)\n", dq);
  return dq;
}


pdeq *
new_pdeq1 (const void *x)
{
  return pdeq_putr (new_pdeq(), x);
}


void
del_pdeq (pdeq *dq)
{
  pdeq *q, *qq;

  VRFY (dq);

  q = dq->l_end;		/* left end of chain */
  /* pdeq trunk empty, but !pdeq_empty() ==> trunk not in chain */
  if (dq->n == 0 && dq->l_end != dq ) {
    free_pdeq_block (dq);
  }

  /* Free all blocks in the pdeq chain */
  do {
    qq = q->r;
    free_pdeq_block (q);
  } while ((q = qq));

  if (d_ (df_pdeq, 1)) printf ("(del_pdeq %p)\n", dq);
}


bool
pdeq_empty (pdeq *dq)
{
  VRFY (dq);
  if (d_ (df_pdeq, 4)) printf ("(pdeq_empty %p ==> %d)\n", dq, dq->l_end->n == 0);
  return dq->l_end->n == 0;
}


int
pdeq_len (pdeq *dq)
{
  int n;
  pdeq *q;

  VRFY (dq);

  n = 0;
  q = dq->l_end;
  do {
    n += q->n;
    q = q->r;
  }  while (q);

  if (d_ (df_pdeq, 4)) printf ("(pdeq_len %p ==> %d)\n", dq, n);
  return n;
}


pdeq *
pdeq_putr (pdeq *dq, const void *x)
{
  pdeq *rdq;
  int n;
  bool pr = 0;

  VRFY (dq);

  rdq = dq->r_end;
  if (rdq->n >= NDATA) {	/* tailblock full */
    pdeq *ndq;

    ndq = dq;			/* try to reuse trunk, but ... */
    if (dq->n) {		/* ... if trunk used */
      /* allocate and init new block */
      ndq = alloc_pdeq_block ();
      pr = d_ (df_pdeq, 2);
#ifndef NDEBUG
      ndq->cookie = PDEQ_COOKIE2;
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

  VRFY (dq);
  if (d_ (df_pdeq, 3) || pr) printf ("(pdeq_putr %p %p)\n", dq, x);
  return dq;
}


pdeq *
pdeq_putl (pdeq *dq, const void *x)
{
  pdeq *ldq;
  int p;
  bool pr = 0;

  VRFY (dq);

  ldq = dq->l_end;
  if (ldq->n >= NDATA) {	/* headblock full */
    pdeq *ndq;

    ndq = dq;			/* try to reuse trunk, but ... */
    if (dq->n) {		/* ... if trunk used */
      /* allocate and init new block */
      ndq = alloc_pdeq_block ();
      pr = d_ (df_pdeq, 2);
#ifndef NDEBUG
      ndq->cookie = PDEQ_COOKIE2;
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
  p = ldq->p - 1;
  if (p < 0) p += NDATA;
  ldq->p = p;

  ldq->data[p] = x;

  VRFY (dq);
  if (d_ (df_pdeq, 3) || pr) printf ("(pdeq_putl %p %p)\n", dq, x);
  return dq;
}


void *
pdeq_getr (pdeq *dq)
{
  pdeq *rdq;
  const void *x;
  int n;
  bool pr = 0;

  VRFY (dq);
  assert (dq->l_end->n);

  rdq = dq->r_end;
  n = rdq->p + --rdq->n;
  if (n>=NDATA) n -= NDATA;
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
      free_pdeq_block (rdq);
      pr = d_ (df_pdeq, 2);
    }
  }

  VRFY (dq);
  if (d_ (df_pdeq, 3) || pr) printf ("(pdeq_getr %p ==> %p)\n", dq, x);
  return (void *)x;
}


void *
pdeq_getl (pdeq *dq)
{
  pdeq *ldq;
  const void *x;
  int p;
  bool pr = 0;

  VRFY (dq);
  assert (dq->l_end->n);

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
      free_pdeq_block (ldq);
      pr = d_ (df_pdeq, 2);
    }
  }

  VRFY (dq);
  if (d_ (df_pdeq, 3) || pr) printf ("(pdeq_getl %p ==> %p)\n", dq, x);
  return (void *)x;
}


bool
pdeq_contains (pdeq *dq, const void *x)
{
  pdeq *q;

  VRFY (dq);

  q = dq->l_end;
  do {
    int p, ep;

    p = q->p; ep = p + q->n;

    if (ep > NDATA) {
      do if (q->data[p] == x) goto found; while (++p < NDATA);
      p = 0;
      ep -= NDATA;
    }

    while (p < ep) if (q->data[p++] == x) goto found;

    q = q->r;
  } while (q);

  if (d_ (df_pdeq, 3)) printf ("(pdeq_contains %p %p ==> 0)\n", dq, x);
  return 0;

found:  /* The two gotos can be optimized away, if !DEBUG */
  if (d_ (df_pdeq, 3)) printf ("(pdeq_contains %p %p ==> 1)\n", dq, x);
  return 1;
}


void *
pdeq_search (pdeq *dq, cmp_fun cmp, const void *key)
{
  pdeq *q;
  int p;

  VRFY (dq);

  q = dq->l_end;
  do {
    int ep;

    p = q->p; ep = p + q->n;

    if (ep > NDATA) {
      do if (!cmp (q->data[p], key)) goto found; while (++p < NDATA);
      p = 0;
      ep -= NDATA;
    }

    while (p < ep) if (!cmp (q->data[p++], key)) goto found;

    q = q->r;
  } while (q);

  if (d_ (df_pdeq, 3)) printf ("(pdeq_search %p %p %p ==> 0)\n", dq, cmp, key);
  return NULL;

found:  /* The two gotos can be optimized away, if !DEBUG */
  if (d_ (df_pdeq, 3)) printf ("(pdeq_contains %p %p %p ==> %p)\n",
			       dq, cmp, key, q->data[p]);
  return (void *)q->data[p-1];
}


void **
pdeq_copyl (pdeq *dq, const void **dst)
{
  pdeq *q;
  const void **d = dst;

  VRFY (dq);

  q = dq->l_end;
  while (q) {
    int p, n;

    p = q->p; n = q->n;

    if (n + p > NDATA) {
      int nn = NDATA - p;
      memcpy (d, &q->data[p], nn * sizeof (void *)); d += nn;
      p = 0; n -= nn;
    }

    memcpy (d, &q->data[p], n * sizeof (void *)); d += n;

    q = q->r;
  }

  if (d_ (df_pdeq, 3)) printf ("(pdeq_copyl %p %p ==> %p)\n", dq, dst, d);
  return (void **)dst;
}


void **
pdeq_copyr (pdeq *dq, const void **dst)
{
  pdeq *q;
  const void **d = dst;

  VRFY (dq);

  q = dq->r_end;
  while (q) {
    int p, i;

    p = q->p; i = q->n + p - 1;
    if (i >= NDATA) {
      i -= NDATA;
      do *d++ = q->data[i]; while (--i >= 0);
      i = NDATA - 1;
    }

    do *d++ = q->data[i]; while (--i >= p);

    q = q->l;
  }

  if (d_ (df_pdeq, 3)) printf ("(pdeq_copyr %p %p ==> %p)\n", dq, dst, d);
  return (void **)dst;
}
