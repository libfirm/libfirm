/* Declarations for pdeq.
   Copyright (C) 1995, 1996 Christian von Roques */

/* $Id$ */

#ifndef _PDEQ_H
#define _PDEQ_H

#include "misc.h"

typedef struct pdeq pdeq;

pdeq *new_pdeq (void);
pdeq *new_pdeq1 (const void *);
void del_pdeq (pdeq *);
int pdeq_len (pdeq *);
bool pdeq_empty (pdeq *);
bool pdeq_contains (pdeq *, const void *);
void *pdeq_search (pdeq *, cmp_fun cmp, const void *key);
void **pdeq_copyl (pdeq *, const void **);
void **pdeq_copyr (pdeq *, const void **);
pdeq *pdeq_putl (pdeq *, const void *);
pdeq *pdeq_putr (pdeq *, const void *);
void *pdeq_getl (pdeq *);
void *pdeq_getr (pdeq *);

#ifdef NDEBUG
#define PDEQ_VRFY(deq) ((void)0)
#else
#define PDEQ_VRFY(deq) _pdeq_vrfy ((deq))
void _pdeq_vrfy(pdeq *dq);
#endif

#endif
