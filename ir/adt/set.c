/* Set --- collection of entries that are unique wrt to a key.
   Copyright (C) 1995, 1996 Markus Armbruster */

/*  This code is derived from:

    From: ejp@ausmelb.oz.AU (Esmond Pitt)
    Date: Tue, 7 Mar 1989 22:06:26 GMT
    Subject: v06i042: dynamic hashing version of hsearch(3)
    Message-ID: <1821@basser.oz>
    Newsgroups: comp.sources.misc
    Sender: msgs@basser.oz

    Posting-number: Volume 6, Issue 42
    Submitted-By: Esmond Pitt <ejp@ausmelb.oz.AU>
    Archive-name: dynamic-hash

    ** Dynamic hashing, after CACM April 1988 pp 446-457, by Per-Ake Larson.
    ** Coded into C, with minor code improvements, and with hsearch(3) interface,
    ** by ejp@ausmelb.oz, Jul 26, 1988: 13:16;

    TODO: Fix Esmond's ugly MixedCapsIdentifiers ;->
 */

/* $Id$ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#ifdef PSET
# define SET pset
# define PMANGLE(pre) pre##_pset
# define MANGLEP(post) pset_##post
# define MANGLE(pre, post) pre##pset##post
# define EQUAL(cmp, elt, key, siz) (!(cmp) ((elt)->entry.dptr, (key)))
#else
# define SET set
# define PMANGLE(pre) pre##_set
# define MANGLEP(post) set_##post
# define MANGLE(pre, post) pre##set##post
# define EQUAL(cmp, elt, key, siz) \
    (((elt)->entry.size == (siz)) && !(cmp) ((elt)->entry.dptr, (key), (siz)))
#endif

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "misc.h"
#ifdef PSET
# include "pset.h"
#else
# include "set.h"
#endif

#define TOBSTACK_ID MANGLEP(tag)
#include "obst.h"


#define SEGMENT_SIZE_SHIFT	8
#define SEGMENT_SIZE		(1 << SEGMENT_SIZE_SHIFT)
#define DIRECTORY_SIZE_SHIFT	8
#define DIRECTORY_SIZE		(1 << DIRECTORY_SIZE_SHIFT)
#define MAX_LOAD_FACTOR	4


typedef struct element {
  struct element *chain;
  MANGLEP (entry) entry;
} Element, *Segment;


struct SET {
  short	p;			/* Next bucket to be split	*/
  short	maxp;			/* upper bound on p during expansion	*/
  int nkey;			/* current # keys	*/
  short	nseg;			/* current # segments	*/
  Segment *dir[DIRECTORY_SIZE];
  MANGLEP(cmp_fun) cmp;		/* function comparing entries */
  int iter_i, iter_j;
  Element *iter_tail;		/* non-NULL while iterating over elts */
#ifdef PSET
  Element *free_list;
#endif
  struct obstack obst;
#ifdef STATS
  int naccess, ncollision, ndups;
  int max_chain_len;
#endif
#ifdef DEBUG
  const char *tag;
#endif
};


#ifdef STATS

void
MANGLEP(stats) (SET *table)
{
  int nfree = 0;
#ifdef PSET
  Element *q = table->free_list;
  while (q) { q = q->chain; ++nfree; }
#endif
  printf ("     accesses  collisions        keys  duplicates     longest      wasted\n%12d%12d%12d%12d%12d%12d\n",
	  table->naccess, table->ncollision, table->nkey, table->ndups, table->max_chain_len, nfree);
}

static inline void
stat_chain_len (SET *table, int chain_len)
{
  table->ncollision += chain_len;
  if (table->max_chain_len < chain_len) table->max_chain_len = chain_len;
}

# define stat_access(table) (++(table)->naccess)
# define stat_dup(table) (++(table)->ndups)

#else /* !STATS */

# define stat_chain_len(table, chain_len) ((void)0)
# define stat_access(table) ((void)0)
# define stat_dup(table) ((void)0)

#endif /* !STATS */

#ifdef DEBUG

const char *MANGLEP(tag);


void
MANGLEP(describe) (SET *table)
{
  int i, j, collide;
  Element *ptr;
  Segment *seg;

  printf ("p=%d maxp=%d nkey=%d nseg=%d\n",
	  table->p, table->maxp, table->nkey, table->nseg);
  for (i = 0;  i < table->nseg;  i++) {
    seg = table->dir[i];
    for (j = 0;  j < SEGMENT_SIZE;  j++) {
      collide = 0;
      ptr = seg[j];
      while (ptr) {
	if (collide) printf ("<%3d>", collide);
	else printf ("table");
	printf ("[%d][%3d]: %u %p\n", i, j, ptr->entry.hash, ptr->entry.dptr);
	ptr = ptr->chain;
	collide++;
      }
    }
  }
}

#endif /* !DEBUG */


SET *
(PMANGLE(new)) (MANGLEP(cmp_fun) cmp, int nslots)
{
  int i;
  SET *table = xmalloc (sizeof (SET));

  /* Adjust nslots up to next power of 2, minimum SEGMENT_SIZE */
  assert (nslots >= 0);
  for (i = SEGMENT_SIZE;  i < nslots;  i <<= 1) assert (i < (i << 1));
  nslots = i >> SEGMENT_SIZE_SHIFT;

  table->nseg = table->p = table->nkey = 0;
  table->maxp = nslots << SEGMENT_SIZE_SHIFT;
  table->cmp = cmp;
  table->iter_tail = NULL;
#ifdef PSET
  table->free_list = NULL;
#endif
  obstack_init (&table->obst);

  /* Make segments */
  for (i = 0;  i < nslots;  ++i) {
    table->dir[i] = (Segment *)obstack_alloc (&table->obst,
					      sizeof (Segment) * SEGMENT_SIZE);

    memset (table->dir[i], 0, sizeof (Segment) * SEGMENT_SIZE);
    table->nseg++;
  }

#ifdef STATS
  table->naccess = table->ncollision = table->ndups = 0;
  table->max_chain_len = 0;
#endif
#ifdef DEBUG
  table->tag = MANGLEP(tag);
#endif
  return table;
}


void
PMANGLE(del) (SET *table)
{
#ifdef DEBUG
  MANGLEP(tag) = table->tag;
#endif
  obstack_free (&table->obst, NULL);
  xfree (table);
}


static inline int
iter_step (SET *table)
{
  if (++table->iter_j >= SEGMENT_SIZE) {
    table->iter_j = 0;
    if (++table->iter_i >= table->nseg) {
      table->iter_i = 0;
      return 0;
    }
  }
  return 1;
}


void *
MANGLEP(first) (SET *table)
{
  assert (!table->iter_tail);
  table->iter_i = 0;
  table->iter_j = 0;
  while (!table->dir[table->iter_i][table->iter_j]) {
    if (!iter_step (table)) return NULL;
  }
  table->iter_tail = table->dir[table->iter_i][table->iter_j];
  assert (table->iter_tail->entry.dptr);
  return table->iter_tail->entry.dptr;
}


void *
MANGLEP(next) (SET *table)
{
  assert (table->iter_tail);
  table->iter_tail = table->iter_tail->chain;
  if (!table->iter_tail) {
    do {
      if (!iter_step (table)) return NULL;
    } while (!table->dir[table->iter_i][table->iter_j]);
    table->iter_tail = table->dir[table->iter_i][table->iter_j];
  }
  assert (table->iter_tail->entry.dptr);
  return table->iter_tail->entry.dptr;
}

void
MANGLEP(break) (SET *table)
{
  assert (table->iter_tail);
  table->iter_tail = NULL;
}


static inline unsigned
Hash (SET *table, unsigned h)
{
  unsigned address;

  address = h & (table->maxp - 1);
  if (address < (unsigned)table->p)
    address = h & ((table->maxp << 1) - 1); /* h % (2*table->maxp) */
  return address;
}


static inline int
loaded (SET *table)
{
  return (  ++table->nkey
	  > (table->nseg << SEGMENT_SIZE_SHIFT) * MAX_LOAD_FACTOR);
}


static void
expand_table (SET *table)
{
  unsigned NewAddress;
  int OldSegmentIndex, NewSegmentIndex;
  int OldSegmentDir, NewSegmentDir;
  Segment *OldSegment;
  Segment *NewSegment;
  Element *Current;
  Element **Previous;
  Element **LastOfNew;

  if (table->maxp + table->p < (DIRECTORY_SIZE << SEGMENT_SIZE_SHIFT)) {
    /* Locate the bucket to be split */
    OldSegmentDir = table->p >> SEGMENT_SIZE_SHIFT;
    OldSegment = table->dir[OldSegmentDir];
    OldSegmentIndex = table->p & (SEGMENT_SIZE-1);

    /* Expand address space; if necessary create a new segment */
    NewAddress = table->maxp + table->p;
    NewSegmentDir = NewAddress >> SEGMENT_SIZE_SHIFT;
    NewSegmentIndex = NewAddress & (SEGMENT_SIZE-1);
    if (NewSegmentIndex == 0) {
      table->dir[NewSegmentDir] =
	(Segment *)obstack_alloc (&table->obst,
				  sizeof(Segment) * SEGMENT_SIZE);
    }
    NewSegment = table->dir[NewSegmentDir];

    /* Adjust state variables */
    table->p++;
    if (table->p == table->maxp) {
      table->maxp <<= 1;	/* table->maxp *= 2	*/
      table->p = 0;
    }
    table->nseg++;

    /* Relocate records to the new bucket */
    Previous = &OldSegment[OldSegmentIndex];
    Current = *Previous;
    LastOfNew = &NewSegment[NewSegmentIndex];
    *LastOfNew = NULL;
    while (Current != NULL) {
      if (Hash (table, Current->entry.hash) == NewAddress) {
	/* move to new chain */
	*LastOfNew = Current;
	*Previous = Current->chain;
	LastOfNew = &Current->chain;
	Current = Current->chain;
	*LastOfNew = NULL;
      } else {
	/* leave on old chain */
	Previous = &Current->chain;
	Current = Current->chain;
      }
    }
  }
}


void *
MANGLE(_,_search) (SET *table,
		   const void *key,
#ifndef PSET
		   size_t size,
#endif
		   unsigned hash,
		   MANGLE(_,_action) action)
{
  unsigned h;
  Segment *CurrentSegment;
  int SegmentIndex;
  MANGLEP(cmp_fun) cmp = table->cmp;
  Segment q;
  int chain_len = 0;

  assert (table);
  assert (!table->iter_tail);
  assert (key);
#ifdef DEBUG
  MANGLEP(tag) = table->tag;
#endif
  stat_access (table);

  /* Find collision chain */
  h = Hash (table, hash);
  SegmentIndex = h & (SEGMENT_SIZE-1);
  CurrentSegment = table->dir[h >> SEGMENT_SIZE_SHIFT];
  assert (CurrentSegment != NULL);
  q = CurrentSegment[SegmentIndex];

  /* Follow collision chain */
  while (q && !EQUAL (cmp, q, key, size)) {
    q = q->chain;
    ++chain_len;
  }

  stat_chain_len (table, chain_len);

  if (!q && (action != MANGLE(_,_find))) { /* not found, insert */
    if (CurrentSegment[SegmentIndex]) stat_dup (table);

#ifdef PSET
    if (table->free_list) {
      q = table->free_list;
      table->free_list = table->free_list->chain;
    } else {
      q = obstack_alloc (&table->obst, sizeof (Element));
    }
    q->entry.dptr = (void *)key;
#else
    obstack_blank (&table->obst, offsetof (Element, entry.dptr));
    obstack_grow (&table->obst, key, size);
    q = obstack_finish (&table->obst);
    q->entry.size = size;
#endif
    q->chain = CurrentSegment[SegmentIndex];
    q->entry.hash = hash;
    CurrentSegment[SegmentIndex] = q;

    if (loaded (table)) {
      expand_table(table);	/* doesn't affect q */
    }
  }

  if (!q) return NULL;
  if (action == MANGLE(_,_hinsert)) return &q->entry;
  return q->entry.dptr;
}


#ifdef PSET

void *
pset_remove (SET *table, const void *key, unsigned hash)
{
  unsigned h;
  Segment *CurrentSegment;
  int SegmentIndex;
  pset_cmp_fun cmp = table->cmp;
  Segment *p;
  Segment q;
  int chain_len = 0;

  assert (table && !table->iter_tail);
  stat_access (table);

  /* Find collision chain */
  h = Hash (table, hash);
  SegmentIndex = h & (SEGMENT_SIZE-1);
  CurrentSegment = table->dir[h >> SEGMENT_SIZE_SHIFT];
  assert (CurrentSegment != NULL);
  p = &CurrentSegment[SegmentIndex];

  /* Follow collision chain */
  while (!EQUAL (cmp, *p, key, size)) {
    p = &(*p)->chain;
    assert (*p);
    ++chain_len;
  }

  stat_chain_len (table, chain_len);

  q = *p;
  *p = (*p)->chain;
  q->chain = table->free_list;
  table->free_list = q;

  return q->entry.dptr;
}


void *
(pset_find) (SET *se, const void *key, unsigned hash)
{
  return pset_find (se, key, hash);
}


void *
(pset_insert) (SET *se, const void *key, unsigned hash)
{
  return pset_insert (se, key, hash);
}


MANGLEP(entry) *
(pset_hinsert) (SET *se, const void *key, unsigned hash)
{
  return pset_hinsert (se, key, hash);
}

#else /* !PSET */

void *
(set_find) (set *se, const void *key, size_t size, unsigned hash)
{
  return set_find (se, key, size, hash);
}


void *
(set_insert) (set *se, const void *key, size_t size, unsigned hash)
{
  return set_insert (se, key, size, hash);
}


set_entry *
(set_hinsert) (set *se, const void *key, size_t size, unsigned hash)
{
  return set_hinsert (se, key, size, hash);
}

#endif /* !PSET */
