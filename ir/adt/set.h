/* Declarations for set.
   Copyright (C) 1995, 1996 Markus Armbruster */

/* $Id$ */

/**
 * @file set.h
 *
 * Declarations for set.
 */

#ifndef _SET_H
#define _SET_H

#include <stddef.h>

/** The type of a set. */
typedef struct set set;

/** an entry of a set */
typedef struct set_entry {
  unsigned hash;
  size_t size;
  int dptr[1];			/**< data copied in must not need more
				   alignment than this */
} set_entry;

/** the type of a set compare function */
typedef int (*set_cmp_fun) (const void *elt, const void *key, size_t size);

/** creates a new set */
set *new_set (set_cmp_fun, int slots);

/** deletes a set */
void del_set (set *);

void *set_find (set *, const void *key, size_t, unsigned hash);
void *set_insert (set *, const void *key, size_t, unsigned hash);
set_entry *set_hinsert (set *, const void *key, size_t, unsigned hash);

/** returns the first element of a set */
void *set_first (set *);

/** returns the next element of a set */
void *set_next (set *);

/** breaks the iteration of a set. Must be called before the next set_first() call */
void set_break (set *);

#define new_set(cmp, slots) (SET_TRACE (new_set) ((cmp), (slots)))
#define set_find(set, key, size, hash) \
  _set_search ((set), (key), (size), (hash), _set_find)
#define set_insert(set, key, size, hash) \
  _set_search ((set), (key), (size), (hash), _set_insert)
#define set_hinsert(set, key, size, hash) \
  ((set_entry *)_set_search ((set), (key), (size), (hash), _set_hinsert))

#define SET_VRFY(set) (void)0

#ifdef STATS
void set_stats (set *);
#else
# define set_stats(s) ((void)0)
#endif

#ifdef DEBUG
void set_describe (set *);
#endif


/* Private */

typedef enum { _set_find, _set_insert, _set_hinsert } _set_action;

void *_set_search (set *, const void *, size_t, unsigned, _set_action);

#if defined(DEBUG) && defined(HAVE_GNU_MALLOC)
extern const char *set_tag;
# ifdef SET_ID
#   define SET_TRACE set_tag = SET_ID,
# else
#   define SET_TRACE set_tag = __FILE__,
# endif
#else /* !(DEBUG && HAVE_GNU_MALLOC) */
#   define SET_TRACE
#endif /* !(DEBUG && HAVE_GNU_MALLOC) */

#endif
