/**
 * Iterators for the several collection types used in firm.
 * Useful for formatted and unified dumping of collections of objects.
 * @author Sebastian Hack
 * @date 29.11.2004
 */
#ifndef _ITERATOR_H
#define _ITERATOR_H

#include "fourcc.h"

#define ITERATOR_MAGIC FOURCC('I', 'T', 'E', 'R')

/**
 * Check, if some memory object appears to be an iterator.
 * @param ptr Some memory.
 * @return 1, if that memory area appears to be an iterator, 0 if not.
 */
#define is_iterator(ptr) (((const iterator_t *) (ptr))->magic == ITERATOR_MAGIC)

typedef struct _iterator_t {
	unsigned magic;
	void *(*start)(void *collection);
	void *(*next)(void *collection, void *curr);
	void (*finish)(void *collection, void *curr);
} iterator_t;

/**
 * An iterator implementation for linked lists.
 */
extern const iterator_t *list_iterator;

/**
 * An iterator implementation for psets.
 */
extern const iterator_t *pset_iterator;

#endif
