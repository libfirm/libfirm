
#include <string.h>

#include "pset.h"
#include "list.h"
#include "iterator.h"

#define ITERATOR_MAGIC "ITR"

int is_iterator(const void *ptr)
{
	const iterator_t *it = ptr;
	return strncmp(it->magic, ITERATOR_MAGIC, sizeof(ITERATOR_MAGIC)) == 0;
}

static void *it_pset_start(void *collection)
{
	return pset_first(collection);
}

static void *it_pset_next(void *collection, void *curr)
{
	return pset_next(collection);
}

static void it_pset_finish(void *collection, void *curr)
{
}

static const iterator_t iterator_pset = {
	ITERATOR_MAGIC,
	it_pset_start,
	it_pset_next,
	it_pset_finish
};

const iterator_t *it_pset = &iterator_pset;


static void *it_list_next(void *coll, void *it)
{
	struct list_head *head = coll;
	struct list_head *curr = it;
	return curr->next != head ? curr->next : NULL;
}

static void *it_list_start(void *coll)
{
	return it_list_next(coll, coll);
}

static void it_list_finish(void *coll, void *curr)
{
}

static const iterator_t iterator_list = {
	ITERATOR_MAGIC,
	it_list_start,
	it_list_next,
	it_list_finish
};

const iterator_t *it_list = &iterator_list;
