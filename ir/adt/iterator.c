
#include <string.h>

#include "pset.h"
#include "list.h"
#include "iterator.h"


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

const iterator_t *pset_iterator = &iterator_pset;


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

const iterator_t *list_iterator = &iterator_list;
