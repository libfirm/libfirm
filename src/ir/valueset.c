/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @author    Michael Beck
 * @brief     A value set, containing expression for values.
 */
#include "valueset.h"

#include "hashptr.h"
#include "irnode_t.h"
#include "iropt_t.h"

static ir_valueset_entry_t null_valueset_entry;

#undef DO_REHASH
#define HashSet                   ir_valueset_t
#define HashSetIterator           ir_valueset_iterator_t
#define ValueType                 ir_valueset_entry_t
#define NullValue                 null_valueset_entry
#define KeyType                   ir_node*
#define ConstKeyType              const ir_node*
#define GetKey(entry)             (entry).value
#define InitData(self,entry,key)  do { (entry).value = (key); (entry).list.next = NULL; (entry).list.prev = NULL; } while (0)
#define Hash(self,key)            ir_node_hash(key)
#define KeysEqual(self,key1,key2) (key1) == (key2)
#define SetRangeEmpty(ptr,size)   memset(ptr, 0, (size) * sizeof((ptr)[0]))
#define EntrySetEmpty(entry)      (entry).value = NULL
#define EntrySetDeleted(entry)    do { (entry).data.value = (ir_node*) -1; list_del(&(entry).data.list); } while (0)
#define EntryIsEmpty(entry)       ((entry).data.value == NULL)
#define EntryIsDeleted(entry)     ((entry).data.value == (ir_node*)-1)

#define hashset_init            ir_valueset_init
#define hashset_init_size       ir_valueset_init_size
#define hashset_destroy         ir_valueset_destroy
ir_valueset_entry_t *ir_valueset_insert_(ir_valueset_t *self, ir_node *value);
#define hashset_insert          ir_valueset_insert_
#define hashset_remove          ir_valueset_remove
ir_valueset_entry_t *ir_valueset_find_(const ir_valueset_t *self,
                                       const ir_node *value);
#define hashset_find            ir_valueset_find_
#define hashset_size            ir_valueset_size

#define ADDITIONAL_INIT         INIT_LIST_HEAD(&self->elem_list); INIT_LIST_HEAD(&self->all_iters);
#define ADDITIONAL_TERM         INIT_LIST_HEAD(&self->elem_list); INIT_LIST_HEAD(&self->all_iters);

#define HAVE_OWN_RESIZE

#include "hashset.c.h"

/**
 * Resize the hashset
 * @internal
 */
static void resize(HashSet *self, size_t new_size)
{
	HashSetEntry *old_entries = self->entries;
	HashSetEntry *new_entries;
	list_head    list = self->elem_list;
	int          res = 1;

	/* allocate a new array with double size */
	new_entries = Alloc(new_size);
	SetRangeEmpty(new_entries, new_size);

	/* use the new array */
	self->entries      = new_entries;
	self->num_buckets  = new_size;
	self->num_elements = 0;
	self->num_deleted  = 0;
#ifndef NDEBUG
	self->entries_version++;
#endif
	reset_thresholds(self);

	assert(!list_empty(&self->elem_list));
	list.next->prev = &list;
	list.prev->next = &list;

	/* reinsert all elements */
	INIT_LIST_HEAD(&self->elem_list);
	list_for_each_entry(ValueType, entry, &list, list) {
		res &= ir_valueset_insert(self, entry->value, entry->expr);
	}
	/* all re-inserted data must be new, if not, we found a node twice ... */
	assert(res == 1);
	(void)res;

	/* now we can free the old array */
	Free(old_entries);
}

int ir_valueset_insert(ir_valueset_t *valueset, ir_node *value, ir_node *expr)
{
	ir_valueset_entry_t *entry = ir_valueset_insert_(valueset, value);

	if (entry->list.next != NULL) {
		/* this value is already inserted, do nothing */
		return 0;
	}

	/* new element added */
	entry->expr = expr;
	list_add_tail(&entry->list, &valueset->elem_list);
	return 1;
}

int ir_valueset_replace(ir_valueset_t *valueset, ir_node *value, ir_node *expr)
{
	int res = 0;
	ir_valueset_entry_t *entry = ir_valueset_insert_(valueset, value);

	if (entry->expr != expr) {
		entry->expr = expr;
		res = 1;
	}
	if (entry->list.next == NULL) {
		/* we have added a new element */
		list_add_tail(&entry->list, &valueset->elem_list);
		return 1;
	}
	return res;
}

void *ir_valueset_lookup(const ir_valueset_t *valueset, const ir_node *value)
{
	ir_valueset_entry_t *entry = ir_valueset_find_(valueset, value);
	if (entry != NULL)
		return entry->expr;
	return NULL;
}

void ir_valueset_iterator_init(ir_valueset_iterator_t *iterator,
                               const ir_valueset_t *valueset)
{
	iterator->iter     = valueset->elem_list.next;
	iterator->valueset = valueset;
}

ir_node *ir_valueset_iterator_next(ir_valueset_iterator_t *iterator, ir_node **expr)
{
	ir_valueset_entry_t *entry;

	if (iterator->iter == &iterator->valueset->elem_list) {
		*expr = NULL;
		return NULL;
	}

	entry = list_entry(iterator->iter, ir_valueset_entry_t, list);
	iterator->iter = iterator->iter->next;

	*expr = entry->expr;
	return entry->value;
}

void ir_valueset_remove_iterator(ir_valueset_t *valueset, ir_valueset_iterator_t *iterator)
{
	ir_valueset_entry_t *rem = list_entry(iterator->iter->prev, ir_valueset_entry_t, list);

	ir_valueset_remove(valueset, rem->value);
}
