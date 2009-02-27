/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @author    Michael Beck
 * @brief     A linked nodemap.
 * @version   $Id$
 */
#include "config.h"

#include "irlinkednodemap.h"
#include "irnode_t.h"
#include "hashptr.h"

static ir_lnk_nodemap_entry_t null_nodemap_entry;

#define DO_REHASH
#define HashSet                   ir_lnk_nodemap_t
#define HashSetIterator           ir_lnk_nodemap_iterator_t
#define ValueType                 ir_lnk_nodemap_entry_t
#define NullValue                 null_nodemap_entry
#define KeyType                   ir_node*
#define ConstKeyType              const ir_node*
#define GetKey(value)             (value).node
#define InitData(self,value,key)  do { (value).node = (key); (value).list.next = NULL; (value).list.prev = NULL; } while(0)
#define Hash(self,key)            ((unsigned)((key)->node_nr))
#define KeysEqual(self,key1,key2) (key1) == (key2)
#define SetRangeEmpty(ptr,size)   memset(ptr, 0, (size) * sizeof((ptr)[0]))
#define EntrySetEmpty(value)      (value).node = NULL
#define EntrySetDeleted(value)    do { (value).node = (ir_node*) -1; list_del(&(value).list); } while(0)
#define EntryIsEmpty(value)       ((value).node == NULL)
#define EntryIsDeleted(value)     ((value).node == (ir_node*)-1)

#define hashset_init            ir_lnk_nodemap_init
#define hashset_init_size       ir_lnk_nodemap_init_size
#define hashset_destroy         ir_lnk_nodemap_destroy
#define hashset_insert          _ir_lnk_nodemap_insert
#define hashset_remove          ir_lnk_nodemap_remove
#define hashset_find            _ir_lnk_nodemap_find
#define hashset_size            ir_lnk_nodemap_size

#define ADDITIONAL_INIT         INIT_LIST_HEAD(&self->elem_list); INIT_LIST_HEAD(&self->all_iters);
#define ADDITIONAL_TERM         INIT_LIST_HEAD(&self->elem_list); INIT_LIST_HEAD(&self->all_iters);

#define NO_ITERATOR
#define HAVE_OWN_RESIZE

#include "hashset.c"

/**
 * Resize the hashset
 * @internal
 */
static
void resize(HashSet *self, size_t new_size)
{
	HashSetEntry *old_entries = self->entries;
	HashSetEntry *new_entries;
	list_head    list = self->elem_list;
	HashSetEntry *entry;
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
		res &= ir_lnk_nodemap_put(self, EntryGetValue(*entry).node, EntryGetValue(*entry).data);
	}
	/* all re-inserted data must be new, if not, we found a node twice ... */
	assert(res == 1);

	/* now we can free the old array */
	Free(old_entries);
}


int ir_lnk_nodemap_put(ir_lnk_nodemap_t *nodemap, ir_node *node, void *data)
{
	ir_lnk_nodemap_entry_t *entry = _ir_lnk_nodemap_insert(nodemap, node);

	entry->data = data;
	if (entry->list.next == NULL) {
		/* we have added a new element */
		list_add_tail(&entry->list, &nodemap->elem_list);
		return 1;
	}
	return 0;
}

void *ir_lnk_nodemap_get(const ir_lnk_nodemap_t *nodemap, const ir_node *node)
{
	ir_lnk_nodemap_entry_t *entry = _ir_lnk_nodemap_find(nodemap, node);
	return entry->data;
}

/**
 * Initializes a nodemap iterator. Sets the iterator before the first element in
 * the linked nodemap.
 *
 * @param iterator   Pointer to already allocated iterator memory
 * @param nodemap       Pointer to the nodemap
 */
void ir_lnk_nodemap_iterator_init(ir_lnk_nodemap_iterator_t *iterator,
                                  const ir_lnk_nodemap_t *nodemap) {
	iterator->iter    = nodemap->elem_list.next;
	iterator->nodemap = nodemap;
}

/**
 * Advances the iterator and returns the current element or NULL if all elements
 * in the linked nodemap have been processed.
 * @attention It is not allowed to use ir_lnk_nodemap_insert or ir_lnk_nodemap_remove while
 *            iterating over a nodemap.
 *
 * @param iterator  Pointer to the nodemap iterator.
 * @returns         Next element in the nodemap or NULL
 */
ir_node *ir_lnk_nodemap_iterator_next(ir_lnk_nodemap_iterator_t *iterator) {
	ir_node *res;
	if (iterator->iter == &iterator->nodemap->elem_list)
		return NULL;

	res = list_entry(iterator->iter, ir_lnk_nodemap_entry_t, list)->node;
	iterator->iter = iterator->iter->next;

	return res;
}

/**
 * Removes the element the iterator currently points to.
 *
 * @param nodemap   Pointer to the linked nodemap
 * @param iterator  Pointer to the nodemap iterator.
 */
void ir_lnk_nodemap_remove_iterator(ir_lnk_nodemap_t *nodemap,
                                    ir_lnk_nodemap_iterator_t *iterator) {
	ir_lnk_nodemap_entry_t *rem = list_entry(iterator->iter->prev, ir_lnk_nodemap_entry_t, list);

	ir_lnk_nodemap_remove(nodemap, rem->node);
}
