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
 * @brief   implementation of pmap_new
 * @author  Olaf Liebe
 * @version $Id$
 */
#include "config.h"

#include "pmap_new.h"
#include "hashptr.h"
#include <assert.h>

#define INVL ((void*)-1)

pmap_new_entry_t _pmap_null = { NULL, NULL };
pmap_new_entry_t _pmap_del  = { INVL, INVL };

#define DO_REHASH
#define SCALAR_RETURN
#define HashSet                   pmap_new_t
#define HashSetIterator           pmap_new_iterator_t
#define ValueType                 pmap_new_entry_t
#define NullValue                _pmap_null
#define DeletedValue             _pmap_del
#define SetRangeEmpty(ptr,size)   memset(ptr, 0, (size) * sizeof(HashSetEntry))
#define KeysEqual(map,key1,key2)  (key1.key == key2.key)
#define Hash(map,key1)            hash_ptr(key1.key)
#define EntryIsEmpty(e)           (EntryGetValue(e).key == NULL)
#define EntryIsDeleted(e)         (EntryGetValue(e).key == INVL)

#define hashset_init             pmap_new_init
#define hashset_init_size        pmap_new_init_size
#define hashset_destroy          pmap_new_destroy
#define hashset_insert          _pmap_new_insert
#define hashset_remove          _pmap_new_remove
#define hashset_find            _pmap_new_find
#define hashset_size             pmap_new_size
#define hashset_iterator_init    pmap_new_iterator_init
#define hashset_iterator_next    pmap_new_iterator_next
#define hashset_remove_iterator  pmap_new_remove_iterator

pmap_new_entry_t _pmap_new_insert(pmap_new_t *map, pmap_new_entry_t key);
void             _pmap_new_remove(pmap_new_t *map, const pmap_new_entry_t key);
pmap_new_entry_t _pmap_new_find(const pmap_new_t *map, const pmap_new_entry_t key);

void pmap_new_insert(pmap_new_t *map, void *key, void *value)
{
	assert((key != NULL) && "Invalid key pointer.");
	assert((key != INVL) && "Invalid key pointer.");

	/* Insert the new entry. */
	pmap_new_entry_t entry = { key, value }, result;
	result = _pmap_new_insert(map, entry);
	assert(result.key == key);

	/* We may get an existing value back. However maps usually replace entries
	 * on insert, so delete and retry if required. */
	if (result.value != value) {
		_pmap_new_remove(map, result);
		result = _pmap_new_insert(map, entry);
		assert((result.key == key) && (result.value == value));
	}
}

int pmap_new_remove(pmap_new_t *map, const void *key)
{
	pmap_new_entry_t entry = { (void*)key, NULL };
	size_t size = pmap_new_size(map);

	/* Detect sucessful deletion from the size change. */
	if (size == 0) return 0;
	_pmap_new_remove(map, entry);
	return size - pmap_new_size(map);
}

pmap_new_entry_t pmap_new_find(const pmap_new_t *map, const void *key)
{
	pmap_new_entry_t entry = { (void*)key, NULL };
	return _pmap_new_find(map, entry);
}

void *pmap_new_get(const pmap_new_t *map, const void *key)
{
	return pmap_new_find(map, key).value;
}

int pmap_new_contains(const pmap_new_t *map, const void *key)
{
	return pmap_new_find(map, key).key != NULL;
}

int pmap_new_is_null(const pmap_new_entry_t entry)
{
	return entry.key == NULL;
}

#include "hashset.c"
