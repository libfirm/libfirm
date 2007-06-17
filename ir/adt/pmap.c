/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief       simplified hashmap for pointer -> pointer mappings
 * @author      Hubert Schmid
 * @date        09.06.2002
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "pmap.h"

#include <assert.h>
#include "set.h"
#include "hashptr.h"


struct pmap {
  	int dummy; /* dummy entry */
};


#define INITIAL_SLOTS 64

/** map a pmap into a set */
#define M2S(map)  (set *)(map)

/**
 * compare the keys of two entry pairs
 */
static int pmap_entry_cmp(const void *p1, const void *p2, size_t size) {
	const pmap_entry *entry1 = p1;
	const pmap_entry *entry2 = p2;
	(void) size;

	return entry1->key != entry2->key;
}

/* Creates a new empty map with an initial number of slots. */
pmap *pmap_create_ex(int slots) {
  	return (pmap *)new_set(pmap_entry_cmp, slots);
}

pmap *pmap_create(void) {
	return pmap_create_ex(INITIAL_SLOTS);
}

void pmap_destroy(pmap *map) {
	del_set(M2S(map));
}

void pmap_insert(pmap *map, const void *key, void *value) {
	pmap_entry entry, *p;

	entry.key = key;
	p = set_insert(M2S(map), &entry, sizeof(pmap_entry), HASH_PTR(key));
	p->value = value;
}

int pmap_contains(pmap *map, const void *key) {
	return set_find(M2S(map), &key, sizeof(pmap_entry), HASH_PTR(key)) != NULL;
}

pmap_entry * pmap_find(pmap *map, const void *key) {
	return (pmap_entry *)set_find(M2S(map), &key, sizeof(pmap_entry), HASH_PTR(key));
}


void * pmap_get(pmap *map, const void *key) {
	pmap_entry * entry = pmap_find(map, key);
	return entry == NULL ? NULL : entry->value;
}

int pmap_count(pmap *map) {
	return set_count(M2S(map));
}

pmap_entry *pmap_first(pmap *map) {
	return (pmap_entry *) set_first(M2S(map));
}

pmap_entry *pmap_next(pmap *map) {
	return (pmap_entry *) set_next(M2S(map));
}

void pmap_break(pmap *map) {
	set_break(M2S(map));
}
