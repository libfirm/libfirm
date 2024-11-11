/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       simplified hashmap for pointer -> pointer mappings
 * @author      Hubert Schmid
 * @date        09.06.2002
 */
#include "pmap.h"

#include "hashptr.h"
#include "set.h"

#define INITIAL_SLOTS 64

/** map a pmap into a set */
#define M2S(map)  (set*)(map)

/**
 * compare the keys of two entry pairs
 */
static int pmap_entry_cmp(void const *p1, void const *p2, size_t size)
{
	const pmap_entry *entry1 = (const pmap_entry*)p1;
	const pmap_entry *entry2 = (const pmap_entry*)p2;
	(void)size;

	return entry1->key != entry2->key;
}

pmap *pmap_create_ex(size_t slots)
{
	return (pmap *)new_set(pmap_entry_cmp, slots);
}

pmap *pmap_create(void)
{
	return pmap_create_ex(INITIAL_SLOTS);
}

void pmap_destroy(pmap *map)
{
	del_set(M2S(map));
}

void pmap_insert(pmap *map, const void *key, void *value)
{
	pmap_entry entry, *p;

	entry.key = key;
	p = set_insert(pmap_entry, M2S(map), &entry, sizeof(pmap_entry), hash_ptr(key));
	p->value = value;
}

int pmap_contains(pmap const *map, const void *key)
{
	return pmap_find(map, key) != NULL;
}

pmap_entry *pmap_find(pmap const *map, const void *key)
{
	pmap_entry const entry = { key, 0 };
	return set_find(pmap_entry, M2S(map), &entry, sizeof(entry), hash_ptr(key));
}


void *(pmap_get)(pmap const *map, const void *key)
{
	pmap_entry * entry = pmap_find(map, key);
	return entry == NULL ? NULL : entry->value;
}

size_t pmap_count(pmap const *map)
{
	return set_count(M2S(map));
}

pmap_entry *pmap_first(pmap *map)
{
	return set_first(pmap_entry, M2S(map));
}

pmap_entry *pmap_next(pmap *map)
{
	return set_next(pmap_entry, M2S(map));
}

void pmap_break(pmap *map)
{
	set_break(M2S(map));
}
