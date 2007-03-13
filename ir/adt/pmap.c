/*
 * Project:     libFIRM
 * File name:   ir/adt/pmap.c
 * Purpose:     Datentyp: Vereinfachte Map (hash-map) zum Speichern von
 *              Zeigern/Adressen -> Zeigern/Adressen.
 * Author:      Hubert Schmid
 * Modified by:
 * Created:     09.06.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
# include <config.h>
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
