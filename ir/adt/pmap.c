/*
 * Project:     libFIRM
 * File name:   ir/adt/eset.c
 * Purpose:     Datentyp: Vereinfachte Map (hash-map) zum Speichern von
 *              Zeigern/Adressen -> Zeigern/Adressen.
 * Author:      Hubert Schmid
 * Modified by:
 * Created:     09.06.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include "pmap.h"

#include <assert.h>
#include "set.h"
#include "hashptr.h"


struct pmap {
  int dummy; /* dummy entry */
};


#define INITIAL_SLOTS 64


static int pmap_entry_cmp(const void *p1, const void *p2, size_t size) {
  const pmap_entry *entry1 = p1;
  const pmap_entry *entry2 = p2;

  return entry1->key != entry2->key;
}


pmap *pmap_create(void) {
  return (pmap *)new_set(pmap_entry_cmp, INITIAL_SLOTS);
}


void pmap_destroy(pmap *map) {
  del_set((set *)map);
}


void pmap_insert(pmap *map, void *key, void *value) {
  if (pmap_contains(map, key)) {
    pmap_entry * entry = pmap_find(map, key);
    entry->value = value;
  } else {
    pmap_entry entry;
    entry.key = key;
    entry.value = value;
    set_insert((set *)map, &entry, sizeof(pmap_entry), HASH_PTR(key));
  }
}


int pmap_contains(pmap *map, void *key) {
  return set_find((set *)map, &key, sizeof(pmap_entry), HASH_PTR(key)) != NULL;
}


pmap_entry * pmap_find(pmap *map, void *key) {
  return (pmap_entry *)set_find((set *)map, &key, sizeof(pmap_entry), HASH_PTR(key));
}


void * pmap_get(pmap *map, void *key) {
  pmap_entry * entry = pmap_find(map, key);
  return entry == NULL ? NULL : entry->value;
}


pmap_entry *pmap_first(pmap *map) {
  return (pmap_entry *) set_first((set *)map);
}


pmap_entry *pmap_next(pmap *map) {
  return (pmap_entry *) set_next((set *)map);
}
