/*
 * Project:     libFIRM
 * File name:   ir/adt/eset.c
 * Purpose:     Datentyp: Vereinfachte Map (hash-map) zum Speichern von
 *              Zeigern/Adressen -> Zeigern/Adressen.
 * Author:      Hubert Schmid
 * Modified by:
 * Created:     09.06.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002 Universit�t Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef _PMAP_H_
#define _PMAP_H_

/**  A map which maps addresses to addresses. */
typedef struct pmap pmap;

/**
 * A key, value pair.
 */
typedef struct pmap_entry {
	const void *key;    /**< The key. */
	void *value;  /**< The value. */
} pmap_entry;


/** Creates a new empty map. */
pmap *pmap_create(void);

/** Creates a new empty map with an initial number of slots. */
pmap *pmap_create_ex(int slots);

/** Deletes a map. */
void pmap_destroy(pmap *);

/**
 *  Inserts a pair (key,value) into the map. If an entry with key
 * "key" already exists, its "value" is overwritten.
 */
void pmap_insert(pmap *map, const void * key, void * value);

/** Checks if an entry with key "key" exists. */
int pmap_contains(pmap *map, const void * key);

/** Returns the key, value pair of "key". */
pmap_entry * pmap_find(pmap *map, const void * key);

/** Returns the value of "key". */
void * pmap_get(pmap *map, const void * key);

int pmap_count(pmap *map);

/**
 * Returns the first entry of a map if the map is not empty.
 */
pmap_entry *pmap_first(pmap *map);

/**
 * Returns the next entry of a map or NULL if all entries were visited.
 */
pmap_entry *pmap_next(pmap *);

#define pmap_foreach(pmap, curr) \
	for (curr = pmap_first(pmap); curr; curr = pmap_next(pmap))

/** Breaks an iteration.
 *  Must be called, if a iteration ends before p_map_next() returns NULL.
 */
void pmap_break(pmap *map);

#endif /* _PMAP_H_ */
