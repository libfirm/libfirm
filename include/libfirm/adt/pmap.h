/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Simplified hashmap for pointer->pointer relations
 * @author      Hubert Schmid
 * @date        09.06.2002
 */
#ifndef FIRM_ADT_PMAP_H
#define FIRM_ADT_PMAP_H

#include <stddef.h>

#include "../begin.h"

/**
 * @ingroup adt
 * @defgroup pmap Pointer Map
 * Pointer->Pointer hashmap
 * @{
 */

/**  A map which maps addresses to addresses. */
typedef struct pmap pmap;

/**
 * A key, value pair.
 */
typedef struct {
	void const *key;    /**< The key. */
	void       *value;  /**< The value. */
} pmap_entry;


/** Creates a new empty map. */
FIRM_API pmap *pmap_create(void);

/** Creates a new empty map with an initial number of slots. */
FIRM_API pmap *pmap_create_ex(size_t slots);

/** Deletes a map. */
FIRM_API void pmap_destroy(pmap *map);

/**
 *  Inserts a pair (key,value) into the map. If an entry with key
 * "key" already exists, its "value" is overwritten.
 */
FIRM_API void pmap_insert(pmap *map, void const *key, void *value);

/** Checks if an entry with key "key" exists. */
FIRM_API int pmap_contains(pmap const *map, void const *key);

/** Returns the key, value pair of "key". */
FIRM_API pmap_entry *pmap_find(pmap const *map, void const *key);

/** Returns the value of "key". */
FIRM_API void *pmap_get(pmap const *map, void const *key);

/**
 * Returns the value of "key".
 * This is a wrapper for pmap_get(map, key); It allows to express the
 * intended type of the set elements (instead of weakly typed void*).
 */
#define pmap_get(type, map, key) ((type*)pmap_get(map, key))

/** Return number of elements in the map */
FIRM_API size_t pmap_count(pmap const *map);

/**
 * Returns the first entry of a map if the map is not empty.
 */
FIRM_API pmap_entry *pmap_first(pmap *map);

/**
 * Returns the next entry of a map or NULL if all entries were visited.
 */
FIRM_API pmap_entry *pmap_next(pmap *map);

/**
 * Iterate over all elements in the map setting curr to the current element.
 */
#define foreach_pmap(pmap, curr) \
	for (pmap_entry *curr = pmap_first(pmap); curr; curr = pmap_next(pmap))

/** Breaks an iteration.
 *  Must be called, if a iteration ends before pmap_next() returns NULL. */
FIRM_API void pmap_break(pmap *map);

/**
 * @}
 */

#include "../end.h"

#endif
