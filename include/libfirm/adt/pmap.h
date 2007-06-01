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
 * @brief       Simplified hashnap for pointer->pointer relations
 * @author      Hubert Schmid
 * @date        09.06.2002
 * @version     $Id$
 */
#ifndef FIRM_ADT_PMAP_H
#define FIRM_ADT_PMAP_H

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

#endif
