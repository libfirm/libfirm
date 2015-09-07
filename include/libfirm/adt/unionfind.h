/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief      Union-Find data structure
 * @author     Matthias Braun
 */
#ifndef FIRM_ADT_UNIONFIND_H
#define FIRM_ADT_UNIONFIND_H

#include <assert.h>

#include "../begin.h"

/**
 * @ingroup adt
 * @defgroup unionfind Union-Find
 *  Union-Find data structure
 *
 *  This implementation uses weighted sets and path compression which results
 *  in (nearly) O(n) complexity for n find and union operations
 * @{
 */

/**
 * Call this to initialize an array of @p count elements to be used by the
 * union find functions.
 *
 * @param data    The array (you have to allocate it yourself)
 * @param n_elems number of elements handled by the data structure
 */
static inline void uf_init(int *const data, size_t const n_elems)
{
	for (size_t i = 0; i < n_elems; ++i) {
		data[i] = -1;
	}
}

/**
 * Merge 2 sets (union operation). Note that you have to pass the
 * representatives of the sets and not just random elements
 *
 * @param data  The union find data
 * @param set1  Representative of set1
 * @param set2  Representative of set2
 * @return      the new representative of the set (which is set1 or set2)
 */
static inline int uf_union(int *const data, int const set1, int const set2)
{
	if (set1 == set2)
		return set1;

	/* need 2 set representatives */
	int const d1 = data[set1];
	int const d2 = data[set2];
	assert(d1 < 0 && d2 < 0);

	int const newcount = d1 + d2;
	if (d1 > d2) {
		data[set1] = set2;
		data[set2] = newcount;
		return set2;
	} else {
		data[set2] = set1;
		data[set1] = newcount;
		return set1;
	}
}

/**
 * Finds the representative for the set with member @p e.
 * The representative of a set is unique, so if the find operations finds
 * the same/different representatives, then the elements are in the
 * the same/different sets.
 *
 * @param data  The union find data
 * @param e     The element
 * @return      The representative of the set that contains @p e
 */
static inline int uf_find(int *const data, int const e)
{
	/* go through list to find representative */
	int repr = e;
	while (data[repr] >= 0) {
		repr = data[repr];
	}

	/* update list to point to new representative (path compression) */
	int t = e;
	while (t != repr) {
		int const next = data[t];
		data[t] = repr;
		t = next;
	}

	return repr;
}

/** @} */

#include "../end.h"

#endif
