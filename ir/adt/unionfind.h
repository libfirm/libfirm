/*
 * Project:     libFIRM
 * File name:   ir/adt/unionfind.h
 * Purpose:     Union-Find datastructure
 * Author:      Matthias Braun
 * Modified by:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2006, Matthias Braun
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file unionfind.h
 *
 * Union-Find datastructure
 *
 * This implementation uses weighted sets and path compression which results
 * in O(n) complexity for n find and union operations (actually it's
 * n * alpha(n) with alpha being the inverse of the ackermann function and
 * therefore smaller than 5 for all 64bit values of n)
 */
#ifndef _UNIONFIND_H
#define _UNIONFIND_H

#include <assert.h>

/**
 * Call this to initialize an array of @p count elements to be used by the
 * union find functions.
 *
 * @param data	The array (you have to allocate it yourself)
 * @param from	The first element that should be intialized
 * @param to	the index of the first element which is not initialized anymore
 */
static INLINE void uf_init(int* data, int from, int to)
{
	int i;
	for(i = from; i < to; ++i) {
		data[i] = -1;
	}
}

/**
 * Merge 2 sets (union operation). Note that you have to pass the
 * representatives of the sets and not just random elements
 *
 * @param data	The union find data
 * @param set1	Representative of set1
 * @param set2	Representative of set2
 * @return		0 if the new union set is represented by set1, 1 if it is
 *              represented by set2
 */
static INLINE int uf_union(int* data, int set1, int set2) {
	int d1 = data[set1];
	int d2 = data[set2];
	int newcount;

	if(set1 == set2)
		return 0;

	// need 2 set represantatives
	assert(d1 < 0 && d2 < 0);

	newcount = d1 + d2;
	if(d1 > d2) {
		data[set1] = set2;
		data[set2] = newcount;
		return 1;
	} else {
		data[set2] = set1;
		data[set1] = newcount;
		return 0;
	}
}

/**
 * Finds the representative for the set with member @p e.
 * The representative of a set is unique, so if the find operations finds
 * the same/different representatives, then the elements are in the
 * the same/different sets.
 *
 * @param data	The union find data
 * @param e		The element
 * @return		The representative of the set that contains @p e
 */
static INLINE int uf_find(int* data, int e) {
	// go through list to find representative
    int repr = e;
	while(data[repr] >= 0) {
		repr = data[repr];
	}

	// update list to point to new representative (path compression)
	while(e != repr) {
		int next = data[e];
		data[e] = repr;
		e = next;
	}

	return repr;
}

#endif
