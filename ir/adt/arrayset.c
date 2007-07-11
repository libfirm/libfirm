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
 * @file   arrayset.c
 * @brief  Implementation of sets with sorted arrays.
 * @date   11.07.2007
 * @author Sebastian Hack
 *
 * Follows the same API/specialization scheme as hashset.c and is thus interchangable.
 * To remain compatible, all specializations as in hashset.c must be done.
 * Additionally, we need a ValueCmp macro comapring two values not only for equality.
 * That macro follows the same scheme as the compare functions in qsort(3).
 */

#ifdef HashSet

#include "firm_config.h"
#include "array.h"

static INLINE int hashset_bsearch(ValueType *arr, const ValueType elm)
{
	int res = 0;
	int lo  = 0;
	int hi  = ARR_LEN(arr);

	while(lo < hi) {
		int md     = lo + ((hi - lo) >> 1);
		int cmp    = ValueCmp(arr[md], elm);

		if(cmp < 0)
			lo = md + 1;
		else if(cmp > 0)
			hi = md;
		else {
			res = md;
			break;
		}

		res = lo;
	}

	return res;
}

static INLINE int hashset_is_at(ValueType *arr, int idx, const ValueType what)
{
	return idx < ARR_LEN(arr) && ValueCmp(arr[idx], what) == 0;
}

void hashset_init_size(HashSet *set, size_t expected_elements)
{
	set->arr = NEW_ARR_F(ValueType, expected_elements);
#ifndef NDEBUG
	set->in_order = 1;
#endif
	ARR_SHRINKLEN(set->arr, 0);
}

void hashset_init(HashSet *set)
{
	hashset_init_size(set, 16);
}

void hashset_destroy(HashSet *set)
{
	DEL_ARR_F(set->arr);
}

int hashset_insert(HashSet *set, ValueType elt)
{
	ValueType *arr = set->arr;
	int i, idx     = hashset_bsearch(arr, elt);

#ifndef NDEBUG
	assert(set->in_order);
#endif
	if (!hashset_is_at(arr, idx, elt)) {
		ARR_EXTEND(ValueType, set->arr, 1);
		arr = set->arr;
		for (i = ARR_LEN(arr) - 1; i > idx; --i)
			arr[i] = arr[i - 1];
		arr[idx] = elt;
		return 1;
	}

	return 0;
}

void hashset_insert_quick(HashSet *set, ValueType elt)
{
	ValueType *arr = set->arr;
	ARR_EXTEND(ValueType, set->arr, 1);
	arr = set->arr;
	arr[ARR_LEN(arr) - 1] = elt;
}

static INLINE int _hashset_remove_idx(ValueType *arr, int idx)
{
	int n;

	for (n = ARR_LEN(arr) - 1; idx < n; ++idx)
		arr[idx] = arr[idx + 1];

	return n;
}

void hashset_remove(HashSet *set, const ValueType elt)
{
	ValueType *arr = set->arr;
	int idx        = hashset_bsearch(arr, elt);

#ifndef NDEBUG
	assert(set->in_order);
#endif
	if (hashset_is_at(arr, idx, elt)) {
		int new_len = _hashset_remove_idx(arr, idx);
		ARR_SHRINKLEN(arr, new_len);
	}
}

void hashset_remove_quick(HashSet *set, const ValueType elt)
{
	ValueType *arr = set->arr;
	int idx        = hashset_bsearch(arr, elt);

#ifndef NDEBUG
	set->in_order = 0;
#endif
	if (hashset_is_at(arr, idx, elt)) {
		int n = ARR_LEN(arr);
		if (idx < n - 1)
			arr[idx] = arr[n - 1];
		ARR_SHRINKLEN(arr, n - 1);
	}
}

void hashset_fixup(HashSet *set)
{
	ValueType *arr = set->arr;
	ValueType *tmp;
	int i, n;

	CLONE_ARR_A(ValueType, tmp, arr);

	memcpy(tmp, arr, n * sizeof(arr[0]));
	ARR_SHRINKLEN(arr, 0);
	for (i = 0, n = ARR_LEN(arr); i < n; ++i)
		hashset_insert(set, tmp[0]);
#ifndef NDEBUG
	set->in_order = 1;
#endif
}

ValueType hashset_find(const HashSet *set, const ValueType elt)
{
	int idx = hashset_bsearch(set->arr, elt);
#ifndef NDEBUG
	assert(set->in_order);
#endif
	return hashset_is_at(set->arr, idx, elt) ? set->arr[idx] : NullValue;
}

size_t hashset_size(const HashSet *set)
{
	return ARR_LEN(set->arr);
}

void hashset_iterator_init(HashSetIterator *iter, const HashSet *set)
{
	iter->arr  = set->arr;
	iter->curr = -1;
}

ValueType ir_nodeset_iterator_next(HashSetIterator *iter)
{
	++iter->curr;
	return iter->curr < ARR_LEN(iter->arr) ? iter->arr[iter->curr]  : NullValue;
}

void hashset_remove_iterator(HashSet *set, const HashSetIterator *iter)
{
	(void) set;
	(void) _hashset_remove_idx(iter->arr, iter->curr);
	ARR_SHRINKLEN(iter->arr, ARR_LEN(iter->arr) - 1);
}

#endif
