/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Array --- dynamic & flexible arrays.
 * @author      Markus Armbruster
 */
#include <stdlib.h>

#include "array_t.h"
#include "util.h"
#include "xmalloc.h"

/**
 * An empty dynamic array descriptor.
 */
ir_arr_descr arr_mt_descr = { ARR_D_MAGIC, 0, 0, { { 0 } } };

void ir_verify_arr(const void *arr)
{
#ifndef NDEBUG
	ir_arr_descr *descr = ARR_DESCR(arr);
	assert(descr->magic == ARR_D_MAGIC || descr->magic == ARR_A_MAGIC
			 || descr->magic == ARR_F_MAGIC);
	assert(descr->magic != ARR_F_MAGIC || descr->allocated >= descr->nelts);
#else
	(void) arr;
#endif
}

/**
 * Creates a dynamic array on a obstack.
 *
 * @param obstack    An struct obstack * were the data will be allocated
 * @param nelts      The number of elements
 * @param elts_size  The size of the array elements.
 *
 * @return A pointer to the dynamic array (can be used as a pointer to the
 *         first element of this array).
 *
 * @remark Helper function, use NEW_ARR_D() instead.
 */
void *ir_new_arr_d(struct obstack *obstack, size_t nelts, size_t elts_size)
{
	ir_arr_descr *dp;

	assert(obstack);

	dp = (ir_arr_descr*)obstack_alloc(obstack, ARR_ELTS_OFFS + elts_size);
	ARR_SET_DBGINF(dp, ARR_D_MAGIC);
	dp->allocated = dp->nelts = nelts;
	return dp->elts;
}

/**
 * Creates a flexible array.
 *
 * @param nelts      The number of elements
 * @param elts_size  The size of the array elements.
 *
 * @return A pointer to the flexible array (can be used as a pointer to the
 *         first element of this array).
 *
 * @remark Helper function, use NEW_ARR_F() instead.
 */
void *ir_new_arr_f(size_t nelts, size_t elts_size)
{
	ir_arr_descr *newa;

	newa = (ir_arr_descr*)xmalloc(ARR_ELTS_OFFS+elts_size);
	ARR_SET_DBGINF(newa, ARR_F_MAGIC);
	newa->allocated = newa->nelts = nelts;
	return newa->elts;
}

/**
 * Delete a flexible array.
 *
 * @param elts    The flexible array (pointer to the first element).
 *
 * @remark Helper function, use DEL_ARR_F() instead.
 */
void ir_del_arr_f(void *elts)
{
	ir_arr_descr *dp = ARR_DESCR (elts);

	ARR_VRFY(elts);
	assert(dp->magic == ARR_F_MAGIC);

#ifndef NDEBUG
	dp->magic = 0xdeadbeef;
#endif
	free(dp);
}

/**
 * Resize a flexible array, always reallocate data.
 *
 * @param elts       The flexible array (pointer to the first element).
 * @param nelts      The new number of elements.
 * @param elts_size  The size of the array elements.
 *
 * @return A resized flexible array, possibly other address than
 *         elts.
 *
 * @remark Helper function, use ARR_SETLEN() instead.
 */
void *ir_arr_setlen (void *elts, size_t nelts, size_t elts_size)
{
	ir_arr_descr *dp = ARR_DESCR (elts);

	assert(dp->magic == ARR_F_MAGIC);
	ARR_VRFY(elts);

	dp = (ir_arr_descr*) xrealloc(dp, ARR_ELTS_OFFS+elts_size);
	dp->allocated = dp->nelts = nelts;

	return dp->elts;
}

/**
 * Resize a flexible array, allocate more data if needed but do NOT
 * reduce.
 *
 * @param elts     The flexible array (pointer to the first element).
 * @param nelts    The new number of elements.
 * @param eltsize  The size of the array elements.
 *
 * @return A resized flexible array, possibly other address than
 *         elts.
 *
 * @remark Helper function, use ARR_RESIZE() instead.
 */
void *ir_arr_resize(void *elts, size_t nelts, size_t eltsize)
{
	ir_arr_descr *dp = ARR_DESCR(elts);
	size_t n;

	assert(dp->magic == ARR_F_MAGIC);
	ARR_VRFY(elts);

	/* @@@ lots of resizes for small nelts */
	n = MAX(1, dp->allocated);
	while (nelts > n) n <<= 1;
	while (3*nelts < n) n >>= 1;
	assert(n >= nelts);

	if (n != dp->allocated) {
		dp = (ir_arr_descr*) xrealloc(dp, ARR_ELTS_OFFS+eltsize*n);
		dp->allocated = n;
	}
	dp->nelts = nelts;

	return dp->elts;
}

#ifdef DEBUG_libfirm
/* forward declarations to avoid warnings */
size_t array_len(const void *arr);
ir_arr_descr *array_descr(const void *arr);

/**
 * This function returns the length of a flexible array.
 * Do NOT use is in code, use ARR_LEN() macro!
 * This function is intended to be called from a debugger.
 */
size_t array_len(const void *arr)
{
	return ARR_LEN(arr);
}

/**
 * This function returns the array descriptor of a flexible array.
 * Do NOT use is in code!.
 * This function is intended to be called from a debugger.
 */
ir_arr_descr *array_descr(const void *arr)
{
	if (! arr)
		return NULL;
	return ARR_DESCR(arr);
}
#endif /* DEBUG_libfirm */
