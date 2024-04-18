/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Array --- dynamic & flexible arrays.
 * @author      Markus Armbruster
 */
#include "array.h"

#include "fourcc.h"
#include "util.h"
#include "xmalloc.h"
#include <stdlib.h>

#define ARR_D_MAGIC FOURCC('A','R','R','D')
#define ARR_F_MAGIC FOURCC('A','R','R','F')

/**
 * An empty dynamic array descriptor.
 */
ir_arr_descr arr_mt_descr = { ARR_D_MAGIC, 0, 0 };

void ir_verify_arr(const void *arr)
{
#ifndef NDEBUG
	ir_arr_descr *const descr = ARR_DESCR(arr);
	assert(descr->magic == ARR_D_MAGIC || descr->magic == ARR_F_MAGIC);
	assert(descr->allocated >= descr->nelts);
#else
	(void) arr;
#endif
}

void *ir_new_arr_d(struct obstack *obstack, size_t nelts, size_t elts_size)
{
	ir_arr_descr *const dp
		= (ir_arr_descr*)obstack_alloc(obstack, sizeof(*dp) + elts_size);
#ifndef NDEBUG
	dp->magic = ARR_D_MAGIC;
#endif
	dp->allocated = dp->nelts = nelts;
	return dp->elts;
}

void *ir_new_arr_f(size_t nelts, size_t elts_size)
{
	ir_arr_descr *const dp = (ir_arr_descr*)xmalloc(sizeof(*dp)+elts_size);
#ifndef NDEBUG
	dp->magic = ARR_F_MAGIC;
#endif
	dp->allocated = dp->nelts = nelts;
	return dp->elts;
}

void DEL_ARR_F(void *elts)
{
	ir_verify_arr(elts);

	ir_arr_descr *const dp = ARR_DESCR(elts);
	assert(dp->magic == ARR_F_MAGIC);
#ifndef NDEBUG
	dp->magic = 0xdeadbeef;
#endif
	free(dp);
}

void *ir_arr_setlen(void *elts, size_t nelts, size_t elts_size)
{
	ir_verify_arr(elts);

	ir_arr_descr *dp = ARR_DESCR(elts);
	assert(dp->magic == ARR_F_MAGIC);
	dp = (ir_arr_descr*)xrealloc(dp, sizeof(*dp)+elts_size);
	dp->allocated = dp->nelts = nelts;
	return dp->elts;
}

void *ir_arr_resize(void *elts, size_t nelts, size_t eltsize)
{
	ir_verify_arr(elts);

	ir_arr_descr *dp = ARR_DESCR(elts);
	assert(dp->magic == ARR_F_MAGIC);

	/* @@@ lots of resizes for small nelts */
	size_t n = MAX(1, dp->allocated);
	while (nelts > n)
		n <<= 1;
	while (3*nelts < n)
		n >>= 1;
	assert(n >= nelts);

	if (n != dp->allocated) {
		dp = (ir_arr_descr*)xrealloc(dp, sizeof(*dp) + eltsize*n);
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
	if (arr == NULL)
		return NULL;
	return ARR_DESCR(arr);
}

#endif /* DEBUG_libfirm */
