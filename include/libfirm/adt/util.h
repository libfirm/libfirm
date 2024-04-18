/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @date   31.05.2005
 * @author Sebastian Hack
 * @brief  Miscellaneous utility macros.
 */
#ifndef FIRM_ADT_UTIL_H
#define FIRM_ADT_UTIL_H

#include <stdbool.h>
#include <string.h>

/**
 * Returns size of a static array. Warning: This returns invalid values for
 * dynamically allocated arrays.
 *
 * @param a    static array
 */
#define ARRAY_SIZE(a) (sizeof(a)/sizeof((a)[0]))

#undef MIN
#undef MAX
#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define MIN(x, y) ((x) < (y) ? (x) : (y))

/**
 * Three valued compare as demanded by e.g. qsort(3)
 * @param c A number.
 * @param d Another number.
 * @return 0 if c == d, -1 if c < d, 1 if c > d.
 */
#define QSORT_CMP(c, d) (((c) > (d)) - ((c) < (d)))

/**
 * convert an integer into pointer
 */
#define INT_TO_PTR(v) ((void*)(intptr_t)(v))

/**
 * convert a pointer into an integer
 */
#define PTR_TO_INT(v) ((intptr_t)(v))

#define QSORT(base, n, cmp) (qsort((base), (n), sizeof(*(base)), cmp))

#define QSORT_ARR(base, cmp) QSORT((base), ARR_LEN((base)), (cmp))

static inline void *safe_memcpy(void* const dst, void const* const src, size_t const n)
{
	/* Calling memcpy with a null pointer leads to undefined behavior,
	 * even if we copy zero bytes (C99 7.21.1.p2). */
	return n != 0 ? memcpy(dst, src, n) : dst;
}

#define MEMCPY(dst, src, n) safe_memcpy((dst), (src), (n) * sizeof(*(1 ? (dst) : (src))))

static inline bool is_digit(char const c)
{
	return '0' <= c && c <= '9';
}

static inline bool streq(char const *const a, char const *const b)
{
	return strcmp(a, b) == 0;
}

static inline char const* strstart(char const* str, char const* start)
{
	do {
		if (*start == '\0')
			return str;
	} while (*str++ == *start++);
	return NULL;
}

#endif
