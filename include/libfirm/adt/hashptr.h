/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Hash functions
 * @author      Michael Beck, Sebastian Hack
 */
#ifndef FIRM_ADT_HASHPTR_H
#define FIRM_ADT_HASHPTR_H

#include <stddef.h>

#include "../begin.h"

/**
 * @ingroup algorithms
 * @defgroup hashptr Hash Functions
 * @{
 */

/** @cond PRIVATE */

#define _FIRM_FNV_OFFSET_BASIS 2166136261U
#define _FIRM_FNV_FNV_PRIME    16777619U

/** @endcond */

/**
 * Returns a hash value for a block of data.
 */
static inline unsigned hash_data(unsigned char const *data, size_t bytes)
{
	unsigned hash = _FIRM_FNV_OFFSET_BASIS;
	for (size_t i = 0; i < bytes; ++i) {
		hash *= _FIRM_FNV_FNV_PRIME;
		hash ^= data[i];
	}

	return hash;
}

/**
 * Returns a hash value for a string.
 * @param str The string (can be const).
 * @return A hash value for the string.
 */
static inline unsigned hash_str(char const *const str)
{
	unsigned hash = _FIRM_FNV_OFFSET_BASIS;
	for(char const *c = str; *c != '\0'; ++c) {
		hash *= _FIRM_FNV_FNV_PRIME;
		hash ^= *c;
	}

	return hash;
}

/**
 * Returns a hash value for a pointer.
 * Pointer addresses are mostly aligned to 4 or 8 bytes. So we remove the
 * lowest 3 bits.
 */
static inline unsigned hash_ptr(void const *const ptr)
{
	return (unsigned)(((char const *)ptr - (char const *)0) >> 3);
}

/**
 * Combines 2 hash values.
 * @param x One hash value.
 * @param y Another hash value.
 * @return  A hash value computed from both.
 */
static inline unsigned hash_combine(unsigned const x, unsigned const y)
{
	unsigned hash = _FIRM_FNV_OFFSET_BASIS;
	hash *= _FIRM_FNV_FNV_PRIME;
	hash ^= x;
	hash *= _FIRM_FNV_FNV_PRIME;
	hash ^= y;
	return hash;
}

/** @} */

#include "../end.h"

#endif
