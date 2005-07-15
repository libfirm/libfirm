/*
 * Project:     libFIRM
 * File name:   ir/adt/hashptr.h
 * Purpose:     Hash function for pointers
 * Author:      Michael Beck, Sebastian Hack
 * Modified by:
 * Created:     2004
 * CVS-ID:      $Id$
 * Copyright:   (C) 2004 University of Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef __HASHPTR_H__
#define __HASHPTR_H__

#include "firm_config.h"

#define _FIRM_FNV_OFFSET_BASIS 2166136261U
#define _FIRM_FNV_FNV_PRIME 16777619U

/* Computing x * _FIRM_FNV_FNV_PRIME */
#define _FIRM_FNV_TIMES_PRIME(x) \
  (((x) << 24) + ((x) << 8) + ((x) << 7) + ((x) << 4) + ((x) << 1) + 1)

static INLINE unsigned firm_fnv_hash(const unsigned char *data, unsigned bytes)
{
	unsigned i;
	unsigned hash = _FIRM_FNV_OFFSET_BASIS;

	for(i = 0; i < bytes; ++i) {
		hash = _FIRM_FNV_TIMES_PRIME(hash);
		hash ^= data[i];
	}

	return hash;
}

/**
 * hash a pointer value: Pointer addresses are mostly aligned to 4
 * or 8 bytes. So we remove the lowest 3 bits
 */
#define HASH_PTR(ptr)    (((char *) (ptr) - (char *)0) >> 3)

/**
 * Hash a string.
 * @param str The string (can be const).
 * @param len The length of the string.
 * @return A hash value for the string.
 */
#define HASH_STR(str,len) firm_fnv_hash((const unsigned char *) (str), (len))

static INLINE unsigned _hash_combine(unsigned x, unsigned y)
{
  unsigned hash = _FIRM_FNV_TIMES_PRIME(_FIRM_FNV_OFFSET_BASIS);
  hash ^= x;
  hash  = _FIRM_FNV_TIMES_PRIME(hash);
  hash ^= y;
  return hash;
}

/**
 * Make one hash value out of two others.
 * @param a One hash value.
 * @param b Another hash value.
 * @return A hash value computed from the both.
 */
#define HASH_COMBINE(a,b) _hash_combine(a, b)

#endif /* __HASHPTR_H__ */
