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
 * @brief       Hash function for pointers
 * @author      Michael Beck, Sebastian Hack
 * @version     $Id$
 */
#ifndef FIRM_ADT_HASHPTR_H
#define FIRM_ADT_HASHPTR_H

#include "firm_config.h"
#include "compiler.h"

#define _FIRM_FNV_OFFSET_BASIS 2166136261U
#define _FIRM_FNV_FNV_PRIME 16777619U

/* Computing x * _FIRM_FNV_FNV_PRIME */
#define _FIRM_FNV_TIMES_PRIME(x) ((x) * _FIRM_FNV_FNV_PRIME)

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

static INLINE unsigned firm_fnv_hash_str(const char *data)
{
	unsigned i;
	unsigned hash = _FIRM_FNV_OFFSET_BASIS;

	for(i = 0; data[i] != '\0'; ++i) {
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

#ifdef _MSC_VER
#pragma warning(disable:4307)
#endif /* _MSC_VER */

static INLINE unsigned _hash_combine(unsigned x, unsigned y)
{
  unsigned hash = _FIRM_FNV_TIMES_PRIME(_FIRM_FNV_OFFSET_BASIS);
  hash ^= x;
  hash  = _FIRM_FNV_TIMES_PRIME(hash);
  hash ^= y;
  return hash;
}

#ifdef _MSC_VER
#pragma warning(default:4307)
#endif /* _MSC_VER */

/**
 * Make one hash value out of two others.
 * @param a One hash value.
 * @param b Another hash value.
 * @return A hash value computed from the both.
 */
#define HASH_COMBINE(a,b) _hash_combine(a, b)

#endif
