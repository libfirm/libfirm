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
 * @date    28.9.2004
 * @brief   Functions from hackers delight.
 * @author  Sebastian Hack, Matthias Braun
 * @version $Id$
 */
#ifndef FIRM_ADT_BITFIDDLE_H
#define FIRM_ADT_BITFIDDLE_H

#include <limits.h>
#include <assert.h>
#include "util.h"

/* some functions here assume ints are 32 bit wide */
#define HACKDEL_WORDSIZE 32
COMPILETIME_ASSERT(sizeof(unsigned) == 4, unsignedsize)
COMPILETIME_ASSERT(UINT_MAX == 4294967295U, uintmax)

/**
 * Add saturated.
 * @param x Summand 1.
 * @param y Summand 2.
 * @return x + y or INT_MAX/INT_MIN if an overflow occurred and x,y was positive/negative.
 *
 * @note See hacker's delight, page 27.
 */
static INLINE __attribute__((const))
int add_saturated(int x, int y)
{
	int sum      = x + y;
	/*
		An overflow occurs, if the sign of the both summands is equal
		and the one of the sum is different from the summand's one.
		The sign bit is 1, if an overflow occurred, 0 otherwise.
		int overflow = ~(x ^ y) & (sum ^ x);
	*/
	int overflow = (x ^ sum) & (y ^ sum);

	/*
		The infinity to use.
		Make a mask of the sign bit of x and y (they are the same if an
		overflow occurred).
		INT_MIN == ~INT_MAX, so if the sign was negative, INT_MAX becomes
		INT_MIN.
	*/
	int inf = (x >> (sizeof(x) * 8 - 1)) ^ INT_MAX;

	return overflow < 0 ? inf : sum;
}

/**
 * Compute the count of set bits in a 32-bit word.
 * @param x A 32-bit word.
 * @return The number of bits set in x.
 */
static INLINE __attribute__((const))
unsigned popcnt(unsigned x) {
	x -= ((x >> 1) & 0x55555555);
	x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
	x = (x + (x >> 4)) & 0x0f0f0f0f;
	x += x >> 8;
	x += x >> 16;
	return x & 0x3f;
}

/**
 * Compute the number of leading zeros in a word.
 * @param x The word.
 * @return The number of leading (from the most significant bit) zeros.
 */
static INLINE __attribute__((const))
unsigned nlz(unsigned x) {
#ifdef USE_X86_ASSEMBLY
	unsigned res;
	if(x == 0)
		return 32;

	__asm__("bsrl %1,%0"
			: "=r" (res)
			: "r" (x));
	return 31 - res;
#else
   unsigned y;
   int n = 32;

   y = x >>16;  if (y != 0) { n -= 16;  x = y; }
   y = x >> 8;  if (y != 0) { n -=  8;  x = y; }
   y = x >> 4;  if (y != 0) { n -=  4;  x = y; }
   y = x >> 2;  if (y != 0) { n -=  2;  x = y; }
   y = x >> 1;  if (y != 0) return n - 2;
   return n - x;
#endif
}

/**
 * Compute the number of trailing zeros in a word.
 * @param x The word.
 * @return The number of trailing zeros.
 */
static INLINE __attribute__((const))
unsigned ntz(unsigned x) {
#ifdef USE_X86_ASSEMBLY
	unsigned res;
	if(x == 0)
		return 32;

	__asm__("bsfl %1,%0"
			: "=r" (res)
			: "r" (x));
	return  res;
#else
	return HACKDEL_WORDSIZE - nlz(~x & (x - 1));
#endif
}

/**
 * Compute the greatest power of 2 smaller or equal to a value.
 * This is also known as the binary logarithm.
 * @param x The value.
 * @return The power of two.
 */
#define log2_floor(x) (HACKDEL_WORDSIZE - 1 - nlz(x))

/**
 * Compute the smallest power of 2 greater or equal to a value.
 * This is also known as the binary logarithm.
 * @param x The value.
 * @return The power of two.
 */
#define log2_ceil(x) (HACKDEL_WORDSIZE - nlz((x) - 1))

/**
 * Round up to the next multiple of a power of two.
 * @param x A value.
 * @param pot A power of two.
 * @return x rounded up to the next multiple of pot.
 */
#define round_up2(x,pot) (((x) + ((pot) - 1)) & (~((pot) - 1)))

/**
 * Returns the biggest power of 2 that is equal or smaller than @p x
 * (see hackers delight power-of-2 boundaries, page 48)
 */
static INLINE __attribute__((const))
unsigned floor_po2(unsigned x)
{
#ifdef USE_X86_ASSEMBLY // in this case nlz is fast
	if(x == 0)
		return 0;
	// note that x != 0 here, so nlz(x) < 32!
	return 0x80000000U >> nlz(x);
#else
	x |= x >> 1;
	x |= x >> 2;
	x |= x >> 4;
	x |= x >> 8;
	x |= x >> 16;
	return x - (x >> 1);
#endif
}

/**
 * Returns the smallest power of 2 that is equal or greater than x
 * @remark x has to be <= 0x8000000 of course
 * @note see hackers delight power-of-2 boundaries, page 48
 */
static INLINE __attribute__((const))
unsigned ceil_po2(unsigned x)
{
	if(x == 0)
		return 0;
	assert(x < (1U << 31));

#ifdef USE_X86_ASSEMBLY // in this case nlz is fast
	// note that x != 0 here!
	return 0x80000000U >> (nlz(x-1) - 1);
#else
	x = x - 1;
	x |= x >> 1;
	x |= x >> 2;
	x |= x >> 4;
	x |= x >> 8;
	x |= x >> 16;
	return x + 1;
#endif
}

/**
 * Tests whether @p x is a power of 2
 */
static INLINE __attribute__((const))
int is_po2(unsigned x)
{
	return (x & (x-1)) == 0;
}

#endif
