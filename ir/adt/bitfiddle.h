/**
 * @file bitfiddle.h
 * @date 28.9.2004
 * @brief Functions from hackers delight.
 *
 * Attention! These functions silently assume, that an int is 32 bit wide.
 * $Id$
 */

#ifndef __FIRM_HACKDEL_H
#define __FIRM_HACKDEL_H

#include <limits.h>

#include "firm_config.h"

#define HACKDEL_WORDSIZE 32

/**
 * Add saturated.
 * @param x Summand 1.
 * @param y Summand 2.
 * @return x + y or INT_MAX/INT_MIN if an overflow occurred and x,y was positive/negative.
 *
 * @note See hacker's delight, page 27.
 */
static INLINE int add_saturated(int x, int y)
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
		Make a mask of the sign bit of x and y (they are the same if an overflow occurred).
		INT_MIN == ~INT_MAX, so if the sign was negative, INT_MAX becomes INT_MIN.
	*/
	int inf      = (x >> sizeof(x) * 8 - 1) ^ INT_MAX;

	return overflow < 0 ? inf : sum;
}

/**
 * Compute the count of set bits in a 32-bit word.
 * @param x A 32-bit word.
 * @return The number of bits set in x.
 */
static INLINE unsigned popcnt(unsigned x) {
  x = x - ((x >> 1) & 0x55555555);
  x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
  x = (x + (x >> 4)) & 0x0f0f0f0f;
  x = x + (x >> 8);
  x = x + (x >> 16);
  return x & 0x3f;
}

/**
 * Compute the number of leading zeroes in a word.
 * @param x The word.
 * @return The number of leading (from the most significant bit) zeroes.
 */
static INLINE unsigned nlz(unsigned x) {
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  return popcnt(~x);
}

/**
 * Compute the number of trailing zeroes in a word.
 * @param x The word.
 * @return The number of trailing zeroes.
 */
#define ntz(x) (HACKDEL_WORDSIZE - nlz(~(x) & ((x) - 1)))

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



#endif
