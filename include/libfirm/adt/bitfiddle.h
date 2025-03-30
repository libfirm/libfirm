/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @date    28.9.2004
 * @brief   Functions from hackers delight.
 * @author  Sebastian Hack, Matthias Braun
 */
#ifndef FIRM_ADT_BITFIDDLE_H
#define FIRM_ADT_BITFIDDLE_H

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>

#include "compiler.h"

/**
 * Compute the count of set bits in a 32-bit word.
 * @param x A 32-bit word.
 * @return The number of bits set in x.
 */
static inline uint32_t popcount(uint32_t x)
{
#if defined(__GNUC__) && __GNUC__ >= 4
	return __builtin_popcount(x);
#else
	x -= ((x >> 1) & 0x55555555);
	x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
	x = (x + (x >> 4)) & 0x0f0f0f0f;
	x += x >> 8;
	x += x >> 16;
	return x & 0x3f;
#endif
}

/**
 * Compute the number of leading zeros in a word.
 * @param x The word.
 * @return The number of leading (from the most significant bit) zeros.
 */
static inline unsigned nlz(uint32_t x)
{
#if defined(__GNUC__) && __GNUC__ >= 4
	if (x == 0)
		return 32;
	return __builtin_clz(x);
#else

   unsigned y;
   unsigned n = 32;
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
static inline unsigned ntz(uint32_t x)
{
#if defined(__GNUC__) && __GNUC__ >= 4
	if (x == 0)
		return 32;
	return __builtin_ctz(x);
#else
	return 32 - nlz(~x & (x - 1));
#endif
}

/**
 * Compute the greatest power of 2 smaller or equal to a value.
 * This is also known as the binary logarithm.
 * @param x The value.
 * @return The power of two.
 */
static inline uint32_t log2_floor(uint32_t x)
{
	return 32 - 1 - nlz(x);
}

/**
 * Compute the smallest power of 2 greater or equal to a value.
 * This is also known as the binary logarithm.
 * @param x The value.
 * @return The power of two.
 */
static inline uint32_t log2_ceil(uint32_t x)
{
	return 32 - nlz(x - 1);
}

/**
 * Returns the biggest power of 2 that is equal or smaller than @p x
 * (see hackers delight power-of-2 boundaries, page 48)
 */
static inline uint32_t floor_po2(uint32_t x)
{
#if defined(__GNUC__) && __GNUC__ >= 4 /* in this case nlz is fast */
	if (x == 0)
		return 0;
	/* note that x != 0 here, so nlz(x) < 32! */
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
static inline uint32_t ceil_po2(uint32_t x)
{
	if (x == 0)
		return 0;
	assert(x < (1U << 31));

#if defined(__GNUC__) && __GNUC__ >= 4 /* in this case nlz is fast */
	/* note that x != 0 here! */
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
 * Returns true if \p x is a power of two or zero.
 */
static inline bool is_po2_or_zero(unsigned x)
{
	return (x & (x-1)) == 0;
}

/**
 * Round up to the next multiple of a power of two.
 * @param x A value.
 * @param pot A power of two.
 * @return x rounded up to the next multiple of pot.
 */
static inline unsigned round_up2(unsigned x, unsigned po2)
{
	assert(is_po2_or_zero(po2) && po2 > 0);
	unsigned const po2_minus_one = po2-1;
	unsigned const po2_mask      = ~po2_minus_one;
	return (x + po2_minus_one) & po2_mask;
}

#endif
