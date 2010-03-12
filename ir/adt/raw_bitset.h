/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief   raw bitsets (low-level bitset operations)
 * @date    15.10.2004
 * @author  Matthias Braun
 * @version $Id$
 *
 *     Raw bitsets are constructed from unsigned int arrays. Additional
 *     information like the size of the bitset or the used memory are not
 *     stored for (memory) efficiency reasons.
 *
 *     These bitsets need less space than bitset_t and their representation
 *     as int arrays allows having constant bitsets in the ro data segment.
 *     They should for smaller bitset, whose length is known through other means
 *     (a typical usage case is a set of cpu registers)
 *
 *     The bitset is built as an array of unsigned integers. The unused bits
 *     must be zero.
 */
#ifndef FIRM_ADT_RAW_BITSET_H
#define FIRM_ADT_RAW_BITSET_H

#include <assert.h>
#include <stdbool.h>
#include "bitfiddle.h"
#include "obst.h"

#define BITS_PER_ELEM                (sizeof(unsigned) * 8)
#define BITSET_SIZE_ELEMS(size_bits) ((size_bits)/BITS_PER_ELEM + 1)
#define BITSET_SIZE_BYTES(size_bits) (BITSET_SIZE_ELEMS(size_bits) * sizeof(unsigned))
#define BITSET_ELEM(bitset,pos)      bitset[pos / BITS_PER_ELEM]

/**
 * Allocate an empty raw bitset on the heap.
 *
 * @param size  number of bits in the bitset
 *
 * @return the new bitset
 */
static inline unsigned *rbitset_malloc(unsigned size)
{
	unsigned  size_bytes = BITSET_SIZE_BYTES(size);
	unsigned *res        = xmalloc(size_bytes);
	memset(res, 0, size_bytes);

	return res;
}

/**
 * Allocate an empty raw bitset on the stack.
 *
 * @param res   will contain the newly allocated bitset
 * @param size  number of bits in the bitset
 */
#define rbitset_alloca(res, size) \
do { \
	unsigned size_bytes = BITSET_SIZE_BYTES(size); \
	res = alloca(size_bytes); \
	memset(res, 0, size_bytes); \
} while(0)

/**
 * Allocate an empty raw bitset on an obstack.
 *
 * @param obst  the obstack where the bitset is allocated on
 * @param size  number of bits in the bitset
 *
 * @return the new bitset
 */
static inline unsigned *rbitset_obstack_alloc(struct obstack *obst,
                                              unsigned size)
{
	unsigned  size_bytes = BITSET_SIZE_BYTES(size);
	unsigned *res        = obstack_alloc(obst, size_bytes);
	memset(res, 0, size_bytes);

	return res;
}

/**
 * Allocate an empty raw bitset including the size on an obstack.
 * The size of this bitset can be accessed by bitset[-1].
 *
 * @param obst  the obstack where the bitset is allocated on
 * @param size  number of bits in the bitset
 *
 * @return the new bitset
 */
static inline unsigned *rbitset_w_size_obstack_alloc(struct obstack *obst,
                                                     unsigned size)
{
	unsigned  size_bytes = BITSET_SIZE_BYTES(size);
	unsigned *res        = obstack_alloc(obst, size_bytes + sizeof(unsigned));
	*res = size;
	++res;
	memset(res, 0, size_bytes);

	return res;
}

/** Return the size of a bitset allocated with a *_w_size_* function */
#define rbitset_size(set)	(set)[-1]

/**
 * Duplicate a raw bitset on an obstack.
 *
 * @param obst       the obstack where the bitset is allocated on
 * @param old_bitset the bitset to be duplicated
 * @param size       number of bits in the bitset
 *
 * @return the new bitset
 */
static inline unsigned *rbitset_duplicate_obstack_alloc(struct obstack *obst,
	const unsigned *old_bitset, unsigned size)
{
	unsigned  size_bytes = BITSET_SIZE_BYTES(size);
	unsigned *res        = obstack_alloc(obst, size_bytes);
	memcpy(res, old_bitset, size_bytes);

	return res;
}

/**
 * Check if a bitset is empty, ie all bits cleared.
 */
static inline bool rbitset_is_empty(const unsigned *bitset, unsigned size)
{
	unsigned i;
	unsigned n = BITSET_SIZE_ELEMS(size);

	for (i = 0; i < n; ++i) {
		if (bitset[i] != 0) {
			return false;
		}
	}
	return true;
}

/**
 * Set a bit at position pos.
 *
 * @param bitset  the bitset
 * @param pos     the position of the bit to be set
 */
static inline void rbitset_set(unsigned *bitset, unsigned pos)
{
	BITSET_ELEM(bitset,pos) |= 1 << (pos % BITS_PER_ELEM);
}

/**
 * Flip a bit at position pos. A zero bit becomes one, a one bit becomes zero.
 *
 * @param bitset  the bitset
 * @param pos     position of the bit to be flipped
 */
static inline void rbitset_flip(unsigned *bitset, unsigned pos)
{
	BITSET_ELEM(bitset, pos) ^= 1 << (pos % BITS_PER_ELEM);
}

static inline unsigned rbitset_last_mask_(unsigned size)
{
	unsigned p;
	if (size == 0)
		return 0;
	p = size % BITS_PER_ELEM;
	return p == 0 ? ~0u : (1u << p)-1u;
}

/**
 * Set all bits in a given bitset.
 *
 * @param bitset  the bitset
 * @param size    number of bits in the bitset
 */
static inline void rbitset_set_all(unsigned *bitset, unsigned size)
{
	unsigned i;
	unsigned n = BITSET_SIZE_ELEMS(size);

	for (i = 0; i < n-1; ++i) {
		bitset[i] = ~0u;
	}
	bitset[i] = rbitset_last_mask_(size);
}

/**
 * Clear a bit at position pos.
 *
 * @param bitset  the bitset
 * @param pos     the position of the bit to be clear
 */
static inline void rbitset_clear(unsigned *bitset, unsigned pos)
{
	BITSET_ELEM(bitset, pos) &= ~(1 << (pos % BITS_PER_ELEM));
}

/**
 * Clear all bits in a given bitset.
 *
 * @param bitset  the bitset
 * @param size    number of bits in the bitset
 */
static inline void rbitset_clear_all(unsigned *bitset, unsigned size)
{
	unsigned size_bytes = BITSET_SIZE_BYTES(size);
	memset(bitset, 0, size_bytes);
}

/**
 * Flip all bits in a given bitset.
 *
 * @param bitset  the bitset
 * @param size    number of bits in the bitset
 */
static inline void rbitset_flip_all(unsigned *bitset, unsigned size)
{
	unsigned pos;
	unsigned n    = BITSET_SIZE_ELEMS(size);
	for (pos = 0; pos < n-1; ++pos) {
		bitset[pos] ^= ~0u;
	}
	bitset[pos] ^= rbitset_last_mask_(size);
}

/**
 * Check if a bit is set at position pos.
 *
 * @param bitset  the bitset
 * @param pos     the position of the bit to check
 */
static inline bool rbitset_is_set(const unsigned *bitset, unsigned pos)
{
	return BITSET_ELEM(bitset, pos) & (1 << (pos % BITS_PER_ELEM));
}

/**
 * Calculate the number of set bits (number of elements).
 *
 * @param bitset  the bitset
 * @param size    size of the bitset in bits
 */
static inline unsigned rbitset_popcount(const unsigned *bitset, unsigned size)
{
	unsigned i;
	unsigned n   = BITSET_SIZE_ELEMS(size);
	unsigned res = 0;

	for (i = 0; i < n; ++i) {
		res += popcount(bitset[i]);
	}

	return res;
}

/**
 * Returns the position of the next bit starting from (and including)
 * a given position.
 *
 * @param bitset  a bitset
 * @param pos     the first position to check
 * @param set     if 0 search for unset bit, else for set bit
 *
 * @return the first position where a matched bit was found
 *
 * @note Does NOT check the size of the bitset, so ensure that a bit
 *       will be found or use a sentinel bit!
 */
static inline unsigned rbitset_next(const unsigned *bitset, unsigned pos,
                                    bool set)
{
	unsigned p;
	unsigned elem_pos = pos / BITS_PER_ELEM;
	unsigned bit_pos = pos % BITS_PER_ELEM;

	unsigned elem = bitset[elem_pos];
	unsigned mask = set ? 0 : ~0u;

	/*
	 * Mask out the bits smaller than pos in the current unit.
	 * We are only interested in bits set higher than pos.
	 */
	unsigned in_elem_mask = (1 << bit_pos) - 1;

	elem ^= mask;
	p = ntz(elem & ~in_elem_mask);

	/* If there is a bit set in the current elem, exit. */
	if (p < BITS_PER_ELEM) {
		return elem_pos * BITS_PER_ELEM + p;
	}

	/* Else search for set bits in the next units. */
	for (;;) {
		elem_pos++;
		elem = bitset[elem_pos] ^ mask;

		p = ntz(elem);
		if (p < BITS_PER_ELEM) {
			return elem_pos * BITS_PER_ELEM + p;
		}
	}
}

/**
 * Returns the position of the next bit starting from (and including)
 * a given position.
 *
 * @param bitset  a bitset
 * @param pos     the first position to check
 * @param last    first position that is not checked anymore
 * @param set     if 0 search for unset bit, else for set bit
 *
 * @return the first position where a matched bit was found.
 *         (unsigned)-1 if no bit was found.
 */
static inline unsigned rbitset_next_max(const unsigned *bitset, unsigned pos,
                                        unsigned last, bool set)
{
	unsigned p;
	unsigned elem_pos = pos / BITS_PER_ELEM;
	unsigned bit_pos  = pos % BITS_PER_ELEM;

	unsigned elem = bitset[elem_pos];
	unsigned mask = set ? 0 : ~0u;
	unsigned res  = (unsigned)-1;

	/*
	 * Mask out the bits smaller than pos in the current unit.
	 * We are only interested in bits set higher than pos.
	 */
	unsigned in_elem_mask = (1 << bit_pos) - 1;

	assert(pos < last);

	elem ^= mask;
	p = ntz(elem & ~in_elem_mask);

	/* If there is a bit set in the current elem, exit. */
	if (p < BITS_PER_ELEM) {
		res = elem_pos * BITS_PER_ELEM + p;
	} else {
		unsigned n = BITSET_SIZE_ELEMS(last);
		/* Else search for set bits in the next units. */
		for (elem_pos++; elem_pos < n; elem_pos++) {
			elem = bitset[elem_pos] ^ mask;

			p = ntz(elem);
			if (p < BITS_PER_ELEM) {
				res = elem_pos * BITS_PER_ELEM + p;
				break;
			}
		}
	}
	if (res >= last)
		res = (unsigned)-1;

	return res;
}

/**
 * Inplace Intersection of two sets.
 *
 * @param dst   the destination bitset and first operand
 * @param src   the second bitset
 * @param size  size of both bitsets in bits
 */
static inline void rbitset_and(unsigned *dst, const unsigned *src, unsigned size)
{
	unsigned i, n = BITSET_SIZE_ELEMS(size);

	for (i = 0; i < n; ++i) {
		dst[i] &= src[i];
	}
}

/**
 * Inplace Union of two sets.
 *
 * @param dst   the destination bitset and first operand
 * @param src   the second bitset
 * @param size  size of both bitsets in bits
 */
static inline void rbitset_or(unsigned *dst, const unsigned *src, unsigned size)
{
	unsigned i, n = BITSET_SIZE_ELEMS(size);

	for (i = 0; i < n; ++i) {
		dst[i] |= src[i];
	}
}

/**
 * Remove all bits in src from dst.
 *
 * @param dst   the destination bitset and first operand
 * @param src   the second bitset
 * @param size  size of both bitsets in bits
 */
static inline void rbitset_andnot(unsigned *dst, const unsigned *src, unsigned size)
{
	unsigned i, n = BITSET_SIZE_ELEMS(size);

	for (i = 0; i < n; ++i) {
		dst[i] &= ~src[i];
	}
}

/**
 * Xor of two bitsets.
 *
 * @param dst   the destination bitset and first operand
 * @param src   the second bitset
 * @param size  size of both bitsets in bits
 */
static inline void rbitset_xor(unsigned *dst, const unsigned *src, unsigned size)
{
	unsigned i, n = BITSET_SIZE_ELEMS(size);

	for (i = 0; i < n; ++i) {
		dst[i] ^= src[i];
	}
}

/**
 * Set bits in a range to zero or one
 * @param bitset   the bitset
 * @param from     first bit to set
 * @param to       last bit (the first bit which is not set anymore)
 * @param val      wether to set to 1 or 0
 */
static inline void rbitset_set_range(unsigned *bitset, unsigned from,
                                     unsigned to, bool val)
{
	assert(from < to);

    /*
     * A small example (for cleaning bits in the same unit).
     * from   = 7
     * to     = 19
     * do_set = 0
     * result:         xxxxxxx000000000000xxxxxxxxxxxxx
     * from_unit_mask: 00000001111111111111111111111111
     * to_unit_mask:   11111111111111111110000000000000
     * scale:          01234567890123456789012345678901
     *                           1         2         3
     */

	unsigned from_bit       = from % BITS_PER_ELEM;
	unsigned from_pos       = from / BITS_PER_ELEM;
	unsigned from_unit_mask = ~((1 << from_bit) - 1);

	unsigned to_bit         = to % BITS_PER_ELEM;
	unsigned to_pos         = to / BITS_PER_ELEM;
	unsigned to_unit_mask   = (1 << to_bit) - 1;

	/* do we want to set the bits in the range? */
	if (val) {
		if (from_pos == to_pos) {
			BITSET_ELEM(bitset, from_pos) |= from_unit_mask & to_unit_mask;
		} else {
			unsigned i;
			BITSET_ELEM(bitset, from_pos) |= from_unit_mask;
			BITSET_ELEM(bitset, to_pos)   |= to_unit_mask;
			for (i = from_pos + 1; i < to_pos; ++i)
				BITSET_ELEM(bitset, i) = ~0u;
		}
	} else {
		/* ... or clear them? */
		if (from_pos == to_pos) {
			BITSET_ELEM(bitset, from_pos) &= ~(from_unit_mask & to_unit_mask);
		} else {
			unsigned i;
			BITSET_ELEM(bitset, from_pos) &= ~from_unit_mask;
			BITSET_ELEM(bitset, to_pos)   &= ~to_unit_mask;
			for (i = from_pos + 1; i < to_pos; ++i)
				BITSET_ELEM(bitset, i) = 0;
		}
	}
}

/**
 * Returns 1 of two bitsets are equal.
 *
 * @param bitset1  the first bitset
 * @param bitset2  the second bitset
 * @param size     size of both bitsets in bits
 */
static inline bool rbitsets_equal(const unsigned *bitset1,
                                  const unsigned *bitset2, unsigned size)
{
	unsigned size_bytes = BITSET_SIZE_BYTES(size);
	return memcmp(bitset1, bitset2, size_bytes) == 0;
}

/**
 * Tests wether 2 bitsets wether at least 1 bit is set in both.
 *
 * @param bitset1  the first bitset
 * @param bitset2  the second bitset
 * @param size     size of both bitsets in bits
 */
static inline bool rbitsets_have_common(const unsigned *bitset1,
                                        const unsigned *bitset2, unsigned size)
{
	unsigned i;
	unsigned n = BITSET_SIZE_ELEMS(size);

	for (i = 0; i < n; ++i) {
		if ((bitset1[i] & bitset2[i]) != 0)
			return true;
	}
	return false;
}

/**
 * Tests wether all bits set in bitset1 are also set in bitset2.
 *
 * @param bitset1  the first bitset
 * @param bitset2  the second bitset
 * @param size     size of both bitsets in bits
 */
static inline bool rbitset_contains(const unsigned *bitset1,
                                    const unsigned *bitset2, unsigned size)
{
	unsigned i;
	unsigned n = BITSET_SIZE_ELEMS(size);

	for (i = 0; i < n; ++i) {
		if ((bitset1[i] & bitset2[i]) != bitset1[i])
			return false;
	}
	return true;
}

/**
 * Treat the bitset as a number and subtract 1.
 * @param  bitset  the bitset.
 * @return size    size of the bitset in bits
 */
static inline void rbitset_minus1(unsigned *bitset, unsigned size)
{
	unsigned i;
	unsigned n         = BITSET_SIZE_ELEMS(size);
	unsigned last_mask = rbitset_last_mask_(size);

	for (i = 0; i < n; ++i) {
		unsigned mask       = i == n-1 ? last_mask : ~0u;
		unsigned val        = bitset[i] & mask;
		unsigned val_minus1 = val - 1;
		bitset[i] = val_minus1 & mask;

		if (((val >> (BITS_PER_ELEM-1)) ^ (val_minus1 >> (BITS_PER_ELEM-1))) == 0)
			break;
	}
}

/**
 * Copy a raw bitset into another.
 *
 * @param dst   the destination set
 * @param src   the source set
 * @param size  size of both bitsets in bits
 */
static inline void rbitset_copy(unsigned *dst, const unsigned *src,
                                unsigned size)
{
	memcpy(dst, src, BITSET_SIZE_BYTES(size));
}

#endif
