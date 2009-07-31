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
 * @brief   helper functions for working with raw bitsets
 * @date    15.10.2004
 * @author  Matthias Braun
 * @version $Id$
 * @summary
 *     Raw bitsets are constructed from unsigned int arrays. Additional information
 *     like the size of the bitset or the used memory are not stored for
 *     efficiency reasons.
 *
 *     These bitsets need less space than bitset_t and their representation
 *     as int arrays allows having constant bitsets in the ro data segment.
 *     They should for smaller bitset, whose length is known through other means
 *     (a typical usage case is a set of cpu registers)
 *
 *     The bitset is built as an array of unsigned integers. It is assumed that
 *     exactly 32 bits may be put into each element of the array. If there are
 *     remaining bits, then they should be 0
 */
#ifndef FIRM_ADT_RAW_BITSET_H
#define FIRM_ADT_RAW_BITSET_H

#include <assert.h>
#include "bitset.h"
#include "obst.h"

/** The base type for raw bitsets. */
typedef unsigned int  rawbs_base_t;

#define BITS_PER_ELEM                   (sizeof(rawbs_base_t) * 8)
#define BITSET_SIZE_ELEMS(size_bits)    ((size_bits)/BITS_PER_ELEM + 1)
#define BITSET_SIZE_BYTES(size_bits)    (BITSET_SIZE_ELEMS(size_bits) * sizeof(rawbs_base_t))
#define BITSET_ELEM(bitset,pos)         bitset[pos / BITS_PER_ELEM]

/**
 * Allocate an empty raw bitset on the heap.
 *
 * @param size  number of bits in the bitset
 *
 * @return the new bitset
 */
static inline unsigned *rbitset_malloc(unsigned size) {
	unsigned size_bytes = BITSET_SIZE_BYTES(size);
	unsigned *res = xmalloc(size_bytes);
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
static inline unsigned *rbitset_obstack_alloc(struct obstack *obst, unsigned size) {
	unsigned size_bytes = BITSET_SIZE_BYTES(size);
	unsigned *res = obstack_alloc(obst, size_bytes);
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
static inline unsigned *rbitset_w_size_obstack_alloc(struct obstack *obst, unsigned size) {
	unsigned size_bytes = BITSET_SIZE_BYTES(size);
	unsigned *res = obstack_alloc(obst, size_bytes + sizeof(unsigned));
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
static inline
unsigned *rbitset_duplicate_obstack_alloc(struct obstack *obst,
                                          const unsigned *old_bitset,
                                          unsigned size)
{
	unsigned size_bytes = BITSET_SIZE_BYTES(size);
	unsigned *res = obstack_alloc(obst, size_bytes);
	memcpy(res, old_bitset, size_bytes);

	return res;
}

/**
 * Check if a bitset is empty, ie all bits cleared.
 */
static inline int rbitset_is_empty(unsigned *bitset, unsigned size) {
	unsigned i, size_bytes = BITSET_SIZE_BYTES(size);
	for (i = 0; i < size_bytes; ++i)
		if (bitset[i]) return 0;
	return 1;
}

/**
 * Set a bit at position pos.
 *
 * @param bitset  the bitset
 * @param pos     the position of the bit to be set
 */
static inline void rbitset_set(unsigned *bitset, unsigned pos) {
	BITSET_ELEM(bitset,pos) |= 1 << (pos % BITS_PER_ELEM);
}

/**
 * Set all bits in a given bitset.
 *
 * @param bitset  the bitset
 * @param size    number of bits in the bitset
 */
static inline void rbitset_set_all(unsigned *bitset, unsigned size) {
	unsigned size_bytes = BITSET_SIZE_BYTES(size);
	memset(bitset, ~0, size_bytes);
}


/**
 * Clear a bit at position pos.
 *
 * @param bitset  the bitset
 * @param pos     the position of the bit to be clear
 */
static inline void rbitset_clear(unsigned *bitset, unsigned pos) {
	BITSET_ELEM(bitset, pos) &= ~(1 << (pos % BITS_PER_ELEM));
}

/**
 * Clear all bits in a given bitset.
 *
 * @param bitset  the bitset
 * @param size    number of bits in the bitset
 */
static inline void rbitset_clear_all(unsigned *bitset, unsigned size) {
	unsigned size_bytes = BITSET_SIZE_BYTES(size);
	memset(bitset, 0, size_bytes);
}

/**
 * Check if a bit is set at position pos.
 *
 * @param bitset  the bitset
 * @param pos     the position of the bit to check
 */
static inline int rbitset_is_set(const unsigned *bitset, unsigned pos) {
	return BITSET_ELEM(bitset, pos) & (1 << (pos % BITS_PER_ELEM));
}

/**
 * Calculate the number of set bits (number of elements).
 *
 * @param bitset  the bitset
 * @param size    size of the bitset
 */
static inline unsigned rbitset_popcnt(const unsigned *bitset, unsigned size) {
	unsigned pos;
	unsigned n = BITSET_SIZE_ELEMS(size);
	unsigned res = 0;
	const unsigned *elem = bitset;

	for (pos = 0; pos < n; ++pos) {
		res += _bitset_inside_pop(elem);
		elem++;
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
static inline unsigned rbitset_next(const unsigned *bitset, unsigned pos, int set) {
	unsigned p;
	unsigned elem_pos = pos / BITS_PER_ELEM;
	unsigned bit_pos = pos % BITS_PER_ELEM;

	unsigned elem = bitset[elem_pos];
	unsigned mask = 0;

	/*
	 * Mask out the bits smaller than pos in the current unit.
	 * We are only interested in bits set higher than pos.
	 */
	unsigned in_elem_mask = (1 << bit_pos) - 1;

	if (!set)
		mask = ~mask;
	elem ^= mask;
	p = _bitset_inside_ntz_value(elem & ~in_elem_mask);

	/* If there is a bit set in the current elem, exit. */
	if (p < BITS_PER_ELEM) {
		return elem_pos * BITS_PER_ELEM + p;
	}

	/* Else search for set bits in the next units. */
	while (1) {
		elem_pos++;
		elem = bitset[elem_pos] ^ mask;

		p = _bitset_inside_ntz_value(elem);
		if (p < BITS_PER_ELEM) {
			return elem_pos * BITS_PER_ELEM + p;
		}
	}
}

/**
 * Inplace Intersection of two sets.
 *
 * @param dst   the destination bitset and first operand
 * @param src   the second bitset
 * @param size  size of both bitsets
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
 * @param size  size of both bitsets
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
 * @param size  size of both bitsets
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
 * @param size  size of both bitsets
 */
static inline void rbitset_xor(unsigned *dst, const unsigned *src, unsigned size)
{
	unsigned i, n = BITSET_SIZE_ELEMS(size);

	for (i = 0; i < n; ++i) {
		dst[i] ^= src[i];
	}
}

/**
 * Returns 1 of two bitsets are equal.
 *
 * @param bitset1  the first bitset
 * @param bitset2  the second bitset
 * @param size     size of both bitsets
 */
static inline int rbitset_equal(const unsigned *bitset1,
                                const unsigned *bitset2, size_t size)
{
	return memcmp(bitset1, bitset2, BITSET_SIZE_BYTES(size)) == 0;
}

/**
 * Copy a raw bitset into another.
 *
 * @param dst   the destination set
 * @param src   the source set
 * @param size  size of both bitsets
 */
static inline void rbitset_copy(unsigned *dst, const unsigned *src, size_t size)
{
	memcpy(dst, src, BITSET_SIZE_BYTES(size));
}

/**
 * Copy a raw bitset into an bitset.
 *
 * @deprecated
 */
static inline void rbitset_copy_to_bitset(const unsigned *rbitset, bitset_t *bitset) {
	// TODO optimize me (or remove me)
	unsigned i, n = bitset_size(bitset);
	for (i = 0; i < n; ++i) {
		if (rbitset_is_set(rbitset, i))
			bitset_set(bitset, i);
	}
}

#endif /* FIRM_ADT_RAW_BITSET_H */
