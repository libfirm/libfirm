/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   raw bitsets (low-level bitset operations)
 * @date    15.10.2004
 * @author  Matthias Braun
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
#define BITSET_SIZE_ELEMS(size_bits) ((size_bits+BITS_PER_ELEM-1)/BITS_PER_ELEM)
#define BITSET_SIZE_BYTES(size_bits) (BITSET_SIZE_ELEMS(size_bits) * sizeof(unsigned))
#define BITSET_ELEM(bitset,pos)      bitset[pos / BITS_PER_ELEM]

/**
 * Allocate an empty raw bitset on the heap.
 *
 * @param size  number of bits in the bitset
 *
 * @return the new bitset
 */
static inline unsigned *rbitset_malloc(size_t size)
{
	return XMALLOCNZ(unsigned, BITSET_SIZE_ELEMS(size));
}

/**
 * Allocate an empty raw bitset on the stack.
 *
 * @param size  number of bits in the bitset
 */
#define rbitset_alloca(size) \
	((unsigned*)memset(alloca(BITSET_SIZE_BYTES(size)), 0, BITSET_SIZE_BYTES(size)))

/**
 * Allocate an empty raw bitset on an obstack.
 *
 * @param obst  the obstack where the bitset is allocated on
 * @param size  number of bits in the bitset
 *
 * @return the new bitset
 */
static inline unsigned *rbitset_obstack_alloc(struct obstack *obst,
                                              size_t size)
{
	return OALLOCNZ(obst, unsigned, BITSET_SIZE_ELEMS(size));
}

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
	const unsigned *old_bitset, size_t size)
{
	size_t    size_bytes = BITSET_SIZE_BYTES(size);
	unsigned *res        = OALLOCN(obst, unsigned, BITSET_SIZE_ELEMS(size));
	memcpy(res, old_bitset, size_bytes);

	return res;
}

/**
 * Check if a bitset is empty, ie all bits cleared.
 */
static inline bool rbitset_is_empty(const unsigned *bitset, size_t size)
{
	for (size_t i = 0, n = BITSET_SIZE_ELEMS(size); i < n; ++i) {
		if (bitset[i] != 0)
			return false;
	}
	return true;
}

/**
 * Set a bit at position pos.
 *
 * @param bitset  the bitset
 * @param pos     the position of the bit to be set
 */
static inline void rbitset_set(unsigned *bitset, size_t pos)
{
	BITSET_ELEM(bitset,pos) |= 1u << (pos % BITS_PER_ELEM);
}

/**
 * Flip a bit at position pos. A zero bit becomes one, a one bit becomes zero.
 *
 * @param bitset  the bitset
 * @param pos     position of the bit to be flipped
 */
static inline void rbitset_flip(unsigned *bitset, size_t pos)
{
	BITSET_ELEM(bitset, pos) ^= 1u << (pos % BITS_PER_ELEM);
}

/* internal helper: return mask for last bitset element, must not be called
 * with size==0 */
static inline unsigned rbitset_last_mask_(size_t size)
{
	size_t p = size % BITS_PER_ELEM;
	return p == 0 ? ~0u : (1u << p)-1u;
}

/**
 * Set all bits in a given bitset.
 *
 * @param bitset  the bitset
 * @param size    number of bits in the bitset
 */
static inline void rbitset_set_all(unsigned *bitset, size_t size)
{
	size_t n = BITSET_SIZE_ELEMS(size);
	if (n == 0)
		return;

	size_t i;
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
static inline void rbitset_clear(unsigned *bitset, size_t pos)
{
	BITSET_ELEM(bitset, pos) &= ~(1u << (pos % BITS_PER_ELEM));
}

/**
 * Clear all bits in a given bitset.
 *
 * @param bitset  the bitset
 * @param size    number of bits in the bitset
 */
static inline void rbitset_clear_all(unsigned *bitset, size_t size)
{
	size_t size_bytes = BITSET_SIZE_BYTES(size);
	memset(bitset, 0, size_bytes);
}

/**
 * Flip all bits in a given bitset.
 *
 * @param bitset  the bitset
 * @param size    number of bits in the bitset
 */
static inline void rbitset_flip_all(unsigned *bitset, size_t size)
{
	size_t n = BITSET_SIZE_ELEMS(size);
	if (n == 0)
		return;

	size_t i;
	for (i = 0; i < n-1; ++i) {
		bitset[i] ^= ~0u;
	}
	bitset[i] ^= rbitset_last_mask_(size);
}

/**
 * Check if a bit is set at position pos.
 *
 * @param bitset  the bitset
 * @param pos     the position of the bit to check
 */
static inline bool rbitset_is_set(const unsigned *bitset, size_t pos)
{
	return (BITSET_ELEM(bitset, pos) & (1u << (pos % BITS_PER_ELEM))) != 0;
}

/**
 * Calculate the number of set bits (number of elements).
 *
 * @param bitset  the bitset
 * @param size    size of the bitset in bits
 */
static inline unsigned rbitset_popcount(const unsigned *bitset, size_t size)
{
	unsigned res = 0;
	for (size_t i = 0, n = BITSET_SIZE_ELEMS(size); i < n; ++i) {
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
static inline size_t rbitset_next(const unsigned *bitset, size_t pos,
                                  bool set)
{
	size_t elem_pos = pos / BITS_PER_ELEM;
	size_t bit_pos  = pos % BITS_PER_ELEM;

	unsigned elem = bitset[elem_pos];
	unsigned mask = set ? 0 : ~0u;

	/*
	 * Mask out the bits smaller than pos in the current unit.
	 * We are only interested in bits set higher than pos.
	 */
	unsigned in_elem_mask = (1u << bit_pos) - 1u;

	elem ^= mask;
	unsigned p = ntz(elem & ~in_elem_mask);

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
 *         (size_t)-1 if no bit was found.
 */
static inline size_t rbitset_next_max(const unsigned *bitset, size_t pos,
                                      size_t last, bool set)
{
	assert(pos <= last);
	if (pos == last)
		return (size_t)-1;

	size_t elem_pos = pos / BITS_PER_ELEM;
	size_t bit_pos  = pos % BITS_PER_ELEM;

	unsigned elem = bitset[elem_pos];
	unsigned mask = set ? 0u : ~0u;
	size_t   res  = (size_t)-1;

	/*
	 * Mask out the bits smaller than pos in the current unit.
	 * We are only interested in bits set higher than pos.
	 */
	unsigned in_elem_mask = (1u << bit_pos) - 1;

	elem ^= mask;
	size_t p = ntz(elem & ~in_elem_mask);

	/* If there is a bit set in the current elem, exit. */
	if (p < BITS_PER_ELEM) {
		res = elem_pos * BITS_PER_ELEM + p;
	} else {
		size_t n = BITSET_SIZE_ELEMS(last);
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
		res = (size_t)-1;

	return res;
}

/**
 * Returns the position of the previous bit starting from (but not including)
 * a given position.
 *
 * @param bitset  a bitset
 * @param pos     the position after the first bit to check
 * @param set     if 0 search for unset bit, else for set bit
 *
 * @return The first position where a matched bit was found or -1 if
 *         none found.
 *
 */
static inline size_t rbitset_prev(const unsigned *bitset, size_t pos,
                                  bool set)
{
	size_t elem_pos = pos / BITS_PER_ELEM;
	size_t bit_pos  = pos % BITS_PER_ELEM;

	unsigned elem = bitset[elem_pos];
	unsigned mask = set ? 0 : ~0u;

	/*
	 * Mask out the bits larger than pos in the current unit.
	 * We are only interested in bits set lower than pos.
	 */
	unsigned in_elem_mask = ~((1u << bit_pos) - 1u);

	elem ^= mask;
	unsigned p = nlz(elem & ~in_elem_mask);

	/* If there is a bit set in the current elem, exit. */
	if (p < BITS_PER_ELEM) {
	  return (1+elem_pos) * BITS_PER_ELEM - p - 1;
	}

	/* Else search for set bits in the previous units. */
	while (elem_pos > 0) {
		elem_pos--;
		elem = bitset[elem_pos] ^ mask;

		p = nlz(elem);
		if (p < BITS_PER_ELEM) {
		  return (1+elem_pos) * BITS_PER_ELEM - p - 1;
		}
	}

	return -1;
}

/**
 * Inplace Intersection of two sets.
 *
 * @param dst   the destination bitset and first operand
 * @param src   the second bitset
 * @param size  size of both bitsets in bits
 */
static inline void rbitset_and(unsigned *dst, const unsigned *src, size_t size)
{
	for (size_t i = 0, n = BITSET_SIZE_ELEMS(size); i < n; ++i) {
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
static inline void rbitset_or(unsigned *dst, const unsigned *src, size_t size)
{
	for (size_t i = 0, n = BITSET_SIZE_ELEMS(size); i < n; ++i) {
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
static inline void rbitset_andnot(unsigned *dst, const unsigned *src,
                                  size_t size)
{
	for (size_t i = 0, n = BITSET_SIZE_ELEMS(size); i < n; ++i) {
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
static inline void rbitset_xor(unsigned *dst, const unsigned *src, size_t size)
{
	for (size_t i = 0, n = BITSET_SIZE_ELEMS(size); i < n; ++i) {
		dst[i] ^= src[i];
	}
}

/**
 * Set bits in a range to zero or one
 * @param bitset   the bitset
 * @param from     first bit to set
 * @param to       last bit (the first bit which is not set anymore)
 * @param val      whether to set to 1 or 0
 */
static inline void rbitset_set_range(unsigned *bitset, size_t from,
                                     size_t to, bool val)
{
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

	size_t from_bit         = from % BITS_PER_ELEM;
	size_t from_pos         = from / BITS_PER_ELEM;
	unsigned from_unit_mask = ~((1u << from_bit) - 1);

	size_t to_bit         = to % BITS_PER_ELEM;
	size_t to_pos         = to / BITS_PER_ELEM;
	unsigned to_unit_mask = (1u << to_bit) - 1;

	assert(from < to);

	/* do we want to set the bits in the range? */
	if (val) {
		if (from_pos == to_pos) {
			bitset[from_pos] |= from_unit_mask & to_unit_mask;
		} else {
			bitset[from_pos] |= from_unit_mask;
			bitset[to_pos]   |= to_unit_mask;
			for (size_t i = from_pos + 1; i < to_pos; ++i)
				bitset[i] = ~0u;
		}
	} else {
		/* ... or clear them? */
		if (from_pos == to_pos) {
			bitset[from_pos] &= ~(from_unit_mask & to_unit_mask);
		} else {
			bitset[from_pos] &= ~from_unit_mask;
			bitset[to_pos]   &= ~to_unit_mask;
			for (size_t i = from_pos + 1; i < to_pos; ++i)
				bitset[i] = 0;
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
                                  const unsigned *bitset2, size_t size)
{
	size_t size_bytes = BITSET_SIZE_BYTES(size);
	return memcmp(bitset1, bitset2, size_bytes) == 0;
}

/**
 * Tests whether 2 bitsets have at least one common set bit.
 *
 * @param bitset1  the first bitset
 * @param bitset2  the second bitset
 * @param size     size of both bitsets in bits
 */
static inline bool rbitsets_have_common(const unsigned *bitset1,
                                        const unsigned *bitset2, size_t size)
{
	for (size_t i = 0, n = BITSET_SIZE_ELEMS(size); i < n; ++i) {
		if ((bitset1[i] & bitset2[i]) != 0)
			return true;
	}
	return false;
}

/**
 * Tests whether all bits set in bitset1 are also set in bitset2.
 *
 * @param bitset1  the first bitset
 * @param bitset2  the second bitset
 * @param size     size of both bitsets in bits
 */
static inline bool rbitset_contains(const unsigned *bitset1,
                                    const unsigned *bitset2, size_t size)
{
	for (size_t i = 0, n = BITSET_SIZE_ELEMS(size); i < n; ++i) {
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
static inline void rbitset_minus1(unsigned *bitset, size_t size)
{
	for (size_t i = 0, n = BITSET_SIZE_ELEMS(size); i < n; ++i) {
		unsigned mask       = i == n-1
							? rbitset_last_mask_(size)
							: ~0u;
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
                                size_t size)
{
	memcpy(dst, src, BITSET_SIZE_BYTES(size));
}

/**
 * Convenience macro for raw bitset iteration.
 * @param bitset The bitset.
 * @param size   Size of the bitset.
 * @param elm    A size_t variable.
 */
#define rbitset_foreach(bitset, size, elm) \
	for (size_t elm = 0; (elm = rbitset_next_max((bitset), elm, size, 1)) != (size_t)-1; ++elm)

/**
 * Convenience macro for raw bitset iteration.
 * @param bitset The bitset.
 * @param size   Size of the bitset.
 * @param elm    A size_t variable.
 */
#define rbitset_foreach_clear(bitset, size, elm) \
	for (size_t elm = 0; (elm = rbitset_next_max((bitset), elm, size, 0)) != (size_t)-1; ++elm)

#endif
