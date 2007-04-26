/*
 * Copyrigth (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 *     Raw bitsets are constructed from int arrays. Additional information
 *     like the size of the bitset or the used memory aren't saved for
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
#include "bitset_std.h"
#include "obst.h"

#define BITS_PER_ELEM                   32
#define BITSET_SIZE_ELEMS(size_bits)    ((size_bits)/32 + 1)
#define BITSET_SIZE_BYTES(size_bits)    (BITSET_SIZE_ELEMS(size_bits)*4)
#define BITSET_ELEM(bitset,pos)         bitset[pos / 32]

static INLINE unsigned *rbitset_alloca(unsigned size)
{
	unsigned size_bytes = BITSET_SIZE_BYTES(size);
	unsigned *res = alloca(size_bytes);
	memset(res, 0, size_bytes);

	return res;
}

static INLINE unsigned *rbitset_obstack_alloc(struct obstack *obst, unsigned size)
{
	unsigned size_bytes = BITSET_SIZE_BYTES(size);
	unsigned *res = obstack_alloc(obst, size_bytes);
	memset(res, 0, size_bytes);

	return res;
}

static INLINE
unsigned *rbitset_duplicate_obstack_alloc(struct obstack *obst,
		                                  const unsigned *old_bitset,
                                          unsigned size)
{
	unsigned size_bytes = BITSET_SIZE_BYTES(size);
	unsigned *res = obstack_alloc(obst, size_bytes);
	memcpy(res, old_bitset, size_bytes);

	return res;
}

static INLINE void rbitset_set(unsigned *bitset, unsigned pos)
{
	BITSET_ELEM(bitset,pos) |= 1 << (pos % BITS_PER_ELEM);
}

static INLINE void rbitset_clear(unsigned *bitset, unsigned pos)
{
	BITSET_ELEM(bitset, pos) &= ~(1 << (pos % BITS_PER_ELEM));
}

static INLINE int rbitset_is_set(const unsigned *bitset, unsigned pos)
{
	return BITSET_ELEM(bitset, pos) & (1 << (pos % BITS_PER_ELEM));
}

static INLINE unsigned rbitset_popcnt(const unsigned *bitset, unsigned size)
{
	unsigned pos;
	unsigned n = BITSET_SIZE_ELEMS(size);
	unsigned res = 0;
	const unsigned *elem = bitset;

	for(pos = 0; pos < n; ++pos) {
		res += _bitset_inside_pop(elem);
		elem++;
	}

	return res;
}

static INLINE unsigned rbitset_next(const unsigned *bitset, unsigned pos, int set)
{
	unsigned p;
	unsigned elem_pos = pos / BITS_PER_ELEM;
	unsigned bit_pos = pos % BITS_PER_ELEM;

	unsigned elem = bitset[elem_pos];

	/*
	 * Mask out the bits smaller than pos in the current unit.
	 * We are only interested in bits set higher than pos.
	 */
	unsigned in_elem_mask = (1 << bit_pos) - 1;

	if(!set)
		elem = ~elem;
	p = _bitset_inside_ntz_value(elem & ~in_elem_mask);

	/* If there is a bit set in the current elem, exit. */
	if(p < BITS_PER_ELEM) {
		return elem_pos * BITS_PER_ELEM + p;
	}

	/* Else search for set bits in the next units. */
	while(1) {
		elem_pos++;
		elem = bitset[elem_pos];
		if(!set)
			elem = ~elem;

		p = _bitset_inside_ntz_value(elem);
		if(p < BITS_PER_ELEM) {
			return elem_pos * BITS_PER_ELEM + p;
		}
	}

	assert(0);
	return 0xdeadbeef;
}

static INLINE void rbitset_and(unsigned *bitset1, const unsigned *bitset2,
                               unsigned size)
{
	unsigned i = 0;
	unsigned n = BITSET_SIZE_ELEMS(size);

	for(i = 0; i < n; ++i) {
		bitset1[i] &= bitset2[i];
	}
}

static INLINE void rbitset_or(unsigned *bitset1, const unsigned *bitset2,
                              unsigned size)
{
	unsigned i = 0;
	unsigned n = BITSET_SIZE_ELEMS(size);

	for(i = 0; i < n; ++i) {
		bitset1[i] |= bitset2[i];
	}
}

static INLINE void rbitset_andnot(unsigned *bitset1, const unsigned *bitset2,
                                  unsigned size)
{
	unsigned i = 0;
	unsigned n = BITSET_SIZE_ELEMS(size);

	for(i = 0; i < n; ++i) {
		bitset1[i] &= ~bitset2[i];
	}
}

static INLINE void rbitset_xor(unsigned *bitset1, const unsigned *bitset2,
                               unsigned size)
{
	unsigned i = 0;
	unsigned n = BITSET_SIZE_ELEMS(size);

	for(i = 0; i < n; ++i) {
		bitset1[i] ^= bitset2[i];
	}
}

/** @deprecated */
static INLINE void rbitset_copy_to_bitset(const unsigned *rbitset, bitset_t *bitset)
{
	// TODO optimize me (or remove me)
	unsigned i;
	unsigned n = bitset_size(bitset);
	for(i = 0; i < n; ++i) {
		if(rbitset_is_set(rbitset, i))
			bitset_set(bitset, i);
	}
}

#endif
