/**
 * Bitsets.
 */

#ifndef __FIRM_BITSET_H
#define __FIRM_BITSET_H

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>


#define INLINE inline

#define BITSET_USE_STD

#include "bitset_std.h"

typedef struct _bitset_t {
	unsigned units;
	bitset_unit_t *data;
} bitset_t;

#define BS_UNIT_SIZE sizeof(bitset_unit_t)
#define BS_UNIT_SIZE_BITS (BS_UNIT_SIZE * 8)
#define BS_UNIT_MASK (BS_UNIT_SIZE_BITS - 1)


/**
 * Units needed for a given highest bit.
 * @param highest_bit The highest bit that should be storable.
 * @return The number of units needed.
 */
#define _bitset_units(highest_bit) (round_up2(highest_bit, BS_UNIT_SIZE_BITS) / BS_UNIT_SIZE_BITS)

/**
 * Compute the size in bytes needed for a bitset.
 * This also include the size for the bitset data structure.
 * @param highest_bit The highest bit that shall be storable.
 * @return The overall amount of bytes needed for that bitset.
 */
#define _bitset_overall_size(highest_bit) \
	(sizeof(bitset_t) + _bitset_units(highest_bit) * BS_UNIT_SIZE)

/**
 * Initialize a bitset.
 * This functions should not be called.
 * @param area A pointer to memory reserved for the bitset.
 * @param units The number of units that are allocated for the bitset.
 * @return A pointer to the initialized bitset.
 */
static INLINE bitset_t *_bitset_prepare(void *area, unsigned units)
{
	bitset_t *ptr = area;
	ptr->units = units;
	ptr->data = (bitset_unit_t *) (ptr + 1);
	memset(ptr->data, 0, BS_UNIT_SIZE * units);
	return ptr;
}

/**
 * Allocate a bitset on an obstack.
 * @param obst The obstack.
 * @param highest_bit The greatest bit that shall be stored in the set.
 * @return A pointer to an empty initialized bitset.
 */
#define bitset_obstack_alloc(obst,highest_bit) \
  _bitset_prepare(obstack_alloc(obst, _bitset_overall_size(highest_bit)), _bitset_units(highest_bit))

/**
 * Allocate a bitset via malloc.
 * @param highest_bit The greatest bit that shall be stored in the set.
 * @return A pointer to an empty initialized bitset.
 */
#define bitset_malloc(highest_bit) \
	_bitset_prepare(malloc(_bitset_overall_size(highest_bit)), _bitset_units(highest_bit))

/**
 * Free a bitset allocated with bitset_malloc().
 * @param bs The bitset.
 */
#define bitset_free(bs) free(bs)

/**
 * Allocate a bitset on the stack via alloca.
 * @param highest_bit The greatest bit that shall be stored in the set.
 * @return A pointer to an empty initialized bitset.
 */
#define bitset_alloca(highest_bit) \
	_bitset_prepare(alloca(_bitset_overall_size(highest_bit)), _bitset_units(highest_bit))

/**
 * Print a bitset to a stream.
 * The bitset is printed as a comma seperated list of bits set.
 * @param file The stream.
 * @param bs The bitset.
 */
static INLINE void bitset_fprint(FILE *file, bitset_t *bs)
{
	const char *prefix = "";
	int i;
	unsigned k = 0;

	putc('[', file);
	for(i = 0; i < bs->units; i++) {
		bitset_unit_t j;
		bitset_unit_t unit = bs->data[i];

#if 0
		printf("%s%08x", prefix, unit);
		prefix=":";
		continue;
#endif
		for(j = 1; j != 0; j <<= 1, k++) {
			if(unit & j) {
				fprintf(file, "%s%u", prefix, k);
				prefix = ",";
			}
		}
	}
	putc(']', file);
}

/**
 * Get the unit which contains a specific bit.
 * This function is internal.
 * @param bs The bitset.
 * @param bit The bit.
 * @return A pointer to the unit containing the bit.
 */
static INLINE bitset_unit_t *_bitset_get_unit(const bitset_t *bs, unsigned bit)
{
	assert(bit < bs->units * BS_UNIT_SIZE_BITS && "Bit too large");
	return bs->data + bit / BS_UNIT_SIZE_BITS;
}

/**
 * Set a bit in the bitset.
 * @param bs The bitset.
 * @param bit The bit to set.
 */
static INLINE void bitset_set(bitset_t *bs, unsigned bit)
{
	bitset_unit_t *unit = _bitset_get_unit(bs, bit);
	_bitset_inside_set(unit, bit & BS_UNIT_MASK);
}

/**
 * Clear a bit in the bitset.
 * @param bs The bitset.
 * @param bit The bit to clear.
 */
static INLINE void bitset_clear(bitset_t *bs, unsigned bit)
{
	bitset_unit_t *unit = _bitset_get_unit(bs, bit);
	_bitset_inside_clear(unit, bit & BS_UNIT_MASK);
}

/**
 * Flip a bit in a bitset.
 * @param bs The bitset.
 * @param bit The bit to flip.
 */
static INLINE void bitset_flip(bitset_t *bs, unsigned bit)
{
	bitset_unit_t *unit = _bitset_get_unit(bs, bit);
	_bitset_inside_flip(unit, bit & BS_UNIT_MASK);
}

/**
 * Find the smallest bit set in the bitset.
 * @param bs The bitset.
 * @return The smallest bit set in the bitset.
 */
static INLINE unsigned bitset_min(bitset_t *bs)
{
	unsigned i, ofs = 0;
	bitset_unit_t *unit;

	for(i = 0, unit = bs->data; i < bs->units; ++i, ++unit) {
		unsigned pos = _bitset_inside_ntz(unit);
		if(pos > 0)
			return ofs + pos;
		ofs += BS_UNIT_SIZE_BITS;
	}

	return 0;
}

/**
 * Find the greatest bit set in the bitset.
 * @param bs The bitset.
 * @return The greatest bit set in the bitset.
 */
static INLINE unsigned bitset_max(bitset_t *bs)
{
	unsigned i, max = 0, ofs = 0;
	bitset_unit_t *unit;

	for(i = 0, unit = bs->data; i < bs->units; ++i, ++unit) {
		unsigned pos = _bitset_inside_nlz(unit);
		if(pos > 0)
			max = ofs + pos;
		ofs += BS_UNIT_SIZE_BITS;
	}

	return max;
}

/**
 * Count the bits set.
 * This can also be seen as the cardinality of the set.
 * @param bs The bitset.
 * @return The number of bits set in the bitset.
 */
static INLINE unsigned bitset_pop(bitset_t *bs)
{
	unsigned i, pop = 0;
	bitset_unit_t *unit;

	for(i = 0, unit = bs->data; i < bs->units; ++i, ++unit)
		pop += _bitset_inside_pop(unit);

	return pop;
}

/*
 * Here, the binary operations follow.
 * And, Or, And Not, Xor are available.
 */

#define BINARY_OP(op) \
static INLINE bitset_t *bitset_ ## op(bitset_t *tgt, bitset_t *src) \
{ \
	int i; \
	int n = tgt->units > src->units ? src->units : tgt->units; \
	bitset_unit_t *tgt_unit, *src_unit; \
	src_unit = src->data; \
	tgt_unit = tgt->data; \
	for(i = 0; i < n; ++i) \
		_bitset_inside_ ## op(tgt_unit++, src_unit++); \
	return tgt; \
}

BINARY_OP(and)
BINARY_OP(andnot)
BINARY_OP(or)
BINARY_OP(xor)


#endif
