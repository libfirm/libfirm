/**
 * @file bitset.h
 * @date 15.10.2004
 * @author Sebastian Hack
 * @brief A bitset implementation.
 */

#ifndef __FIRM_BITSET_H
#define __FIRM_BITSET_H

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "config.h"
#include "bitfiddle.h"

#include "bitset_std.h"

#if defined(__GNUC__) && defined(__i386__)
#include "bitset_ia32.h"
#endif

typedef struct _bitset_t {
	unsigned long units;
	unsigned long *data;
} bitset_t;

#define BS_UNIT_SIZE sizeof(unsigned long)
#define BS_UNIT_SIZE_BITS (BS_UNIT_SIZE * 8)
#define BS_UNIT_MASK (BS_UNIT_SIZE_BITS - 1)

/**
 * Initialize a bitset.
 * This functions should not be called.
 *
 * Note that this function needs three macros which must be provided by the
 * bitfield implementor:
 * - _bitset_overall_size(highest_bit) The overall size that must be
 *   allocated for the bitfield in bytes.
 * - _bitset_units(highest_bit) The number of units that will be
 *   present in the bitfield for a given highest bit.
 * - _bitset_data_ptr(data, highest_bit) This produces as pointer to the
 *   first unit in the allocated memory area. The main reason for this
 *   macro is, that some bitset implementors want control over memory
 *   alignment.
 *
 * @param area A pointer to memory reserved for the bitset.
 * @param units The number of units that are allocated for the bitset.
 * @return A pointer to the initialized bitset.
 */
static INLINE bitset_t *_bitset_prepare(void *area, unsigned long highest_bit)
{
	bitset_t *ptr = area;
	memset(area, 0, _bitset_overall_size(sizeof(bitset_t), highest_bit));
	ptr->units = _bitset_units(highest_bit);
	ptr->data = _bitset_data_ptr(area, sizeof(bitset_t), highest_bit);
	return ptr;
}

/**
 * Get the capacity of the bitset in bits.
 * @param bs The bitset.
 * @return The capacity in bits of the bitset.
 */
#define bitset_capacity(bs) ((bs)->units * BS_UNIT_SIZE_BITS)

/**
 * Allocate a bitset on an obstack.
 * @param obst The obstack.
 * @param highest_bit The greatest bit that shall be stored in the set.
 * @return A pointer to an empty initialized bitset.
 */
#define bitset_obstack_alloc(obst,highest_bit) \
  _bitset_prepare(obstack_alloc(obst, _bitset_overall_size(sizeof(bitset_t), highest_bit)), highest_bit)

/**
 * Allocate a bitset via malloc.
 * @param highest_bit The greatest bit that shall be stored in the set.
 * @return A pointer to an empty initialized bitset.
 */
#define bitset_malloc(highest_bit) \
	_bitset_prepare(malloc(_bitset_overall_size(sizeof(bitset_t), highest_bit)), highest_bit)

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
	_bitset_prepare(alloca(_bitset_overall_size(sizeof(bitset_t), highest_bit)), highest_bit)


/**
 * Get the unit which contains a specific bit.
 * This function is internal.
 * @param bs The bitset.
 * @param bit The bit.
 * @return A pointer to the unit containing the bit.
 */
static INLINE unsigned long *_bitset_get_unit(const bitset_t *bs, unsigned long bit)
{
	assert(bit < bs->units * BS_UNIT_SIZE_BITS && "Bit too large");
	return bs->data + bit / BS_UNIT_SIZE_BITS;
}

/**
 * Set a bit in the bitset.
 * @param bs The bitset.
 * @param bit The bit to set.
 */
static INLINE void bitset_set(bitset_t *bs, unsigned long bit)
{
	unsigned long *unit = _bitset_get_unit(bs, bit);
	_bitset_inside_set(*unit, bit & BS_UNIT_MASK);
}

/**
 * Clear a bit in the bitset.
 * @param bs The bitset.
 * @param bit The bit to clear.
 */
static INLINE void bitset_clear(bitset_t *bs, unsigned long bit)
{
	unsigned long *unit = _bitset_get_unit(bs, bit);
	_bitset_inside_clear(unit, bit & BS_UNIT_MASK);
}

static INLINE int bitset_is_set(const bitset_t *bs, unsigned long bit)
{
	unsigned long *unit = _bitset_get_unit(bs, bit);
	return _bitset_inside_is_set(unit, bit & BS_UNIT_MASK);
}

/**
 * Flip a bit in a bitset.
 * @param bs The bitset.
 * @param bit The bit to flip.
 */
static INLINE void bitset_flip(bitset_t *bs, unsigned long bit)
{
	unsigned long *unit = _bitset_get_unit(bs, bit);
	_bitset_inside_flip(unit, bit & BS_UNIT_MASK);
}

/**
 * Copy a bitset to another.
 * @param tgt The target bitset.
 * @param src The source bitset.
 * @return The target bitset.
 */
static INLINE bitset_t *bitset_copy(bitset_t *tgt, const bitset_t *src)
{
	unsigned long tu = tgt->units;
	unsigned long su = src->units;
	unsigned long min_units = tu < su ? tu : su;
	memcpy(tgt->data, src->data, min_units * BS_UNIT_SIZE);
	if(tu > min_units)
		memset(tgt->data + min_units, 0, BS_UNIT_SIZE * (tu - min_units));
	return tgt;
}

/**
 * Find the smallest bit set in the bitset.
 * @param bs The bitset.
 * @return The smallest bit set in the bitset.
 */
static INLINE unsigned long bitset_min(const bitset_t *bs)
{
	unsigned long i, ofs = 0;

	for(i = 0; i < bs->units; ++i) {
		unsigned long *unit = &bs->data[i];
		unsigned long pos = _bitset_inside_ntz(unit);
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
static INLINE unsigned long bitset_max(const bitset_t *bs)
{
	unsigned long i, max = 0, ofs = 0;

	for(i = 0; i < bs->units; ++i) {
		unsigned long *unit = &bs->data[i];
		unsigned long pos = _bitset_inside_nlz(unit);
		if(pos > 0)
			max = ofs + pos;
		ofs += BS_UNIT_SIZE_BITS;
	}

	return max;
}

/**
 * Find the next set bit from a given bit.
 * @note Note that if pos is set, pos is returned.
 * @param bs The bitset.
 * @param pos The bit from which to search for the next set bit.
 * @return The next set bit from pos on, or -1, if no set bit was found
 * after pos.
 */
static INLINE unsigned long _bitset_next(const bitset_t *bs,
		unsigned long pos, int set)
{
	unsigned long unit_number = pos / BS_UNIT_SIZE_BITS;
	unsigned long bit_in_unit = pos & BS_UNIT_MASK;
	unsigned long in_unit_mask = (1 << bit_in_unit) - 1;

	/*
	 * Mask out the bits smaller than pos in the current unit.
	 * We are only interested in bits set higher than pos.
	 */
	unsigned long curr_unit = bs->data[unit_number] & ~in_unit_mask;

	/* Find the next bit set in the unit. */
	unsigned long next_in_this_unit
		= _bitset_inside_ntz_value(set ? curr_unit : ~curr_unit);

	/* If there is a bit set in the current unit, exit. */
	if(next_in_this_unit < BS_UNIT_SIZE_BITS)
		return next_in_this_unit + unit_number * BS_UNIT_SIZE_BITS;

	/* Else search for set bits in the next units. */
	else {
		unsigned long i;
		for(i = unit_number + 1; i < bs->units; ++i) {
			unsigned long data = bs->data[i];
			unsigned long first_set = _bitset_inside_ntz_value(set ? data : ~data);
			if(first_set < BS_UNIT_SIZE_BITS)
				return first_set + i * BS_UNIT_SIZE_BITS;
		}
	}

	return -1;
}

#define bitset_next_clear(bs,pos) _bitset_next((bs), (pos), 0)
#define bitset_next_set(bs,pos) _bitset_next((bs), (pos), 1)

/**
 * Convenience macro for bitset iteration.
 * @param bitset The bitset.
 * @param elm A unsigned long variable.
 */
#define bitset_foreach(bitset,elm) \
  for(elm = bitset_next_set(bitset,0); elm != -1; elm = bitset_next_set(bitset,elm+1))

/**
 * Count the bits set.
 * This can also be seen as the cardinality of the set.
 * @param bs The bitset.
 * @return The number of bits set in the bitset.
 */
static INLINE unsigned long bitset_popcnt(const bitset_t *bs)
{
	unsigned long i, pop = 0;
	unsigned long *unit;

	for(i = 0, unit = bs->data; i < bs->units; ++i, ++unit)
		pop += _bitset_inside_pop(unit);

	return pop;
}

/**
 * Clear the bitset.
 * This sets all bits to zero.
 * @param bs The bitset.
 */
static INLINE void bitset_clear_all(bitset_t *bs)
{
	memset(bs->data, 0, BS_UNIT_SIZE * bs->units);
}

/**
 * Check, if one bitset is contained by another.
 * That is, each bit set in lhs is also set in rhs.
 * @param lhs A bitset.
 * @param rhs Another bitset.
 * @return 1, if all bits in lhs are also set in rhs, 0 otherwise.
 */
static INLINE int bitset_contains(const bitset_t *lhs, const bitset_t *rhs)
{
	unsigned long n = lhs->units < rhs->units ? lhs->units : rhs->units;
	unsigned long i;

	for(i = 0; i < n; ++i) {
		unsigned long lu = lhs->data[i];
		unsigned long ru = rhs->data[i];

		if((lu | ru) & ~ru)
			return 0;
	}

	/*
	 * If the left hand sinde is a larger bitset than rhs,
	 * we have to check, that all extra bits in lhs are 0
	 */
	if(lhs->units > n) {
		for(i = n; i < lhs->units; ++i) {
			if(lhs->data[i] != 0)
				return 0;
		}
	}

	return 1;
}

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

	putc('[', file);
	for(i = bitset_next_set(bs, 0); i != -1; i = bitset_next_set(bs, i + 1)) {
		fprintf(file, "%s%u", prefix, i);
		prefix = ",";
	}
	putc(']', file);
}

/*
 * Here, the binary operations follow.
 * And, Or, And Not, Xor are available.
 */

#define BINARY_OP(op) \
static INLINE bitset_t *bitset_ ## op(bitset_t *tgt, const bitset_t *src) \
{ \
	int i; \
	int n = tgt->units > src->units ? src->units : tgt->units; \
	for(i = 0; i < n; i += _BITSET_BINOP_UNITS_INC) \
		_bitset_inside_binop_ ## op(&tgt->data[i], &src->data[i]); \
	for(i = n; i < tgt->units; i += _BITSET_BINOP_UNITS_INC) \
		_bitset_inside_binop_with_zero_ ## op(&tgt->data[i]); \
	return tgt; \
}

BINARY_OP(and)
BINARY_OP(andnot)
BINARY_OP(or)
BINARY_OP(xor)


#endif
