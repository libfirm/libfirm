/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   convenience layer over raw_bitsets (stores number of bits
 *          with the bitfield)
 * @author  Matthias Braun
 */
#ifndef FIRM_ADT_BITSET_H
#define FIRM_ADT_BITSET_H

#include <stdio.h>
#include <assert.h>

#include "xmalloc.h"
#include "bitfiddle.h"
#include "raw_bitset.h"

typedef struct {
	size_t   size;    /**< size of the bitset in bits */
	unsigned data[];
} bitset_t;

/**
 * Return the number of bytes a bitset would need
 */
static inline size_t bitset_total_size(size_t const n_bits)
{
	return sizeof(bitset_t) + BITSET_SIZE_BYTES(n_bits);
}

/**
 * Initialize a bitset for bitsize size (bitset should point to memory
 * with a size calculated by bitset_total_size)
 */
static inline bitset_t *bitset_init(void *memory, size_t size)
{
	bitset_t *result = (bitset_t*)memory;
	result->size = size;
	rbitset_clear_all(result->data, size);
	return result;
}

/**
 * Allocate a bitset on an obstack.
 * @param obst The obstack.
 * @param size The greatest bit that shall be stored in the set.
 * @return A pointer to an empty initialized bitset.
 */
static inline bitset_t *bitset_obstack_alloc(struct obstack *obst,
                                             size_t n_bits)
{
	size_t size   = bitset_total_size(n_bits);
	void  *memory = obstack_alloc(obst, size);
	return bitset_init(memory, n_bits);
}

/**
 * Allocate a bitset via malloc.
 * @param size The greatest bit that shall be stored in the set.
 * @return A pointer to an empty initialized bitset.
 */
static inline bitset_t *bitset_malloc(size_t n_bits)
{
	size_t  size   = bitset_total_size(n_bits);
	void   *memory = xmalloc(size);
	return bitset_init(memory, n_bits);
}

/**
 * Allocate a bitset on the stack via alloca.
 * @param size The greatest bit that shall be stored in the set.
 * @return A pointer to an empty initialized bitset.
 */
#define bitset_alloca(size) \
	bitset_init(alloca(bitset_total_size(size)), (size))

/**
 * Get the size of the bitset in bits.
 * @note Note the difference between capacity and size.
 * @param bs The bitset.
 * @return The highest bit which can be set or cleared plus 1.
 */
static inline size_t bitset_size(bitset_t const *bitset)
{
	return bitset->size;
}

/**
 * Set a bit in the bitset.
 * @param bs The bitset.
 * @param bit The bit to set.
 */
static inline void bitset_set(bitset_t *bs, size_t bit)
{
	assert(bit < bs->size);
	rbitset_set(bs->data, bit);
}

/**
 * Clear a bit in the bitset.
 * @param bs The bitset.
 * @param bit The bit to clear.
 */
static inline void bitset_clear(bitset_t *bs, size_t bit)
{
	assert(bit < bs->size);
	rbitset_clear(bs->data, bit);
}

/**
 * Check, if a bit is set.
 * @param bs The bitset.
 * @param bit The bit to check for.
 * @return 1, if the bit was set, 0 if not.
 */
static inline bool bitset_is_set(bitset_t const *bs, size_t bit)
{
	assert(bit < bs->size);
	return rbitset_is_set(bs->data, bit);
}

/**
 * Flip a bit in a bitset.
 * @param bs The bitset.
 * @param bit The bit to flip.
 */
static inline void bitset_flip(bitset_t *bs, size_t bit)
{
	assert(bit < bs->size);
	rbitset_flip(bs->data, bit);
}

/**
 * Flip the whole bitset.
 * @param bs The bitset.
 */
static inline void bitset_flip_all(bitset_t *bs)
{
	rbitset_flip_all(bs->data, bs->size);
}

/**
 * Copy a bitset to another. Both bitset must be initialized and have the same
 * number of bits.
 * @param tgt The target bitset.
 * @param src The source bitset.
 * @return The target bitset.
 */
static inline void bitset_copy(bitset_t *tgt, bitset_t const *src)
{
	assert(tgt->size == src->size);
	rbitset_copy(tgt->data, src->data, src->size);
}

/**
 * Find the next unset bit from a given bit.
 * @note Note that if pos is unset, pos is returned.
 * @param bs The bitset.
 * @param pos The bit from which to search for the next set bit.
 * @return The next set bit from pos on, or (size_t)-1, if no unset bit was
 * found after pos.
 */
static inline size_t bitset_next_clear(bitset_t const *bs, size_t pos)
{
	return rbitset_next_max(bs->data, pos, bs->size, false);
}

/**
 * Find the next set bit from a given bit.
 * @note Note that if pos is set, pos is returned.
 * @param bs The bitset.
 * @param pos The bit from which to search for the next set bit.
 * @return The next set bit from pos on, or (size_t)-1, if no set bit was
 * found after pos.
 */
static inline size_t bitset_next_set(bitset_t const *bs, size_t pos)
{
	return rbitset_next_max(bs->data, pos, bs->size, true);
}

/**
 * Convenience macro for bitset iteration.
 * @param bitset The bitset.
 * @param elm A size_t variable.
 */
#define bitset_foreach(bitset, elm) \
	for (size_t elm = 0; (elm = bitset_next_set((bitset), elm)) != (size_t)-1; ++elm)


#define bitset_foreach_clear(bitset, elm) \
	for (size_t elm = 0; (elm = bitset_next_clear((bitset), elm)) != (size_t)-1; ++elm)

/**
 * Find the previous unset bit from a given bit.
 * @param bs The bitset.
 * @param pos The bit from which to search for the previous unset bit.
 * @return The previous unset bit from pos on, or (size_t)-1, if no unset bit was
 * found before pos.
 */
static inline size_t bitset_prev_clear(bitset_t const *bs, size_t pos)
{
	return rbitset_prev(bs->data, pos, false);
}

/**
 * Find the previous set bit from a given bit.
 * @param bs The bitset.
 * @param pos The bit from which to search for the next set bit.
 * @return The previous set bit from pos on, or (size_t)-1, if no set bit was
 * found before pos.
 */
static inline size_t bitset_prev_set(bitset_t const *bs, size_t pos)
{
	return rbitset_prev(bs->data, pos, true);
}


#define bitset_foreach_rev(bitset, elm) \
  for (size_t elm = bitset->size; (elm = rbitset_prev(bitset->data, elm, true)) != (size_t)-1;)


#define bitset_foreach_clear_rev(bitset, elm) \
  for (size_t elm = bitset->size; (elm = rbitset_prev(bitset->data, elm, false)) != (size_t)-1;)

/**
 * Count the bits set.
 * This can also be seen as the cardinality of the set.
 * @param bs The bitset.
 * @return The number of bits set in the bitset.
 */
static inline size_t bitset_popcount(bitset_t const *bs)
{
	return rbitset_popcount(bs->data, bs->size);
}

/**
 * Clear the bitset.
 * This sets all bits to zero.
 * @param bs The bitset.
 */
static inline void bitset_clear_all(bitset_t *bs)
{
	rbitset_clear_all(bs->data, bs->size);
}

/**
 * Set the bitset.
 * This sets all bits to one.
 * @param bs The bitset.
 */
static inline void bitset_set_all(bitset_t *bs)
{
	rbitset_set_all(bs->data, bs->size);
}

/**
 * Check, if one bitset is contained by another.
 * That is, each bit set in lhs is also set in rhs.
 * @param lhs A bitset.
 * @param rhs Another bitset.
 * @return 1, if all bits in lhs are also set in rhs, 0 otherwise.
 */
static inline bool bitset_contains(bitset_t const *lhs, bitset_t const *rhs)
{
	assert(lhs->size == rhs->size);
	return rbitset_contains(lhs->data, rhs->data, lhs->size);
}

/**
 * Treat the bitset as a number and subtract 1.
 * @param bs The bitset.
 * @return The same bitset.
 */
static inline void bitset_minus1(bitset_t *bs)
{
	rbitset_minus1(bs->data, bs->size);
}

/**
 * Check if two bitsets intersect.
 * @param a The first bitset.
 * @param b The second bitset.
 * @return 1 if they have a bit in common, 0 if not.
 */
static inline bool bitset_intersect(bitset_t const *a, bitset_t const *b)
{
	assert(a->size == b->size);
	return rbitsets_have_common(a->data, b->data, a->size);
}

/**
 * set or clear all bits in the range [from;to[.
 * @param a      The bitset.
 * @param from   The first index to set to one.
 * @param to     The last index plus one to set to one.
 * @param do_set If 1 the bits are set, if 0, they are cleared.
 */
static inline void bitset_mod_range(bitset_t *a, size_t from, size_t to,
                                    bool do_set)
{
	if (from == to)
	    return;

	if (to < from) {
		size_t tmp = from;
		from = to;
		to = tmp;
	}

	if (to > a->size)
		to = a->size;

	rbitset_set_range(a->data, from, to, do_set);
}

#define bitset_set_range(bs, from, to)   bitset_mod_range((bs), (from), (to), 1)
#define bitset_clear_range(bs, from, to) bitset_mod_range((bs), (from), (to), 0)

/**
 * Check, if a bitset is empty.
 * @param a The bitset.
 * @return 1, if the bitset is empty, 0 if not.
 */
static inline bool bitset_is_empty(bitset_t const *bs)
{
	return rbitset_is_empty(bs->data, bs->size);
}

/**
 * Print a bitset to a stream.
 * The bitset is printed as a comma separated list of bits set.
 * @param file The stream.
 * @param bs The bitset.
 */
void bitset_fprint(FILE *file, bitset_t const *bs);

/**
 * Perform tgt = tgt & src operation.
 * @param tgt  The target bitset.
 * @param src  The source bitset.
 * @return the tgt set.
 */
static inline void bitset_and(bitset_t *tgt, bitset_t const *src)
{
	assert(tgt->size == src->size);
	rbitset_and(tgt->data, src->data, src->size);
}

/**
 * Perform tgt = tgt & ~src operation.
 * @param tgt  The target bitset.
 * @param src  The source bitset.
 * @return the tgt set.
 */
static inline void bitset_andnot(bitset_t *tgt, bitset_t const *src)
{
	assert(tgt->size == src->size);
	rbitset_andnot(tgt->data, src->data, src->size);
}

/**
 * Perform Union, tgt = tgt u src operation.
 * @param tgt  The target bitset.
 * @param src  The source bitset.
 * @return the tgt set.
 */
static inline void bitset_or(bitset_t *tgt, bitset_t const *src)
{
	assert(tgt->size == src->size);
	rbitset_or(tgt->data, src->data, src->size);
}

/**
 * Perform tgt = tgt ^ src operation.
 * @param tgt  The target bitset.
 * @param src  The source bitset.
 * @return the tgt set.
 */
static inline void bitset_xor(bitset_t *tgt, bitset_t const *src)
{
	assert(tgt->size == src->size);
	rbitset_xor(tgt->data, src->data, src->size);
}

/**
 * Copy a raw bitset into an bitset.
 */
static inline void rbitset_copy_to_bitset(const unsigned *rbitset,
                                          bitset_t *bitset)
{
	rbitset_copy(bitset->data, rbitset, bitset->size);
}

#endif
