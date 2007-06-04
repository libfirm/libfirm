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
 * @brief    intel 80x86 implementation of bitsets
 * @version  $Id$
 */
#ifndef _BITSET_IA32_H
#define _BITSET_IA32_H

#undef _bitset_inside_clear
#undef _bitset_inside_set
#undef _bitset_inside_flip

#define _bitset_inside_set(unit, bit) \
	__asm__ __volatile__( "btsl %1,%0" :"=m" (*unit) : "Ir" (bit) : "cc")

#define _bitset_inside_clear(unit, bit) \
	__asm__ __volatile__( "btrl %1,%0" :"=m" (*unit) : "Ir" (bit) : "cc")

#define _bitset_inside_flip(unit, bit) \
	__asm__ __volatile__( "btcl %1,%0" :"=m" (*unit) : "Ir" (bit) : "cc")

#undef _bitset_inside_is_set
#undef _bitset_inside_nlz
#undef _bitset_inside_ntz
#undef _bitset_inside_ntz_value

#define _bitset_inside_is_set(unit, bit) _bitset_ia32_inside_is_set(unit, bit)
#define _bitset_inside_nlz(unit)         _bitset_ia32_inside_nlz(unit)
#define _bitset_inside_ntz(unit)         _bitset_ia32_inside_ntz(unit)
#define _bitset_inside_ntz_value(unit)   _bitset_ia32_inside_ntz_value(unit)

static INLINE int _bitset_ia32_inside_is_set(bitset_unit_t *unit, unsigned bit)
{
	int res;
	__asm__("btl   %2, %1\n\t"
			"mov   $0, %0\n\t"
			"adc   $0, %0"
			: "=r" (res)
			: "m" (*unit), "Ir" (bit)
			: "cc");
	return res;
}

static INLINE unsigned _bitset_ia32_inside_nlz(bitset_unit_t *unit)
{
	unsigned res, tmp;
	__asm__("bsrl   %1, %0\n\t"
			"mov    $ffffffff, %2\n\t"
			"cmovz  %2, %0\n\t"
			"neg    %0\n\t"
			"add   $31, %0"
			: "=&r" (res)
			: "m" (*unit), "r" (tmp)
			: "cc");
	return res;
}

static INLINE unsigned _bitset_ia32_inside_ntz(bitset_unit_t *unit) {
	unsigned res, tmp;
	__asm__("bsf l  %1, %0\n\t"
			"mov   $32, %2\n\t"
			"cmovz  %2, %0\n\t"
			: "=&r" (res)
			: "m" (*unit), "r" (tmp)
			: "cc");
	return res;
}

static INLINE unsigned _bitset_ia32_inside_ntz_value(bitset_unit_t unit) {
	unsigned res, tmp;
	__asm__("bsfl   %1, %0\n\t"
			"mov   $32, %2\n\t"
			"cmovz  %2, %0\n\t"
			: "=&r" (res)
			: "r" (unit), "r" (tmp)
			: "cc");
	return res;
}

#if defined(__GNUC__) && defined(__SSE2__)

#include <stddef.h>
#include <xmmintrin.h>

#undef _bitset_units
#undef _bitset_overall_size
#undef _bitset_data_ptr

#undef _BITSET_BINOP_UNITS_INC

#undef _bitset_inside_binop_and
#undef _bitset_inside_binop_andnot
#undef _bitset_inside_binop_or
#undef _bitset_inside_binop_xor

#define _bitset_units(highest_bit) (round_up2(highest_bit, 128) / BS_UNIT_SIZE_BITS)

#define _bitset_overall_size(bitset_base_size,highest_bit) \
	((bitset_base_size) + 16 + _bitset_units(highest_bit) * BS_UNIT_SIZE)

#define _bitset_data_ptr(data,bitset_base_size,highest_bit) \
  _bitset_sse_data_ptr(data, bitset_base_size, highest_bit)

static INLINE bitset_unit_t *_bitset_sse_data_ptr(void *data, size_t bitset_base_size,
		bitset_pos_t highest_bit)
{
	ptrdiff_t diff;
	char *units = data;

	diff = (units - (char *) 0) + bitset_base_size;
	diff = round_up2(diff, 16);
	units = (char *) 0 + diff;
	return (bitset_unit_t *) units;
}

#define _BITSET_BINOP_UNITS_INC 4
#define _bitset_inside_binop_and(tgt,src) _bitset_sse_inside_binop_and(tgt,src)
#define _bitset_inside_binop_andnot(tgt,src) _bitset_sse_inside_binop_andnot(tgt,src)
#define _bitset_inside_binop_or(tgt,src) _bitset_sse_inside_binop_or(tgt,src)
#define _bitset_inside_binop_xor(tgt,src) _bitset_sse_inside_binop_xor(tgt,src)

#define _BITSET_SSE_BINOP(name) \
static INLINE void _bitset_sse_inside_binop_ ## name(bitset_unit_t *tgt, bitset_unit_t *src) \
{ \
	__m128i src_op = _mm_load_si128((__m128i *) src); \
	__m128i tgt_op = _mm_load_si128((__m128i *) tgt); \
	__m128i res = _mm_ ## name ## _si128(tgt_op, src_op); \
	_mm_store_si128((void *) tgt, res); \
}


static INLINE void _bitset_sse_inside_binop_with_zero_and(bitset_unit_t *tgt)
{
	tgt[0] = 0;
	tgt[1] = 0;
	tgt[2] = 0;
	tgt[3] = 0;
}

static INLINE void _bitset_sse_inside_binop_andnot(bitset_unit_t *tgt, bitset_unit_t *src)
{
	__m128i src_op = _mm_load_si128((void *) src);
	__m128i tgt_op = _mm_load_si128((void *) tgt);
	__m128i res = _mm_andnot_si128(src_op, tgt_op);
	_mm_store_si128((__m128i *) tgt, res);
}

_BITSET_SSE_BINOP(and)
_BITSET_SSE_BINOP(or)
_BITSET_SSE_BINOP(xor)


#endif
#endif
