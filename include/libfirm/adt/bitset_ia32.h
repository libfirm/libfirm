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
	__asm__("bt    %2, %1\n\t"
			"rcl   $1, %0\n\t"
			"and   $1, %0"
			: "=r" (res)
			: "m" (*unit), "Ir" (bit)
			: "cc");
	return res;
}

static INLINE unsigned _bitset_ia32_inside_nlz(bitset_unit_t *unit)
{
	unsigned res;
	__asm__("bsr    %1, %0\n\t"
			"cmovz  %2, %0\n\t"
			"neg    %0\n\t"
			"add   $31, %0"
			: "=&r" (res)
			: "m" (*unit), "r" (-1)
			: "cc");
	return res;
}

static INLINE unsigned _bitset_ia32_inside_ntz(bitset_unit_t *unit) {
	unsigned res;
	__asm__("bsfl   %1, %0\n\t"
			"cmovz  %2, %0\n\t"
			: "=&r" (res)
			: "m" (*unit), "r" (32)
			: "cc");
	return res;
}

static INLINE unsigned _bitset_ia32_inside_ntz_value(bitset_unit_t unit) {
	unsigned res;
	__asm__("bsfl   %1, %0\n\t"
			"cmovz  %2, %0\n\t"
			: "=&r" (res)
			: "r" (unit), "r" (32)
			: "cc");
	return res;
}

#endif
