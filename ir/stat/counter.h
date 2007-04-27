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

/*
 * Project:     libFIRM
 * File name:   ir/ir/counter.h
 * Purpose:     Statistics for Firm. Counter implementation.
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 */
#ifndef _COUNTER_H_
#define _COUNTER_H_

#include <string.h>
#include <limits.h>

/*
 * 32 bit should be enough for most cases
 */
#ifndef STAT_CNT_NUM
#define STAT_CNT_NUM 1
#endif

typedef struct _counter_t {
	unsigned cnt[STAT_CNT_NUM];
} counter_t;

/** initializes a counter with zero */
#define ZERO_CNT { { 0 } }

/**
 * increase a counter
 */
static INLINE void cnt_inc(counter_t *cnt)
{
	int i;

	for (i = 0; i < STAT_CNT_NUM; ++i) {
		if (++cnt->cnt[i])
			break;
	}
}

/**
 * decrease a counter
 */
static INLINE void cnt_dec(counter_t *cnt)
{
	int i;

	for (i = 0; i < STAT_CNT_NUM; ++i) {
		if (--cnt->cnt[i] != -1)
			break;
	}
}

/**
 * set a counter to zero
 */
static INLINE void cnt_clr(counter_t *cnt)
{
	memset(cnt->cnt, 0, sizeof(cnt->cnt));
}

/**
 * add a counter to another
 */
static INLINE void cnt_add(counter_t *dst, const counter_t *src)
{
	int i, carry = 0;

	for (i = 0; i < STAT_CNT_NUM; ++i) {
		unsigned x = dst->cnt[i];
		unsigned y = src->cnt[i];
		unsigned a = x + y + carry;

		carry = (int)((x & y) | ((x | y) & ~a)) < 0 ? 1 : 0;

		dst->cnt[i] = a;
	}
}

/**
 * add an (positive) integer to an counter
 */
static INLINE void cnt_add_i(counter_t *dst, int src)
{
	int i;
	unsigned carry = src;

	for (i = 0; i < STAT_CNT_NUM; ++i) {
		unsigned a = dst->cnt[i] + carry;

		carry = a < dst->cnt[i];

		dst->cnt[i] = a;

		if (! carry)
			break;
	}
}

/**
 * compare two counter
 */
static INLINE int cnt_cmp(const counter_t *a, const counter_t *b)
{
	int i;
	unsigned va, vb;

	for (i = STAT_CNT_NUM - 1 ; i >= 0; --i) {
		va = a->cnt[i];
		vb = b->cnt[i];

		if (va != vb)
			break;
	}

	if (va != vb)
		return va < vb ? -1 : 1;
	return 0;
}

/**
 * convert a counter into a double
 */
static INLINE double cnt_to_dbl(const counter_t *a)
{
	int i;
	double res = 0.0, scale = 1.0, tmp;

	i = (1 << (sizeof(a->cnt[0]) * 4));
	tmp = ((double)i) * ((double)i);

	for (i = 0; i < STAT_CNT_NUM; ++i) {
		res += scale * (double)a->cnt[i];

		scale *= tmp;
	}
	return res;
}

/**
 * convert a counter into an unsigned
 */
static INLINE unsigned cnt_to_uint(const counter_t *a)
{
	int i;

	for (i = 1; i < STAT_CNT_NUM; ++i)
		if (a->cnt[i])
			return UINT_MAX;

	return a->cnt[0];
}

/**
 * check, if a counter is equal to an unsigned
 */
static INLINE int cnt_eq(const counter_t *a, unsigned value)
{
	int i;

	for (i = 1; i < STAT_CNT_NUM; ++i)
		if (a->cnt[i])
			return 0;

	return a->cnt[0] == value;
}

/**
 * check, if a counter as greater than an unsigned
 */
static INLINE int cnt_gt(const counter_t *a, unsigned value)
{
	int i;

	for (i = 1; i < STAT_CNT_NUM; ++i)
		if (a->cnt[i])
			return 1;

	return a->cnt[0] > value;
}

#endif /* _COUNTER_H_ */
