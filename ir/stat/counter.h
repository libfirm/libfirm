/*
 * Project:     libFIRM
 * File name:   ir/ir/counter.h
 * Purpose:     Statistics for Firm. Counter implementation.
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _COUNTER_H_
#define _COUNTER_H_

#include <string.h>

/*
 * 32 bit should be enough for now
 */
#define STAT_CNT_NUM 1

typedef struct _counter_t {
  unsigned cnt[STAT_CNT_NUM];
} counter_t;

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
 * decreace a counter
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
    unsigned a = dst->cnt[i] + src->cnt[i] + carry;

    if (carry)
      carry = a <= dst->cnt[i];
    else
      carry = a < dst->cnt[i];

    dst->cnt[i] = a;

    if (! carry)
      break;
  }
}

/**
 * add an integer to an counter
 */
static INLINE void cnt_add_i(counter_t *dst, int src)
{
  int i;
  unsigned a = dst->cnt[0] + src;
  unsigned carry = a < dst->cnt[0];

  dst->cnt[0] = a;
  if (! carry)
    return;

  for (i = 1; i < STAT_CNT_NUM; ++i) {
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

  i = (1 << (sizeof(a->cnt[i]) * 4));
  tmp = ((double)i) * ((double)i);

  for (i = 0; i < STAT_CNT_NUM; ++i) {
    res += scale * (double)a->cnt[i];

    scale *= tmp;
  }
  return res;
}

#endif /* _COUNTER_H_ */
