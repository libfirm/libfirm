/*
 * Project:     libFIRM
 * File name:   ir/ir/firmstat.c
 * Purpose:     Statistics for Firm.
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#include "firmstat_t.h"
#include "tv.h"

static stat_info_t *status;

/**
 * calculated the dual logarithmus of |value|
 */
static unsigned log2(long value) {
  unsigned res = 0;

  if (value < 0)
    value = -value;

  if (value > 0xFFFF) {
    res += 16;
    value >>= 16;
  }
  if (value > 0xFF) {
    res += 8;
    value >>= 8;
  }
  if (value > 0xF) {
    res += 4;
    value >>= 4;
  }
  if (value > 3) {
    res += 2;
    value >>= 2;
  }
  if (value > 1) {
    res += 1;
  }

  return res;
}

/**
 * update info on Consts
 *
 * @param node   The Const node
 * @param graph  The graph entry containing the call
 */
void stat_update_const(stat_info_t *status, ir_node *node, graph_entry_t *graph)
{
  ir_mode *mode = get_irn_mode(node);
  tarval *tv;
  unsigned bits;

  /* we handle integer modes only here */
  if (! mode_is_int(mode)) {
    cnt_inc(&status->const_info.others);
    return;
  }

  tv   = get_Const_tarval(node);
  bits = log2(get_tarval_long(tv));

  if (bits > ARR_SIZE(status->const_info.bits_count))
    bits = ARR_SIZE(status->const_info.bits_count);

  cnt_inc(&status->const_info.bits_count[bits]);
}

/* clears the const statistics for a new snapshot */
void stat_const_clear(stat_info_t *status)
{
  int i;

  for (i = 0; i < ARR_SIZE(status->const_info.bits_count); ++i)
    cnt_clr(&status->const_info.bits_count[i]);
  cnt_clr(&status->const_info.others);
}

/* initialize the Const statistic. */
void stat_init_const_cnt(stat_info_t *status)
{
}
