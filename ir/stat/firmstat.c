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

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

# include <string.h>

# include "irop_t.h"
# include "irnode_t.h"

# include "xmalloc.h"

/*
 * 64 bit should be enough for now
 */
#define STAT_CNT_NUM 2

typedef struct _counter_t {
  unsigned cnt[STAT_CNT_NUM];
} counter_t;


static INLINE cnt_inc(counter_t *cnt)
{
  int i;

  for (i = 0; i < STAT_CNT_NUM; ++i) {
    if (++cnt->cnt[i])
      break;
}

static INLINE cnt_dec(counter_t *cnt)
{
  int i;

  for (i = 0; i < STAT_CNT_NUM; ++i) {
    if (--cnt->cnt[i] != -1)
      break;
}

/* A new IR op is registered. */
void stat_new_ir_op(const ir_op *op)
{
}

/* An IR op is freed. */
void stat_free_ir_op(const ir_op *op)
{
}

/* initialize the statistics module. */
void stat_init(void)
{
}

/* A new node is created. */
void stat_new_node(const ir_node *node)
{
}
