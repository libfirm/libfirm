/**
 * @file   beuse.h
 * @date   27.06.2005
 * @author Sebastian Hack, Matthias Braun
 *
 * Determine future usages of values.
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */
#ifndef _BEUSES_H
#define _BEUSES_H

#include "bearch_t.h"
#include "belive.h"

typedef struct _be_next_use_t {
	unsigned time;
	int outermost_loop;
} be_next_use_t;

#define USES_INFINITY                 10000000
#define USES_PENDING                   9999999

static INLINE int USES_IS_INFINITE(unsigned time)
{
	return time >= USES_INFINITY;
}

static INLINE int USES_IS_PENDING(unsigned time)
{
	return time == USES_PENDING;
}

typedef struct _be_uses_t be_uses_t;

be_next_use_t be_get_next_use(be_uses_t *uses, ir_node *from,
                         unsigned from_step, const ir_node *def,
                         int skip_from_uses);

be_uses_t *be_begin_uses(ir_graph *irg, const be_lv_t *lv);

void be_end_uses(be_uses_t *uses);

#endif /* _BEUSES_H */
