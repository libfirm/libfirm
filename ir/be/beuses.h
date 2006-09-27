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

#include "bearch.h"
#include "belive.h"

#define USES_INFINITY                 1000000

static INLINE int USES_IS_INFINITE(unsigned time)
{
	return time >= USES_INFINITY;
}

typedef struct _be_uses_t be_uses_t;

unsigned be_get_next_use(be_uses_t *uses, const ir_node *from,
                         unsigned from_step, const ir_node *def,
                         int skip_from_uses);

be_uses_t *be_begin_uses(ir_graph *irg, const ir_exec_freq *execfreqs,
                         const be_lv_t *lv);

void be_end_uses(be_uses_t *uses);

#endif /* _BEUSES_H */
