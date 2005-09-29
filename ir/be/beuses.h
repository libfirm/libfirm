/**
 * @file   beuse.h
 * @date   27.06.2005
 * @author Sebastian Hack
 *
 * Determine future usages of values.
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

#ifndef _BEUSES_H
#define _BEUSES_H

#include "bearch.h"

#define USES_INFINITY                 1000000
#define USES_IS_INIFINITE(x)          ((x) >= USES_INFINITY)

typedef struct _loc_t loc_t;
typedef struct _be_uses_t be_uses_t;

unsigned be_get_next_use(be_uses_t *uses, const ir_node *from,
    unsigned from_step, const ir_node *def, int skip_from_uses);

be_uses_t *be_begin_uses(ir_graph *irg,
    const arch_env_t *arch_env,
    const arch_register_class_t *cls);

void be_end_uses(be_uses_t *uses);

#endif /* _BEUSES_H */
