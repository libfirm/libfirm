/**
 * @file   absgraph.h
 * @date   20.04.2007
 * @author Sebastian Hack
 *
 * An abstract graph "interface". Currently
 * only used by the DFS facility.
 *
 * This is just that we can do some graph algos
 * on the CFG, dominance tree, etc.
 *
 * Copyright (C) 2007 Universitaet Karlsruhe
 * Released under the GPL
 */

#ifndef _ABSGRAPH_H
#define _ABSGRAPH_H

#include "obst.h"

typedef struct _absgraph_t {
	void *(*get_root)(void *self);
	void (*grow_succs)(void *self, void *node, struct obstack *obst);
} absgraph_t;

const absgraph_t absgraph_irg_cfg_succ;
const absgraph_t absgraph_irg_cfg_pred;

#endif /* _ABSGRAPH_H */
