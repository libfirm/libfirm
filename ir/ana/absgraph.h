/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @author  Sebastian Hack
 * @date    20.04.2007
 * @brief
 *
 * An abstract graph "interface". Currently
 * only used by the DFS facility.
 *
 * This is just that we can do some graph algos
 * on the CFG, dominance tree, etc.
 */
#ifndef FIRM_ANA_ABSGRAPH_H
#define FIRM_ANA_ABSGRAPH_H

#include "obst.h"

typedef struct absgraph_t {
	void *(*get_root)(void *self);
	void (*grow_succs)(void *self, void *node, struct obstack *obst);
	void *(*get_end)(void *self);
} absgraph_t;

extern const absgraph_t absgraph_irg_cfg_succ;
extern const absgraph_t absgraph_irg_cfg_pred;

#endif
