/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Methods to compute when a value will be used again.
 * @author      Sebastian Hack, Matthias Braun
 * @date        27.06.2005
 */
#ifndef FIRM_BE_BEUSES_H
#define FIRM_BE_BEUSES_H

#include <stdbool.h>
#include "firm_types.h"
#include "belive.h"

/**
 * Describes a use of a value.
 */
typedef struct be_next_use_t {
	unsigned       time;
	unsigned       outermost_loop;
	/* point of the next use is at the beginning of this node. */
	const ir_node *before;
} be_next_use_t;

#define USES_INFINITY  10000000
#define USES_PENDING   9999999

static inline bool USES_IS_INFINITE(unsigned time)
{
	return time >= USES_INFINITY;
}

static inline bool USES_IS_PENDING(unsigned time)
{
	return time == USES_PENDING;
}

typedef struct be_uses_t be_uses_t;

be_next_use_t be_get_next_use(be_uses_t *uses, ir_node *from,
                              const ir_node *def, bool skip_from_uses);

/**
 * Creates a new uses environment for a graph.
 *
 * @param irg  the graph
 * @param lv   liveness information for the graph
 */
be_uses_t *be_begin_uses(ir_graph *irg, const be_lv_t *lv);

/**
 * Destroys the given uses environment.
 *
 * @param uses  the environment
 */
void be_end_uses(be_uses_t *uses);

#endif
