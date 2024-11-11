/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Helper functions for handling offsets into stack frames/
 *              activation records.
 * @author      Matthias Braun
 */
#ifndef FIRM_BE_BESTACK_H
#define FIRM_BE_BESTACK_H

#include <limits.h>
#include <stdbool.h>

#include "be_types.h"
#include "firm_types.h"

typedef struct stack_pointer_state_t {
	/** Misalignment of stack pointer when offset == 0 */
	unsigned misalign;
	/** Target alignment for stack pointer in log 2. */
	unsigned p2align;
	/** Offset of stack pointer value to value at function begin. */
	int      offset;
	/** Extra bytes allocated to keep the stack pointer alignment. */
	unsigned align_padding;
	bool     no_change;
} stack_pointer_state_t;

/**
 * Rewire all stack modifying nodes and their users to assure SSA property.
 * @param sp    The stack pointer register
 */
void be_fix_stack_nodes(ir_graph *irg, arch_register_t const *sp);

typedef void (*sp_sim_func)(ir_node *node, stack_pointer_state_t *state);

/**
 * From function begin simulate relative stack pointer offset along the
 * function.  Note that the code already contains a special case for IncSP node
 * which needs no handling in the callback.
 */
void be_sim_stack_pointer(ir_graph *irg, unsigned misalign, unsigned p2align,
                          sp_sim_func func);

/**
 * Layout entities in frame type. This will not touch entities which already
 * have offsets assigned.
 */
void be_layout_frame_type(ir_type *frame, int begin, unsigned misalign);

void be_sort_frame_entities(ir_type *const frame, bool spillslots_first);

#endif
