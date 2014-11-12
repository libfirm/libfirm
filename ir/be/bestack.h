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

#include "firm_types.h"
#include "be_types.h"

/**
 * Rewire all stack modifying nodes and their users to assure SSA property.
 * @param sp    The stack pointer register
 */
void be_fix_stack_nodes(ir_graph *irg, arch_register_t const *sp);

/**
 * Fix the stack bias for all nodes accessing the stack frame using the
 * stack pointer.
 */
void be_abi_fix_stack_bias(ir_graph *irg);

int be_get_stack_entity_offset(be_stack_layout_t *frame, ir_entity *ent,
                               int bias);

#endif
