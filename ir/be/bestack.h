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
 * this constant is returned by the get_sp_bias_func functions if the stack
 * is reset (usually because the frame pointer is copied to the stack
 * pointer
 */
#define SP_BIAS_RESET      INT_MIN

typedef void (*set_frame_offset_func)(ir_node *node, int offset);

typedef int (*get_sp_bias_func)(const ir_node *node);

/**
 * Rewire all stack modifying nodes and their users to assure SSA property.
 * @param sp    The stack pointer register
 */
void be_fix_stack_nodes(ir_graph *irg, arch_register_t const *sp);

/**
 * Fix the stack bias for all nodes accessing the stack frame using the
 * stack pointer.
 * @p get_sp_bias should return the delta of the stackpointer for nodes
 * that increment or decrement the stackpointer with constant values.
 * (Such as push and pop variants, be_IncSP, ...). A positive value stands
 * for an expanding stack area, a negative value for a shrinking one,
 * regardless of the actual stack direction of the calling convention.
 * Note that the code already contains a special case for IncSP nodes so the
 * callback does not need to handle them.
 */
void be_abi_fix_stack_bias(ir_graph *irg, get_sp_bias_func get_sp_bias,
                           set_frame_offset_func set_frame_offset,
                           get_frame_entity_func get_frame_entity);

int be_get_stack_entity_offset(be_stack_layout_t *frame, ir_entity *ent,
                               int bias);

#endif
