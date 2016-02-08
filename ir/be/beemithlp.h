/*
 * This file is part of libFirm.
 * Copyright (C) 2016 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Helper functions for emitting assembly from a firm graph.
 * @author      Matthias Braun
 *
 * You typically register an emission function for each node type of your
 * backend with be_set_emitter().
 */
#ifndef FIRM_BE_BEEMITHLP_H
#define FIRM_BE_BEEMITHLP_H

#include "be.h"
#include "irop_t.h"

/**
 * Emit spaces until the comment position is reached.
 */
void be_emit_pad_comment(void);

/**
 * The type of a emitter function.
 */
typedef void emit_func(ir_node const *node);

static inline void be_set_emitter(ir_op *const op, emit_func *const func)
{
	set_generic_function_ptr(op, func);
}

void be_init_emitters(void);

void be_emit_nothing(ir_node const *node);

/**
 * Emit code for a node by calling a handler registeres with be_set_emitter().
 */
void be_emit_node(ir_node const *node);

#endif
