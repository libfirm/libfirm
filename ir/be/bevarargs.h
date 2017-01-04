/*
 * This file is part of libFirm.
 * Copyright (C) 2015 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Default (pointer-based) implementation of variadic functions
 * @author  Andreas Fried
 */

#ifndef FIRM_BE_BEVARARGS_H
#define FIRM_BE_BEVARARGS_H

#include "be_types.h"
#include "firm_types.h"
#include <stdbool.h>

/**
 * Default implementation to lower a va_arg node.
 *
 * If \p compound_is_ptr is true, then this implementation assumes that all
 * arguments except compound types are stored on the stack and that the va_list
 * value points to the next variadic argument.  For compound types a pointer to
 * the object is stored on the stack.
 *
 * If \p compound_is_ptr is false, then this implementation assumes that all
 * arguments are stored on the stack and that the va_list value points to the
 * next variadic argument.
 *
 * @param node A Builtin node with kind ir_bk_va_arg to be lowered
 */
void be_default_lower_va_arg(ir_node *node, bool compound_is_ptr,
                             unsigned stack_param_align);

ir_entity *be_make_va_start_entity(ir_type *frame_type, int offset);

#endif
