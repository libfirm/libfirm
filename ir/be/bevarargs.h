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

#include "be.h"
#include "firm_types.h"

/**
 * Default implementation to lower a va_arg node.
 *
 * This implementation assumes that all arguments except compound types are
 * stored on the stack and that the va_list value points to the next variadic
 * argument.  For compound types a pointer to the object is stored on the stack.
 *
 * @param node A Builtin node with kind ir_bk_va_arg to be lowered
 */
void be_default_lower_va_arg_compound_ptr(ir_node *node);

/**
 * Default implementation to lower a va_arg node.
 *
 * This implementation assumes that all arguments are stored on the
 * stack and that the va_list value points to the next variadic
 * argument.
 *
 * @param node A Builtin node with kind ir_bk_va_arg to be lowered
 */
void be_default_lower_va_arg_compound_val(ir_node *node);

void be_set_va_list_type_pointer(backend_params *p);

ir_entity *be_make_va_start_entity(ir_type *frame_type, int offset);

#endif
