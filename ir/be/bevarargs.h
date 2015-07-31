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

#include "firm_types.h"

/**
 * Default implementation to lower a va_arg node.
 *
 * This implementation assumes that all arguments are stored on the
 * stack and that the va_list value points to the next variadic
 * argument.
 *
 * @param node A Builtin node with kind ir_bk_va_arg to be lowered
 */
void be_default_lower_va_arg(ir_node *node);

#endif
