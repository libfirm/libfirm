/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Lowering of builtins to compiler-lib calls
 * @author  Matthias Braun
 */
#ifndef FIRM_LOWER_BUILTINS_H
#define FIRM_LOWER_BUILTINS_H

#include "firm_types.h"
#include <stddef.h>

typedef void(*lower_func)(ir_node*);

void lower_builtins(size_t n_exceptions, ir_builtin_kind const *exceptions,
                    lower_func lower_va_arg);

#endif
