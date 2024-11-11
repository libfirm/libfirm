/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Panic routine
 * @author   Michael Beck
 */
#ifndef FIRM_COMMON_PANIC_H
#define FIRM_COMMON_PANIC_H

#include "funcattr.h"

/**
 * Prints a panic message to stderr and exits.
 */
FIRM_NORETURN print_panic(char const *file, int line, char const *func,
                          char const *fmt, ...);

#define panic(...) print_panic(__FILE__, __LINE__, __func__, __VA_ARGS__)

#define TODO(node) panic("%+F in function '%F': TODO", node, get_irn_irg(node))

# endif
