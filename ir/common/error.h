/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Error handling for libFirm
 * @author   Michael Beck
 */
#ifndef FIRM_COMMON_ERROR_H
#define FIRM_COMMON_ERROR_H

#include "funcattr.h"

/**
 * Prints a panic message to stderr and exits.
 */
FIRM_NORETURN panic(char const *file, int line, char const *func,
                    char const *fmt, ...);

#define panic(...) panic(__FILE__, __LINE__, __func__, __VA_ARGS__)

# endif
