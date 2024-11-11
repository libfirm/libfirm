/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief     Error handling for libFirm
 * @author    Michael Beck
 */
#include "panic.h"

#include "irprintf.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

FIRM_NORETURN print_panic(char const *const file, int const line,
                          char const *const func, char const *const fmt, ...)
{
	va_list ap;

	fprintf(stderr, "%s:%d: libFirm panic in %s: ", file, line, func);
	va_start(ap, fmt);
	ir_vfprintf(stderr, fmt, ap);
	va_end(ap);
	putc('\n', stderr);
	abort();
}
