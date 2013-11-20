/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    printf variants understanding firm objects.
 * @author   Sebastian Hack, Matthias Braun
 */
#ifndef FIRM_IR_IRPRINTF_H
#define FIRM_IR_IRPRINTF_H

#include <stddef.h>
#include <stdarg.h>
#include <stdio.h>
#include "begin.h"

struct obstack;

/**
 * @ingroup printing
 * @defgroup printf      String Formatting
 * These functions allow printing of formated strings with support for
 * printing firm objects in a human readable form.
 * @{
 */

/**
 * A string formatting routine for ir objects.
 *
 * @param fmt  The format string.
 *
 * This function rudimentary implements a kind of printf(3) for ir
 * nodes and adds the following additional conversion specifiers.
 * - @%t A type name.
 * - @%e An entity name.
 * - @%E An entity ld name.
 * - @%T A tarval.
 * - @%n A full description of a node.
 * - @%O The opcode name of an ir node.
 * - @%N The node number of an ir node.
 * - @%m The mode name of an ir mode.
 * - @%B The block node number of the nodes block.
 * - @%I An ident.
 * - @%D Print as many white spaces as given in the parameter.
 * - @%G A debug info (if available) from the given ir node.
 * - @%B A bitset.
 * - @%F A Firm object (automatically detected).
 */
FIRM_API int ir_printf(const char *fmt, ...);

/**
 * @see ir_printf.
 */
FIRM_API int ir_fprintf(FILE *f, const char *fmt, ...);

/**
 * @see ir_printf.
 */
FIRM_API int ir_snprintf(char *buf, size_t n, const char *fmt, ...);

/**
 * @see ir_printf.
 */
FIRM_API int ir_vprintf(const char *fmt, va_list args);

/**
 * @see ir_printf.
 */
FIRM_API int ir_vfprintf(FILE *f, const char *fmt, va_list args);

/**
 * @see ir_printf.
 */
FIRM_API int ir_vsnprintf(char *buf, size_t len, const char *fmt, va_list args);

/**
 * @see ir_printf.
 */
FIRM_API int ir_obst_vprintf(struct obstack *obst, const char *fmt,
                             va_list args);

/** @} */

#include "end.h"

#endif
