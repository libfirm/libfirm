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

#include <stdarg.h>
#include <stdio.h>
#include "firm_types.h"
struct obstack;

#include "begin.h"

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

/**
 * Output a tarval in human readable format to a string buffer.
 *
 * This function is meant for debugging purposes, the string is formated in a
 * way easily understandable for humans. You should not use this for storage or
 * output for other tools (like assemblers), use get_tarval_sub_bits() for these
 * cases.
 *
 * The final string in "buf" is guaranteed to be zero-terminated (if buflen is
 * at least 1).
 *
 * @param buf     the output buffer
 * @param buflen  the length of the buffer
 * @param tv      the tarval
 */
FIRM_API int tarval_snprintf(char *buf, size_t buflen, ir_tarval const *tv);

/** @} */
#include "end.h"

#endif
