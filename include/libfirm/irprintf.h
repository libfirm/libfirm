/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief    A little printf understanding some firm types.
 * @author   Sebastian Hack
 * @date     29.11.2004
 */
#ifndef FIRM_IR_IRPRINTF_H
#define FIRM_IR_IRPRINTF_H

#include <stddef.h>
#include <stdarg.h>
#include <stdio.h>
#include "begin.h"

/* forward definition */
struct obstack;

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
 * - @%P A compound graph path.
 * - @%I An ident.
 * - @%D Print as many white spaces as given in the parameter.
 * - @%G A debug info (if available) from the given ir node.
 * - @%B A bitset.
 * - @%F A Firm object (automatically detected).
 *
 * Note that some of the standard format capabilities are not available
 * due to this new conversion specifiers, use lc_printf() if needed.
 */
FIRM_API void ir_printf(const char *fmt, ...);

/**
 * @see ir_printf.
 */
FIRM_API void ir_fprintf(FILE *f, const char *fmt, ...);

/**
 * @see ir_printf.
 */
FIRM_API void ir_snprintf(char *buf, size_t n, const char *fmt, ...);

/**
 * @see ir_printf.
 */
FIRM_API void ir_vprintf(const char *fmt, va_list args);

/**
 * @see ir_printf.
 */
FIRM_API void ir_vfprintf(FILE *f, const char *fmt, va_list args);

/**
 * @see ir_printf.
 */
FIRM_API void ir_vsnprintf(char *buf, size_t len, const char *fmt,
                           va_list args);

/**
 * @see ir_printf.
 */
FIRM_API void ir_obst_vprintf(struct obstack *obst, const char *fmt,
                              va_list args);

#include "end.h"

#endif
