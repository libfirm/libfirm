/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @version  $Id$
 */
#ifndef FIRM_IR_IRPRINTF_H
#define FIRM_IR_IRPRINTF_H

#include "firm_config.h"

#include <stddef.h>
#include <stdarg.h>
#include <stdio.h>

/* forward definition */
struct obstack;

/**
 * Something that can append strings and chars to something.
 */
typedef struct _appender_t {
	void (*init)(void *object, size_t n);
	void (*append_char)(void *object, size_t n, char ch);
	void (*append_str)(void *object, size_t n, const char *str);
} appender_t;

/**
 * A callback function type to add something to an appender.
 *
 * @param app    The appender.
 * @param object The object for the appender.
 * @param limit  The limit for the appender.
 * @param arg    The thing to append.
 */
typedef void (ir_printf_cb_t)(const appender_t *app, void *object, size_t limit, const void *arg);

/**
 * A string formatting routine for ir objects.
 *
 * @param fmt  The format string.
 *
 * This function rudimentary implements a kind of printf(3) for ir
 * nodes. Following conversion specifiers. No length, special or field
 * width specifiers are accepted.
 * - @%% Print a '%' character.
 * - @%> Print as many white spaces as given in the parameter.
 * - @%c Print a character
 * - @%s A string.
 * - @%p A pointer.
 * - @%d A decimal integer.
 * - @%x A hexadecimal integer.
 * - @%o An octal integer.
 * - @%I An ident.
 * - @%t A type name.
 * - @%e An entity name.
 * - @%E An entity ld name.
 * - @%T A tarval.
 * - @%n A full description of a node.
 * - @%O The opcode name of an ir node.
 * - @%N The node number of an ir node.
 * - @%m The mode name of an ir mode.
 * - @%B The block node number of the nodes block.
 * - @%b A bitset.
 * - @%= A pnc value
 * - @%G A debug info (if available)
 * - @%P A compound graph path
 *
 * Each of these can be prepend by a '+' which means, that the given
 * pointer is a collection of items specified by the format. In this
 * case you also have to pass an iterator interface to ir_printf()
 * suitable for the instance of the collection. So, imagine you have a
 * @c pset of ir_nodes and want to dump it, you write:
 * @code
 *   pset *nodes;
 *   ...
 *   ir_printf("Some nodes: %*n\n", it_pset, nodes);
 * @endcode
 * The @c it_pset is an iterator interface (of type
 * @c iterator_t that allows the dumper to traverse the set.
 *
 * As special case when working with collections, you can also give a
 * callback function which will be invoked on each element in the
 * collection. It gets the appender (the thing where the textual
 * representation of the element is written to) and its parameters
 * passed by the dumping function. Suppose you have your own data type
 * @c xyz_t and want to dump a pset of it, you have:
 * @code
 *   void xyz_dump(const appender_t *app, void *object, size_t limit,
 *       const void *arg)
 *   {
 *     const xyz_t *xyz = arg;
 *     app->append_str(object, limit, xyz->name);
 *   }
 *   ...
 *   pset *xyzs;
 *
 *   ir_printf("A set of xyz\'s: %*C\n", it_pset, xyzs, xyz_dump);
 * @endcode
 */
void ir_printf(const char *fmt, ...);

/**
 * @see irn_printf.
 */
void ir_fprintf(FILE *f, const char *fmt, ...);

/**
 * @see irn_printf.
 */
void ir_snprintf(char *buf, size_t n, const char *fmt, ...);

/**
 * @see irn_printf.
 */
void ir_vprintf(const char *fmt, va_list args);

/**
 * @see irn_printf.
 */
void ir_vfprintf(FILE *f, const char *fmt, va_list args);

/**
 * @see irn_printf.
 */
void ir_vsnprintf(char *buf, size_t len, const char *fmt, va_list args);

/**
 * @see irn_printf.
 */
void ir_obst_vprintf(struct obstack *obst, const char *fmt, va_list args);

#ifdef WITH_LIBCORE
#include <libcore/lc_printf.h>
#endif /* WITH_LIBCORE */

#endif
