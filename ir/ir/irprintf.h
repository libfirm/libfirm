/*
 * Project:     libFIRM
 * File name:   ir/ir/irprintf.h
 * Purpose:     A little printf understanding some firm types.
 * Author:      Sebastian Hack
 * Created:     29.11.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file irprintf.h
 *
 * A little printf understanding some firm types.
 * @author Sebastian Hack
 * @date 29.11.2004
 */

#ifndef _IRPRINTF_H
#define _IRPRINTF_H

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
 * This function rudimentarily implements a kind of printf(3) for ir
 * nodes. Following conversion specifiers. No length, special or field
 * width specifiers are accepted.
 * - @%p A pointer.
 * - @%s A string.
 * - @%I An ident.
 * - @%t A type name.
 * - @%e An entity name.
 * - @%E An entity ld name.
 * - @%n A full description of a node.
 * - @%O The opcode name of an ir node.
 * - @%m The mode name of an ir mode.
 * - @%N The node number of an ir node.
 * - @%B The block node number of the nodes block.
 * - @%b A bitset.
 * - @%T A tarval.
 *
 * Each of these can be prepended by a '+' which means, that the given
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
 * passed by the dumping function. Suppose you have your own datatype
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
 *
 * @param fmt The format string.
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

#endif
