/*
 * Project:     libFIRM
 * File name:   ir/ir/irprintf.c
 * Purpose:     A little printf helper unterstanding firm types
 * Author:      Sebastian Hack
 * Created:     29.11.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file irprinf.c
 *
 * A little printf helper unterstanding firm types.
 * @author Sebastian Hack
 * @date 29.11.2004
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include "ident.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "entity_t.h"
#include "tv.h"
#include "irprintf.h"
#include "pset.h"
#include "iterator.h"


/**
 * append a char to a string buffer
 */
static void str_append_char(void *object, size_t n, char ch)
{
	char buf[2];

	buf[0] = ch;
	buf[1] = 0;

	strncat(object, buf, n);
}

/**
 * append a string to a string buffer
 */
static void str_append_str(void *object, size_t n, const char *str)
{
	strncat(object, str, n);
}

/**
 * append a char to a file
 */
static void file_append_char(void *object, size_t n, char ch)
{
	fputc(ch, object);
}

/**
 * append a string to a file
 */
static void file_append_str(void *object, size_t n, const char *str)
{
	fputs(str, object);
}

/**
 * the file appender
 */
static const appender_t file_appender = {
	file_append_char,
	file_append_str
};

/**
 * the string buffer appender
 */
static const appender_t str_appender = {
	str_append_char,
	str_append_str
};

static void ir_common_vprintf(const appender_t *app, void *object,
		size_t limit, const char *fmt, va_list args);

static INLINE void ir_common_printf(const appender_t *app, void *object,
		size_t limit, const char *fmt, ...)
{
	va_list args;

	va_start(args, fmt);
	ir_common_vprintf(app, object, limit, fmt, args);
	va_end(args);
}

/**
 * A small printf helper routine for ir nodes.
 * @param app An appender (this determines where the stuff is dumped
 * to).
 * @param object A target passed to the appender.
 * @param limit The maximum number of characters to dump.
 * @param fmt The format string.
 * @param args A va_list.
 */
static void ir_common_vprintf(const appender_t *app, void *object,
		size_t limit, const char *fmt, va_list args)
{
	char buf[256];
	int i, n;

#define DUMP_STR(s) app->append_str(object, limit, s)
#define DUMP_CH(ch) app->append_char(object, limit, ch)

	for(i = 0, n = strlen(fmt); i < n; ++i) {
		char ch = fmt[i];

		if(ch == '%') {
			char next_ch = fmt[++i];

			/* Clear the temporary buffer */
			buf[0] = '\0';

			switch(next_ch) {
				case '%':
					DUMP_CH('%');
					break;
				case 's':
					DUMP_STR(va_arg(args, const char *));
					break;

                                case 'I':
                                        DUMP_STR(get_id_str(va_arg(args, ident *)));
                                        break;

                                case 'e':
                                        DUMP_STR(get_entity_name(va_arg(args, entity *)));
                                        break;

                                case 'E':
                                        DUMP_STR(get_entity_ld_name(va_arg(args, entity *)));
                                        break;

				case 'p':
					snprintf(buf, sizeof(buf), "%p", va_arg(args, void *));
					break;

				case 't':
					tarval_snprintf(buf, sizeof(buf), va_arg(args, tarval *));
					break;

				case 'n':
					{
						ir_node *irn = va_arg(args, ir_node *);
						snprintf(buf, sizeof(buf), "%s%s:%ld",
								get_irn_opname(irn), get_mode_name(get_irn_mode(irn)), get_irn_node_nr(irn));
					}
					break;

				case 'o':
					DUMP_STR(get_irn_opname(va_arg(args, ir_node *)));
					break;

				case 'N':
					snprintf(buf, sizeof(buf), "%ld", get_irn_node_nr(va_arg(args, ir_node *)));
					break;

				case 'm':
					DUMP_STR(get_mode_name(va_arg(args, ir_mode *)));
					break;

				case 'b':
					snprintf(buf, sizeof(buf), "%ld",
							get_irn_node_nr(get_nodes_block(va_arg(args, ir_node *))));
					break;

				case '+':
					{
						iterator_t *it = va_arg(args, iterator_t *);
						void *collection = va_arg(args, void *);
						void *curr;
						const char *prefix = "";
						char format = fmt[++i];
						ir_printf_cb_t *cb = format == 'C' ? va_arg(args, ir_printf_cb_t *) : NULL;

						assert(is_iterator(it) && "Pass an iterator interface and the collection");

						snprintf(buf, sizeof(buf), "%%%c", format);

						DUMP_CH('[');
						for(curr = it->start(collection); curr; curr = it->next(collection, curr)) {
							DUMP_STR(prefix);

							if(cb)
								cb(app, object, limit, curr);
							else
								ir_common_printf(app, object, limit, buf, curr);

							prefix = ", ";
						}
						it->finish(collection, curr);

						DUMP_CH(']');
					}

					/* clean the buffer again */
					buf[0] = '\0';
					break;
			}

			/* Dump the temporary buffer, if something is in it. */
			if(buf[0] != '\0')
				DUMP_STR(buf);
		}

		else
			DUMP_CH(ch);
	}

#undef DUMP_STR
#undef DUMP_CH
}

/**
 * Convencience for stdout dumping.
 */
void ir_printf(const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	ir_common_vprintf(&file_appender, stdout, 0, fmt, args);
	va_end(args);
}

/**
 * Convencience for file dumping.
 */
void ir_fprintf(FILE *f, const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	ir_common_vprintf(&file_appender, f, 0, fmt, args);
	va_end(args);
}

/**
 * Convencience for string dumping.
 */
void ir_snprintf(char *buf, size_t len, const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	ir_common_vprintf(&str_appender, buf, len, fmt, args);
	va_end(args);
}
