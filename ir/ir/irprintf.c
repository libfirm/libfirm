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
#include "config.h"
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include <ctype.h>

#include "ident.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "entity_t.h"
#include "type_t.h"
#include "tv.h"
#include "irprintf.h"
#include "obst.h"
#include "pset.h"
#include "iterator.h"
#include "bitset.h"


/**
 * Init the string.
 */
static void str_init(void *object, size_t n)
{
	strcpy(object, "");
}

/**
 * append a char to a string buffer.
 */
static void str_append_char(void *object, size_t n, char ch)
{
	char buf[2];

	buf[0] = ch;
	buf[1] = 0;

	strncat(object, buf, n);
}

/**
 * append a string to a string buffer.
 */
static void str_append_str(void *object, size_t n, const char *str)
{
	strncat(object, str, n);
}


/**
 * Init the file. i.e. do nothing.
 */
static void file_init(void *object, size_t n)
{
}

/**
 * append a char to a file.
 */
static void file_append_char(void *object, size_t n, char ch)
{
	fputc(ch, object);
}

/**
 * append a string to a file.
 */
static void file_append_str(void *object, size_t n, const char *str)
{
	fputs(str, object);
}

/**
 * Init the obstack. i.e. do nothing.
 */
static void obst_init(void *object, size_t n)
{
}

/**
 * append a char to a obstack.
 */
static void obst_append_char(void *object, size_t n, char ch)
{
	struct obstack *obst = object;
	obstack_1grow(obst, ch);
}

/**
 * append a string to a obstack.
 */
static void obst_append_str(void *object, size_t n, const char *str)
{
	struct obstack *obst = object;
	obstack_grow(obst, str, strlen(str));
}


/**
 * the file appender
 */
static const appender_t file_appender = {
	file_init,
	file_append_char,
	file_append_str
};

/**
 * the string buffer appender
 */
static const appender_t str_appender = {
	str_init,
	str_append_char,
	str_append_str
};

/**
 * the obstack appender.
 */
static const appender_t obst_appender = {
	obst_init,
	obst_append_char,
	obst_append_str
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

#if 0
static int is_std_fmt(const char *fmt)
{
	static const char *fmt_re_str =
		"^[0 -+#']?[1-9]*(\\.[1-9]*)?[hlLqjzt]?[diouxXeEfFgGaAc]";

	static regex_t fmt_re;
	static int preapred_re = 0;

	regmatch_t match[1];
	int res;

	if(!preapred_re) {
		int res = regcomp(&fmt_re, fmt_re_str, REG_EXTENDED);
		assert(res == 0 && "Could not prepare regex");
		preapred_re = 1;
	}

	res = regexec(&fmt_re, fmt, 1, &match[0], 0);

#if 0
	if(res != 0) {
		char buf[256];
		regerror(res, &fmt_re, buf, sizeof(buf));
		printf("%s ", buf);
	}

	printf("res: %d, start: %d, end: %d\n",
			res, match[0].rm_so, match[0].rm_eo);
#endif

	return res == 0 ? match[0].rm_eo : -1;
}
#endif

struct settings {
	char pad;
	int width;
	int left_just;
	int put_plus;
	int alternate;
};

/* Length specifiers. */
enum {
	len_char,
	len_short,
	len_int,
	len_long,
	len_long_long
};


#define MIN(x,y) ((x) < (y) ? (x) : (y))
#define MAX(x,y) ((x) > (y) ? (x) : (y))

static void dump_with_settings(const appender_t *app, void *object, size_t limit,
		const struct settings *settings, const char *str)
{
	if(settings->width >= 0) {
		int i;
		size_t n = strlen(str);
		int lim = MIN(settings->width, limit);
		int to_print = MIN(lim, n);
		int to_pad = to_print - lim;

		if(!settings->left_just)
			for(i = 0; i < to_pad; ++i)
				app->append_char(object, lim, settings->pad);

		app->append_str(object, to_print, str);

		if(!settings->left_just)
			for(i = 0; i < to_pad; ++i)
				app->append_char(object, lim, settings->pad);
	}

	else
		app->append_str(object, limit, str);
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
	const char *str;
	char buf[4096];
	int i, n;

#define DUMP_STR(s) app->append_str(object, limit, s)
#define DUMP_CH(ch) app->append_char(object, limit, ch)

	app->init(object, limit);

	for(i = 0, n = strlen(fmt); i < n; ++i) {
		char ch = fmt[i];

		if(ch == '%') {
			int len;
			const char *len_str = "";

			struct settings settings;

			settings.alternate = 0;
			settings.pad = ' ';
			settings.width = -1;
			settings.left_just = 0;
			settings.put_plus = 0;

			ch = fmt[++i];

			/* Clear the temporary buffer */
			buf[0] = '\0';

			/* Set the string to print to the buffer by default. */
			str = buf;

			while(strchr("#0-+", ch)) {
				switch(ch) {
					case '#':
						settings.alternate = 1;
						break;
					case '0':
						settings.pad = '0';
						break;
					case '-':
						settings.left_just = 1;
						break;
					case '+':
						settings.put_plus = 1;
						break;
				}

				ch = fmt[++i];
			}


			/* Read the field width */
			{
				char *endptr;
				int increase;

				settings.width = (int) strtol(&fmt[i], &endptr, 10);
				increase = (char *) endptr - &fmt[i];
				ch = fmt[i += increase];
				if(increase == 0)
					settings.width = -1;
			}

			/* Ignore the precision */
			if(ch == '.')
				while(isdigit(ch = fmt[++i]));

			/* read the length modifier. */
			switch(ch) {
				case 'h':
					len_str = "h";
					len = len_short;
					if((ch = fmt[++i]) == 'h') {
						len_str = "hh";
						len = len_char;
					}
					break;

				case 'l':
					len_str = "l";
					len = len_long;
					if((ch = fmt[++i]) == 'l') {
						len_str = "ll";
						len = len_long_long;
					}
					break;

				default:
					len = len_int;
			}

			/* Do the conversion specifier. */
			switch(ch) {

				/* The percent itself */
				case '%':
					buf[0] = '%';
					buf[1] = '\0';
					break;

				case 'c':
					buf[0] = va_arg(args, int);
					buf[1] = '\0';
					break;

				case 's':
					str = va_arg(args, const char *);
					break;

				case 'p':
					snprintf(buf, sizeof(buf), "%p", va_arg(args, void *));
					break;

				case 'd':
				case 'x':
				case 'X':
				case 'o':
					{
						char fmt_str[16];
						snprintf(fmt_str, sizeof(fmt_str), "%%%s%c", len_str, ch);

						switch(len) {
							case len_char:
							case len_short:
							case len_int:
								{
									int arg = va_arg(args, int);
									snprintf(buf, sizeof(buf), fmt_str, arg);
								}
								break;

							case len_long:
								{
									long arg = va_arg(args, long);
									snprintf(buf, sizeof(buf), fmt_str, arg);
								}
								break;

							case len_long_long:
								{
									long long arg = va_arg(args, long long);
									snprintf(buf, sizeof(buf), fmt_str, arg);
								}
								break;
						}
					}
					break;

				case 'I':
					str = get_id_str(va_arg(args, ident *));
					break;

				case 't':
					str = get_type_name(va_arg(args, type *));
					break;

				case 'e':
					str = get_entity_name(va_arg(args, entity *));
					break;

				case 'E':
					str = get_entity_ld_name(va_arg(args, entity *));
					break;

				case 'T':
					tarval_snprintf(buf, sizeof(buf), va_arg(args, tarval *));
					break;

				case 'n':
					{
						ir_node *irn = va_arg(args, ir_node *);
						snprintf(buf, sizeof(buf), "%s%s:%ld",
								get_irn_opname(irn), get_mode_name(get_irn_mode(irn)), get_irn_node_nr(irn));
					}
					break;

				case 'O':
					str = get_irn_opname(va_arg(args, ir_node *));
					break;

				case 'N':
					snprintf(buf, sizeof(buf), "%ld", get_irn_node_nr(va_arg(args, ir_node *)));
					break;

				case 'm':
					str = get_mode_name(va_arg(args, ir_mode *));
					break;

				case 'B':
					snprintf(buf, sizeof(buf), "%ld",
							get_irn_node_nr(get_nodes_block(va_arg(args, ir_node *))));
					break;

				case 'b':
					{
						const bitset_t *bs = va_arg(args, const bitset_t *);
						const char *prefix = "";
						unsigned long i;

						DUMP_CH('[');
						for(i = bitset_next_set(bs, 0); i != -1; i = bitset_next_set(bs, i + 1)) {
							snprintf(buf, sizeof(buf), "%ld", i);
							DUMP_STR(prefix);
							DUMP_STR(buf);
							prefix = ", ";
						}
						DUMP_CH(']');
						buf[0] = '\0';
					}
					break;

				case '*':
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

			dump_with_settings(app, object, limit, &settings, str);
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

/**
 * Convencience for string dumping.
 */
void ir_obst_printf(struct obstack *obst, const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	ir_common_vprintf(&obst_appender, obst, 0, fmt, args);
	va_end(args);
}

void ir_vprintf(const char *fmt, va_list args)
{
	ir_common_vprintf(&file_appender, stdout, 0, fmt, args);
}

void ir_vfprintf(FILE *f, const char *fmt, va_list args)
{
	ir_common_vprintf(&file_appender, f, 0, fmt, args);
}

void ir_vsnprintf(char *buf, size_t len, const char *fmt, va_list args)
{
	ir_common_vprintf(&str_appender, buf, len, fmt, args);
}

void ir_obst_vprintf(struct obstack *obst, const char *fmt, va_list args)
{
	ir_common_vprintf(&obst_appender, obst, 0, fmt, args);
}
