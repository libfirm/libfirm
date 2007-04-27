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
 * @brief   A little printf helper unterstanding firm types
 * @author  Sebastian Hack
 * @date    29.11.2004
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include <ctype.h>

#include "firm_config.h"
#include "ident.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "entity_t.h"
#include "type_t.h"
#include "tv_t.h"
#include "irprintf.h"
#include "obst.h"
#include "pset.h"
#include "iterator.h"
#include "bitset.h"
#include "dbginfo_t.h"

#define STRNIL "(nil)"

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

#ifndef WITH_LIBCORE

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
  char flag_zero;
  int width;
  int flag_minus;
  int flag_plus;
  int flag_hash;
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
  if (settings->width >= 0) {
    int i;
    size_t n = strlen(str);
    int lim = MIN(settings->width, (int)limit);
    int to_print = MIN(lim, (int)n);
    int to_pad = to_print - lim;

    if (!settings->flag_minus)
      for(i = 0; i < to_pad; ++i)
	app->append_char(object, lim, settings->flag_zero);

    app->append_str(object, to_print, str);

    if (!settings->flag_minus)
      for(i = 0; i < to_pad; ++i)
	app->append_char(object, lim, settings->flag_zero);
  }

  else
    app->append_str(object, limit, str);
}

/**
 * Beware: do not set the entity ld_name
 */
static const char *get_entity_ld_name_ex(entity *ent) {
  if (ent->ld_name)
    return get_entity_ld_name(ent);
  return get_entity_name(ent);
}

/**
 * emit a Firm object. Backported from irargs.
 */
static void firm_emit(char *buf, int buflen, char conversion,
    const struct settings *occ, void *X)
{
#define A(s)    occ->flag_hash ? s " ": ""

  firm_kind *obj = X;
  int i, n;
  ir_node *block;
  char add[64];
  char tv_buf[256];
  entity *ent;

  buf[0] = '\0';
  add[0] = '\0';

  if (! X)
    strncpy(buf, "(null)", buflen);
  else {
    switch (*obj) {
    case k_BAD:
      snprintf(buf, buflen, "BAD");
      snprintf(add, sizeof(add), "[%p]", X);
      break;
    case k_entity:
      snprintf(buf, buflen, "%s%s", A("ent"),
          isupper(conversion) ? get_entity_ld_name_ex(X): get_entity_name(X));
      snprintf(add, sizeof(add), "[%ld]", get_entity_nr(X));
      break;
    case k_type:
      snprintf(buf, buflen, "%s%s:%s", A("type"), get_type_tpop_name(X), get_type_name(X));
      snprintf(add, sizeof(add), "[%ld]", get_type_nr(X));
      break;
    case k_ir_graph:
      if (X == get_const_code_irg())
        snprintf(buf, buflen, "const_code_irg ");
      else
        snprintf(buf, buflen, "%s%s", A("irg"), get_entity_name(get_irg_entity(X)));
      snprintf(add, sizeof(add), "[%ld]", get_irg_graph_nr(X));
      break;
    case k_ir_node:
      switch (conversion) {
      case 'B':
        block = is_no_Block(X) ? get_nodes_block(X) : X;
        snprintf(buf, buflen, "%s%s%s", A("irn"), get_irn_opname(block),
            get_mode_name(get_irn_mode(block)));
        snprintf(add, sizeof(add), "[%ld]", get_irn_node_nr(block));
        break;
      case 'N':
        snprintf(buf, buflen, "%ld", get_irn_node_nr(X));
        break;
      default:
        if (is_Const(X)) {
          tarval *tv = get_Const_tarval(X);

          if (tv)
            tarval_snprintf(tv_buf, sizeof(tv_buf), tv);
          snprintf(buf, buflen, "%s%s%s<%s>", A("irn"), get_irn_opname(X),
            get_mode_name(get_irn_mode(X)), tv ? tv_buf : ">NULL<");
        }
        else if (get_irn_op(X) == op_SymConst) {
          switch (get_SymConst_kind(X)) {
          case symconst_type_tag:    /* type tag */
            snprintf(tv_buf, sizeof(tv_buf), "<ID:%s>", get_type_name(get_SymConst_type(X)));
            break;
          case symconst_type_size:   /* type size */
            snprintf(tv_buf, sizeof(tv_buf), "<SIZE:%s>", get_type_name(get_SymConst_type(X)));
            break;
          case symconst_type_align:  /* type alignment */
            snprintf(tv_buf, sizeof(tv_buf), "<ALIGN:%s>", get_type_name(get_SymConst_type(X)));
            break;
          case symconst_addr_name:   /* linker name */
            snprintf(tv_buf, sizeof(tv_buf), "<EXT:%s>", get_id_str(get_SymConst_name(X)));
            break;
          case symconst_addr_ent:    /* entity name */
            snprintf(tv_buf, sizeof(tv_buf), "<%s>", get_entity_name(get_SymConst_entity(X)));
            break;
          case symconst_enum_const:  /* enumeration constant */
            snprintf(tv_buf, sizeof(tv_buf), "<ENUM:%s>", get_enumeration_name(get_SymConst_enum(X)));
            break;
          default:
            tv_buf[0] = '\0';
          }
          snprintf(buf, buflen, "%s%s%s%s", A("irn"), get_irn_opname(X),
            get_mode_name(get_irn_mode(X)), tv_buf);
        }
        else
          snprintf(buf, buflen, "%s%s%s", A("irn"), get_irn_opname(X),
            get_mode_name(get_irn_mode(X)));
        snprintf(add, sizeof(add), "[%ld:%d]", get_irn_node_nr(X), get_irn_idx(X));
      }
      break;
    case k_ir_mode:
      snprintf(buf, buflen, "%s%s", A("mode"), get_mode_name(X));
      break;
    case k_tarval:
      tarval_snprintf(tv_buf, sizeof(tv_buf), X);
      snprintf(buf, buflen, "%s%s", A("tv"), tv_buf);
      break;
    case k_ir_loop:
      snprintf(buf, sizeof(buf), "loop[%d:%d]", get_loop_loop_nr(X), get_loop_depth(X));
      break;
    case k_ir_op:
      snprintf(buf, buflen, "%s%s", A("op"), get_op_name(X));
      break;
    case k_ir_compound_graph_path:
      n = get_compound_graph_path_length(X);

      for (i = 0; i < n; ++i) {
        ent = get_compound_graph_path_node(X, i);

        strncat(buf, ".", buflen);
        strncat(buf, get_entity_name(ent), buflen);
        if (is_Array_type(get_entity_owner(ent))) {
          snprintf(add, sizeof(add), "[%d]",
            get_compound_graph_path_array_index(X, i));
          strncat(buf, add, buflen);
        }
      }
      add[0] = '\0';
      break;

    default:
      snprintf(buf, buflen, "UNKWN");
      snprintf(add, sizeof(add), "[%p]", X);
    }
  }

  if (occ->flag_plus)
  	strncat(buf, add, buflen);

#undef A
}

/**
 * A small printf helper routine for ir nodes.
 * @param app An appender (this determines where the stuff is dumped to).
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

	for (i = 0, n = strlen(fmt); i < n; ++i) {
		char ch = fmt[i];

		if (ch == '%') {
			int len;
			const char *len_str = "";

			struct settings settings;

			settings.flag_hash = 0;
			settings.flag_zero = ' ';
			settings.width = -1;
			settings.flag_minus = 0;
			settings.flag_plus  = 0;

			ch = fmt[++i];

			/* Clear the temporary buffer */
			buf[0] = '\0';

			/* Set the string to print to the buffer by default. */
			str = buf;

			while (strchr("#0-+", ch)) {
				switch(ch) {
					case '#':
						settings.flag_hash = 1;
						break;
					case '0':
						settings.flag_zero = '0';
						break;
					case '-':
						settings.flag_minus = 1;
						break;
					case '+':
						settings.flag_plus = 1;
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
			if (ch == '.')
				while(isdigit(ch = fmt[++i]));

			/* read the length modifier. */
			switch(ch) {
				case 'h':
					len_str = "h";
					len = len_short;
					++i;
					if((ch = fmt[i]) == 'h') {
						len_str = "hh";
						len = len_char;
						++i;
					}
					break;

				case 'l':
					len_str = "l";
					len = len_long;
					++i;
					if ((ch = fmt[i]) == 'l') {
						len_str = "ll";
						len = len_long_long;
						++i;
					}
					else if ((ch = fmt[i]) == 'u') {
						len_str = "lu";
						len = len_long_long;
						++i;
					}
					break;

				default:
					len = len_int;
			}

			/* Do the conversion specifier. */
			switch (ch) {

				/* The percent itself */
				case '%':
					buf[0] = '%';
					buf[1] = '\0';
					break;

				/* Indent */
				case '>':
					{
						int i, n = va_arg(args, int);
						for(i = 0; i < n && i < sizeof(buf) - 1; ++i)
							buf[i] = ' ';

						buf[i] = '\0';
					}
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

				case 'i':
				case 'd':
				case 'u':
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
									int64_t arg = va_arg(args, int64_t);
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
        case 'e':
        case 'E':
        case 'T':
        case 'n':
        case 'O':
        case 'm':
        case 'B':
        case 'P':
        case 'F':
        case 'f':
          firm_emit(buf, sizeof(buf), ch, &settings, va_arg(args, void *));
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

				case '=':
					str = get_pnc_string(va_arg(args, int));
					break;
				case 'G':
					{
						ir_node *irn = va_arg(args, ir_node *);
						dbg_info *dbg = get_irn_dbg_info(irn);
						buf[0] = '\0';
						if (dbg && __dbg_info_snprint) {
							if (__dbg_info_snprint(buf, sizeof(buf), dbg) <= 0)
								buf[0] = '\0';
						}
						break;
					}
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
 * Convenience for stdout dumping.
 */
void ir_printf(const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	ir_common_vprintf(&file_appender, stdout, 0, fmt, args);
	va_end(args);
}

/**
 * Convenience for file dumping.
 */
void ir_fprintf(FILE *f, const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	ir_common_vprintf(&file_appender, f, 0, fmt, args);
	va_end(args);
}

/**
 * Convenience for string dumping.
 */
void ir_snprintf(char *buf, size_t len, const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	ir_common_vprintf(&str_appender, buf, len, fmt, args);
	va_end(args);
}

/**
 * Convenience for string dumping.
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

#else /* WITH_LIBCORE */

#include "irargs_t.h"

void ir_printf(const char *fmt, ...)
{
	va_list args;

	va_start(args, fmt);
	lc_evprintf(firm_get_arg_env(), fmt, args);
	va_end(args);
}

void ir_fprintf(FILE *f, const char *fmt, ...)
{
	va_list args;

	va_start(args, fmt);
	lc_evfprintf(firm_get_arg_env(), f, fmt, args);
	va_end(args);
}

void ir_snprintf(char *buf, size_t n, const char *fmt, ...)
{
	va_list args;

	va_start(args, fmt);
	lc_evsnprintf(firm_get_arg_env(), buf, n, fmt, args);
	va_end(args);
}

void ir_vprintf(const char *fmt, va_list args)
{
	lc_evprintf(firm_get_arg_env(), fmt, args);
}

void ir_vfprintf(FILE *f, const char *fmt, va_list args)
{
	lc_evfprintf(firm_get_arg_env(), f, fmt, args);
}

void ir_vsnprintf(char *buf, size_t len, const char *fmt, va_list args)
{
	lc_evsnprintf(firm_get_arg_env(), buf, len, fmt, args);
}

void ir_obst_vprintf(struct obstack *obst, const char *fmt, va_list args)
{
	lc_evoprintf(firm_get_arg_env(), obst, fmt, args);
}

#endif
