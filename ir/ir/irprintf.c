/**
 * A little printf helper funterstanding firm types.
 * @author Sebastian Hack
 * @date 29.11.2004
 */

#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include "irmode.h"
#include "irnode.h"
#include "tv.h"
#include "irprintf.h"

/**
 * Something that can append strings and chars to somewhere.
 */
typedef struct _appender_t {
	void (*append_char)(void *subject, size_t n, char ch);
	void (*append_str)(void *subject, size_t n, const char *str);
} appender_t;

static void str_append_char(void *subject, size_t n, char ch)
{
	char buf[2];

	buf[0] = ch;
	buf[1] = 0;

	strncat(subject, buf, n);
}

static void str_append_str(void *subject, size_t n, const char *str)
{
	strncat(subject, str, n);
}

static void file_append_char(void *subject, size_t n, char ch)
{
	fputc(ch, subject);
}

static void file_append_str(void *subject, size_t n, const char *str)
{
	fputs(str, subject);
}

static const appender_t file_appender = {
	file_append_char,
	file_append_str
};

static const appender_t str_appender = {
	str_append_char,
	str_append_str
};


/**
 * A small printf helper routine for ir nodes.
 * @param app An appender (this determines where the stuff is dumped
 * to).
 * @param subject A target passed to the appender.
 * @param limit The maximum number of characters to dump.
 * @param fmt The format string.
 * @param args A va_list.
 */
static void ir_common_vprintf(const appender_t *app, void *subject,
		size_t limit, const char *fmt, va_list args)
{
	char buf[256];
	int i, n;

#define DUMP_STR(s) app->append_str(subject, limit, s)
#define DUMP_CH(ch) app->append_char(subject, limit, ch)

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

				case 'p':
					snprintf(buf, sizeof(buf), "%p", va_arg(args, void *));
					break;

				case 't':
					tarval_snprintf(buf, sizeof(buf), va_arg(args, tarval *));
					break;

				case 'N':
					{
						ir_node *irn = va_arg(args, ir_node *);
						snprintf(buf, sizeof(buf), "%s%s:%ld",
								get_irn_opname(irn), get_mode_name(get_irn_mode(irn)), get_irn_node_nr(irn));
					}
					break;

				case 'o':
					DUMP_STR(get_irn_opname(va_arg(args, ir_node *)));
					break;

				case 'n':
					snprintf(buf, sizeof(buf), "%ld", get_irn_node_nr(va_arg(args, ir_node *)));
					break;

				case 'm':
					DUMP_STR(get_mode_name(va_arg(args, ir_mode *)));
					break;

				case 'b':
					snprintf(buf, sizeof(buf), "%ld",
							get_irn_node_nr(get_nodes_block(va_arg(args, ir_node *))));
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
