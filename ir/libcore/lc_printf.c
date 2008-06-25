/*
  libcore: library for basic data structures and algorithms.
  Copyright (C) 2005  IPD Goos, Universit"at Karlsruhe, Germany

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/


/**
 * A customizable printf clone.
 * @author Sebastian Hack
 * @date 4.12.2005
 */

#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>

#include "lc_common_t.h"
#include "xmalloc.h"
#include "lc_printf.h"
#include "lc_defines.h"
#include "hashptr.h"
#include "set.h"

/* printf implementation */

typedef struct _lc_arg_t {
	struct _lc_arg_t *next;
	const char *name;
	char letter;
	int lc_arg_type;
	const lc_arg_handler_t *handler;
} lc_arg_t;

struct _lc_arg_env_t {
	set *args;			   		/**< Map for named arguments. */
	lc_arg_t *lower[26];		/**< Map for lower conversion specifiers. */
	lc_arg_t *upper[26];		/**< Map for upper conversion specifiers. */
};

/** The default argument environment. */
static lc_arg_env_t *default_env = NULL;

static INLINE lc_arg_env_t *_lc_arg_get_default_env(void)
{
	if(!default_env)
		default_env = lc_arg_add_std(lc_arg_new_env());

	return default_env;
}

lc_arg_env_t *lc_arg_get_default_env(void)
{
	return _lc_arg_get_default_env();
}

static int lc_arg_cmp(const void *p1, const void *p2, UNUSED(size_t size))
{
	const lc_arg_t *a1 = p1;
	const lc_arg_t *a2 = p2;
	return strcmp(a1->name, a2->name);
}


lc_arg_env_t *lc_arg_new_env(void)
{
	lc_arg_env_t *env = xmalloc(sizeof(*env));
	memset(env, 0, sizeof(*env));
	env->args = new_set(lc_arg_cmp, 16);
	return env;
}

void lc_arg_free_env(lc_arg_env_t *env)
{
	del_set(env->args);
	free(env);
}

int lc_arg_register(lc_arg_env_t *env, const char *name, char letter, const lc_arg_handler_t *handler)
{
	lc_arg_t arg;
	lc_arg_t *ent;
	int base = 0;
	lc_arg_t **map = NULL;

	arg.name = name;
	arg.letter = letter;
	arg.handler = handler;

	if(isupper(letter)) {
		map = env->upper;
		base = 'A';
	}
	else if(islower(letter)) {
		map = env->lower;
		base = 'a';
	}

	ent = set_insert(env->args, &arg, sizeof(arg), HASH_STR(name, strlen(name)));

	if(ent && base != 0)
		map[letter - base] = ent;

	return ent != NULL;
}

void lc_arg_unregister(UNUSED(lc_arg_env_t *env), UNUSED(const char *name))
{
}

int lc_arg_append(lc_appendable_t *app, const lc_arg_occ_t *occ, const char *str, size_t len)
{
	char pad = ' ';

	/* Set the padding to zero, if the zero is given and we are not left
	 * justified. (A minus overrides the zero). See printf(3). */
	if(!occ->flag_minus && occ->flag_zero)
		pad = '0';

	return lc_appendable_snwadd(app, str, len, LC_MAX(0, occ->width), occ->flag_minus, pad);
}


static int dispatch_snprintf(char *buf, size_t len, const char *fmt,
		int lc_arg_type, const lc_arg_value_t *val)
{
	int res = 0;

	switch(lc_arg_type) {
#define LC_ARG_TYPE(type,name) \
		case lc_arg_type_ ## name: res = snprintf(buf, len, fmt, val->v_ ## name); break;
#include "lc_printf_arg_types.def"
#undef LC_ARG_TYPE
	}

	return res;
}

static char *make_fmt(char *buf, size_t len, const lc_arg_occ_t *occ)
{
	char mod[64];
	char prec[16];
	char width[16];

	prec[0] = '\0';
	width[0] = '\0';

	if(occ->precision > 0)
		snprintf(prec, sizeof(prec), ".%d", occ->precision);

	if(occ->width > 0)
		snprintf(width, sizeof(width), "%d", occ->width);

	assert(occ->modifier && "modifier must not be NULL");
	strncpy(mod, occ->modifier, sizeof(mod) - 1);
	mod[LC_MIN(sizeof(mod) - 1, occ->modifier_length)] = '\0';

	snprintf(buf, len, "%%%s%s%s%s%s%s%s%s%c",
			occ->flag_space ? "#" : "",
			occ->flag_hash ? "#" : "",
			occ->flag_plus ? "+" : "",
			occ->flag_minus ? "-" : "",
			occ->flag_zero ? "0" : "",
			width, prec,
			mod, occ->conversion);

	return buf;
}

/**
 * Standard argument handler.
 */
static int std_get_lc_arg_type(const lc_arg_occ_t *occ)
{
	int modlen = occ->modifier_length;

	/* check, if the type can be derived from the modifier */
	if (modlen > 0) {
		const char *mod = occ->modifier;
		switch (mod[0]) {
			case 'l':
				return modlen > 1 && mod[1] == 'l' ? lc_arg_type_long_long : lc_arg_type_long;
#define TYPE_CASE(letter,type) case letter: return lc_arg_type_ ## type;
#if 0
			TYPE_CASE('j', intmax_t);
			TYPE_CASE('z', size_t);
			TYPE_CASE('t', ptrdiff_t);
#endif
			TYPE_CASE('L', long_double);
#undef TYPE_CASE
		}
	}

	/* The type is given by the conversion specifier and cannot be
	 * determined from the modifier. */
	switch (occ->conversion) {
		case 'e':
		case 'E':
		case 'f':
		case 'F':
		case 'g':
		case 'G':
			return lc_arg_type_double;
		case 's':
		case 'n':
		case 'p':
			return lc_arg_type_ptr;
		default:
			return lc_arg_type_int;
	}
}

static int std_emit(lc_appendable_t *app, const lc_arg_occ_t *occ, const lc_arg_value_t *val)
{
	char fmt[32];
	int res = 0;

	make_fmt(fmt, sizeof(fmt), occ);

	switch(occ->conversion) {

		/* Store the number of written characters in the given
		 * int pointer location */
		case 'n':
			{
				int *num = val->v_ptr;
				*num = app->written;
			}
			break;

		/* strings are dumped directly, since they can get really big. A
		 * buffer of 128 letters for all other types should be enough. */
		case 's':
			{
				const char *str = val->v_ptr;
				res = lc_arg_append(app, occ, str, strlen(str));
			}
			break;

		default:
			{
				int len = LC_MAX(128, occ->width + 1);
				char *buf = xmalloc(len * sizeof(char));
				res = dispatch_snprintf(buf, len, fmt, occ->lc_arg_type, val);
				res = lc_appendable_snadd(app, buf, res);
				xfree(buf);
			}
	}

	return res;
}

static const lc_arg_handler_t std_handler = {
	std_get_lc_arg_type,
	std_emit
};

lc_arg_env_t *lc_arg_add_std(lc_arg_env_t *env)
{
	lc_arg_register(env, "std:c", 'c', &std_handler);
	lc_arg_register(env, "std:i", 'i', &std_handler);
	lc_arg_register(env, "std:d", 'd', &std_handler);
	lc_arg_register(env, "std:o", 'o', &std_handler);
	lc_arg_register(env, "std:u", 'u', &std_handler);
	lc_arg_register(env, "std:x", 'x', &std_handler);
	lc_arg_register(env, "std:X", 'X', &std_handler);

	lc_arg_register(env, "std:e", 'e', &std_handler);
	lc_arg_register(env, "std:E", 'E', &std_handler);
	lc_arg_register(env, "std:f", 'f', &std_handler);
	lc_arg_register(env, "std:F", 'F', &std_handler);
	lc_arg_register(env, "std:g", 'g', &std_handler);
	lc_arg_register(env, "std:G", 'G', &std_handler);

	lc_arg_register(env, "std:s", 's', &std_handler);
	lc_arg_register(env, "std:p", 'p', &std_handler);
	lc_arg_register(env, "std:n", 'n', &std_handler);

	return env;
}

static char *read_int(const char *s, int *value)
{
	char *endptr;
	int res = (int) strtol(s, &endptr, 10);
	*value = endptr == s ? -1 : res;
	return endptr;
}

/* Generic printf() function. */

int lc_evpprintf(const lc_arg_env_t *env, lc_appendable_t *app, const char *fmt, va_list args)
{
	int res = 0;
	const char *s;
	const char *last = fmt + strlen(fmt);

	/* Find the first % */
	s = strchr(fmt, '%');

	/* Emit the text before the first % was found */
	lc_appendable_snadd(app, fmt, (s ? s : last) - fmt);

	while(s != NULL) {
		lc_arg_occ_t occ;
		lc_arg_value_t val;
		const lc_arg_t *arg = NULL;
		const char *old;
		char ch;

		/* We must be at a '%' */
		assert(*s == '%');

		/* Reset the occurrence structure */
		memset(&occ, 0, sizeof(occ));

		/* Eat all flags and set the corresponding flags in the occ struct */
		for(++s; strchr("#0-+", *s); ++s) {
			switch(*s) {
				case '#':
					occ.flag_hash = 1;
					break;
				case '0':
					occ.flag_zero = 1;
					break;
				case '-':
					occ.flag_minus = 1;
					break;
				case '+':
					occ.flag_plus = 1;
					break;
				case ' ':
					occ.flag_space = 1;
					break;
			}
		}

		/* Read the width if given */
		s = read_int(s, &occ.width);

		occ.precision = -1;

		/* read the precision if given */
		if(*s == '.') {
			int val;
			s = read_int(s + 1, &val);

			/* Negative or lacking precision after a '.' is treated as
			 * precision 0. */
			occ.precision = LC_MAX(0, val);
		}

		/*
		 * Now, we can either have:
		 * - a named argument like {node}
		 * - some modifiers followed by a conversion specifier
		 * - or some other character, which ends this format invalidly
		 */
		ch = *s;
		switch(ch) {
			case '%':
				s++;
				res += lc_appendable_chadd(app, '%');
				break;
			case '{':
				{
					const char *named = ++s;

					/* Read until the closing brace or end of the string. */
					for(ch = *s; ch != '}' && ch != '\0'; ch = *++s);

					if(s - named) {
						int n = s - named;
						char *name;
						lc_arg_t tmp;

						name = malloc(sizeof(char) * (n + 1));
						memcpy(name, named, sizeof(char) * n);
						name[n] = '\0';
						tmp.name = name;

						arg = set_find(env->args, &tmp, sizeof(tmp), HASH_STR(named, n));
						occ.modifier = "";
						occ.modifier_length = 0;

						/* Set the conversion specifier of the occurrence to the
						 * letter specified in the argument description. */
						if(arg)
							occ.conversion = arg->letter;

						free(name);

						/* If we ended with a closing brace, move the current
						 * pointer after it, since it is not to be dumped. */
						if(ch == '}')
							s++;
					}
				}
				break;

			default:
				{
					const char *mod = s;

					/* Read, as long there are letters */
					while(isalpha(ch) && !arg) {
						int base = 'a';
						lc_arg_t * const *map = env->lower;

						/* If uppercase, select the uppercase map from the environment */
						if(isupper(ch)) {
							base = 'A';
							map = env->upper;
						}

						if(map[ch - base] != NULL) {
							occ.modifier = mod;
							occ.modifier_length = s - mod;
							occ.conversion = ch;
							arg = map[ch - base];
						}

						ch = *++s;
					}
				}
		}

		/* Call the handler if an argument was determined */
		if(arg != NULL && arg->handler != NULL) {
			const lc_arg_handler_t *handler = arg->handler;

			/* Let the handler determine the type of the argument based on the
			 * information gathered. */
			occ.lc_arg_type = handler->get_lc_arg_type(&occ);

			/* Store the value according to argument information */
			switch(occ.lc_arg_type) {
#define LC_ARG_TYPE(type,name) case lc_arg_type_ ## name: val.v_ ## name = va_arg(args, type); break;
#include "lc_printf_arg_types.def"
#undef LC_ARG_TYPE
			}

			/* Finally, call the handler. */
			res += handler->emit(app, &occ, &val);
		}

		old = s;
		s = strchr(s, '%');
		res += lc_appendable_snadd(app, old, (s ? s : last) - old);
	}

	return res;
}

/* Convenience implementations */

int lc_epprintf(const lc_arg_env_t *env, lc_appendable_t *app, const char *fmt, ...)
{
	int res;
	va_list args;
	va_start(args, fmt);
	res = lc_evpprintf(env, app, fmt, args);
	va_end(args);
	return res;
}

int lc_pprintf(lc_appendable_t *app, const char *fmt, ...)
{
	int res;
	va_list args;
	va_start(args, fmt);
	res = lc_vpprintf(app, fmt, args);
	va_end(args);
	return res;
}

int lc_vpprintf(lc_appendable_t *app, const char *fmt, va_list args)
{
	return lc_evpprintf(_lc_arg_get_default_env(), app, fmt, args);
}

int lc_eprintf(const lc_arg_env_t *env, const char *fmt, ...)
{
	int res;
	va_list args;
	va_start(args, fmt);
	res = lc_efprintf(env, stdout, fmt, args);
	va_end(args);
	return res;
}

int lc_esnprintf(const lc_arg_env_t *env, char *buf, size_t len, const char *fmt, ...)
{
	int res;
	va_list args;
	va_start(args, fmt);
	res = lc_evsnprintf(env, buf, len, fmt, args);
	va_end(args);
	return res;
}

int lc_efprintf(const lc_arg_env_t *env, FILE *file, const char *fmt, ...)
{
	int res;
	va_list args;
	va_start(args, fmt);
	res = lc_evfprintf(env, file, fmt, args);
	va_end(args);
	return res;
}

int lc_eoprintf(const lc_arg_env_t *env, struct obstack *obst, const char *fmt, ...)
{
	int res;
	va_list args;
	va_start(args, fmt);
	res = lc_evoprintf(env, obst, fmt, args);
	va_end(args);
	return res;
}

int lc_evprintf(const lc_arg_env_t *env, const char *fmt, va_list args)
{
	return lc_evfprintf(env, stdout, fmt, args);
}

int lc_evsnprintf(const lc_arg_env_t *env, char *buf, size_t len, const char *fmt, va_list args)
{
	int res;
	lc_appendable_t app;

	lc_appendable_init(&app, lc_appendable_string, buf, len);
	res = lc_evpprintf(env, &app, fmt, args);
	lc_appendable_finish(&app);
	return res;
}

int lc_evfprintf(const lc_arg_env_t *env, FILE *f, const char *fmt, va_list args)
{
	int res;
	lc_appendable_t app;

	lc_appendable_init(&app, lc_appendable_file, f, 0);
	res = lc_evpprintf(env, &app, fmt, args);
	lc_appendable_finish(&app);
	return res;
}

int lc_evoprintf(const lc_arg_env_t *env, struct obstack *obst, const char *fmt, va_list args)
{
	int res;
	lc_appendable_t app;

	lc_appendable_init(&app, lc_appendable_obstack, obst, 0);
	res = lc_evpprintf(env, &app, fmt, args);
	lc_appendable_finish(&app);
	return res;
}


int lc_printf(const char *fmt, ...)
{
	int res;
	va_list args;
	va_start(args, fmt);
	res = lc_vprintf(fmt, args);
	va_end(args);
	return res;
}

int lc_snprintf(char *buf, size_t len, const char *fmt, ...)
{
	int res;
	va_list args;
	va_start(args, fmt);
	res = lc_vsnprintf(buf, len, fmt, args);
	va_end(args);
	return res;
}

int lc_fprintf(FILE *f, const char *fmt, ...)
{
	int res;
	va_list args;
	va_start(args, fmt);
	res = lc_vfprintf(f, fmt, args);
	va_end(args);
	return res;
}

int lc_oprintf(struct obstack *obst, const char *fmt, ...)
{
	int res;
	va_list args;
	va_start(args, fmt);
	res = lc_voprintf(obst, fmt, args);
	va_end(args);
	return res;
}


int lc_vprintf(const char *fmt, va_list args)
{
	return lc_evprintf(_lc_arg_get_default_env(), fmt, args);
}

int lc_vsnprintf(char *buf, size_t len, const char *fmt, va_list args)
{
	return lc_evsnprintf(_lc_arg_get_default_env(), buf, len, fmt, args);
}

int lc_vfprintf(FILE *f, const char *fmt, va_list args)
{
	return lc_evfprintf(_lc_arg_get_default_env(), f, fmt, args);
}

int lc_voprintf(struct obstack *obst, const char *fmt, va_list args)
{
	return lc_evoprintf(_lc_arg_get_default_env(), obst, fmt, args);
}
