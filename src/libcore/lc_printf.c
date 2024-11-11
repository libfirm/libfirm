/*
 * This file is part of libFirm.
 * Copyright (C) 2012 IPD Goos, Universit"at Karlsruhe, Germany
 */


/**
 * A customizable printf clone.
 * @author Sebastian Hack
 * @date 4.12.2005
 */
#include "lc_printf.h"

#include "hashptr.h"
#include "set.h"
#include "util.h"
#include "xmalloc.h"
#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* printf implementation */

typedef struct lc_arg_t {
	struct lc_arg_t        *next;
	const char             *name;
	char                    letter;
	int                     lc_arg_type;
	const lc_arg_handler_t *handler;
} lc_arg_t;

struct lc_arg_env_t {
	set      *args;             /**< Map for named arguments. */
	lc_arg_t *lower[26];        /**< Map for lower conversion specifiers. */
	lc_arg_t *upper[26];        /**< Map for upper conversion specifiers. */
};

static int lc_arg_cmp(const void *p1, const void *p2, size_t size)
{
	const lc_arg_t *a1 = (const lc_arg_t*)p1;
	const lc_arg_t *a2 = (const lc_arg_t*)p2;
	(void) size;
	return strcmp(a1->name, a2->name);
}

lc_arg_env_t *lc_arg_new_env(void)
{
	lc_arg_env_t *env = XMALLOCZ(lc_arg_env_t);
	env->args = new_set(lc_arg_cmp, 16);
	return env;
}

void lc_arg_free_env(lc_arg_env_t *env)
{
	del_set(env->args);
	free(env);
}

int lc_arg_register(lc_arg_env_t *env, const char *name, char letter,
                    const lc_arg_handler_t *handler)
{
	lc_arg_t arg;
	arg.name = name;
	arg.letter = letter;
	arg.handler = handler;

	lc_arg_t **map  = NULL;
	int        base = 0;
	if (isupper((unsigned char)letter)) {
		map = env->upper;
		base = 'A';
	} else if (islower((unsigned char)letter)) {
		map = env->lower;
		base = 'a';
	}

	lc_arg_t *ent = set_insert(lc_arg_t, env->args, &arg, sizeof(arg), hash_str(name));

	if (ent && base != 0)
		map[letter - base] = ent;

	return ent != NULL;
}

void lc_arg_unregister(lc_arg_env_t *env, const char *name)
{
	(void) env;
	(void) name;
}

int lc_arg_append(lc_appendable_t *app, const lc_arg_occ_t *occ, const char *str, size_t len)
{
	char pad = ' ';

	/* Set the padding to zero, if the zero is given and we are not left
	 * justified. (A minus overrides the zero). See printf(3). */
	if (!occ->flag_minus && occ->flag_zero)
		pad = '0';

	return lc_appendable_snwadd(app, str, len, MAX(0, occ->width), occ->flag_minus, pad);
}


static int dispatch_snprintf(char *buf, size_t len, const char *fmt,
                             int lc_arg_type, const lc_arg_value_t *val)
{
	int res = 0;

	switch (lc_arg_type) {
#define LC_ARG_TYPE(type,name,va_type) \
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

	if (occ->precision > 0)
		snprintf(prec, sizeof(prec), ".%d", occ->precision);

	if (occ->width > 0)
		snprintf(width, sizeof(width), "%d", occ->width);

	assert(occ->modifier && "modifier must not be NULL");
	strncpy(mod, occ->modifier, sizeof(mod) - 1);
	mod[MIN(sizeof(mod) - 1, occ->modifier_length)] = '\0';

#ifdef _MSC_VER
	/* work-around for buggy mscrt not supporting z, j,  and t modifier */
	if (occ->modifier_length == 1) {
		if (mod[0] == 'z') {
			if (sizeof(size_t) == sizeof(int))
				mod[0] = '\0';
			if (sizeof(size_t) == sizeof(__int64)) {
				mod[0] = 'I';
				mod[1] = '6';
				mod[2] = '4';
				mod[3] = '\0';
			}
		} else if (mod[0] == 't') {
			if (sizeof(ptrdiff_t) == sizeof(int))
				mod[0] = '\0';
			if (sizeof(ptrdiff_t) == sizeof(__int64)) {
				mod[0] = 'I';
				mod[1] = '6';
				mod[2] = '4';
				mod[3] = '\0';
			}
		} else if (mod[0] == 'j') {
			if (sizeof(intmax_t) == sizeof(int))
				mod[0] = '\0';
			if (sizeof(intmax_t) == sizeof(__int64)) {
				mod[0] = 'I';
				mod[1] = '6';
				mod[2] = '4';
				mod[3] = '\0';
			}
		}
	} else if (occ->modifier_length == 2) {
		if (mod[0] == 'h' && mod[1] == 'h') {
			/* no support for char in mscrt, but we can safely ignore it
			 * because the size is handled by the argument reader code */
			mod[0] = '\0';
		}
	}
#endif
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
	size_t modlen = occ->modifier_length;

	/* check, if the type can be derived from the modifier */
	if (modlen > 0) {
		const char *mod = occ->modifier;
		switch (mod[0]) {
			case 'h':
				return modlen > 1 && mod[1] == 'h' ? lc_arg_type_char : lc_arg_type_short;
			case 'l':
				return modlen > 1 && mod[1] == 'l' ? lc_arg_type_long_long : lc_arg_type_long;
#define TYPE_CASE(letter,type) case letter: return lc_arg_type_ ## type
			TYPE_CASE('j', intmax_t);
			TYPE_CASE('z', size_t);
			TYPE_CASE('t', ptrdiff_t);
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

	switch (occ->conversion) {
		/* Store the number of written characters in the given
		 * int pointer location */
		case 'n': {
			int *num = (int*)val->v_ptr;
			*num = (int)app->written;
			break;
		}

		/* strings are dumped directly, since they can get really big. A
		 * buffer of 512 letters for all other types should be enough. */
		case 's': {
			const char *str = (const char*)val->v_ptr;
			size_t size = strlen(str);
			lc_arg_append(app, occ, str, size);
			res = size;
			break;
		}

		default: {
			int len = MAX(512, occ->width + 1);
			char *buf = XMALLOCN(char, len);
			res = dispatch_snprintf(buf, len, fmt, occ->lc_arg_type, val);
			assert(res < len);
			lc_appendable_snadd(app, buf, res);
			free(buf);
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

int lc_evpprintf(const lc_arg_env_t *env, lc_appendable_t *app, const char *fmt,
                 va_list args)
{
	(void)args;

	int         res  = 0;
	const char *last = fmt + strlen(fmt);

	/* Find the first % */
	const char *s = strchr(fmt, '%');

	/* Emit the text before the first % was found */
	size_t addlen = (s ? s : last) - fmt;
	lc_appendable_snadd(app, fmt, addlen);
	res += addlen;

	while (s != NULL) {
		lc_arg_value_t val;

		/* We must be at a '%' */
		assert(*s == '%');

		/* Reset the occurrence structure */
		lc_arg_occ_t occ;
		memset(&occ, 0, sizeof(occ));

		/* Eat all flags and set the corresponding flags in the occ struct */
		for (++s; strchr("#0-+", *s); ++s) {
			switch (*s) {
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
		if (*s == '.') {
			int precision;
			s = read_int(s + 1, &precision);

			/* Negative or lacking precision after a '.' is treated as
			 * precision 0. */
			occ.precision = MAX(0, precision);
		}

		/*
		 * Now, we can either have:
		 * - a named argument like {node}
		 * - some modifiers followed by a conversion specifier
		 * - or some other character, which ends this format invalidly
		 */
		char            ch  = *s;
		const lc_arg_t *arg = NULL;
		switch (ch) {
			case '%':
				s++;
				lc_appendable_chadd(app, '%');
				++res;
				break;
			case '{': {
				const char *named = ++s;

				/* Read until the closing brace or end of the string. */
				for (ch = *s; ch != '}' && ch != '\0'; ch = *++s) {
				}

				if (s - named) {
					size_t n = s - named;
					char *name;
					lc_arg_t tmp;

					name = (char*) malloc(sizeof(char) * (n + 1));
					MEMCPY(name, named, n);
					name[n] = '\0';
					tmp.name = name;

					arg = set_find(lc_arg_t, env->args, &tmp, sizeof(tmp), hash_str(named));
					occ.modifier = "";
					occ.modifier_length = 0;

					/* Set the conversion specifier of the occurrence to the
					 * letter specified in the argument description. */
					if (arg)
						occ.conversion = arg->letter;

					free(name);

					/* If we ended with a closing brace, move the current
					 * pointer after it, since it is not to be dumped. */
					if (ch == '}')
						s++;
				}
				break;
			}

			default: {
				const char *mod = s;

				/* Read, as long there are letters */
				while (isalpha((unsigned char)ch) && !arg) {
					int              base = 'a';
					lc_arg_t *const *map  = env->lower;

					/* If uppercase, select the uppercase map from the
					 * environment */
					if (isupper((unsigned char)ch)) {
						base = 'A';
						map = env->upper;
					}

					if (map[ch - base] != NULL) {
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
		if (arg != NULL && arg->handler != NULL) {
			const lc_arg_handler_t *handler = arg->handler;

			/* Let the handler determine the type of the argument based on the
			 * information gathered. */
			occ.lc_arg_type = handler->get_lc_arg_type(&occ);

			/* Store the value according to argument information */
			switch (occ.lc_arg_type) {
#define LC_ARG_TYPE(type,name,va_type) \
			case lc_arg_type_ ## name: val.v_ ## name = va_arg(args, va_type); break;
#include "lc_printf_arg_types.def"
#undef LC_ARG_TYPE
			}

			/* Finally, call the handler. */
			res += handler->emit(app, &occ, &val);
		}

		const char *old = s;
		s = strchr(s, '%');
		size_t addlen = (s ? s : last) - old;
		lc_appendable_snadd(app, old, addlen);
		res += addlen;
	}

	return res;
}

/* Convenience implementations */

int lc_eoprintf(const lc_arg_env_t *env, struct obstack *obst, const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	int res = lc_evoprintf(env, obst, fmt, args);
	va_end(args);
	return res;
}

int lc_evprintf(const lc_arg_env_t *env, const char *fmt, va_list args)
{
	return lc_evfprintf(env, stdout, fmt, args);
}

int lc_evsnprintf(const lc_arg_env_t *env, char *buf, size_t len, const char *fmt, va_list args)
{
	lc_appendable_t app;

	lc_appendable_init(&app, lc_appendable_string, buf, len);
	int res = lc_evpprintf(env, &app, fmt, args);
	lc_appendable_finish(&app);
	return res;
}

int lc_evfprintf(const lc_arg_env_t *env, FILE *f, const char *fmt, va_list args)
{
	lc_appendable_t app;

	lc_appendable_init(&app, lc_appendable_file, f, 0);
	int res = lc_evpprintf(env, &app, fmt, args);
	lc_appendable_finish(&app);
	return res;
}

int lc_evoprintf(const lc_arg_env_t *env, struct obstack *obst, const char *fmt, va_list args)
{
	lc_appendable_t app;

	lc_appendable_init(&app, lc_appendable_obstack, obst, 0);
	int res = lc_evpprintf(env, &app, fmt, args);
	lc_appendable_finish(&app);
	return res;
}
