/*
 * This file is part of libFirm.
 * Copyright (C) 2012 IPD Goos, Universit"at Karlsruhe, Germany
 */

/**
 * Flexible printf().
 * @author Sebastian Hack
 * @date 3.1.2005
 */
#ifndef _LIBCORE_LC_PRINTF_H
#define _LIBCORE_LC_PRINTF_H

#include <stdarg.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

#include <obstack.h>

#include "lc_appendable.h"

typedef struct lc_arg_occ_t {
	int width;                 /**< The width, or 0 if not given. */
	int precision;             /**< The precision, or 0 if not given */

	const char *modifier;      /**< A string of of modifiers preceding the
	                                conversion specifier. Attention: This string
	                                is not zero terminated. Use
	                                @c modifier_length to get the number of
	                                valid chars in it. */
	size_t modifier_length;    /**< The number of valid chars in @c modifier. */
	char conversion;           /**< The conversion specifier. */
	int lc_arg_type;           /**< The type of the argument as determined by
	                                the @c get_lc_arg_type member function of
	                                the handler. */

	bool flag_hash  : 1;   /**< @c # flag was seen. */
	bool flag_zero  : 1;   /**< @c 0 flag was seen. */
	bool flag_minus : 1;   /**< @c - flag was seen. */
	bool flag_plus  : 1;   /**< @c + flag was seen. */
	bool flag_space : 1;   /**< A space flag was seen. */
} lc_arg_occ_t;

/**
 * A value from the ... arguments of the printf function.
 * Look at the file 'xprintf_lc_arg_types.def'. The second argument of the
 * @c ARG_TYPE macro is the name of the union member preceded by $c v_
 */
typedef union {
#define LC_ARG_TYPE(type,name,va_type) type v_ ## name;
#include "lc_printf_arg_types.def"
#undef LC_ARG_TYPE
} lc_arg_value_t;

enum {
#define LC_ARG_TYPE(type,name,va_type) lc_arg_type_ ## name,
#include "lc_printf_arg_types.def"
#undef LC_ARG_TYPE
  lc_arg_type_last
};

typedef struct lc_arg_handler {
	int (*get_lc_arg_type)(const lc_arg_occ_t *occ);
	int (*emit)(lc_appendable_t *app, const lc_arg_occ_t *occ, const lc_arg_value_t *arg);
} lc_arg_handler_t;

typedef struct lc_arg_env_t lc_arg_env_t;

lc_arg_env_t *lc_arg_new_env(void);
void lc_arg_free_env(lc_arg_env_t *env);

int lc_arg_register(lc_arg_env_t *env, const char *name, char letter, const lc_arg_handler_t *handler);
void lc_arg_unregister(lc_arg_env_t *env, const char *name);

lc_arg_env_t *lc_arg_add_std(lc_arg_env_t *env);

int lc_arg_append(lc_appendable_t *app, const lc_arg_occ_t *occ, const char *str, size_t len);

int lc_evpprintf(const lc_arg_env_t *env, lc_appendable_t *app, const char *fmt, va_list args);

int lc_eoprintf(const lc_arg_env_t *env, struct obstack *obst, const char *fmt, ...);

int lc_evprintf(const lc_arg_env_t *env, const char *fmt, va_list args);
int lc_evsnprintf(const lc_arg_env_t *env, char *buf, size_t len, const char *fmt, va_list args);
int lc_evfprintf(const lc_arg_env_t *env, FILE *f, const char *fmt, va_list args);
int lc_evoprintf(const lc_arg_env_t *env, struct obstack *obst, const char *fmt, va_list args);

#endif
