/*
 * This file is part of libFirm.
 * Copyright (C) 2012 IPD Goos, Universit"at Karlsruhe, Germany
 */

#ifndef _LC_OPTS_ENUM_T
#define _LC_OPTS_ENUM_T

#include "lc_opts.h"

#define _LC_OPT_DECL_ENUM(T, N)                 \
typedef struct {                                \
	const char *name;                             \
	T value;                                      \
} lc_opt_enum_ ## N ## _items_t;                \
                                                \
typedef struct {                                \
	T* value;                                     \
	const lc_opt_enum_ ## N ## _items_t *items;   \
} lc_opt_enum_ ## N ## _var_t;                  \
\
lc_opt_callback_t lc_opt_enum_ ## N ## _cb; \
lc_opt_dump_t lc_opt_enum_ ## N ## _dump; \
lc_opt_dump_vals_t lc_opt_enum_ ## N ## _dump_vals; \

#define _LC_OPT_ENT_ENUM(N, name, desc, var) \
	_LC_OPT_ENT(name, desc, lc_opt_type_enum, lc_opt_enum_ ## N ## _var_t, var, 0, lc_opt_enum_ ## N ## _cb, lc_opt_enum_ ## N ## _dump, lc_opt_enum_ ## N ## _dump_vals)

_LC_OPT_DECL_ENUM(int, int)
_LC_OPT_DECL_ENUM(unsigned, mask)

#define LC_OPT_ENT_ENUM_INT(name, desc, var)  _LC_OPT_ENT_ENUM(int, name, desc, var)
#define LC_OPT_ENT_ENUM_MASK(name, desc, var) _LC_OPT_ENT_ENUM(mask, name, desc, var)

typedef struct {
	const char *name;
	int (*value)(void);
} lc_opt_enum_func_ptr_items_t;

typedef struct {
	int (**value)(void);
	const lc_opt_enum_func_ptr_items_t *items;
} lc_opt_enum_func_ptr_var_t;

#define LC_OPT_ENT_ENUM_FUNC_PTR(name, desc, var)       _LC_OPT_ENT_ENUM(func_ptr, name, desc, var)

lc_opt_callback_t lc_opt_enum_func_ptr_cb;
lc_opt_dump_t lc_opt_enum_func_ptr_dump;
lc_opt_dump_vals_t lc_opt_enum_func_ptr_dump_vals;

#endif
