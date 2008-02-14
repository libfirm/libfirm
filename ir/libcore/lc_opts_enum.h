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


  Option management library. Enum extensions.
*/

#ifndef _LC_OPTS_ENUM_T
#define _LC_OPTS_ENUM_T

#include <libcore/lc_opts.h>

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
int lc_opt_enum_ ## N ## _cb(const char *name, lc_opt_type_t type, void *data, size_t len, ...); \
int lc_opt_enum_ ## N ## _dump(char *buf, size_t n, const char *name, lc_opt_type_t type, void *data, size_t len); \
int lc_opt_enum_ ## N ## _dump_vals(char *buf, size_t n, const char *name, lc_opt_type_t type, void *data, size_t len); \

#define _LC_OPT_ENT_ENUM(N, name, desc, var) \
	_LC_OPT_ENT(name, desc, lc_opt_type_enum, var, 0, lc_opt_enum_ ## N ## _cb, lc_opt_enum_ ## N ## _dump, lc_opt_enum_ ## N ## _dump_vals)

_LC_OPT_DECL_ENUM(int, int)
_LC_OPT_DECL_ENUM(unsigned, mask)
_LC_OPT_DECL_ENUM(void *, ptr)
_LC_OPT_DECL_ENUM(const void *, const_ptr)

#define LC_OPT_ENT_ENUM_INT(name, desc, var)				_LC_OPT_ENT_ENUM(int, name, desc, var)
#define LC_OPT_ENT_ENUM_MASK(name, desc, var)				_LC_OPT_ENT_ENUM(mask, name, desc, var)
#define LC_OPT_ENT_ENUM_PTR(name, desc, var)				_LC_OPT_ENT_ENUM(ptr, name, desc, var)
#define LC_OPT_ENT_ENUM_CONST_PTR(name, desc, var)			_LC_OPT_ENT_ENUM(const_ptr, name, desc, var)

typedef struct {
	const char *name;
	int (*value)(void);
} lc_opt_enum_func_ptr_items_t;

typedef struct {
	int (**value)(void);
	const lc_opt_enum_func_ptr_items_t *items;
} lc_opt_enum_func_ptr_var_t;

#define LC_OPT_ENT_ENUM_FUNC_PTR(name, desc, var)       _LC_OPT_ENT_ENUM(func_ptr, name, desc, var)

int lc_opt_enum_func_ptr_cb(const char *name, lc_opt_type_t type, void *data, size_t len, ...);
int lc_opt_enum_func_ptr_dump(char *buf, size_t n, const char *name, lc_opt_type_t type, void *data, size_t len);
int lc_opt_enum_func_ptr_dump_vals(char *buf, size_t n, const char *name, lc_opt_type_t type, void *data, size_t len);

#endif
