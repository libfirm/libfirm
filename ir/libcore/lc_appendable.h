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
 * Something to which can be appended.
 * @author Sebastian Hack
 * @date 3.1.2005
 */

#ifndef _LIBCORE_APPENDABLE_H
#define _LIBCORE_APPENDABLE_H

#include <stddef.h>

struct _lc_appendable_funcs_t;

typedef struct _lc_appendable_t {
	void *obj;
	size_t written;
	size_t limit;
	const struct _lc_appendable_funcs_t *app;
} lc_appendable_t;

typedef struct _lc_appendable_funcs_t {
	void (*init)(lc_appendable_t *obj);
	void (*finish)(lc_appendable_t *obj);
	int (*snadd)(lc_appendable_t *obj, const char *str, size_t len);
	int (*chadd)(lc_appendable_t *obj, int ch);
} lc_appendable_funcs_t;

#define lc_appendable_snadd(obj,str,len) ((obj)->app->snadd(obj, str, len))
#define lc_appendable_chadd(obj,ch) ((obj)->app->chadd(obj, ch))
#define lc_appendable_finish(obj) ((obj)->app->finish(obj))

extern void lc_appendable_init(lc_appendable_t *app, const lc_appendable_funcs_t *funcs,
		void *obj, size_t limit);

extern int lc_appendable_snwadd(lc_appendable_t *app, const char *str,
		size_t len, unsigned int width, int left_just, char pad);

extern const lc_appendable_funcs_t *lc_appendable_file;
extern const lc_appendable_funcs_t *lc_appendable_string;
extern const lc_appendable_funcs_t *lc_appendable_obstack;

#endif
