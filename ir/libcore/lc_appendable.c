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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "lc_common_t.h"
#include "lc_defines.h"
#include "lc_printf.h"

/* Default appendable implementations */

int lc_appendable_snwadd(lc_appendable_t *app, const char *str, size_t len,
		unsigned int width, int left_just, char pad)
{
	int res = 0;
	int i;
	int to_pad = width - len;

	/* If not left justified, pad left */
	for(i = 0; !left_just && i < to_pad; ++i)
		res += lc_appendable_chadd(app, pad);

	/* Send the visible portion of the string to the output. */
	res += lc_appendable_snadd(app, str, len);

	/* If left justified, pad right. */
	for(i = 0; left_just && i < to_pad; ++i)
		res += lc_appendable_chadd(app, pad);

	return res;
}


void lc_appendable_init(lc_appendable_t *env, const lc_appendable_funcs_t *app,
		void *obj, size_t limit)
{
	env->obj = obj;
	env->limit = limit;
	env->written = 0;
	env->app =app;

	app->init(env);
}

static void default_init(UNUSED(lc_appendable_t *env))
{
}

static void default_finish(UNUSED(lc_appendable_t *env))
{
}

/*
 * File appendable.
 */

static int file_snadd(lc_appendable_t *obj, const char *str, size_t n)
{
	obj->written += n;
	fwrite(str, sizeof(char), n, obj->obj);
	return n;
}

static int file_chadd(lc_appendable_t *obj, int ch)
{
	fputc(ch, obj->obj);
	obj->written++;
	return 1;
}

static lc_appendable_funcs_t app_file = {
	default_init,
	default_finish,
	file_snadd,
	file_chadd
};

const lc_appendable_funcs_t *lc_appendable_file = &app_file;


/*
 * String appendable.
 */

static void str_init(lc_appendable_t *obj)
{
	strncpy(obj->obj, "", obj->limit);
}

static int str_snadd(lc_appendable_t *obj, const char *str, size_t n)
{
	size_t to_write = LC_MIN(obj->limit - obj->written - 1, n);
	char *tgt = obj->obj;
	strncpy(tgt + obj->written, str, to_write);
	obj->written += to_write;
	return to_write;
}

static int str_chadd(lc_appendable_t *obj, int ch)
{
	if(obj->limit - obj->written > 1) {
		char *tgt = obj->obj;
		tgt[obj->written++] = (char) ch;
		return 1;
	}

	return 0;
}

static void str_finish(lc_appendable_t *obj)
{
	char *str = obj->obj;
	str[obj->written] = '\0';
}

static lc_appendable_funcs_t app_string = {
	str_init,
	str_finish,
	str_snadd,
	str_chadd
};

const lc_appendable_funcs_t *lc_appendable_string = &app_string;

/*
 * Obstack appendable
 */

static int obst_snadd(lc_appendable_t *obj, const char *str, size_t n)
{
	struct obstack *obst = obj->obj;
	obj->written += n;
	obstack_grow(obst, str, n);
	return n;
}

static int obst_chadd(lc_appendable_t *obj, int ch)
{
	struct obstack *obst = obj->obj;
	obstack_1grow(obst, (char) ch);
	obj->written++;
	return 1;
}

static lc_appendable_funcs_t app_obstack = {
	default_init,
	default_finish,
	obst_snadd,
	obst_chadd
};

const lc_appendable_funcs_t *lc_appendable_obstack = &app_obstack;
