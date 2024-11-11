/*
 * This file is part of libFirm.
 * Copyright (C) 2012 IPD Goos, Universit"at Karlsruhe, Germany
 */
#include "lc_printf.h"
#include "util.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Default appendable implementations */

int lc_appendable_snwadd(lc_appendable_t *app, const char *str, size_t len,
                         unsigned int width, int left_just, char pad)
{
	int res = 0;
	size_t i;
	size_t to_pad = width > len ? width - len : 0;

	/* If not left justified, pad left */
	for (i = 0; !left_just && i < to_pad; ++i)
		res += lc_appendable_chadd(app, pad);

	/* Send the visible portion of the string to the output. */
	res += lc_appendable_snadd(app, str, len);

	/* If left justified, pad right. */
	for (i = 0; left_just && i < to_pad; ++i)
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

static void default_init(lc_appendable_t *env)
{
	(void) env;
}

static void default_finish(lc_appendable_t *env)
{
	(void) env;
}

/*
 * File appendable.
 */

static int file_snadd(lc_appendable_t *obj, const char *str, size_t n)
{
	obj->written += n;
	fwrite(str, sizeof(char), n, (FILE*)obj->obj);
	return n;
}

static int file_chadd(lc_appendable_t *obj, int ch)
{
	fputc(ch, (FILE*)obj->obj);
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
	strncpy((char*)obj->obj, "", obj->limit);
}

static int str_snadd(lc_appendable_t *obj, const char *str, size_t n)
{
	size_t to_write = MIN(obj->limit - obj->written - 1, n);
	char *tgt = (char*)obj->obj;
	strncpy(tgt + obj->written, str, to_write);
	obj->written += to_write;
	return to_write;
}

static int str_chadd(lc_appendable_t *obj, int ch)
{
	if (obj->limit - obj->written > 1) {
		char *tgt = (char*)obj->obj;
		tgt[obj->written++] = (char) ch;
		return 1;
	}

	return 0;
}

static void str_finish(lc_appendable_t *obj)
{
	char *str = (char*)obj->obj;
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
	struct obstack *obst = (struct obstack*)obj->obj;
	obj->written += n;
	obstack_grow(obst, str, n);
	return n;
}

static int obst_chadd(lc_appendable_t *obj, int ch)
{
	struct obstack *obst = (struct obstack*)obj->obj;
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
