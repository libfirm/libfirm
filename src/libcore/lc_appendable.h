/*
 * This file is part of libFirm.
 * Copyright (C) 2012 IPD Goos, Universit"at Karlsruhe, Germany
 */



/**
 * Something to which can be appended.
 * @author Sebastian Hack
 * @date 3.1.2005
 */

#ifndef _LIBCORE_APPENDABLE_H
#define _LIBCORE_APPENDABLE_H

#include <stddef.h>

struct lc_appendable_funcs_t;

typedef struct lc_appendable_t {
	void *obj;
	size_t written;
	size_t limit;
	const struct lc_appendable_funcs_t *app;
} lc_appendable_t;

typedef struct lc_appendable_funcs_t {
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
