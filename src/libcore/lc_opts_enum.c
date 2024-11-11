/*
 * This file is part of libFirm.
 * Copyright (C) 2012 Universitaet Karlsruhe
 */
#include "lc_opts_enum.h"

#include "lc_opts_t.h"
#include "util.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DECL_CB(N, op) \
bool lc_opt_enum_##N##_cb(void *const data, size_t const len, char const *const arg) \
{ \
	lc_opt_enum_##N##_var_t const *const var = (lc_opt_enum_##N##_var_t*)data; \
	(void)len; \
	\
	bool res = false; \
	for (char const *a = arg; *a != '\0';) { \
		char const *const delim = " \t|,"; \
		a += strspn(a, delim); \
		char const *const end = a + strcspn(a, delim); \
		for (lc_opt_enum_##N##_items_t const *i = var->items; i->name; ++i) { \
			if (strstart(a, i->name) == end) { \
				*var->value op i->value; \
				res = true; \
			} \
		} \
		a = end; \
	} \
	return res; \
} \

DECL_CB(int, =)
DECL_CB(mask, |=)
DECL_CB(func_ptr, =)

#define DECL_DUMP(T, N, cond) \
int lc_opt_enum_ ## N ## _dump(char *buf, size_t n, void *data) \
{ \
	lc_opt_enum_ ## N ## _var_t *var           = (lc_opt_enum_ ## N ## _var_t*)data;       \
	const lc_opt_enum_ ## N ## _items_t *items = var->items; \
	const char *prefix                         = "";         \
	TYPE(value) = *var->value; \
	int i; \
	size_t l = strlen(buf); \
 \
	if (l >= n) \
		return (int)l; \
	n -= l; \
	n += 2; \
	for (i = 0; items[i].name != NULL; ++i) { \
		TYPE(item_value) = items[i].value; \
		if (cond) { \
			if (n <= 2) \
				break; \
			strcat(buf, prefix); \
			l = strlen(items[i].name); \
			if (n <= l) \
				break; \
			strcat(buf, items[i].name); \
			prefix = ", "; \
		} \
	} \
 \
	return (int)strlen(buf); \
} \


#define DECL_DUMP_VALS(T, N) \
int lc_opt_enum_ ## N ## _dump_vals(char *buf, size_t n, void *data) \
{ \
	lc_opt_enum_ ## N ## _var_t *var           = (lc_opt_enum_ ## N ## _var_t*) data;       \
	const lc_opt_enum_ ## N ## _items_t *items = var->items; \
	const char *prefix                         = "";         \
	int i; \
	size_t l = strlen(buf); \
 \
	if (l >= n) \
		return (int)l; \
	n -= l; \
	n += 2; \
	for (i = 0; items[i].name != NULL; ++i) { \
		if (n <= 2) \
			break; \
		strcat(buf, prefix); n -= 2; \
		l = strlen(items[i].name); \
		if (n <= l) \
			break; \
		strcat(buf, items[i].name); \
		n -= l; \
		prefix = ", "; \
	} \
 \
	return (int)strlen(buf); \
} \



#define TYPE(x) int x
DECL_DUMP(int, int, item_value == value)
DECL_DUMP_VALS(int, int)
#undef TYPE

#define TYPE(x) unsigned x
DECL_DUMP(unsigned, mask, (item_value & value) == item_value)
DECL_DUMP_VALS(unsigned, mask)
#undef TYPE

#define TYPE(x) int (*x)(void)
DECL_DUMP(int (*)(void), func_ptr, item_value == value)
DECL_DUMP_VALS(int (*)(void), func_ptr)
#undef TYPE
