/**
 * @file   lc_opts_enum.c
 * @date   24.11.2005
 * @author Sebastian Hack
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 *
 * Enum callback and dump implementation.
 */

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#if defined(__FreeBSD__)
#include <stdlib.h>
#elif defined(_WIN32)
#include <malloc.h>
#else
#include <alloca.h>
#endif

#include "lc_opts_t.h"
#include "lc_opts_enum.h"

static const char *delim			= " \t|,";

#define DECL_CB(N, op) \
int lc_opt_enum_ ## N ## _cb(LC_UNUSED(const char *name), LC_UNUSED(lc_opt_type_t type), void *data, size_t len, ...) \
{ \
	lc_opt_enum_ ## N ## _var_t *var						= data; \
	const lc_opt_enum_ ## N ## _items_t *items	= var->items; \
 \
	va_list args; \
	char *s, *tmp; \
	size_t begin, end; \
	const char *arg; \
	int res = 0; \
 \
	va_start(args, len); \
	arg = va_arg(args, const char *); \
	va_end(args); \
	\
	end     = strlen(arg); \
	tmp = s = malloc((end + 1) * sizeof(arg[0])); \
	strcpy(s, arg); \
	s[end]  = '\0'; \
	\
	end = 0; \
	while(arg[end] != '\0') { \
		unsigned int i; \
		\
		begin  = end + strspn(arg + end, delim); \
		end    = begin + strcspn(arg + begin, delim); \
		s      = tmp + begin; \
		s[end - begin] = '\0'; \
		\
		for(i = 0; items[i].name != NULL; ++i) { \
			if(strcmp(s, items[i].name) == 0) { \
				*var->value op items[i].value; \
				res = 1; \
			} \
		} \
	} \
	free(tmp); \
	return res; \
} \

DECL_CB(int, =)
DECL_CB(mask, |=)
DECL_CB(ptr, =)
DECL_CB(const_ptr, =)
DECL_CB(func_ptr, =)

#define DECL_DUMP(T, N, cond) \
int lc_opt_enum_ ## N ## _dump(char *buf, size_t n, LC_UNUSED(const char *name), LC_UNUSED(lc_opt_type_t type), void *data, LC_UNUSED(size_t len)) \
{ \
	lc_opt_enum_ ## N ## _var_t *var						= data; \
	const lc_opt_enum_ ## N ## _items_t *items	= var->items; \
	const char *prefix								= "";			 \
	TYPE(value) = *var->value; \
	int i; \
 \
	for(i = 0; items[i].name != NULL; ++i) { \
		TYPE(item_value) = items[i].value; \
		if(cond) { \
			strncat(buf, prefix, n); \
			strncat(buf, items[i].name, n); \
			prefix = ", "; \
		} \
	} \
 \
	return strlen(buf); \
} \


#define DECL_DUMP_VALS(T, N) \
int lc_opt_enum_ ## N ## _dump_vals(char *buf, size_t n, LC_UNUSED(const char *name), LC_UNUSED(lc_opt_type_t type), void *data, LC_UNUSED(size_t len)) \
{ \
	lc_opt_enum_ ## N ## _var_t *var						= data; \
	const lc_opt_enum_ ## N ## _items_t *items	= var->items; \
	const char *prefix								= "";			 \
	int i; \
 \
	for(i = 0; items[i].name != NULL; ++i) { \
		strncat(buf, prefix, n); \
		strncat(buf, items[i].name, n); \
		prefix = ", "; \
	} \
 \
	return strlen(buf); \
} \



#define TYPE(x) int x
DECL_DUMP(int, int, item_value == value)
DECL_DUMP_VALS(int, int)
#undef TYPE

#define TYPE(x) unsigned x
DECL_DUMP(unsigned, mask, (item_value & value) == item_value)
DECL_DUMP_VALS(unsigned, mask)
#undef TYPE

#define TYPE(x) void *x
DECL_DUMP(void *, ptr, item_value == value)
DECL_DUMP_VALS(void *, ptr)
#undef TYPE

#define TYPE(x) const void *x
DECL_DUMP(const void *, const_ptr, item_value == value)
DECL_DUMP_VALS(const void *, const_ptr)
#undef TYPE

#define TYPE(x) int (*x)(void)
DECL_DUMP(int (*)(void), func_ptr, item_value == value)
DECL_DUMP_VALS(int (*)(void), func_ptr)
#undef TYPE
