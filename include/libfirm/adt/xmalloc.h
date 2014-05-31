/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       never failing wrappers for malloc() & friends.
 * @author      Markus Armbruster
 * @note        The functions here never fail because they simply abort your
 *              program in case of an error.
 */
#ifndef FIRM_ADT_XMALLOC_H
#define FIRM_ADT_XMALLOC_H

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

/* Includes for alloca() */
#ifndef alloca
#	if defined(__GNUC__)
#		define alloca(x)       __builtin_alloca(x)
#	elif defined(__WIN32)
#		include <malloc.h>
#	else
#		error do not know how to get alloca
#	endif
#endif

#include "../begin.h"

/**
 * @ingroup adt
 * @defgroup xmalloc Memory Allocation
 * @{
 */

/**
 * Allocate @p size bytes on the heap.
 * This is a wrapper for malloc which calls panic() in case of errors, so no
 * error handling is required for code using it.
 */
FIRM_API void *xmalloc(size_t size);
/**
 * Chane size of a previously allocated memory block to @p size bytes.
 * This is a wrapper for realloc which calls panic() in case of errors, so no
 * error handling is required for code using it.
 */
FIRM_API void *xrealloc(void *ptr, size_t size);
/**
 * Allocates memory and copies string @p str into it.
 * This is a wrapper for strdup which calls panic() in case of errors, so no
 * error handling is required for code using it.
 */
FIRM_API char *xstrdup(const char *str);

/**
 * Allocate n objects of a certain type
 */
#define XMALLOCN(type, n) ((type*)xmalloc(sizeof(type) * (n)))

/**
 * Allocate n objects of a certain type and zero them
 */
#define XMALLOCNZ(type, n) ((type*)memset(xmalloc(sizeof(type) * (n)), 0, sizeof(type) * (n)))

/**
 * Allocate one object of a certain type
 */
#define XMALLOC(type) XMALLOCN(type, 1)

/**
 * Allocate one object of a certain type and zero it
 */
#define XMALLOCZ(type) XMALLOCNZ(type, 1)

/**
 * Reallocate n objects of a certain type
 */
#define XREALLOC(ptr, type, n) ((type*)xrealloc(ptr, sizeof(type) * (n)))

/**
 * Allocate an object with n elements of a flexible array member
 */
#define XMALLOCF(type, member, n) ((type*)xmalloc(offsetof(type, member) + sizeof(*((type*)0)->member) * (n)))

/**
 * Allocate an object with n elements of a flexible array member and zero the
 * whole object
 */
#define XMALLOCFZ(type, member, n) ((type*)memset(XMALLOCF(type, member, (n)), 0, offsetof(type, member) + sizeof(*((type*)0)->member) * (n)))

/**
 * Allocate n objects of a certain type on the stack
 */
#define ALLOCAN(type, n) ((type*)alloca(sizeof(type) * (n)))

/**
 * Allocate n objects of a certain type on the stack and zero them
 */
#define ALLOCANZ(type, n) ((type*)memset((type*)alloca(sizeof(type) * (n)), 0, sizeof(type) * (n)))

/**
 * Allocate an object with n elements of a flexible array member on the stack
 */
#define ALLOCAF(type, member, n) ((type*)alloca(offsetof(type, member) + sizeof(*((type*)0)->member) * (n)))

/**
 * Allocate n objects of a certain type on the given obstack
 */
#define OALLOCN(obst, type, n) ((type*)obstack_alloc((obst), sizeof(type) * (n)))

/**
 * Allocate n objects of a certain type on the given obstack and zero them
 */
#define OALLOCNZ(obst, type, n) ((type*)memset(OALLOCN((obst), type, (n)), 0, sizeof(type) * (n)))

/**
 * Allocate one object of a certain type on the given obstack
 */
#define OALLOC(obst, type) OALLOCN(obst, type, 1)

/**
 * Allocate one object of a certain type on the given obstack and zero it
 */
#define OALLOCZ(obst, type) OALLOCNZ(obst, type, 1)

/**
 * Allocate an object with n elements of a flexible array member on the given
 * obstack.
 */
#define OALLOCF(obst, type, member, n) ((type*)obstack_alloc((obst), offsetof(type, member) + sizeof(*((type*)0)->member) * (n)))

/**
 * Allocate an object with n elements of a flexible array member on the given
 * obstack and zero the whole object
 */
#define OALLOCFZ(obst, type, member, n) ((type*)memset(OALLOCF((obst), type, member, (n)), 0, offsetof(type, member) + sizeof(*((type*)0)->member) * (n)))

/** @} */

#include "../end.h"

#endif
