/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief       never failing wrappers for malloc() & friends.
 * @author      Markus Armbruster
 * @version     $Id$
 * @note        The functions here never fail because they simply abort your
 *              program in case of an error.
 */
#ifndef FIRM_ADT_XMALLOC_H
#define FIRM_ADT_XMALLOC_H

#include <stddef.h>
#include <stdlib.h>

/* xmalloc() & friends. */

void *xmalloc(size_t size);
void *xrealloc(void *ptr, size_t size);
char *xstrdup(const char *str);

#define xfree(ptr)      free(ptr)

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
#define XMALLOCF(type, member, n) ((type*)xmalloc(offsetof(type, member) + sizeof(((type*)0)->member) * (n)))

/**
 * Allocate an object with n elements of a flexible array member and zero the
 * whole object
 */
#define XMALLOCFZ(type, member, n) ((type*)memset(xmalloc(offsetof(type, member) + sizeof(((type*)0)->member) * (n)), 0, offsetof(type, member) + sizeof(((type*)0)->member) * (n)))


/* Includes for alloca() */
#if defined(__FreeBSD__)
#include <stdlib.h>
#elif defined(_WIN32)
#include <malloc.h>
#else
#include <alloca.h>
#endif

#endif
