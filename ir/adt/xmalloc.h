/*
 * Copyrigth (C) 1995-2007 University of Karlsruhe.  All right reserved.
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

/* xmalloc() & friends. */

void *xmalloc(size_t size);
void *xcalloc(size_t num, size_t size);
void *xrealloc(void *ptr, size_t size);
char *xstrdup(const char *str);
void free(void *ptr);

#define xfree(ptr)      free(ptr)


/* Includes for alloca() */
#if defined(__FreeBSD__)
#include <stdlib.h>
#elif defined(_WIN32)
#include <malloc.h>
#else
#include <alloca.h>
#endif

#endif
