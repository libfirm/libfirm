/*
 * Project:     libFIRM
 * File name:   ir/adt/xmalloc.h
 * Purpose:     More comfortable allocations.
 * Author:      Markus Armbruster
 * Modified by:
 * Created:     1999 by getting from fiasco
 * CVS-ID:      $Id$
 * Copyright:   (c) 1995, 1996 Markus Armbruster
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


#ifndef _XMALLOC_H_
#define _XMALLOC_H_

#include <stddef.h>

/* xmalloc() & friends. */

void *xmalloc(size_t size);
void *xrealloc(void *ptr, size_t size);
char *xstrdup(const char *str);
void xnomem(void);
void free(void *ptr);

#define xfree(ptr)      free(ptr)

#endif /* _XMALLOC_H_ */
