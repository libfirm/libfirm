/*
 * Project:     libFIRM
 * File name:   ir/adt/xmalloc.c
 * Purpose:     Xmalloc --- never failing wrappers for malloc() & friends.
 * Author:      Markus Armbruster
 * Modified by:
 * Created:     1999 by getting from fiasco
 * CVS-ID:      $Id$
 * Copyright:   (c) 1995, 1996 Markus Armbruster
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/* @@@ ToDo: replace this file with the one from liberty.
   [reimplement xstrdup, ... ] */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include "xmalloc.h"
#include "error.h"

static NORETURN xnomem(void) {
  panic("out of memory");
}

void *xmalloc(size_t size) {
  void *res = malloc(size);

  if (!res) xnomem();
  return res;
}

void *xcalloc(size_t num, size_t size) {
  void *res = calloc(num, size);

  if (!res) xnomem();
  return res;
}

void *xrealloc(void *ptr, size_t size) {
  /* ANSI blesses realloc (0, x) but SunOS chokes on it */
  void *res = ptr ? realloc (ptr, size) : malloc (size);

  if (!res) xnomem();
  return res;
}

char *xstrdup(const char *str) {
  size_t len = strlen (str) + 1;
  return memcpy((xmalloc) (len), str, len);
}
