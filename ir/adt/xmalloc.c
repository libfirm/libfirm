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
# include <config.h>
#endif

#include <stdlib.h>
#include <string.h>

#include "xmalloc.h"
#include "panic.h"


void *
(xmalloc) (size_t size)
{
  void *res = malloc (size);

  if (!res) xnomem ();
  return res;
}


void *
(xrealloc) (void *ptr, size_t size)
{
  /* ANSI blesses realloc (0, x) but SunOS chokes on it */
  void *res = ptr ? realloc (ptr, size) : malloc (size);

  if (!res) xnomem ();
  return res;
}


char *
(xstrdup) (const char *str)
{
  size_t len = strlen (str) + 1;
  return memcpy ((xmalloc) (len), str, len);
}


void
xnomem (void)
{
  panic ("out of memory");
}
