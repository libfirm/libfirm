/*
 * Project:     libFIRM
 * File name:   ir/common/xoprintf.c
 * Purpose:     Xoprintf --- extended formatted output to obstacks.
 * Author:      Christian von Roques
 * Modified by:
 * Created:     1999 by getting from fiasco
 * CVS-ID:      $Id$
 * Copyright:   (c) 1995, 1996 Christian von Roques
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#ifndef USE_PRINTF

#include <obstack.h>
#include <string.h>
#include <assert.h>
#include "xprintf.h"

/* bcopy is not ISO C */
#define bcopy(X, Y, Z) memcpy((Y), (X), (Z))

static int
xoprinter (void *obst, const char *data, size_t len)
{
  obstack_grow ((struct obstack *)obst, data, len);
  return len;
}


int
xoprintf (struct obstack *obst, const char *fmt, ...)
{
  va_list args;
  int res;

  va_start (args, fmt);
  res = xvgprintf (xoprinter, obst, fmt, args);
  va_end (args);
  return res;
}

int
xvoprintf (struct obstack *obst, const char *fmt, va_list args)
{
  return xvgprintf (xoprinter, obst, fmt, args);
}

#endif /* USE_PRINTF */
