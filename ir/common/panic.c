/*
 * Project:     libFIRM
 * File name:   ir/common/panic.c
 * Purpose:
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include <stdlib.h>

# include "panic.h"
# include <stdio.h>
# include <stdarg.h>


void
panic (const char *fmt, ...)
{
  va_list ap;

  fputs ("(panic) ", stderr);
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);
  putc ('\n', stderr);
  exit (1);
}
