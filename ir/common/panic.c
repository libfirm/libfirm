/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include "panic.h"
# include "xprintf.h"

void
panic (const char *fmt, ...)
{
  va_list ap;

  fputs ("(panic) ", stderr);
  va_start (ap, fmt);
  xvfprintf (stderr, fmt, ap);
  va_end (ap);
  putc ('\n', stderr);
  exit (1);
}
