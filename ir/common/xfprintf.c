/* Xfprintf --- extended formatted output to files.
   Copyright (C) 1995, 1996 Christian von Roques */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#ifndef USE_PRINTF

#include <assert.h>
#include "xprintf.h"


static int
xfprinter (void *f, const char *data, size_t len)
{
  size_t togo = len;

  while (togo > 0) {
    size_t n = fwrite (data, 1, togo, (FILE*)f);

    if (!n) return -1;
    togo -= n;
    data += n;
  }

  return len;
}


int
xfprintf (FILE *F, const char *fmt, ...)
{
  va_list args;
  int res;

  va_start (args, fmt);
  res = xvgprintf (xfprinter, F, fmt, args);
  va_end (args);
  return res;
}


int
xvfprintf (FILE *F, const char *fmt, va_list args)
{
  return xvgprintf (xfprinter, F, fmt, args);
}


int
xprintf (const char *fmt, ...)
{
  va_list args;
  int res;

  va_start (args, fmt);
  res = xvgprintf (xfprinter, stdout, fmt, args);
  va_end (args);
  return res;
}

int
xvprintf (const char *fmt, va_list args)
{
  return xvgprintf (xfprinter, stdout, fmt, args);
}

#endif /* USE_PRINTF */
