/*
 * Project:     libFIRM
 * File name:   ir/adt/debug.c
 * Purpose:     Debug --- run time debug level management
 * Author:      Christian von Roques
 * Modified by:
 * Created:     1999 by getting from fiasco
 * CVS-ID:      $Id$
 * Copyright:   (c) 1995, 1996 Markus Armbruster
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdlib.h>
#include <string.h>
#include "debug.h"
#include "xmalloc.h"


int
(d_) (int flag, unsigned level)
{
  return d_ (flag, level);
}


int
(d_level) (int flag)
{
  return d_level (flag);
}


int
(d_set_level) (int flag, unsigned level)
{
  return d_set_level (flag, level);
}


#ifdef DEBUG
int nflags;
unsigned char *d_vec;
#endif


/*
 * Set debug flags according to the following syntax:
 * number ["-" number] ["." number]
 * 1st number is number of first flag to set.
 * 2nd number is number of last flag to set. [defaults to 1st number]
 * 3rd number is level to set the flags to. [defaults to 1]
 */
void
(d_parse) (const char *s)
{
#ifdef DEBUG
  long first, last, level;
  char *end;

  first = strtol(s, &end, 10);
  last = (s != end) ? first : nflags-1;

  s = end;
  if (*s == '-') {
    ++s;
    last = strtol(s, &end, 10);
    if (end == s) last = nflags-1; /* 3-  ==> 3..nflags-1 */
  }

  s = end;
  if (*s == '.') {
    ++s;
    level = strtol(s, &end, 10);
    /* . with no number gives level 0 */
  } else {
    level = 1;
  }

  if (first<0) first=0;
  if (last>=nflags) last = nflags-1;

  while (first<=last) d_vec[first++] = level;
#endif /* DEBUG */
}


void
(d_init) (int n)
{
#ifdef DEBUG
  nflags = n;
  d_vec = xmalloc (sizeof (unsigned char) * n);
  memset(d_vec, 0, sizeof (unsigned char) * n);
#endif
}
