/* -*- c -*- */

/*
 * Time-stamp: <26.10.2004 11:57:13h liekweg>
 * Project:     libFIRM
 * File name:   ir/ana2/timing.c
 * Purpose:     generic timing routines
 * Author:      Florian
 * Modified by:
 * Created:     Mon 18 Oct 2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/*
  Timing stuff.  Not really part of ana2, but where else should it go.
*/

#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

# include "timing.h"


/*
Helpers
*/
static int
timeval_subtract (struct timeval *x, struct timeval *y)
{
  /* Perform the carry for the later subtraction by updating Y. */
  if (x->tv_usec < y->tv_usec) {
    int nsec = (y->tv_usec - x->tv_usec) / 1000000 + 1;
    y->tv_usec -= 1000000 * nsec;
    y->tv_sec += nsec;
  }

  if (x->tv_usec - y->tv_usec > 1000000) {
    int nsec = (x->tv_usec - y->tv_usec) / 1000000;
    y->tv_usec += 1000000 * nsec;
    y->tv_sec -= nsec;
  }

  return ((x->tv_sec - y->tv_sec) * 1000000 + (x->tv_usec - y->tv_usec));
}


/*
  Public Interface
*/
timing_t *
start_timing ()
{
  timing_t *t = (timing_t*) malloc (sizeof (timing_t));

  t->start = (struct timeval*) malloc (sizeof (struct timeval));
  t->end   = (struct timeval*) malloc (sizeof (struct timeval));

  gettimeofday (t->start, NULL);

  return (t);
}

int
end_timing (timing_t *t)
{
  int time;

  gettimeofday (t->end, NULL);

  time = timeval_subtract (t->end, t->start);

  memset (t->start, 0x0, sizeof (struct timeval));
  free (t->start);

  memset (t->end,   0x0, sizeof (struct timeval));
  free (t->end);

  memset (t, 0x00, sizeof (timing_t));
  free (t);

  return (time);
}


/*
  $Log$
  Revision 1.1  2004/10/29 18:55:52  liekweg
  (mostly) generic timimg


*/
