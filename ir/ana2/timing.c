/* -*- c -*- */

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
 * @brief    generic timing routines
 * @author   Florian
 * @date     Mon 18 Oct 2004
 * @version  $Id$
 * @note
 *   Timing stuff.  Not really part of ana2, but where else should it go.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <string.h>

#include "timing.h"
#include "xmalloc.h"

struct timing_env
{
  struct timeval *start;
  struct timeval *end;
};

#ifdef _WIN32
/* no support yet */
timing_t *start_timing (void) {}
int end_timing (timing_t *t) {}

#else
#include <sys/time.h>


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
start_timing (void)
{
  timing_t *t = (timing_t*) xmalloc (sizeof (timing_t));

  t->start = (struct timeval*) xmalloc (sizeof (struct timeval));
  t->end   = (struct timeval*) xmalloc (sizeof (struct timeval));

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
#endif /* _WIN32 */


/*
  $Log$
  Revision 1.5  2006/09/12 12:17:37  matze
  more warning fixes

  Revision 1.4  2006/06/06 12:06:27  beck
  use xmalloc instead of malloc

  Revision 1.3  2005/01/05 14:25:38  beck
  added Win32 "support"

  Revision 1.2  2004/12/21 15:52:23  beck
  moved struct timing_env to .c file, added config.h

  Revision 1.1  2004/10/29 18:55:52  liekweg
  (mostly) generic timimg
*/
