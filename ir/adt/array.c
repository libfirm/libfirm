/*
 * Project:     libFIRM
 * File name:   ir/adt/array.c
 * Purpose:     Array --- dynamic & flexible arrays.
 * Author:      Markus Armbruster
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
#include "array.h"

/* Undefine the macros to get the functions instead, cf tmalloc.c.  */
#undef xmalloc
#undef xrealloc
#undef xstrdup
#undef xfree


_arr_descr arr_mt_descr
#ifndef NDEBUG
  = { ARR_D_MAGIC }
#endif
;

void *
_new_arr_d (struct obstack *obstack, int nelts, size_t elts_size)
{
  _arr_descr *new;

  assert (obstack && (nelts >= 0));

  new = obstack_alloc (obstack, _ARR_ELTS_OFFS+elts_size);
  _ARR_SET_DBGINF (new, ARR_D_MAGIC, elts_size/nelts);
  new->u.obstack = obstack;
  new->nelts = nelts;
  return new->v.elts;
}


void *
_new_arr_f (int nelts, size_t elts_size)
{
  _arr_descr *new;

  assert (nelts >= 0);
  new = xmalloc (_ARR_ELTS_OFFS+elts_size);
  _ARR_SET_DBGINF (new, ARR_F_MAGIC, nelts ? elts_size/nelts : 0);
  new->u.allocated = new->nelts = nelts;
  return new->v.elts;
}


void
_del_arr_f (void *elts)
{
  _arr_descr *dp = _ARR_DESCR (elts);

  ARR_VRFY (elts);
  assert (dp->cookie == ARR_F_MAGIC);

#ifndef NDEBUG
  dp->cookie = 0xdeadbeef;
#endif
  free (dp);
}


void *
_arr_setlen (void *elts, int nelts, size_t elts_size)
{
  _arr_descr *dp = _ARR_DESCR (elts);

  assert ((dp->cookie == ARR_F_MAGIC) && (nelts >= 0));
  ARR_VRFY (elts);
  assert (!dp->eltsize || !nelts || (dp->eltsize == elts_size/nelts));

  dp = xrealloc (dp, _ARR_ELTS_OFFS+elts_size);
  dp->u.allocated = dp->nelts = nelts;

  return dp->v.elts;
}

\
void *
_arr_resize (void *elts, int nelts, size_t eltsize)
{
  _arr_descr *dp = _ARR_DESCR (elts);
  int n;

  assert ((dp->cookie == ARR_F_MAGIC) && (nelts >= 0));
  ARR_VRFY (elts);
  assert (dp->eltsize ? dp->eltsize == eltsize : (dp->eltsize = eltsize, 1));

  /* @@@ lots of resizes for small nelts */
  n = MAX (1, dp->u.allocated);
  while (nelts > n) n <<= 1;
  while (3*nelts < n) n >>= 1;
  assert (n >= nelts);

  if (n != dp->u.allocated) {
    dp = xrealloc (dp, _ARR_ELTS_OFFS+eltsize*n);
    dp->u.allocated = n;
#if defined(DEBUG) && defined(HAVE_GNU_MALLOC)
  } else {
    tmalloc_tag = NULL;
#endif
  }
  dp->nelts = nelts;

  return dp->v.elts;
}
