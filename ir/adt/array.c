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
# include "config.h"
#endif

#include <stdlib.h>

#include "array.h"
#include "xmalloc.h"

/* Undefine the macros to get the functions instead, cf tmalloc.c.  */
#undef xmalloc
#undef xrealloc
#undef xstrdup
#undef xfree

#ifndef MAX
# define MAX(a,b) ((a) > (b) ? (a) : (b))
#endif
#ifndef MIN
# define MIN(a,b) ((a) > (b) ? (b) : (a))
#endif

/**
 * An empty dynamic array
 */
_arr_descr arr_mt_descr
#ifndef NDEBUG
  = { ARR_D_MAGIC }
#endif
;

/**
 * Creates a dynamic array on a obstack.
 *
 * @param obstack    An struct obstack * were the data will be allocated
 * @param nelts      The number of elements
 * @param elts_size  The size of the array elements.
 *
 * @return A pointer to the dynamic array (can be used as a pointer to the
 *         first element of this array).
 *
 * @remark Helper function, use NEW_ARR_D() instead.
 */
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

/**
 * Creates a flexible array.
 *
 * @param nelts      The number of elements
 * @param elts_size  The size of the array elements.
 *
 * @return A pointer to the flexible array (can be used as a pointer to the
 *         first element of this array).
 *
 * @remark Helper function, use NEW_ARR_F() instead.
 */
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

/**
 * Delete a flexible array.
 *
 * @param elts    The flexible array (pointer to the first element).
 *
 * @remark Helper function, use DEL_ARR_F() instead.
 */
void
_del_arr_f (void *elts)
{
  _arr_descr *dp = _ARR_DESCR (elts);

  ARR_VRFY (elts);
  assert (dp->magic == ARR_F_MAGIC);

#ifndef NDEBUG
  dp->magic = 0xdeadbeef;
#endif
  free (dp);
}

/**
 * Resize a flexible array, always reallocate data.
 *
 * @param elts       The flexible array (pointer to the first element).
 * @param nelts      The new number of elements.
 * @param elts_size  The size of the array elements.
 *
 * @return A resized flexible array, possibly other address than
 *         elts.
 *
 * @remark Helper function, use ARR_SETLEN() instead.
 */
void *
_arr_setlen (void *elts, int nelts, size_t elts_size)
{
  _arr_descr *dp = _ARR_DESCR (elts);

  assert ((dp->magic == ARR_F_MAGIC) && (nelts >= 0));
  ARR_VRFY (elts);
  assert (!dp->eltsize || !nelts || (dp->eltsize == elts_size/nelts));

  dp = xrealloc (dp, _ARR_ELTS_OFFS+elts_size);
  dp->u.allocated = dp->nelts = nelts;

  return dp->v.elts;
}

/**
 * Resize a flexible array, allocate more data if needed but do NOT
 * reduce.
 *
 * @param elts     The flexible array (pointer to the first element).
 * @param nelts    The new number of elements.
 * @param eltsize  The size of the array elements.
 *
 * @return A resized flexible array, possibly other address than
 *         elts.
 *
 * @remark Helper function, use ARR_RESIZE() instead.
 */
void *
_arr_resize (void *elts, int nelts, size_t eltsize)
{
  _arr_descr *dp = _ARR_DESCR (elts);
  int n;

  assert ((dp->magic == ARR_F_MAGIC) && (nelts >= 0));
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
