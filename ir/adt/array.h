/* Declarations for Array.
   Copyright (C) 1995, 1996 Markus Armbruster
   All rights reserved. */

/* $Id$ */

/* @@@ growing a dynamic on an obstack */

#ifndef _ARRAY_H
#define _ARRAY_H

#include <assert.h>
#include <stddef.h>
#include <obstack.h>
#include "cookies.h"
#include "misc.h"


/* Flexible create / delete */
#define NEW_ARR_F(type, nelts)						\
  (XMALLOC_TRACE (type *)_new_arr_f ((nelts), sizeof(type) * (nelts)))
#define CLONE_ARR_F(type, arr)			\
  NEW_ARR_F (type, ARR_LEN ((arr)))
#define DUP_ARR_F(type, arr)							\
  memcpy (CLONE_ARR_F (type, (arr)), (arr), sizeof(type) * ARR_LEN((arr)))
#define DEL_ARR_F(arr) (XMALLOC_TRACE _del_arr_f ((arr)))

/* Dynamic create on obstacks */
#define NEW_ARR_D(type, obstack, nelts)					\
  (  nelts								\
   ? (type *)_new_arr_d ((obstack), (nelts), sizeof(type) * (nelts))	\
   : (type *)arr_mt_descr.v.elts)
#define CLONE_ARR_D(type, obstack, arr)		\
  NEW_ARR_D (type, (obstack), ARR_LEN ((arr)))
#define DUP_ARR_D(type, obstack, arr)							\
  memcpy (CLONE_ARR_D (type, (obstack), (arr)), (arr), sizeof(type) * ARR_LEN ((arr)))

/* Automatic create; delete is automatic at return from function */
/* Quick'n'dirty! */
#define NEW_ARR_A(type, var, n)									\
  do {												\
    int _nelts = (n);										\
    assert (_nelts >= 0);									\
    (var) = (void *)((_arr_descr *)alloca (_ARR_ELTS_OFFS + sizeof(type) * _nelts))->v.elts;	\
    _ARR_SET_DBGINF (_ARR_DESCR ((var)), ARR_A_MAGIC, sizeof (type));				\
    (void)(_ARR_DESCR ((var))->nelts = _nelts);							\
  } while (0)
#define CLONE_ARR_A(type, var, arr)		\
  NEW_ARR_A (type, (var), ARR_LEN ((arr)))

#define DUP_ARR_A(type, var, arr)					\
  do { CLONE_ARR_A(type, (var), (arr));					\
       memcpy ((var), (arr), sizeof (type) * ARR_LEN ((arr))); }	\
  while (0)

/* Declare an initialized array of fixed size */
#define DECL_ARR_S(type, var, _nelts)					\
  ARR_STRUCT(type, (_nelts) ? (_nelts) : 1) _##var;			\
  type *var = (_ARR_SET_DBGINF (&_##var, ARR_A_MAGIC, sizeof (type)),	\
	       _##var.nelts = _nelts,					\
	       _##var.v.elts)

/* Length */
#define ARR_LEN(arr) (ARR_VRFY ((arr)), _ARR_DESCR((arr))->nelts)

/* Resize
   Applicable to flexibles only, change arr which must be an lvalue.  */
/* resize arr to hold n elts */
#define ARR_RESIZE(type, arr, n)					\
  (XMALLOC_TRACE (arr) = _arr_resize ((arr), (n), sizeof(type)))
/* resize arr to hold exactly n elts */
#define ARR_SETLEN(type, arr, n)					\
  (XMALLOC_TRACE (arr) = _arr_setlen ((arr), (n), sizeof(type) * (n)))
/* resize arr by delta elts */
#define ARR_EXTEND(type, arr, delta)			\
  ARR_RESIZE (type, (arr), ARR_LEN ((arr)) + (delta))
/* resize arr to hold n elts only if it is currently shorter */
#define ARR_EXTO(type, arr, n)						\
  ((n) >= ARR_LEN ((arr)) ? ARR_RESIZE (type, (arr), (n)+1) : (arr))
/* append one elt to arr */
#define ARR_APP1(type, arr, elt)					\
  (ARR_EXTEND (type, (arr), 1), (arr)[ARR_LEN ((arr))-1] = (elt))


#ifdef NDEBUG
# define ARR_VRFY(arr) ((void)0)
# define ARR_IDX_VRFY(arr, idx) ((void)0)
#else
# define ARR_VRFY(arr)									\
    assert (   (   (_ARR_DESCR((arr))->cookie == ARR_D_MAGIC)				\
		|| (_ARR_DESCR((arr))->cookie == ARR_A_MAGIC)				\
		|| (_ARR_DESCR((arr))->cookie == ARR_F_MAGIC))				\
	    && (   (_ARR_DESCR((arr))->cookie != ARR_F_MAGIC)				\
		|| (_ARR_DESCR((arr))->u.allocated >= _ARR_DESCR((arr))->nelts))	\
	    && (_ARR_DESCR((arr))->nelts >= 0))
# define ARR_IDX_VRFY(arr, idx)				\
    assert ((0 <= (idx)) && ((idx) < ARR_LEN ((arr))))
#endif


/* Private !!!
   Don't try this at home, kids, we're trained professionals ;->
   ... or at the IPD, either. */
#ifdef NDEBUG
# define _ARR_DBGINF_DECL
# define _ARR_SET_DBGINF(descr, co, es) ((co), (es))
#else
# define _ARR_DBGINF_DECL int cookie; size_t eltsize;
# define _ARR_SET_DBGINF(descr, co, es)					\
    ((descr)->cookie = (co), (descr)->eltsize = (es))
#endif

#define ARR_STRUCT(type, _nelts)						\
  struct {									\
    _ARR_DBGINF_DECL								\
    union {									\
      struct obstack *obstack;	/* dynamic: allocated on this obstack */	\
      int allocated;			/* flexible: #slots allocated */	\
    } u;									\
    int nelts;									\
    union {									\
      type elts[(_nelts)];							\
      aligned_type align[1];							\
    } v;									\
  }

typedef ARR_STRUCT (aligned_type, 1) _arr_descr;

extern _arr_descr arr_mt_descr;

void *_new_arr_f (int, size_t);
void _del_arr_f (void *);
void *_new_arr_d (struct obstack *, int, size_t);
void *_arr_resize (void *, int, size_t);
void *_arr_setlen (void *, int, size_t);

#define _ARR_ELTS_OFFS offsetof (_arr_descr, v.elts)
#define _ARR_DESCR(elts) ((_arr_descr *)(void *)((char *)(elts) - _ARR_ELTS_OFFS))

#endif
