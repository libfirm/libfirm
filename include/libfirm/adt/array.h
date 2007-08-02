/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief     Dynamic and flexible arrays for C.
 * @author    Markus Armbruster, Michael Beck, Matthias Braun, Sebastian Hack
 * @version   $Id$
 */
#ifndef FIRM_ADT_ARRAY_H
#define FIRM_ADT_ARRAY_H

#include <assert.h>
#include <stddef.h>

#include "firm_config.h"
#include "obst.h"
#include "fourcc.h"
#include "align.h"
#include "xmalloc.h"

#define ARR_D_MAGIC	FOURCC('A','R','R','D')
#define ARR_A_MAGIC	FOURCC('A','R','R','A')
#define ARR_F_MAGIC	FOURCC('A','R','R','F')

/**
 * Creates a flexible array.
 *
 * @param type     The element type of the new array.
 * @param nelts    a size_t expression evaluating to the number of elements
 *
 * This macro creates a flexible array of a given type at runtime.
 * The size of the array can be changed later.
 *
 * @return A pointer to the flexible array (can be used as a pointer to the
 *         first element of this array).
 */
#define NEW_ARR_F(type, nelts)						\
  ((type *)_new_arr_f ((nelts), sizeof(type) * (nelts)))

/**
 * Creates a new flexible array with the same number of elements as a
 * given one.
 *
 * @param type     The element type of the new array.
 * @param arr      An array from which the number of elements will be taken
 *
 * This macro creates a flexible array of a given type at runtime.
 * The size of the array can be changed later.
 *
 * @return A pointer to the flexible array (can be used as a pointer to the
 *         first element of this array).
 */
#define CLONE_ARR_F(type, arr)			\
  NEW_ARR_F (type, ARR_LEN ((arr)))

/**
 * Duplicates an array and returns the new flexible one.
 *
 * @param type     The element type of the new array.
 * @param arr      An array from which the elements will be duplicated
 *
 * This macro creates a flexible array of a given type at runtime.
 * The size of the array can be changed later.
 *
 * @return A pointer to the flexible array (can be used as a pointer to the
 *         first element of this array).
 */
#define DUP_ARR_F(type, arr)							\
  memcpy (CLONE_ARR_F (type, (arr)), (arr), sizeof(type) * ARR_LEN((arr)))

/**
 * Delete a flexible array.
 *
 * @param arr    The flexible array.
 */
#define DEL_ARR_F(arr) (_del_arr_f ((arr)))

/**
 * Creates a dynamic array on an obstack.
 *
 * @param type     The element type of the new array.
 * @param obstack  A struct obstack * were the data will be allocated
 * @param nelts    A size_t expression evaluating to the number of elements
 *
 * This macro creates a dynamic array of a given type at runtime.
 * The size of the array cannot be changed later.
 *
 * @return A pointer to the dynamic array (can be used as a pointer to the
 *         first element of this array).
 */
#define NEW_ARR_D(type, obstack, nelts)					\
  (  nelts								\
   ? (type *)_new_arr_d ((obstack), (nelts), sizeof(type) * (nelts))	\
   : (type *)arr_mt_descr.v.elts)

/**
 * Creates a new dynamic array with the same number of elements as a
 * given one.
 *
 * @param type     The element type of the new array.
 * @param obstack  An struct obstack * were the data will be allocated
 * @param arr      An array from which the number of elements will be taken
 *
 * This macro creates a dynamic array of a given type at runtime.
 * The size of the array cannot be changed later.
 *
 * @return A pointer to the dynamic array (can be used as a pointer to the
 *         first element of this array).
 */
#define CLONE_ARR_D(type, obstack, arr)		\
  NEW_ARR_D (type, (obstack), ARR_LEN ((arr)))

/**
 * Duplicates an array and returns the new dynamic one.
 *
 * @param type     The element type of the new array.
 * @param obstack  An struct obstack * were the data will be allocated
 * @param arr      An array from which the elements will be duplicated
 *
 * This macro creates a dynamic array of a given type at runtime.
 * The size of the array cannot be changed later.
 *
 * @return A pointer to the dynamic array (can be used as a pointer to the
 *         first element of this array).
 */
#define DUP_ARR_D(type, obstack, arr)							\
  memcpy (CLONE_ARR_D (type, (obstack), (arr)), (arr), sizeof(type) * ARR_LEN ((arr)))

/**
 * Create an automatic array which will be deleted at return from function.
 * Beware, the data will be allocated on the function stack!
 *
 * @param type     The element type of the new array.
 * @param var      A lvalue of type (type *) which will hold the new array.
 * @param n        number of elements in this array.
 *
 * This macro creates a dynamic array on the functions stack of a given type at runtime.
 * The size of the array cannot be changed later.
 */
#define NEW_ARR_A(type, var, n)									\
  do {												\
    int _nelts = (n);										\
    assert (_nelts >= 0);									\
    (var) = (void *)((_arr_descr *)alloca (_ARR_ELTS_OFFS + sizeof(type) * _nelts))->v.elts;	\
    _ARR_SET_DBGINF (_ARR_DESCR ((var)), ARR_A_MAGIC, sizeof (type));				\
    (void)(_ARR_DESCR ((var))->nelts = _nelts);							\
  } while (0)

/**
 * Creates a new automatic array with the same number of elements as a
 * given one.
 *
 * @param type     The element type of the new array.
 * @param var      A lvalue of type (type *) which will hold the new array.
 * @param arr      An array from which the elements will be duplicated
 *
 * This macro creates a dynamic array of a given type at runtime.
 * The size of the array cannot be changed later.
 *
 * @return A pointer to the dynamic array (can be used as a pointer to the
 *         first element of this array).
 */
#define CLONE_ARR_A(type, var, arr)		\
  NEW_ARR_A (type, (var), ARR_LEN ((arr)))

/**
 * Duplicates an array and returns a new automatic one.
 *
 * @param type     The element type of the new array.
 * @param var      A lvalue of type (type *) which will hold the new array.
 * @param arr      An array from with the number of elements will be taken
 *
 * This macro creates a dynamic array of a given type at runtime.
 * The size of the array cannot be changed later.
 *
 * @return A pointer to the dynamic array (can be used as a pointer to the
 *         first element of this array).
 */
#define DUP_ARR_A(type, var, arr)					\
  do { CLONE_ARR_A(type, (var), (arr));					\
       memcpy ((var), (arr), sizeof (type) * ARR_LEN ((arr))); }	\
  while (0)

/**
 * Declare an initialized (zero'ed) array of fixed size.
 * This macro should be used at file scope only.
 *
 * @param type     The element type of the new array.
 * @param var      A lvalue of type (type *) which will hold the new array.
 * @param _nelts   Number of elements in this new array.
 */
#define DECL_ARR_S(type, var, _nelts)					\
  ARR_STRUCT(type, (_nelts) ? (_nelts) : 1) _##var;			\
  type *var = (_ARR_SET_DBGINF (&_##var, ARR_A_MAGIC, sizeof (type)),	\
	       _##var.nelts = _nelts,					\
	       _##var.v.elts)

/**
 * Returns the length of an array
 *
 * @param arr  a flexible, dynamic, automatic or static array.
 */
#define ARR_LEN(arr) (ARR_VRFY ((arr)), _ARR_DESCR((arr))->nelts)

/**
 * Resize a flexible array, allocate more data if needed but do NOT
 * reduce.
 *
 * @param type     The element type of the array.
 * @param arr      The array, which must be an lvalue.
 * @param n        The new size of the array.
 *
 * @remark  This macro may change arr, so update all references!
 */
#define ARR_RESIZE(type, arr, n)					\
  ((arr) = _arr_resize ((arr), (n), sizeof(type)))

/**
 * Resize a flexible array, always reallocate data.
 *
 * @param type     The element type of the array.
 * @param arr      The array, which must be an lvalue.
 * @param n        The new size of the array.
 *
 * @remark  This macro may change arr, so update all references!
 */
#define ARR_SETLEN(type, arr, n)					\
  ((arr) = _arr_setlen ((arr), (n), sizeof(type) * (n)))

/** Set a length smaller than the current length of the array.  Do not
 *  resize. len must be <= ARR_LEN(arr). */
#define ARR_SHRINKLEN(arr,len)                                          \
   (ARR_VRFY ((arr)), assert(_ARR_DESCR((arr))->nelts >= len),             \
    _ARR_DESCR((arr))->nelts = len)

/**
 * Resize a flexible array by growing it by delta elements.
 *
 * @param type     The element type of the array.
 * @param arr      The array, which must be an lvalue.
 * @param delta    The delta number of elements.
 *
 * @remark  This macro may change arr, so update all references!
 */
#define ARR_EXTEND(type, arr, delta)			\
  ARR_RESIZE (type, (arr), ARR_LEN ((arr)) + (delta))

/**
 * Resize a flexible array to hold n elements only if it is currently shorter
 * than n.
 *
 * @param type     The element type of the array.
 * @param arr      The array, which must be an lvalue.
 * @param n        The new size of the array.
 *
 * @remark  This macro may change arr, so update all references!
 */
#define ARR_EXTO(type, arr, n)						\
  ((n) >= ARR_LEN ((arr)) ? ARR_RESIZE (type, (arr), (n)+1) : (arr))

/**
 * Append one element to a flexible array.
 *
 * @param type     The element type of the array.
 * @param arr      The array, which must be an lvalue.
 * @param elt      The new element, must be of type (type).
 */
#define ARR_APP1(type, arr, elt)					\
  (ARR_EXTEND (type, (arr), 1), (arr)[ARR_LEN ((arr))-1] = (elt))

#ifdef NDEBUG
# define ARR_VRFY(arr) ((void)0)
# define ARR_IDX_VRFY(arr, idx) ((void)0)
#else
# define ARR_VRFY(arr)									\
    assert (   (   (_ARR_DESCR((arr))->magic == ARR_D_MAGIC)				\
		|| (_ARR_DESCR((arr))->magic == ARR_A_MAGIC)				\
		|| (_ARR_DESCR((arr))->magic == ARR_F_MAGIC))				\
	    && (   (_ARR_DESCR((arr))->magic != ARR_F_MAGIC)				\
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
# define _ARR_SET_DBGINF(descr, co, es)
#else
# define _ARR_DBGINF_DECL int magic; size_t eltsize;
# define _ARR_SET_DBGINF(descr, co, es)					\
    ( (descr)->magic = (co), (descr)->eltsize = (es) )
#endif

/**
 * Construct an array header.
 */
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

/**
 * The array descriptor header type.
 */
typedef ARR_STRUCT (aligned_type, 1) _arr_descr;

extern _arr_descr arr_mt_descr;

void *_new_arr_f (int, size_t);
void _del_arr_f (void *);
void *_new_arr_d (struct obstack *obstack, int nelts, size_t elts_size);
void *_arr_resize (void *, int, size_t);
void *_arr_setlen (void *, int, size_t);

#define _ARR_ELTS_OFFS offsetof (_arr_descr, v.elts)
#define _ARR_DESCR(elts) ((_arr_descr *)(void *)((char *)(elts) - _ARR_ELTS_OFFS))

/*
 ____             _           _      _
/ ___|  ___  _ __| |_ ___  __| |    / \   _ __ _ __ __ _ _   _ ___
\___ \ / _ \| '__| __/ _ \/ _` |   / _ \ | '__| '__/ _` | | | / __|
 ___) | (_) | |  | ||  __/ (_| |  / ___ \| |  | | | (_| | |_| \__ \
|____/ \___/|_|   \__\___|\__,_| /_/   \_\_|  |_|  \__,_|\__, |___/
                                                         |___/
*/

typedef int (_arr_cmp_func_t)(const void *a, const void *b);

/**
 * Do a binary search in an array.
 * @param arr      The array.
 * @param elm_size The size of an array element.
 * @param cmp      A comparison function for two array elements (see qsort(3) for example).
 * @param elm      A pointer to the element we are looking for.
 * @return         This is somewhat tricky. Let <code>res</code> be the return value.
 *                 If the return value is negative, then <code>elm</code> was not in the array
 *                 but <code>-res - 1</code> gives the proper location where it should be inserted.
 *                 If <code>res >= 0</code> then the element is in the array and <code>res</code>
 *                 represents its index.
 *                 That allows for testing membership and finding proper insertion indices.
 * @note           The differences to bsearch(3) which does not give proper insert locations
 *                 in the case that the element is not conatined in the array.
 */
static INLINE __attribute__((const, unused)) int
_arr_bsearch(const void *arr, size_t elm_size, _arr_cmp_func_t *cmp, const void *elm)
{
	int hi = ARR_LEN(arr);
	int lo = 0;

	while(lo < hi) {
		int md     = lo + ((hi - lo) >> 1);
		int res    = cmp((char *) arr + md * elm_size, elm);
		if(res < 0)
			lo = md + 1;
		else if(res > 0)
			hi = md;
		else
			return md;
	}

	return -(lo + 1);
}

#define ARR_SET_INSERT(arr, cmp, elm) \
do { \
	int idx = _arr_bsearch((arr), sizeof((arr)[0]), (cmp), (elm)); \
	if (idx < 0) { \
		idx = -idx - 1; \
		memmove(&(arr)[idx+1], &(arr)[idx], sizeof((arr)[0]) * (_ARR_DESCR((arr))->nelts - idx)); \
		(arr)[idx] = (elm); \
		++_ARR_DESCR((arr))->nelts; \
	} \
} while(0)

#define ARR_SET_REMOVE(arr, cmp, elm) \
do { \
	int idx = _arr_bsearch((arr), sizeof((arr)[0]), (cmp), (elm)); \
	if (idx >= 0) { \
		--_ARR_DESCR((arr))->nelts; \
		memmove(&(arr)[idx], &(arr)[idx+1], sizeof((arr)[0]) * (_ARR_DESCR((arr))->nelts - idx)); \
	} \
} while(0)

/**
 * Return the index of an element in an array set.
 * To check for containment, use the expression:
 *     (ARR_SET_GET_IDX(arr, cmp, elm) >= 0)
 *
 * @return The index or some value < 0 if the element was not in the set.
 */
#define ARR_SET_GET_IDX(arr, cmp, elm) \
	(ARR_VRFY((arr)), _arr_bsearch((arr), sizeof((arr)[0]), cmp, elm))

#define ARR_SET_CONTAINS(arr, cmp, elm) \
	(ARR_SET_GET_IDX((arr), (cmp), (elm)) >= 0)

/**
 * Reset the array set.
 * This just initializes the size to zero but does not wipe out any element.
 */
#define ARR_SET_CLEAR(arr) ARR_SHRINKLEN(arr, 0)


#endif /* FIRM_ADT_ARRAY_H */
