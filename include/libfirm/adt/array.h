/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief     Dynamic and flexible arrays for C.
 * @author    Markus Armbruster, Michael Beck, Matthias Braun, Sebastian Hack
 */
#ifndef FIRM_ADT_ARRAY_H
#define FIRM_ADT_ARRAY_H

#include <assert.h>
#include <stddef.h>

#include "obst.h"

#include "../begin.h"

/**
 * @ingroup adt
 * @defgroup array Arrays
 * @{
 */

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
#define NEW_ARR_F(type, nelts) \
  ((type *)ir_new_arr_f((nelts), sizeof(type) * (nelts)))

/**
 * Create a flexible array and null its contents.
 */
#define NEW_ARR_FZ(type, nelts) \
	((type*)memset(NEW_ARR_F(type, (nelts)), 0, sizeof(type) * (nelts)))

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
#define DUP_ARR_F(type, arr) \
  ((type*)memcpy(NEW_ARR_F(type, ARR_LEN((arr))), (arr), sizeof(type) * ARR_LEN((arr))))

/**
 * Delete a flexible array.
 *
 * @param arr    The flexible array.
 */
#define DEL_ARR_F(arr) (ir_del_arr_f((void *)(arr)))

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
#define NEW_ARR_D(type, obstack, nelts)                                 \
  (  nelts                                                              \
   ? (type *)ir_new_arr_d((obstack), (nelts), sizeof(type) * (nelts))   \
   : (type *)arr_mt_descr.elts)

/**
 * Create a dynamic array on an obstack and null its contents.
 */
#define NEW_ARR_DZ(type, obstack, nelts) \
	((type*)memset(NEW_ARR_D(type, (obstack), (nelts)), 0, sizeof(type) * (nelts)))

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
#define DUP_ARR_D(type, obstack, arr) \
  ((type*)memcpy(NEW_ARR_D(type, (obstack), ARR_LEN((arr))), (arr), sizeof(type) * ARR_LEN ((arr))))

/**
 * Returns the length of an array
 *
 * @param arr  a flexible, dynamic, automatic or static array.
 */
#define ARR_LEN(arr) (ARR_VRFY((arr)), ARR_DESCR((arr))->nelts)

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
#define ARR_RESIZE(type, arr, n) \
  ((arr) = (type*) ir_arr_resize((void *)(arr), (n), sizeof(type)))

/**
 * Resize a flexible array, always reallocate data.
 *
 * @param type     The element type of the array.
 * @param arr      The array, which must be an lvalue.
 * @param n        The new size of the array.
 *
 * @remark  This macro may change arr, so update all references!
 */
#define ARR_SETLEN(type, arr, n) \
  ((arr) = (type*) ir_arr_setlen((void *)(arr), (n), sizeof(type) * (n)))

/**
 * Resize a flexible array by growing it by delta elements.
 *
 * @param type     The element type of the array.
 * @param arr      The array, which must be an lvalue.
 * @param delta    The delta number of elements.
 *
 * @remark  This macro may change arr, so update all references!
 */
#define ARR_EXTEND(type, arr, delta) \
  ARR_RESIZE(type, (arr), ARR_LEN((arr)) + (delta))

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
#define ARR_EXTO(type, arr, n) \
	do { \
		if ((n) >= ARR_LEN(arr)) { ARR_RESIZE(type, arr, (n)+1); } \
	} while(0)

/**
 * Append one element to a flexible array.
 *
 * @param type     The element type of the array.
 * @param arr      The array, which must be an lvalue.
 * @param elt      The new element, must be of type (type).
 */
#define ARR_APP1(type, arr, elt) \
  (ARR_EXTEND(type, (arr), 1), (arr)[ARR_LEN((arr))-1] = (elt))

#ifdef NDEBUG
# define ARR_VRFY(arr)          ((void)0)
# define ARR_IDX_VRFY(arr, idx) ((void)0)
#else
/** Check array for consistency */
# define ARR_VRFY(arr)          ir_verify_arr(arr)
/** Check if index is within array bounds */
# define ARR_IDX_VRFY(arr, idx) \
    assert((0 <= (idx)) && ((idx) < ARR_LEN((arr))))
#endif

/** @cond PRIVATE */

/** A type that has most constrained alignment.  */
typedef union {
  long double d;
  void *p;
  long l;
} aligned_type;

/**
 * The array descriptor header type.
 */
typedef struct {
	int magic;                    /**< array magic. */
	size_t allocated;         /**< number of allocated elements. */
	size_t nelts;                 /**< current length of the array. */
	aligned_type elts[1];         /**< start of the array data. */
} ir_arr_descr;

extern ir_arr_descr arr_mt_descr;

FIRM_API void *ir_new_arr_f(size_t nelts, size_t elts_size);
FIRM_API void ir_del_arr_f(void *elts);
FIRM_API void *ir_new_arr_d(struct obstack *obstack, size_t nelts, size_t elts_size);
FIRM_API void *ir_arr_resize(void *elts, size_t nelts, size_t elts_size);
FIRM_API void *ir_arr_setlen(void *elts, size_t nelts, size_t elts_size);
FIRM_API void ir_verify_arr(const void *elts);

#define ARR_ELTS_OFFS offsetof(ir_arr_descr, elts)
#define ARR_DESCR(elts) ((ir_arr_descr *)(void *)((char *)(elts) - ARR_ELTS_OFFS))

/** Set a length smaller than the current length of the array.  Do not
 *  resize. len must be <= ARR_LEN(arr). */
static inline void ARR_SHRINKLEN(void *arr, size_t new_len)
{
	ARR_VRFY(arr);
	assert(ARR_DESCR(arr)->nelts >= new_len);
	ARR_DESCR(arr)->nelts = new_len;
}

/** @endcond */

/** @} */

#include "../end.h"

#endif
