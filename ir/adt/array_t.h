/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Array --- dynamic & flexible arrays.
 */
#ifndef FIRM_ADT_ARRAY_T_H
#define FIRM_ADT_ARRAY_T_H

#include "array.h"
#include "fourcc.h"

#define ARR_D_MAGIC FOURCC('A','R','R','D')
#define ARR_A_MAGIC FOURCC('A','R','R','A')
#define ARR_F_MAGIC FOURCC('A','R','R','F')

#ifdef NDEBUG
# define ARR_SET_DBGINF(descr, co)
#else
# define ARR_SET_DBGINF(descr, co) \
    ((descr)->magic = (co))
#endif

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
#define NEW_ARR_A(type, var, n)                                    \
  do {                                                             \
    size_t nelts = (n);                                            \
    (var) = (type *)((ir_arr_descr *)alloca(ARR_ELTS_OFFS + sizeof(type) * nelts))->elts; \
    ARR_SET_DBGINF(ARR_DESCR ((var)), ARR_A_MAGIC); \
    (void)(ARR_DESCR((var))->nelts = nelts);                       \
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
#define CLONE_ARR_A(type, var, arr)        \
  NEW_ARR_A(type, (var), ARR_LEN((arr)))

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
#define DUP_ARR_A(type, var, arr)                               \
  do { CLONE_ARR_A(type, (var), (arr));                         \
       memcpy((var), (arr), sizeof (type) * ARR_LEN((arr))); }  \
  while (0)

#endif
