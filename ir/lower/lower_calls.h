/*
 * Project:     libFIRM
 * File name:   ir/lower/lower_calls.h
 * Purpose:     lowering of Calls with compound parameters
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file lower_calls.h
 *
 * Lowering of Calls with compound return types.
 *
 * @author Michael Beck
 */
#ifndef _LOWER_CALLS_H_
#define _LOWER_CALLS_H_

/**
 * A type telling where to add hidden parameters.
 */
typedef enum add_hidden_params {
  ADD_HIDDEN_ALWAYS_IN_FRONT = 0,   /**< always add hidden parameters in front (default). */
  ADD_HIDDEN_ALWAYS_LAST     = 1,   /**< always add hidden parameters last, did not work for variadic functions. */
  ADD_HIDDEN_SMART           = 2,   /**< add hidden parameters last for non-variadic and first for variadic functions. */
} add_hidden;

typedef struct {
  int        def_ptr_alignment;   /**< default alignment for data pointer */
  add_hidden hidden_params;       /**< where to add hidden params. */

  /** a function returning a pointer type for a given type */
  ir_type *(*find_pointer_type)(ir_type *e_type, ir_mode *mode, int alignment);
} lower_params_t;

/**
 * Lower calls with compound return types.
 * This function does the following transformations:
 *
 * - Adds a new (hidden) pointer parameter for
 *   any return compound type.
 *
 * - Use of the hidden parameters in the function code.
 *
 * - Change all calls to functions with compound return
 *   by providing space for the hidden parameter on the callers
 *   stack.
 *
 * - Replace a possible block copy after the function call.
 */
void lower_compound_ret_calls(const lower_params_t *params);

#endif /* _LOWER_CALLS_H_ */
