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

/*
 * Project:     libFIRM
 * File name:   ir/lower/lower_calls.h
 * Purpose:     lowering of Calls with compound parameters
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
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
  ADD_HIDDEN_SMART           = 2    /**< add hidden parameters last for non-variadic and first for variadic functions. */
} add_hidden;

/**
 * Additional flags for the lowering.
 */
enum lowering_flags {
  LF_NONE              = 0, /**< no additional flags */
  LF_COMPOUND_PARAM    = 1, /**< lower calls with compound parameters */
  LF_COMPOUND_RETURN   = 2, /**< lower calls with compound returns */
  LF_RETURN_HIDDEN     = 4, /**< return the hidden address instead of void */
  LF_SMALL_CMP_IN_REGS = 8  /**< return small compound values in registers */
};

/** Maximum number of registers that can be used to return compound values. */
#define MAX_REGISTER_RET_VAL 2

/**
 * A struct containing all control parameters for
 * lower_compound_ret_calls().
 */
typedef struct {
  int        def_ptr_alignment;   /**< Default alignment for data pointer. */
  unsigned   flags;               /**< A bitmask of enum lowering_flags. */
  add_hidden hidden_params;       /**< Where to add hidden parameters. */

  /**
   * A function returning a pointer type for a given type.
   * If this pointer is NULL, a new pointer type is always created.
   */
  ir_type *(*find_pointer_type)(ir_type *e_type, ir_mode *mode, int alignment);

  /**
   * If the LF_SMALL_CMP_IN_REGS flag is set, this function will be called
   * to decide, whether a compound value should be returned in registers.
   * This function must return the number of used registers and fill in the modes
   * of the registers to use. Up to MAX_REGISTER_RET_VAL registers can be used.
   */
  int (*ret_compound_in_regs)(ir_type *compound_tp, ir_mode **modes);
} lower_params_t;

/**
 * Lower calls with compound parameter and return types.
 * This function does the following transformations:
 *
 * If LF_COMPOUND_PARAM is set:
 *
 * - Copy compound parameters to a new location on the callers
 *   stack and transmit the address of this new location
 *
 * If LF_COMPOUND_RETURN is set:
 *
 * - Adds a new (hidden) pointer parameter for
 *   any return compound type. The return type is replaced by void
 *   or if LOWERING_FLAGS_RETURN_HIDDEN is set by the address.
 *
 * - Use of the hidden parameters in the function code.
 *
 * - Change all calls to functions with compound return
 *   by providing space for the hidden parameter on the callers
 *   stack.
 *
 * - Replace a possible block copy after the function call.
 *
 * General:
 *
 * - Changes the types of methods and calls to the lowered ones
 *
 * - lower all method types of existing entities
 *
 * In pseudo-code, the following transformation is done:
 *
   @code
   struct x ret = func(a, b);
   @endcode
 *
 * is translated into
   @code
   struct x ret;
   func(&ret, a, b);
   @endcode
 *
 * If the function returns only one possible result, the copy-on-return
 * optimization is done, ie.
   @code
   struct x func(a) {
     struct x ret;
     ret.a = a;
     return ret;
   }
   @endcode
 *
 * is transformed into
 *
   @code
   void func(struct x *ret, a) {
     ret->a = a;
   }
   @endcode
 *
 * @param params  A structure containing the control parameter for this
 *                transformation.
 *
 * During the transformation, pointer types must be created or reused.
 * The caller can provide params->find_pointer_type for this task to
 * reduce the number of created pointer types.
 * If params->find_pointer_type is NULL, new pointer types
 * are always created automatically.
 */
void lower_calls_with_compounds(const lower_params_t *params);

#endif /* _LOWER_CALLS_H_ */
