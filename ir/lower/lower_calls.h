/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Lowering of calls with compound arguments
 * @author  Michael Beck
 */
#ifndef FIRM_LOWER_CALLS_H
#define FIRM_LOWER_CALLS_H

#include "firm_types.h"

/**
 * Additional flags for the lowering.
 */
typedef enum compound_call_lowering_flags {
	LF_NONE                 = 0,      /**< no additional flags */
	LF_RETURN_HIDDEN        = 1 << 0, /**< return the hidden address instead of void */
	LF_DONT_LOWER_ARGUMENTS = 1 << 1, /**< don't lower compound call arguments
	                                       (some backends can handle them themselves) */
	/** Return and pass small structs in registers according to
	 * the AMD64 ABI */
	LF_AMD64_ABI_STRUCTS    = 1 << 2,
} compound_call_lowering_flags;
ENUM_BITSET(compound_call_lowering_flags)

/**
 * Specify how exactly a compound type should be returned in values (which will
 * probably be mapped to registers in the backend).
 * For now this is specified as an array of ir_modes. The complete struct is
 * decomposed into these values. So the sum of all mode sizes must equal the
 * struct size.
 */
typedef struct aggregate_spec_t {
	unsigned        n_values;
	ir_mode *const *modes;
} aggregate_spec_t;

extern aggregate_spec_t const no_values_aggregate_spec;

/**
 * Callback to decide how the specified struct should be returned by a
 * function.
 */
typedef aggregate_spec_t const* (*decide_aggregate_ret_func)
	(ir_type const *type);

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
 */
void lower_calls_with_compounds(compound_call_lowering_flags flags,
                                decide_aggregate_ret_func func);

#endif
