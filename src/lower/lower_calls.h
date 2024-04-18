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

#include <stdbool.h>

#include "firm_types.h"

/*
 * Note: This API is designed with future support for ARM and MIPS in
 * mind. Their calling conventions require two further features:
 *
 * - Passing part of a struct in registers and part in memory.
 *
 * - Passing dynamically-sized objects via pointers, but
 *   statically-sized objects directly on stack.
 */

/**
 * The maximum number of registers used to split up an aggregate type
 * across all ABIs.
 */
#define MAX_REGS_PER_AGGREGATE 2

/**
 * Additional flags for the lowering.
 */
typedef enum compound_call_lowering_flags {
	LF_NONE                 = 0,      /**< no additional flags */
	LF_RETURN_HIDDEN        = 1 << 0, /**< return the hidden address instead of void */
} compound_call_lowering_flags;
ENUM_BITSET(compound_call_lowering_flags)

/**
 * Specifies how an argument or result should be passed. Each argument
 * or result may be split into multiple values, which will become
 * registers in the backend. The array 'modes' holds the mode of each
 * of these values. Two modes have a special meaning. If these are
 * used, they must be the only element of the array.
 *
 * mode_M: The lowering phase does not change the function's type, and
 * provides the argument as-is. The backend is responsible for
 * managing the actual stack. mode_M may not be used for return
 * values.
 *
 * mode_P: The argument or return value should be copied into
 * temporary memory by the caller, and a pointer to this temporary
 * memory should be passed in its stead.
 *
 * When mode_P is used for a return value, the pointer to the
 * temporary memory is transmitted as a hidden first argument, and
 * also returned from the function is LF_RETURN_HIDDEN is set.
 *
 * 'length' holds the number of values or special modes used for the
 * argument or result.
 */
typedef struct aggregate_spec_t {
	unsigned  length;
	ir_mode  *modes[MAX_REGS_PER_AGGREGATE];
} aggregate_spec_t;

/**
 * Callback to decide how the specified type should be passed to or
 * returned by a function. env points to a piece of backend-specific
 * data, which is the env argument of lower_calls_with_compounds for
 * the first callback, and may be updated by the callback.
 *
 * In order to support stateful ABIs, which need to know about every
 * parameter, this function is also called for non-compound types. It
 * should then return an aggregate_spec_t with length = 1 and
 * modes[0] = get_type_mode(type).
 */
typedef aggregate_spec_t (*lower_call_func)(void *env, ir_type const *type);

/**
 * Callback to reset the state of the ABI for the next function.
 */
typedef void (*reset_abi_state_func)(void *param_env, void *result_env);

/**
 * Lower calls with compound parameter and return types.
 * This function does the following transformations:
 *
 * - Transform compound results according to the aggregate_spec_t
 *   returned from the backend. If necessary, insert a hidden
 *   parameter.
 *
 * - Transform compound parameters according to the
 *   aggregate_spec_t returned from the backend.
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
 */
void lower_calls_with_compounds(compound_call_lowering_flags flags,
                                lower_call_func lower_parameter,
                                void *lower_parameter_env,
                                lower_call_func lower_return,
                                void *lower_return_env,
                                reset_abi_state_func reset_state);

/**
 * Lower aggregate types by replacing them with a pointer to the
 * actual data. The caller allocates space for this data in its
 * frame. This is firm's old default behaviour. This implements the
 * SPARC ABI.
 *
 * This function can be used as a provisional ABI for unfinished
 * backends. However, these backends will only be able to link to
 * other code produced by firm.
 *
 * This function is stateless and does not require an environment (you
 * may pass NULL).
 */
aggregate_spec_t lower_aggregates_as_pointers(void *env, ir_type const *type);

/**
 * Do not lower aggregate types. This may only be used to lower
 * parameters, return values must always be lowered. When using this
 * function, the call lowering does not change aggregate parameters,
 * and the backend is responsible for correctly handling them.
 *
 * This function is stateless and does not require an environment (you
 * may pass NULL).
 */
aggregate_spec_t dont_lower_aggregates(void *env, ir_type const *type);

/**
 * This function may be passed as the reset_state argument for a
 * stateless ABI. It ignores its arguments.
 */
void reset_stateless_abi(void *param_env, void *result_env);

#endif
