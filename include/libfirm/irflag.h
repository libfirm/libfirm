/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Flags to control optimizations.
 * @author  Christian Schaefer, Goetz Lindenmaier, Michael Beck
 */
#ifndef FIRM_IR_IRFLAG_H
#define FIRM_IR_IRFLAG_H

#include "firm_types.h"

#include "begin.h"

/**
 * @ingroup iroptimize
 * @defgroup Optimization Flags
 * Flags to customize the behavior of libfirm.
 *
 * There are the following groups of flags:
 * -# Optimization flags.
 *    -#  There is a flag, 'optimize' to turn on/off all optimizations.
 *    -#  There are flags for each individual optimization.  Some flags turns
 *        transformations in several algorithms on/off.
 * -# Normalization flags.
 *    These flags steer transformations of the ir that improve it, as removing
 *    dump Phi nodes (one predecessor, all predecessors are equal ...), Ids, Tuples ...
 * -# Verbosity flags.
 *    -# Flags to steer the level of the information.
 *    -# Flags to steer in which phase information should be dumped.
 *@{
 */

/**
 * A container type to load/restore all optimizations
 */
typedef unsigned optimization_state_t;

/**
 * This function enables/disables optimizations globally.
 *
 * If optimize == 0 no optimizations are performed at all.
 * Default: optimize == 1.
 */
FIRM_API void set_optimize(int value);
/** Returns global optimizations flag.
 * @see set_optimize() */
FIRM_API int get_optimize(void);

/** Enables/Disables constant folding optimization.
 *
 *  If opt_constant_folding == 1 perform
 *  constant expression evaluation (2 + 5 ==> 7, 3 < 2 ==> false)
 * Default: opt_constant_folding == 1.
 */
FIRM_API void set_opt_constant_folding(int value);
/** returns 0 if constant_folding is disabled, !=0 otherwise */
FIRM_API int get_opt_constant_folding(void);

/** Enables/Disables algebraic simplifications.
 *
 *  If opt_algebraic_simplification == 1 perform
 *  - algebraic simplification  (a * 0 ==> 0, a or a ==> a)
 *  - simplification of tests   ( !(a < b) ==> (a >= b))
 * Default: opt_algebraic_simplification == 1.
 */
FIRM_API void set_opt_algebraic_simplification(int value);

/** Returns algebraic simplification setting. */
FIRM_API int get_opt_algebraic_simplification(void);

/** Enables/Disables common subexpression elimination.
 *
 * If opt_cse == 1 perform common subexpression elimination.
 * Default: opt_cse == 1.
 */
FIRM_API void set_opt_cse(int value);

/** Returns constant folding optimization setting. */
FIRM_API int get_opt_cse(void);

/** Enables/Disables global constant subexpression elimination.
 *
 * If opt_global_cse == 1 and opt_cse == 1 perform intra procedure
 * constant subexpression elimination for floating nodes.  Intra
 * procedure cse gets the graph into state "floating".  It is necessary
 * to run pre/code motion to get the graph back into state "op_pin_state_pinned".
 * right after a call to local_optimize with global cse turned on.
 * Default: opt_global_cse == 0.
 */
FIRM_API void set_opt_global_cse(int value);

/** Returns global constant subexpression elimination setting. */
FIRM_API int get_opt_global_cse(void);

/**
 * Enable/Disable Global Null Pointer Test Elimination.
 *
 * In languages where it is illegal to dereference NULL pointer, doing
 * so makes the pointer "valid non-null", else the program will stop
 * anyway by a fault.
 *
 * This flag should be set for C style languages.
 */
FIRM_API void set_opt_global_null_ptr_elimination(int value);

/** Returns global null pointer test elimination setting. */
FIRM_API int get_opt_global_null_ptr_elimination(void);

/**
 * Save the current optimization state.
 */
FIRM_API void save_optimization_state(optimization_state_t *state);

/**
 * Restore the current optimization state.
 */
FIRM_API void restore_optimization_state(const optimization_state_t *state);

/**
 * Switches ALL optimizations off.
 */
FIRM_API void all_optimizations_off(void);

/** @} */

#include "end.h"

#endif
