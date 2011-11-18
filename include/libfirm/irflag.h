/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief   Flags to control optimizations.
 * @author  Christian Schaefer, Goetz Lindenmaier, Michael Beck
 * @brief
 * Flags to customize the behavior of libfirm.
 *
 * There are the following groups of flags:
 * 1. Optimization flags.
 *    a)  There is a flag, 'optimize' to turn on/off all optimizations.
 *    b)  There are flags for each individual optimization.  Some flags turns
 *        transformations in several algorithms on/off.
 * 2. Normalization flags.
 *    These flags steer transformations of the ir that improve it, as removing
 *    dump Phi nodes (one predecessor, all predecessors are equal ...), Ids, Tuples ...
 * 3. Verbosity flags.
 *    a) Flags to steer the level of the information.
 *    b) Flags to steer in which phase information should be dumped.
 * 4. Verification flag
 *    This one controls the behavior of node and type verifications
 */
#ifndef FIRM_IR_IRFLAG_H
#define FIRM_IR_IRFLAG_H

#include "firm_types.h"
#include "begin.h"

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
FIRM_API int get_optimize(void);

/** Enables/Disables constant folding optimization.
 *
 *  If opt_constant_folding == 1 perform
 *  constant expression evaluation (2 + 5 ==> 7, 3 < 2 ==> false)
 * Default: opt_constant_folding == 1.
 */
FIRM_API void set_opt_constant_folding(int value);

/** Enables/Disables algebraic simplifications.
 *
 *  If opt_algebraic_simplification == 1 perform
 *  - algebraic simplification  (a * 0 ==> 0, a or a ==> a)
 *  - simplification of tests   ( !(a < b) ==> (a >= b))
 * Default: opt_algebraic_simplification == 1.
 */
FIRM_API void set_opt_algebraic_simplification(int value);

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

/** Restricts the behavior of cast optimization.
 *
 *  If set, downcast are not optimized if they might be
 *  illegal as in (Super)(Sub) (new Super()).  Default:
 *  0 == not suppressed.
 */
FIRM_API void set_opt_suppress_downcast_optimization(int value);
FIRM_API int get_opt_suppress_downcast_optimization(void);

/**
 * Enable/Disable Null exception in Load and Store nodes only.
 *
 * If enabled, only Null pointer exception can occur at Load and
 * store nodes. If it can be proved that the address input of these
 * nodes is non-null, the exception edge can safely be removed.
 * If disabled, other exceptions (like unaligned access, read-only memory,
 * etc.) can occur.
 *
 * This flag is enabled by default.
 */
FIRM_API void set_opt_ldst_only_null_ptr_exceptions(int value);

/**
 * Enable/Disable Selection based Null pointer check elimination.
 *
 * In languages, where all addresses are always Sel nodes, Null
 * pointers can only occur as input to Sel nodes.
 * If Null pointers are the only source for exceptions in Load and
 * Store nodes (as typical in high level languages), we can eliminate
 * exception edges from Load and Store when can prove that the Sel
 * nodes representing the Load/Store address have non-null inputs.
 * Enabling this flag enables this elimination.
 *
 * Enabling this flag is meaningless if ldst_non_null_exceptions is
 * enabled.
 *
 * This flag should be set for Java style languages.
 */
FIRM_API void set_opt_sel_based_null_check_elim(int value);

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

/**
 * Enable/Disable Automatic construction of Sync nodes during
 * Firm construction.
 *
 * If this flags is set, sequential non-volatile Loads are automatically
 * rearranged so that they can be executed in parallel by creating Sync nodes.
 *
 * This flag should be set for Java style languages.
 */
FIRM_API void set_opt_auto_create_sync(int value);

/** Enable/Disable Alias analysis.
 *
 * If enabled, memory disambiguation by alias analysis is used.
 */
FIRM_API void set_opt_alias_analysis(int value);

/** Enable/Disable closed world assumption.
 *
 * If enabled, optimizations expect to know the "whole world", i.e. no
 * external types or callers exist.
 * This enables some powerful optimizations.
 */
FIRM_API void set_opt_closed_world(int value);

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

/**
 * Possible verification modes.
 */
typedef enum firm_verification_t {
  FIRM_VERIFICATION_OFF        = 0, /**< do not verify nodes at all */
  FIRM_VERIFICATION_ON         = 1, /**< do node verification and assert on error in debug version */
  FIRM_VERIFICATION_REPORT     = 2, /**< do node verification, but report to stderr only */
  FIRM_VERIFICATION_ERROR_ONLY = 3  /**< do node verification, but NEVER do assert nor report */
} firm_verification_t;

/**
 * Select verification of IR nodes and types.
 *
 * Per default the  verification is in mode NODE_VERIFICATION_ASSERT.
 * Turn the verification off during development to check partial
 * implementations.
 */
FIRM_API void do_node_verification(firm_verification_t mode);

#include "end.h"

#endif
