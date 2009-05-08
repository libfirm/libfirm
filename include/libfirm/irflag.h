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
 * @version $Id$
 * @summary
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
void set_optimize(int value);
int  get_optimize(void);

/** Enables/Disables constant folding optimization.
 *
 *  If opt_constant_folding == 1 perform
 *  constant expression evaluation (2 + 5 ==> 7, 3 < 2 ==> false)
 * Default: opt_constant_folding == 1.
 */
void set_opt_constant_folding(int value);

/** Enables/Disables algebraic simplifications.
 *
 *  If opt_algebraic_simplification == 1 perform
 *  - algebraic simplification  (a * 0 ==> 0, a or a ==> a)
 *  - simplification of tests   ( !(a < b) ==> (a >= b))
 * Default: opt_algebraic_simplification == 1.
 */
void set_opt_algebraic_simplification(int value);

/** Enables/Disables common subexpression elimination.
 *
 * If opt_cse == 1 perform common subexpression elimination.
 * Default: opt_cse == 1.
 */
void set_opt_cse(int value);

/** Returns constant folding optimization setting. */
int get_opt_cse(void);

/** Enables/Disables global constant subexpression elimination.
 *
 * If opt_global_cse == 1 and opt_cse == 1 perform intra procedure
 * constant subexpression elimination for floating nodes.  Intra
 * procedure cse gets the graph into state "floating".  It is necessary
 * to run pre/code motion to get the graph back into state "op_pin_state_pinned".
 * right after a call to local_optimize with global cse turned on.
 * Default: opt_global_cse == 0.
 */
void set_opt_global_cse(int value);

/** Enables/Disables usage of combo algorithm.
 *
 *  If opt_combo == 1 perform combo optimization
 *  instead of combinations of optimiza_graph_df()/
 *  optimize_graph_cf()
 * Default: opt_combo == 1.
 */
void set_opt_combo(int value);

/** Enables/Disables strength reduction.
 *
 * If opt_strength_red == 1 perform strength reduction.
 * See strenth_red.h.
 *
 * Default: opt_strength_red = 1;
 */
void set_opt_strength_red(int value);

/** Enables/Disables unreachable code elimination.
 *
 * If set, evaluate conditions of conditional branch and replace the
 * branch with a Jmp/Bad Tuple.
 *
 * If opt_unreachable_code == 1 replace nodes (except Block,
 * Phi and Tuple) with a Bad predecessor by the Bad node.
 * Default: opt_unreachable_code == 1.
 */
void set_opt_unreachable_code(int value);

/** Enables/Disables control flow optimizations.
 *
 * Performs Straightening, if simplifications and loop simplifications.
 * Sets all separate control flow flags (control_flow_straightening,
 * weak_simplification, strong_simplification and critical_edges).
 */
void set_opt_control_flow(int value);

/** Enables/Disables Straightening. */
void set_opt_control_flow_straightening(int value);

/** Enables/Disables if simplifications in local optimizations. */
void set_opt_control_flow_weak_simplification(int value);

/** Enables/Disables strong if and loop simplification (in optimize_cf). */
void set_opt_control_flow_strong_simplification(int value);

/** Enable/Disable optimization of dynamic method dispatch.
 *
 * This flag enables/disables the optimization of dynamic method dispatch.
 * If the flag is turned on Sel nodes can be replaced by Const nodes representing
 * the address of a function.
 */
void set_opt_dyn_meth_dispatch(int value);
int  get_opt_dyn_meth_dispatch(void);

/** Enable/Disable type optimization of cast nodes.
 *
 * Controls the optimizations in tropt.h.  Default: on.
 */
void set_opt_optimize_class_casts(int value);

/** Restricts the behavior of cast optimization.
 *
 *  If set, downcast are not optimized if they might be
 *  illegal as in (Super)(Sub) (new Super()).  Default:
 *  0 == not suppressed.
 */
void set_opt_suppress_downcast_optimization(int value);
int  get_opt_suppress_downcast_optimization(void);

/** Enable/Disable floating of fragile ops.
 *
 * This flags enables/disables the floating of fragile operations.
 * If this flag is on, fragile operations which are known to NOT raise
 * an exception can be place to other basic blocks.
 * Otherwise they remain in the block they were created.
 */
void set_opt_fragile_ops(int value);

/**
 * Enable/Disable Confirm node removal during local optimization.
 */
void set_opt_remove_confirm(int value);

/**
 * Enable/Disable scalar replacement optimization.
 */
void set_opt_scalar_replacement(int value);

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
void set_opt_ldst_only_null_ptr_exceptions(int value);

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
void set_opt_sel_based_null_check_elim(int value);

/**
 * Enable/Disable Global Null Pointer Test Elimination.
 *
 * In languages where it is illegal to dereference NULL pointer, doing
 * so makes the pointer "valid non-null", else the program will stop
 * anyway by a fault.
 *
 * This flag should be set for C style languages.
 */
void set_opt_global_null_ptr_elimination(int value);

/**
 * Enable/Disable Automatic construction of Sync nodes during
 * Firm construction.
 *
 * If this flags is set, sequential non-volatile Loads are automatically
 * rearranged so that they can be executed in parallel by creating Sync nodes.
 *
 * This flag should be set for Java style languages.
 */
void set_opt_auto_create_sync(int value);

/** Enable/Disable normalizations of the firm representation.
 *
 *  This flag guards transformations that normalize the Firm representation
 *  as removing Ids and Tuples, useless Phis, replacing SymConst(id) by
 *  Const(entity) and others.
 *  The transformations guarded by this flag are not guarded by flag
 *  "optimize".
 *  Many algorithms operating on Firm can not deal with constructs in
 *  the non-normalized representation.
 *  default: ON
 *
 *  @note ATTENTION: not all such transformations are guarded by a flag.
 */
void set_opt_normalize(int value);

/**
 * Enable/Disable ConvB() nodes with a "semantic behavior", i.e. a real
 * operation that must be executed.
 */
void set_opt_allow_conv_b(int value);

/** Enable/Disable precise exception context.
 *
 * If enabled, all exceptions form a barrier for values, as in the
 * following example:
 *
 * @code
 * a = 1;
 * b = 3 / 0;
 * a = 2;
 * @endcode
 *
 * If precise exception handling is enabled, an exception handler see a == 1,
 * else it might see a == 2.
 * Enable this for languages with strict exception order like Java.
 */
void set_opt_precise_exc_context(int value);

/** Enable/Disable Alias analysis.
 *
 * If enabled, memory disambiguation by alias analysis is used.
 */
void set_opt_alias_analysis(int value);

/** Enable/Disable closed world assumption.
 *
 * If enabled, optimizations expect to know the "whole world", i.e. no
 * external types or callers exist.
 * This enables some powerful optimizations.
 */
void set_opt_closed_world(int value);

/**
 * Save the current optimization state.
 */
void save_optimization_state(optimization_state_t *state);

/**
 * Restore the current optimization state.
 */
void restore_optimization_state(const optimization_state_t *state);

/**
 * Switches ALL optimizations off.
 */
void all_optimizations_off(void);

/**
 * Possible verification modes.
 */
typedef enum _firm_verification_t {
  FIRM_VERIFICATION_OFF        = 0,	/**< do not verify nodes at all */
  FIRM_VERIFICATION_ON         = 1,	/**< do node verification and assert on error in debug version */
  FIRM_VERIFICATION_REPORT     = 2,	/**< do node verification, but report to stderr only */
  FIRM_VERIFICATION_ERROR_ONLY = 3	/**< do node verification, but NEVER do assert nor report */
} firm_verification_t;

/** Select verification of IR nodes and types.
 *
 *  Per default the  verification is in mode NODE_VERIFICATION_ASSERT.
 *  Turn the verification off during development to check partial implementations.
 */
void do_node_verification(firm_verification_t mode);

#endif
