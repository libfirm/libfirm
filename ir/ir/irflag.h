/*
 * Project:     libFIRM
 * File name:   ir/ir/irflag.h
 * Purpose:     Flags to control optimizations.
 * Author:      Christian Schaefer, Goetz Lindenmaier
 * Modified by: Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file irflag.h
 *
 * Flags to customize the behaviour of libfirm.
 *
 * @author Christian Schaefer
 *
 * There are the following groups of flags:
 * 1. Optimization flags.
 *    a)  There is a flag, 'optimize' to turn on/off all optimizations.
 *    b)  There are flags for each individual optimization.  Some flags turns
 *        transformations in several algorithms on/off.
 * 2. Normalization flags.
 *    These flags steer transformations of the ir that improve it, as removing
 *    dump Phi nodes (one predecessor, all preds are equal ...), Ids, Tuples ...
 * 3. Verbosity flags.
 *    a) Flags to steer the level of the information.
 *    b) Flags to steer in which phase information should be dumped.
 *
 */

#ifndef _IRFLAG_H_
#define _IRFLAG_H_

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
void set_optimize (int value);
int  get_optimize(void);

/** This function enables/disables output of information about phases and
 *  controls the verbosity level.
 *
 *  0: no output at all.
 *  1: very short output
 *  >>1: very verbose output.
 */
void set_firm_verbosity (int value);
int  get_firm_verbosity (void);

/** Enables/Disables constant folding optimization.
 *
 *  If opt_constant_folding == 1 perform
 *  - constant expression evaluation (2 + 5 ==> 7, 3 < 2 ==> false)
 *  - algebraic simplification  (a * 0 ==> 0, a or a ==> a)
 *  - simplification of tests   ( !(a < b) ==> (a >= b))
 *  - refining the memory representation
 *  - remove store after load
 * Default: opt_constant_folding == 1.
 */
void set_opt_constant_folding (int value);

/** Enables/Disables loop unrolling.
 *
 * If opt_loop_unrolling == 1 perform loop_unrolling.
 * See loop_unrolling.h.
 *
 * Default: opt_loop_unrolling = 1;
 */
void set_opt_loop_unrolling (int value);

/** Enables/Disables output of information about loop unrolling.
 */
void set_opt_loop_unrolling_verbose (int value);

/** Enables/Disables removal of redundant Loads and Stores.
 *
 *  - Remove Store that overwrites a just stored value (WAW).
 *  - Remove Store if it stores a value just loaded (WAR with the same value).
 *  - Remove Load that loads a value just saved (RAW with the same value).
 *  - remove Load that loads a value already loaded (RAR)
 *  - replace Load of constant values with constants (RC)
 */
void set_opt_redundant_LoadStore(int value);

/** Enables/Disables constant subexpression elimination.
 *
 * If opt_cse == 1 perform constant subexpression elimination.
 * Default: opt_cse == 1.
 */
void set_opt_cse (int value);

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
void set_opt_global_cse (int value);

/** Enables/Disables strength reduction.
 *
 * If opt_strength_red == 1 perform strength reduction.
 * See strenth_red.h.
 *
 * Default: opt_strength_red = 1;
 */
void set_opt_strength_red (int value);

/** Enables/Disables output of information about strength reduction.
 */
void set_opt_strength_red_verbose (int value);

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

/** Enables/Disables removal of critical control flow edges. */
void set_opt_critical_edges(int value);

/** Enables/Disables reassociation.
 *
 * If opt_reassociation == 1 reassociation is performed.
 * Default: opt_reassociation == 1.
 */
void set_opt_reassociation(int value);

/** Enables/Disables dead node elimination.
 *
 * If opt_dead_node_elimination == 1 deallocate all dead nodes
 * by copying the firm graph.
 * Default: opt_dead_node_elimination == 1. */
void set_opt_dead_node_elimination (int value);

/** Enables/Disables dead method elimination.
 *
 * If opt_dead_method_elimination == 1 methods never called are
 * removed.
 * Default: opt_dead_method_elimination == 1. */
void set_opt_dead_method_elimination (int value);
void set_opt_dead_method_elimination_verbose (int value);

/** Enable/Disables inlining.
 *
 * If opt_inline == 1 the inlining transformation is performed.
 */
void set_opt_inline (int value);

/** Enable/Disable optimization of dynamic method dispatch
 *
 * This flag enables/disables the optimization of dynamic method dispatch.
 * If the flag is turned on Sel nodes can be replaced by Const nodes representing
 * the address of a function.
 */
void set_opt_dyn_meth_dispatch (int value);

/** Enable/Disable optimization of tail-recursion calls.
 *
 * This flag enables/disables the optimization tail-recursion call.
 * If the flag is turned on tail-recursion calls are optimized into loops.
 */
void set_opt_tail_recursion(int value);
void set_opt_tail_recursion_verbose(int value);

/** Enable/Disable floating of fragile ops.
 *
 * This flags enables/disables the floating of fragile operations.
 * If this flag is on, fragile operations which are known to NOT raise
 * an exception can be place to other basic blocks.
 * Otherwise they remain in the block they were created.
 */
void set_opt_fragile_ops(int value);

/**
 * Enable/Disable if conversion.
 *
 * If conversion tries to turn Conds into Mux nodes to eliminate
 * control flow.
 */
void set_opt_if_conversion(int value);

/** Enable/Disable normalizations of the firm representation.
 *
 *  This flag guards transformations that normalize the firm representation
 *  as removing Ids and Tuples, useless Phis, replacing SymConst(id) by
 *  Const(entity) and others.
 *  The transformations guarded by this flag are not guarded by flag
 *  "optimize".
 *  Many algorithms operating on firm can not deal with constructs in
 *  the non-normalized representation.
 *  default: 1
 *  @@@ ATTENTION: not all such transformations are guarded by a flag.
 */
void set_opt_normalize (int value);


/** Enable/Disable precise exception context. */
void set_opt_precise_exc_context(int value);

/**
 * Save the current optimization state.
 */
void save_optimization_state(optimization_state_t *state);

/**
 * Restore the current optimization state.
 */
void restore_optimization_state(const optimization_state_t *state);

#endif
