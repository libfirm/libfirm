/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Available Optimizations of libFirm.
 */
#ifndef FIRM_IROPTIMIZE_H
#define FIRM_IROPTIMIZE_H

#include "firm_types.h"

#include "begin.h"

/**
 * @defgroup iroptimize  Transformations and Optimizations
 * @{
 */

/**
 * Control flow optimization.
 *
 * Removes empty blocks doing if simplifications and loop simplifications.
 * A block is empty if it contains only a Jmp node and Phi nodes.
 * Merges single entry single exit blocks with their predecessor
 * and propagates dead control flow by calling equivalent_node().
 * Independent of compiler flag it removes Tuples from cf edges,
 * Bad predecessors from Blocks and Phis, and unnecessary predecessors of End.
 * Destroys backedge information.
 */
FIRM_API void optimize_cf(ir_graph *irg);

/**
 * Perform path-sensitive jump threading on the given graph.
 *
 * @param irg  the graph
 */
FIRM_API void opt_jumpthreading(ir_graph* irg);

/**
 * Simplifies boolean expression in the given ir graph.
 * eg. x < 5 && x < 6 becomes x < 5
 *
 * @param irg  the graph
 */
FIRM_API void opt_bool(ir_graph *irg);

/**
 * Reduces the number of Conv nodes in the given ir graph.
 *
 * @param irg  the graph
 */
FIRM_API void conv_opt(ir_graph *irg);

/**
 * A callback that checks whether a entity is an allocation
 * routine.
 */
typedef int (*check_alloc_entity_func)(ir_entity *ent);

/**
 * Optimize function calls by handling const functions.
 *
 * This optimization first detects all "const functions", i.e.,
 * IR graphs that neither read nor write memory (and hence did
 * not create exceptions, as these use memory in Firm).
 *
 * The result of calls to such functions depends only on its
 * arguments, hence those calls are no more pinned.
 *
 * This is a rather strong criteria, so do not expect that a
 * lot of functions will be found. Moreover, all of them might
 * already be inlined if inlining is activated.
 * Anyway, it might be good for handling builtin's
 * even if the later read/write memory (but we know how).
 *
 * This optimization reads the irg_const_function property of
 * entities, and sets the irg_const_function property of
 * graphs.
 *
 * If callee information is valid, we also optimize polymorphic Calls.
 */
FIRM_API void optimize_funccalls(void);

/**
 * Does Partial Redundancy Elimination combined with
 * Global Value Numbering.
 * Can be used to replace place_code() completely.
 *
 * Based on VanDrunen and Hosking 2004.
 *
 * @param irg  the graph
 */
FIRM_API void do_gvn_pre(ir_graph *irg);

/**
 * This function is called to evaluate, if a
 * mux(@p sel, @p mux_false, @p mux_true) should be built for the current
 * architecture.
 * If it returns non-zero, a mux is created, else the code
 * is not modified.
 * @param sel        A selector of a Cond.
 * @param phi_list   phi node to be converted
 * @param i          First data predecessor involved in if conversion
 * @param j          Second data predecessor involved in if conversion
 */
typedef int (*arch_allow_ifconv_func)(ir_node const *sel,
                                      ir_node const *mux_false,
                                      ir_node const *mux_true);

/**
 * Perform If conversion on a graph.
 *
 * @param irg The graph.
 *
 * Cannot handle blocks with Bad control predecessors, so call it after control
 * flow optimization.
 */
FIRM_API void opt_if_conv(ir_graph *irg);

/**
 * Perform If conversion on a graph - callback version.
 *
 * @param irg      The graph.
 * @param callback The predicate.
 *
 * Like above, but let the caller decide about valid transformations
 * by supplying a predicate function.
 */
FIRM_API void opt_if_conv_cb(ir_graph *irg, arch_allow_ifconv_func callback);

/**
 * Tries to reduce dependencies for memory nodes where possible by parallelizing
 * them and synchronizing with Sync nodes
 * @param irg   the graph where memory operations should be parallelized
 */
FIRM_API void opt_parallelize_mem(ir_graph *irg);

/**
 * Check if we can replace the load by a given const from
 * the const code irg.
 *
 * @param load   the load to replace
 * @param c      the constant
 *
 * @return if the modes match or can be transformed using a reinterpret cast
 *         returns a copy of the constant (possibly Conv'ed) in the graph where
 *         the load is.
 */
FIRM_API ir_node *can_replace_load_by_const(const ir_node *load, ir_node *c);

/**
 * Load/Store optimization.
 *
 * Removes redundant non-volatile Loads and Stores.
 * May introduce Bad nodes if exceptional control flow
 * is removed. The following cases are optimized:
 *
 * Load without result: A Load which has only a memory use
 *   is removed.
 *
 * Load after Store: A Load after a Store is removed, if
 *   the Load doesn't have an exception handler OR is in
 *   the same block as the Store.
 *
 * Load after Load: A Load after a Load is removed, if the
 *   Load doesn't have an exception handler OR is in the
 *   same block as the previous Load.
 *
 * Store before Store: A Store immediately before another
 *   Store in the same block is removed, if the Store doesn't
 *   have an exception handler.
 *
 * Store after Load: A Store after a Load is removed, if the
 *   Store doesn't have an exception handler.
 */
FIRM_API void optimize_load_store(ir_graph *irg);

/**
 * Combine adjacent "small" load/store operations into bigger ones.
 */
FIRM_API void combine_memops(ir_graph *irg);

/**
 * New experimental alternative to optimize_load_store.
 * Based on a dataflow analysis, so load/stores are moved out of loops
 * where possible
 */
FIRM_API void opt_ldst(ir_graph *irg);

/**
 * Optimize the frame type of an irg by removing
 * never touched entities.
 *
 * @param irg  The graph whose frame type will be optimized
 *
 * This function did not change the graph, only its frame type.
 * The layout state of the frame type will be set to layout_undefined
 * if entities were removed.
 */
FIRM_API void opt_frame_irg(ir_graph *irg);

/** Possible flags for the Operator Scalar Replacement. */
typedef enum osr_flags {
	osr_flag_none               = 0,  /**< no additional flags */
	osr_flag_lftr_with_ov_check = 1,  /**< do linear function test replacement
	                                       only if no overflow can occur. */
	osr_flag_ignore_x86_shift   = 2,  /**< ignore Multiplications by 2, 4, 8 */
	osr_flag_keep_reg_pressure  = 4   /**< do NOT increase register pressure by introducing new
	                                       induction variables. */
} osr_flags;

/** default setting */
#define osr_flag_default osr_flag_lftr_with_ov_check

/**
 * Performs the Operator Scalar Replacement optimization and linear
 * function test replacement for loop control.
 *
 * @param irg    the graph which should be optimized
 * @param flags  set of osr_flags
 *
 * The linear function replacement test is controlled by the flags.
 * If the osr_flag_lftr_with_ov_check is set, the replacement is only
 * done if do overflow can occur.
 * Otherwise it is ALWAYS done which might be insecure.
 *
 * For instance:
 *
 * for (i = 0; i < 100; ++i)
 *
 * might be replaced by
 *
 * for (i = 0; i < 400; i += 4)
 *
 * But
 *
 * for (i = 0; i < 0x7FFFFFFF; ++i)
 *
 * will not be replaced by
 *
 * for (i = 0; i < 0xFFFFFFFC; i += 4)
 *
 * because of overflow.
 *
 * More bad cases:
 *
 * for (i = 0; i <= 0xF; ++i)
 *
 * will NOT be transformed into
 *
 * for (i = 0xFFFFFFF0; i <= 0xFFFFFFFF; ++i)
 *
 * although here is no direct overflow. The OV occurs when the ++i
 * is executed (and would created an endless loop here!).
 *
 * For the same reason, a loop
 *
 * for (i = 0; i <= 9; i += x)
 *
 * will NOT be transformed because we cannot estimate whether an overflow
 * might happen adding x.
 *
 * Note that i < a + 400 is also not possible with the current implementation
 * although this might be allowed by other compilers...
 *
 * Note further that tests for equality can be handled some simpler (but are not
 * implemented yet).
 *
 * This algorithm destroys the link field of nodes.
 */
FIRM_API void opt_osr(ir_graph *irg, unsigned flags);

/**
 * Removes useless Phi cycles, i.e cycles of Phi nodes with only one
 * non-Phi node.
 * This is automatically done in opt_osr(), so there is no need to call it
 * additionally.
 *
 * @param irg    the graph which should be optimized
 *
 * This algorithm destroys the link field of nodes.
 */
FIRM_API void remove_phi_cycles(ir_graph *irg);

/** A default threshold. */
#define DEFAULT_CLONE_THRESHOLD 20

/**
 * Performs procedure cloning. Evaluate a heuristic weight for every
 * Call(..., Const, ...). If the weight is bigger than threshold,
 * clone the entity and fix the calls.
 *
 * @param threshold   the threshold for cloning
 *
 * The threshold is an estimation of how many instructions are saved
 * when executing a cloned method. If threshold is 0.0, every possible
 * call is cloned.
 */
FIRM_API void proc_cloning(float threshold);

/**
 * Reassociation.
 *
 * Applies Reassociation rules to integer expressions.
 * Beware: Works only if integer overflow might be ignored, as for C, Java
 * and for address expression.
 * Works only if Constant folding is activated.
 *
 * Uses loop information to detect loop-invariant (i.e. constant
 * inside the loop) values.
 *
 * See Muchnik 12.3.1 Algebraic Simplification and Reassociation of
 * Addressing Expressions.
 */
FIRM_API void optimize_reassociation(ir_graph *irg);

/**
 * Normalize the Returns of a graph by creating a new End block
 * with One Return(Phi).
 * This is the preferred input for the if-conversion.
 *
 * In pseudocode, it means:
 *
 * if (a)
 *   return b;
 * else
 *   return c;
 *
 * is transformed into
 *
 * if (a)
 *   res = b;
 * else
 *   res = c;
 * return res;
 */
FIRM_API void normalize_one_return(ir_graph *irg);

/**
 * Normalize the Returns of a graph by moving
 * the Returns upwards as much as possible.
 * This might be preferred for code generation.
 *
 * In pseudocode, it means:
 *
 * if (a)
 *   res = b;
 * else
 *   res = c;
 * return res;
 *
 * is transformed into
 *
 * if (a)
 *   return b;
 * else
 *   return c;
 */
FIRM_API void normalize_n_returns(ir_graph *irg);

/**
 * Performs the scalar replacement optimization.
 * Replaces local compound entities (like structures and arrays)
 * with atomic values if possible. Does not handle classes yet.
 *
 * @param irg  the graph which should be optimized
 */
FIRM_API void scalar_replacement_opt(ir_graph *irg);

/**
 * Optimizes tail-recursion calls by converting them into loops.
 * Depends on the flag opt_tail_recursion.
 * Currently supports the following forms:
 *  - return func();
 *  - return x + func();
 *  - return func() - x;
 *  - return x * func();
 *  - return -func();
 *
 * Does not work for Calls that use the exception stuff.
 *
 * @param irg   the graph to be optimized
 */
FIRM_API void opt_tail_rec_irg(ir_graph *irg);

/**
 * CLiff Click's combo algorithm from
 *   "Combining Analyses, combining Optimizations".
 *
 * Does conditional constant propagation, unreachable code elimination and
 * optimistic global value numbering at once.
 *
 * @param irg  the graph to run on
 */
FIRM_API void combo(ir_graph *irg);

/** pointer to an optimization function */
typedef void (*opt_ptr)(ir_graph *irg);

/**
 * Heuristic inliner. Calculates a benefice value for every call and inlines
 * those calls with a value higher than the threshold.
 *
 * @param maxsize             Do not inline any calls if a method has more than
 *                            maxsize firm nodes.  It may reach this limit by
 *                            inlining.
 * @param inline_threshold    inlining threshold
 * @param after_inline_opt    optimizations performed immediately after inlining
 *                            some calls
 */
FIRM_API void inline_functions(unsigned maxsize, int inline_threshold,
                               opt_ptr after_inline_opt);

/**
 * Combines congruent blocks into one.
 *
 * @param irg   The IR-graph to optimize.
 */
FIRM_API void shape_blocks(ir_graph *irg);

/**
 * Perform loop inversion on a given graph.
 * Loop inversion transforms a head controlled loop (like while(...) {} and
 * for(...) {}) into a foot controlled loop (do {} while(...)).
 */
FIRM_API void do_loop_inversion(ir_graph *irg);

/**
 * Perform loop unrolling on a given graph.
 * Loop unrolling multiplies the number loop completely by a number found
 * through a heuristic.
 */
FIRM_API void do_loop_unrolling(ir_graph *irg);

/**
 * Perform loop unrolling on a given graph.
 *
 * @param irg       the IR-graph to optimize
 * @param factor    the unroll factor
 * @param maxsize   the maximum number of nodes in a loop
 */
FIRM_API void unroll_loops(ir_graph *irg, unsigned factor, unsigned maxsize);

/**
 * Perform loop peeling on a given graph.
 */
FIRM_API void do_loop_peeling(ir_graph *irg);

/**
 * Removes all entities which are unused.
 *
 * Unused entities have ir_visibility_local and are not used directly or
 * indirectly through entities/code visible outside the compilation unit.
 * This is usually conservative than gc_irgs, but does not respect properties
 * of object-oriented programs.
 */
FIRM_API void garbage_collect_entities(void);

/**
 * Performs dead node elimination by copying the ir graph to a new obstack.
 *
 *  The major intention of this pass is to free memory occupied by
 *  dead nodes and outdated analyzes information.  Further this
 *  function removes Bad predecessors from Blocks and the corresponding
 *  inputs to Phi nodes.  This opens optimization potential for other
 *  optimizations.  Further this phase reduces dead Block<->Jmp
 *  self-cycles to Bad nodes.
 *
 *  Dead_node_elimination is only performed if options `optimize' and
 *  `opt_dead_node_elimination' are set.  The graph may
 *  not be in state phase_building.  The outs data structure is freed,
 *  the outs state set to outs_none.  Backedge information is conserved.
 *  Removes old attributes of nodes.  Sets link field to NULL.
 *  Callee information must be freed (irg_callee_info_none).
 *
 * @param irg  The graph to be optimized.
 */
FIRM_API void dead_node_elimination(ir_graph *irg);

/**
 * Code Placement.
 *
 * Pins all floating nodes to a block where they
 * will be executed only if needed.   Depends on the flag opt_global_cse.
 * Graph may not be in phase_building.  Does not schedule control dead
 * code.  Uses dominator information which it computes if the irg is not
 * in state dom_consistent.  Destroys the out information as it moves nodes
 * to other blocks.  Optimizes Tuples in Control edges.
 *
 * Call remove_critical_cf_edges() before place_code().  This normalizes
 * the control flow graph so that for all operations a basic block exists
 * where they can be optimally placed.
 */
FIRM_API void place_code(ir_graph *irg);

/**
 * This optimization finds values where the bits are either constant or irrelevant
 * and exchanges them for a corresponding constant.
 */
FIRM_API void occult_consts(ir_graph*);

/**
 * Returns true if the value @p n is known not be zero/null.
 *
 * @param n        a node representing the value
 * @param confirm  if n is confirmed to be != 0, returns
 *                 the the Confirm-node, else NULL
 */
FIRM_API int value_not_null(const ir_node *n, const ir_node **confirm);

/**
 * Returns the value of a Cmp if one or both predecessors are Confirm nodes.
 *
 * @param left      the left operand of the Cmp
 * @param right     the right operand of the Cmp
 * @param relation  the compare relation
 */
FIRM_API ir_tarval *computed_value_Cmp_Confirm(ir_node *left, ir_node *right,
                                               ir_relation relation);

/**
 * Constructs the entity for a given function using the current compilerlib
 * entity creation callback.
 */
FIRM_API ir_entity *create_compilerlib_entity(char const *name, ir_type *mt);

/** @} */

#include "end.h"

#endif
