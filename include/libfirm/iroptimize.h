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

/**
 * @file
 * @brief   Available Optimisations of libFirm.
 * @version $Id: cfopt.h 13543 2007-04-29 19:29:02Z beck $
 */
#ifndef FIRM_IROPTIMIZE_H
#define FIRM_IROPTIMIZE_H

#include "firm_types.h"

/**
 * Control flow optimization.
 *
 * Removes empty blocks doing if simplifications and loop simplifications.
 * A block is empty if it contains only a Jmp node and Phi nodes.
 * Merges single entry single exit blocks with their predecessor
 * and propagates dead control flow by calling equivalent_node().
 * Independent of compiler flag it removes Tuples from cf edges,
 * Bad predecessors from Blocks and Phis, and unnecessary predecessors of End.
 *
 * @bug So far destroys backedge information.
 * @bug Chokes on Id nodes if called in a certain order with other
 *      optimizations.  Call local_optimize_graph() before to remove
 *      Ids.
 */
void optimize_cf(ir_graph *irg);

/**
 * Perform partial conditional evaluation on the given graph.
 *
 * @param irg  the graph
 */
void opt_cond_eval(ir_graph* irg);

/**
 * Try to reduce the number of conv nodes in the given ir graph.
 *
 * @param irg  the graph
 */
void conv_opt(ir_graph *irg);

/**
 * Do the scalar replacement optimization.
 * Make a date flow analyze and split the
 * data flow edges.
 *
 * @param irg  the graph which should be optimized
 */
void data_flow_scalar_replacement_opt(ir_graph *irg);

/**
 * A callback that checks whether a entity is an allocation
 * routine.
 */
typedef int (*check_alloc_entity_func)(ir_entity *ent);

/**
 * Do simple and fast escape analysis for one graph.
 *
 * @param irg       the graph
 * @param callback  a callback function to check whether a
 *                  given entity is a allocation call
 */
void escape_enalysis_irg(ir_graph *irg, check_alloc_entity_func callback);

/**
 * Do simple and fast escape analysis for all graphs.
 *
 * This optimization implements a simple and fast but inexact
 * escape analysis. Some addresses might be marked as 'escaped' even
 * if they are not.
 * The advantage is a low memory footprint and fast speed.
 *
 * @param run_scalar_replace  if this flag in non-zero, scalar
 *                            replacement optimization is run on graphs with removed
 *                            allocation
 * @param callback            a callback function to check whether a
 *                            given entity is a allocation call
 *
 * This optimization removes allocation which are not used (rare) and replace
 * allocation that can be proved dead at the end of the graph which stack variables.
 *
 * The creation of stack variable allows scalar replacement to be run only
 * on those graphs that have been changed.
 *
 * This is most effective on Java where no other stack variables exists.
 */
void escape_analysis(int run_scalar_replace, check_alloc_entity_func callback);

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
 * Anyway, it might be good for handling builtin's or pseudo-graphs,
 * even if the later read/write memory (but we know how).
 *
 * This optimizations read the irg_const_function property of
 * entities and and sets the irg_const_function property of
 * graphs.
 *
 * If callee information is valid, we also optimize polymorphic Calls.
 *
 * @param force_run  if non-zero, an optimization run is started even
 *                   if no const function graph was detected.
 *                   Else calls are only optimized if at least one
 *                   const function graph was detected.
 *
 * If the fontend created external entities with the irg_const_function
 * property set, the force_run parameter should be set, else
 * should be unset.
 *
 * @note This optimization destroys the link fields of nodes.
 */
void optimize_funccalls(int force_run);

/**
 * Does Partial Redundancy Elimination combined with
 * Global Value Numbering.
 * Can be used to replace place_code() completely.
 *
 * Based on VanDrunen and Hosking 2004.
 *
 * @param irg  the graph
 *
 * @note
 * Currently completely broken because the used sets do NOT
 * preserve the topological sort of its elements.
 */
void do_gvn_pre(ir_graph *irg);

/**
 * This function is called to evaluate, if a mux can build
 * of the current architecture.
 * If it returns non-zero, a mux is created, else the code
 * is not modified.
 * @param sel        A selector of a Cond.
 * @param phi_list   List of Phi nodes about to be converted (linked via link field)
 * @param i          First data predecessor involved in if conversion
 * @param j          Second data predecessor involved in if conversion
 */
typedef int (*arch_allow_ifconv_func)(ir_node *sel, ir_node* phi_list, int i, int j);

/**
 * The parameters structure.
 */
typedef struct _opt_if_conv_info_t {
  int                 max_depth;    /**< The maximum depth up to which expressions
                                         are examined when it has to be decided if they
                                         can be placed into another block. */
  arch_allow_ifconv_func allow_ifconv; /**< Evaluator function, if not set all possible Psi
                                         nodes will be created. */
} opt_if_conv_info_t;

/**
 * Perform If conversion on a graph.
 *
 * @param irg The graph.
 * @param params The parameters for the if conversion.
 *
 * Cannot handle blocks with Bad control predecessors, so call it after control
 * flow optimization.
 */
void opt_if_conv(ir_graph *irg, const opt_if_conv_info_t *params);

void opt_ldst2(ir_graph *irg);

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
void optimize_load_store(ir_graph *irg);

/**
 * Do Loop unrolling in the given graph.
 */
void optimize_loop_unrolling(ir_graph *irg);

/**
 * Optimize the frame type of an irg by removing
 * never touched entities.
 *
 * @param irg  The graph whose frame type will be optimized
 *
 * This function did not change the graph, only it's frame type.
 * The layout state of the frame type will be set to layout_undefined
 * if entities were removed.
 */
void opt_frame_irg(ir_graph *irg);

/** Possible flags for the Operator Scalar Replacement. */
typedef enum osr_flags {
	osr_flag_none               = 0,  /**< no additional flags */
	osr_flag_lftr_with_ov_check = 1,  /**< do linear function test replacement
	                                       only if no overflow can occur. */
	osr_flag_ignore_x86_shift   = 2   /**< ignore Multiplications by 2, 4, 8 */
} osr_flags;

/* FirmJNI cannot handle identical enum values... */

/** default setting */
#define osr_flag_default osr_flag_lftr_with_ov_check

/**
 * Do the Operator Scalar Replacement optimization and linear
 * function test replacement for loop control.
 * Can be switched off using the set_opt_strength_red() flag.
 * In that case, only remove_phi_cycles() is executed.
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
void opt_osr(ir_graph *irg, unsigned flags);

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
void remove_phi_cycles(ir_graph *irg);

/** A default threshold. */
#define DEFAULT_CLONE_THRESHOLD 300

/**
 * Do procedure cloning. Evaluate a heuristic weight for every
 * Call(..., Const, ...). If the weight is bigger than threshold,
 * clone the entity and fix the calls.
 *
 * @param threshold   the threshold for cloning
 *
 * The threshold is an estimation of how many instructions are saved
 * when executing a cloned method. If threshold is 0.0, every possible
 * call is cloned.
 */
void proc_cloning(float threshold);

/**
 * Reassociation.
 *
 * Applies Reassociation rules to integer expressions.
 * Beware: Works only if integer overflow might be ignored, as for C, Java
 * and for address expression.
 * Works only if Constant folding is activated.
 *
 * Uses loop information to detect loop-invariant (ie contant
 * inside the loop) values.
 *
 * See Muchnik 12.3.1 Algebraic Simplification and Reassociation of
 * Addressing Expressions.
 *
 *
 */
void optimize_reassociation(ir_graph *irg);

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
void normalize_one_return(ir_graph *irg);

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
void normalize_n_returns(ir_graph *irg);

/**
 * Do the scalar replacement optimization.
 * Replace local compound entities (like structures and arrays)
 * with atomic values if possible. Does not handle classes yet.
 *
 * @param irg  the graph which should be optimized
 */
void scalar_replacement_opt(ir_graph *irg);

/** Performs strength reduction for the passed graph. */
void reduce_strength(ir_graph *irg);

/**
 * Optimizes simple tail-recursion calls by
 * converting them into loops. Depends on the flag opt_tail_recursion.
 *
 * Does not work for Calls that use the exception stuff.
 *
 * @param irg   the graph to be optimized
 *
 * @return non-zero if the optimization could be applied, 0 else
 */
int opt_tail_rec_irg(ir_graph *irg);

/*
 * Optimize tail-recursion calls for all IR-Graphs.
 * Depends on the flag opt_tail_recursion.
 */
void opt_tail_recursion(void);

/** This is the type for a method, that returns a pointer type to
 *  tp.  This is needed in the normalization. */
typedef ir_type *(*gen_pointer_type_to_func)(ir_type *tp);

/**  Insert Casts so that class type casts conform exactly with the type hierarchy.
 *
 *  Formulated in Java, this achieves the following:
 *
 *  For a class hierarchy
 *    class A {}
 *    class B extends A {}
 *    class C extends B {}
 *  we transforms a cast
 *    (A)new C()
 *  to
 *    (A)((B)new C()).
 *
 *  The algorithm works for Casts with class types, but also for Casts
 *  with all pointer types that point (over several indirections,
 *  i.e. ***A) to a class type.  Normalizes all graphs.  Computes type
 *  information (@see irtypeinfo.h) if not available.
 *  Invalidates trout information as new casts are generated.
 *
 *  @param gppt_fct A function that returns a pointer type that points
 *    to the type given as argument.  If this parameter is NULL, a default
 *    function is used that either uses trout information or performs a O(n)
 *    search to find an existing pointer type.  If it can not find a type,
 *    generates a pointer type with mode_P_mach and suffix "cc_ptr_tp".
 */
void normalize_irp_class_casts(gen_pointer_type_to_func gppt_fct);


/**  Insert Casts so that class type casts conform exactly with the type hierarchy
 *   in given graph.
 *
 *   For more details see normalize_irp_class_casts().
 *
 *  This transformation requires that type information is computed. @see irtypeinfo.h.
 */
void normalize_irg_class_casts(ir_graph *irg, gen_pointer_type_to_func gppt_fct);


/** Optimize casting between class types.
 *
 *    class A { m(); }
 *    class B extends A { }
 *    class C extends B {}
 *  Performs the following transformations:
 *    C c = (C)(B)(A)(B)new C()  --> C c = (C)(B)newC() --> C c = new C()
 *    (Optimizing downcasts as A a = (A)(B)(new A()) --> A a = new A() can
 *     be suppressed by setting the flag opt_suppress_downcast_optimization.
 *     Downcasting A to B might cause an exception.  It is not clear
 *     whether this is modeled by the Firm Cast node, as it has no exception
 *     outputs.);
 *  If there is inh_m() that overwrites m() in B:
 *    ((A) new B()).m()  --> (new B()).inh_m()
 *  Phi((A)x, (A)y)  --> (A) Phi (x, y)  if (A) is an upcast.
 *
 *  Computes type information if not available. @see irtypeinfo.h.
 *  Typeinformation is valid after optimization.
 *  Invalidates trout information.
 */
void optimize_class_casts(void);

#endif
