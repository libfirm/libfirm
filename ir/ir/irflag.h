/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
* All rights reserved.
*/

/**
 * @file irflag.h
 *
 * Optimization flags.
 *
 * @author Christian Schaefer
 */

/* $Id$ */

#ifndef _IRFLAG_H_
#define _IRFLAG_H_

/**
 * This function enables/disables optimizations globally.
 *
 * If optimize == 0 no optimizations are performed at all.
 * Default: optimize == 1.
 */
void set_optimize (int value);
/** Returns global optimization setting */
int  get_optimize (void);

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
/** Returns constant folding optimization setting. */
int  get_opt_constant_folding (void);

/** Enables/Disables constant subexpression elimination.
 *
 * If opt_cse == 1 perform constant subexpression elimination.
 * Default: opt_cse == 1.
 */
void set_opt_cse (int value);
/** Returns constant subexpression elimination setting. */
int  get_opt_cse (void);

/** Enables/Disables global constant subexpression elimination.
 *
 * If opt_global_cse == 1 and opt_cse == 1 perform intra procedure
 * constant subexpression elimination for floating nodes.  Intra
 * procedure cse gets the graph into state "floating".  It is necessary
 * to run pre/code motion to get the graph back into state "pinned".
 * Default: opt_global_cse == 1.
 */
void set_opt_global_cse (int value);
/** Returns global constant subexpression elimination setting. */
int  get_opt_global_cse (void);

/** Enables/Disables unreachble code elimination.
 *
 * If opt_unreachable_code == 1 replace nodes (except Block,
 * Phi and Tuple) with a Bad predecessor by the Bad node.
 * Default: opt_unreachable_code == 1.
 */
void set_opt_unreachable_code(int value);
/** Returns unreachble code elimination setting. */
int  get_opt_unreachable_code(void);

/** Enables/Disables control flow optimizations.
 *
 * Performs Straightening, if simplifications and loop simplifications.
 * Sets all separate control flow flags (control_flow_straightening,
 * weak_simplification and strong_simplification).
 */
void set_opt_control_flow(int value);

/** Enables/Disables Straightening. */
void set_opt_control_flow_straightening(int value);
/** Returns Straightening setting. */
int  get_opt_control_flow_straightening(void);

/** Enables/Disables if simplifications in local optimizations. */
void set_opt_control_flow_weak_simplification(int value);
/** Returns if simplifications in local optimizations setting. */
int  get_opt_control_flow_weak_simplification(void);

/** Enables/Disables strong if and loop simplification (in optimize_cf). */
void set_opt_control_flow_strong_simplification(int value);
/** Returns strong if and loop simplification setting */
int  get_opt_control_flow_strong_simplification(void);

/** Enables/Disables reassociation.
 *
 * If opt_reassociation == 1 reassociation is performed.
 * Default: opt_reassociation == 1.
 */
void set_opt_reassociation(int value);
/** Returns reassociation setting. */
int  get_opt_reassociation(void);

/** Enables/Disables dead node elimination.
 *
 * If opt_dead_node_elimination == 1 deallocate all dead nodes
 * by copying the firm graph.
 * Default: opt_dead_node_elimination == 1. */
void set_opt_dead_node_elimination (int value);
/** Returns dead node elimination setting. */
int  get_opt_dead_node_elimination (void);

/** Enable/Disables inlining.
 *
 * If opt_inline == 1 the inlining transformation is performed.
 */
void set_opt_inline (int value);
/** Returns inlining setting. */
int  get_opt_inline (void);

#endif
