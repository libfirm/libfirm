/*
 * Project:     libFIRM
 * File name:   ir/ir/irgopt.h
 * Purpose:     Optimizations for a whole ir graph, i.e., a procedure.
 * Author:      Christian Schaefer, Goetz Lindenmaier
 * Modified by: Sebastian Felis
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file irgopt.h
 *
 * Optimizations for a whole ir graph, i.e., a procedure.
 *
 * @author Christian Schaefer, Goetz Lindenmaier
 */

# ifndef _IRGOPT_H_
# define _IRGOPT_H_

# include "irgraph.h"

/** Applies local optimizations (see iropt.h) to all nodes reachable from node n.
 *
 * @param n The node to be optimized.
 */
void local_optimize_node(ir_node *n);

/** Applies local optimizations (see iropt.h) to all nodes in the graph.
 *
 * @param irg  The graph to be optimized.
 *
 * After appliying local_optimize_graph() to a IR-graph, Bad nodes
 * only occure as predecessor of Block and Phi nodes.
 */
void local_optimize_graph (ir_graph *irg);

/** Performs dead node elimination by copying the ir graph to a new obstack.
 *
 *  The major intention of this pass is to free memory occupied by
 *  dead nodes and outdated analyses information.  Further this
 *  function removes Bad predecesors from Blocks and the corresponding
 *  inputs to Phi nodes.  This opens optmization potential for other
 *  optimizations.  Further this phase reduces dead Block<->Jmp
 *  self-cycles to Bad nodes.
 *
 *  Dead_node_elimination is only performed if options `optimize' and
 *  `opt_dead_node_elimination' are set.  The graph may
 *  not be in state phase_building.  The outs datasturcture is freed,
 *  the outs state set to outs_none.  Backedge information is conserved.
 *  Removes old attributes of nodes.  Sets link field to NULL.
 *  Callee information must be freed (irg_callee_info_none).
 *
 *  Attention: the numbers assigned to nodes if the library is compiled for
 *  development/debugging are not conserved by copying.
 *
 * @param irg  The graph to be optimized.
 */
void dead_node_elimination(ir_graph *irg);

/**  Cleans the control flow from Bad predecesors.
 *
 * Removes Bad predecesors from Blocks and the corresponding
 * inputs to Phi nodes as in dead_node_elimination but without
 * copying the graph.
 *
 * @param irg  The graph to be optimized.
 */
void remove_bad_predecessors(ir_graph *irg);

/** Inlines a method at the given call site.
 *
 *  Removes the call node and splits the basic block the call node
 *  belongs to.  Inserts a copy of the called graph between these nodes.
 *  Assumes that call is a Call node in current_ir_graph and that
 *  the type in the Call nodes type attribute is the same as the
 *  type of the called graph.
 *  Further it assumes that all Phi nodes in a block of current_ir_graph
 *  are assembled in a "link" list in the link field of the corresponding
 *  block nodes.  Further assumes that all Proj nodes are in a "link" list
 *  in the nodes producing the tuple.  (This is only an optical feature
 *  for the graph.)  Conserves this feature for the old
 *  nodes of the graph.  This precondition can be established by a call to
 *  collect_phisprojs(), see irgmod.h.
 *  As dead_node_elimination this function reduces dead Block<->Jmp
 *  self-cycles to Bad nodes.
 *
 *  Called_graph must be unequal to current_ir_graph.   Will not inline
 *  if they are equal.
 *  Sets visited masterflag in current_ir_graph to the max of the flag in
 *  current and called graph.
 *  Assumes that both, the called and the calling graph are in state
 *  "op_pin_state_pinned".
 *  It is recommended to call local_optimize_graph() after inlining as this
 *  function leaves a set of obscure Tuple nodes, e.g. a Proj-Tuple-Jmp
 *  combination as control flow operation.
 *
 *  @param call          the call node that should be inlined
 *  @param called_graph  the IR-graph that is called at call
 *
 *  @return zero if method could not be inlined (recursion for instance),
 *          non-zero if all went ok
 */
int inline_method(ir_node *call, ir_graph *called_graph);

/** Inlines all small methods at call sites where the called address comes
 *  from a SymConst node that references the entity representing the called
 *  method.
 *
 *  The size argument is a rough measure for the code size of the method:
 *  Methods where the obstack containing the firm graph is smaller than
 *  size are inlined.  Further only a limited number of calls are inlined.
 *  If the method contains more than 1024 inlineable calls none will be
 *  inlined.
 *  Inlining is only performed if flags `optimize' and `inlineing' are set.
 *  The graph may not be in state phase_building.
 *  It is recommended to call local_optimize_graph() after inlining as this
 *  function leaves a set of obscure Tuple nodes, e.g. a Proj-Tuple-Jmp
 *  combination as control flow operation.
 */
void inline_small_irgs(ir_graph *irg, int size);


/** Inlineing with a different heuristic than inline_small_irgs().
 *
 *  Inlines leave functions.  If inlinening creates new leave
 *  function inlines these, too. (If g calls f, and f calls leave h,
 *  h is first inlined in f and then f in g.)
 *
 *  Then inlines all small functions (this is not recursive).
 *
 *  For a heuristic this inlineing uses firm node counts.  It does
 *  not count auxiliary nodes as Proj, Tuple, End, Start, Id, Sync.
 *
 *  @param maxsize   Do not inline any calls if a method has more than
 *                   maxsize firm nodes.  It may reach this limit by
 *                   inlineing.
 *  @param leavesize Inline leave functions if they have less than leavesize
 *                   nodes.
 *  @param size      Inline all function smaller than size.
 */
void inline_leave_functions(int maxsize, int leavesize, int size);

/** Code Placement.
 *
 * Pinns all floating nodes to a block where they
 * will be executed only if needed.   Depends on the flag opt_global_cse.
 * Graph may not be in phase_building.  Does not schedule control dead
 * code.  Uses dominator information which it computes if the irg is not
 * in state dom_consistent.  Destroys the out information as it moves nodes
 * to other blocks.  Optimizes Tuples in Control edges.
 * @todo This is not tested!
 *
 * Call remove_critical_cf_edges() before place_code().  This normalizes
 * the control flow graph so that for all operations a basic block exists
 * where they can be optimally placed.
 *
 * @todo A more powerful code placement would move operations past Phi nodes
 * out of loops.
 */
void place_code(ir_graph *irg);

/** Places an empty basic block on critical control flow edges thereby
 * removing them.
 *
 * A critical control flow edge is an edge from a block with several
 * control exits to a block with several control entries (See Muchnic
 * p. 407).
 * Is only executed if flag set_opt_critical_edges() is set.
 *
 * @param irg IR Graph
 */
void remove_critical_cf_edges(ir_graph *irg);

# endif /* _IRGOPT_H_ */
