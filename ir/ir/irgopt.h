/* Copyright (C) 1998 - 2001 by Universitaet Karlsruhe
* All rights reserved.
*/

/**
* @file irgopt.h
*
* Optimizations for a whole ir graph, i.e., a procedure.
*
* @author Christian Schaefer, Goetz Lindenmaier
*/

/* $Id$ */

# ifndef _IRGOPT_H_
# define _IRGOPT_H_

# include "irgraph.h"

/** Applies local optimizations (see iropt.h) to all nodes in the graph. */
void local_optimize_graph (ir_graph *irg);

/** Performs dead node elimination by copying the ir graph to a new obstack.

   Further removes Bad predecesors from Blocks and the corresponding
   inputs to Phi nodes.
   Optimization is only performed if options `optimize' and
   `opt_dead_node_elimination' are set.
   The graph may not be in state phase_building.  The outs datasturcture
   is freed, the outs state set to no_outs.
   @todo Change this? -> inconsistent.

   Backedge information is conserved.
   Removes old attributes of nodes.  Sets link field to NULL.
   Attention: the numbers assigned to nodes if the library is compiled for
   development/debugging are not conserved by copying. */
void dead_node_elimination(ir_graph *irg);

/** Removes Bad Bad predecesors from Blocks and the corresponding
   inputs to Phi nodes as in dead_node_elimination but without
   copying the graph.

   @todo not implemented! */
void remove_bad_predecessors(ir_graph *irg);

/** Inlines a method at the given call site.

   Removes the call node and splits the basic block the call node
   belongs to.  Inserts a copy of the called graph between these nodes.
   Assumes that call is a Call node in current_ir_graph and that
   the type in the Call nodes type attribute is the same as the
   type of the called graph.
   Further it assumes that all Phi nodes in a block of current_ir_graph
   are assembled in a "link" list in the link field of the corresponding
   block nodes.  Further assumes that all Proj nodes are in a "link" list
   in the nodes producing the tuple.  (This is only an optical feature
   for the graph.)  Conserves this feature for the old
   nodes of the graph.  This precondition can be established by a call to
   collect_phisprojs(), see irgmod.h.
   Called_graph must be unequal to current_ir_graph.   Will not inline
   if they are equal.
   Sets visited masterflag in current_ir_graph to the max of the flag in
   current and called graph.
   Assumes that both, the called and the calling graph are in state
   "pinned".
   It is recommended to call local_optimize_graph after inlining as this
   function leaves a set of obscure Tuple nodes, e.g. a Proj-Tuple-Jmp
   combination as control flow operation. */
void inline_method(ir_node *call, ir_graph *called_graph);

/** Inlines all small methods at call sites where the called address comes
   from a Const node that references the entity representing the called
   method.
   The size argument is a rough measure for the code size of the method:
   Methods where the obstack containing the firm graph is smaller than
   size are inlined.  Further only a limited number of calls are inlined.
   If the method contains more than 1024 inlineable calls none will be
   inlined.
   Inlining is only performed if flags `optimize' and `inlineing' are set.
   The graph may not be in state phase_building.
   It is recommended to call local_optimize_graph after inlining as this
   function leaves a set of obscure Tuple nodes, e.g. a Proj-Tuple-Jmp
   combination as control flow operation.  */
void inline_small_irgs(ir_graph *irg, int size);

/** Code Placement.  Pinns all floating nodes to a block where they
   will be executed only if needed.   Depends on the flag opt_global_cse.
   Graph may not be in phase_building.  Does not schedule control dead
   code.  Uses dominator information which it computes if the irg is not
   in state dom_consistent.  Destroys the out information as it moves nodes
   to other blocks.  Optimizes Tuples in Control edges.
   @todo This is not tested!

   Call remove_critical_cf_edges() before place_code().  This normalizes
   the control flow graph so that for all operations a basic block exists
   where they can be optimally placed.

   @todo A more powerful code placement would move operations past Phi nodes
   out of loops.  */
void place_code(ir_graph *irg);

/** Control flow optimization.
 * Removes empty blocks doing if simplifications and loop simplifications.
 * A block is empty if it contains only a Jmp node and Phi nodes.
 * Merges single entry single exit blocks with their predecessor
 * and propagates dead control flow by calling equivalent_node.
 * Independent of compiler flag it removes Tuples from cf edges,
 * Bad predecessors form blocks and unnecessary predecessors of End.
 *
 * @bug So far destroys backedge information.
 * @bug Chokes on Id nodes if called in a certain order with other
 *      optimizations.  Call local_optimize_graph before to remove
 *      Ids.
 */
void optimize_cf(ir_graph *irg);


/** Places an empty basic block on critical control flow edges thereby
   removing them.
   A critical control flow edge is an edge from a block with several
   control exits to a block with several control entries (See Muchnic
   p. 407).
   Is only executed if flag set_opt_critical_edges() is set.
   @param irg IR Graph
*/
void remove_critical_cf_edges(ir_graph *irg);

# endif /* _IRGOPT_H_ */
