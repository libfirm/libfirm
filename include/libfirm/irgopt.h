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
 * @brief   Optimizations for a whole ir graph, i.e., a procedure.
 * @author  Christian Schaefer, Goetz Lindenmaier, Sebastian Felis
 * @version $Id$
 */
#ifndef FIRM_IR_IRGOPT_H
#define FIRM_IR_IRGOPT_H

#include "firm_types.h"

/** Applies local optimizations (see iropt.h) to all nodes reachable from node n.
 *
 * @param n The node to be optimized.
 */
void local_optimize_node(ir_node *n);

/** Applies local optimizations (see iropt.h) to all nodes in the graph.
 *
 * @param irg  The graph to be optimized.
 *
 * After applying local_optimize_graph() to a IR-graph, Bad nodes
 * only occur as predecessor of Block and Phi nodes.
 */
void local_optimize_graph(ir_graph *irg);

/** Applies local optimizations (see iropt.h) to all nodes in the graph.
 *
 * After applying optimize_graph_df() to a IR-graph, Bad nodes
 * only occur as predecessor of Block and Phi nodes.
 *
 * This version uses fixpoint iteration.
 *
 * @param irg  The graph to be optimized.
 *
 * @return non-zero if the optimization could be applied, 0 else
 */
int optimize_graph_df(ir_graph *irg);

/**
 * Creates an ir_graph pass for optimize_graph_df().
 *
 * @param name     the name of this pass or NULL
 * @param verify   should this pass be verified?
 * @param dump     should this pass result be dumped?
 *
 * @return  the newly created ir_graph pass
 */
ir_graph_pass_t *optimize_graph_df_pass(const char *name, int verify, int dump);

/** Performs dead node elimination by copying the ir graph to a new obstack.
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
 *  not be in state phase_building.  The outs datasturcture is freed,
 *  the outs state set to outs_none.  Backedge information is conserved.
 *  Removes old attributes of nodes.  Sets link field to NULL.
 *  Callee information must be freed (irg_callee_info_none).
 *
 * @param irg  The graph to be optimized.
 */
void dead_node_elimination(ir_graph *irg);

typedef struct _survive_dce_t survive_dce_t;

/**
 * Make a new Survive DCE environment.
 */
survive_dce_t *new_survive_dce(void);

/**
 * Free a Survive DCE environment.
 */
void free_survive_dce(survive_dce_t *sd);

/**
 * Register a node pointer to be patched upon DCE.
 * When DCE occurs, the node pointer specified by @p place will be
 * patched to the new address of the node it is pointing to.
 *
 * @param sd    The Survive DCE environment.
 * @param place The address of the node pointer.
 */
void survive_dce_register_irn(survive_dce_t *sd, ir_node **place);

/**  Cleans the control flow from Bad predecessors.
 *
 * Removes Bad predecessors from Blocks and the corresponding
 * inputs to Phi nodes as in dead_node_elimination but without
 * copying the graph.
 *
 * Conserves loop information.
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

/** Code Placement.
 *
 * Pins all floating nodes to a block where they
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
 * p. 407). Exception edges are always ignored.
 *
 * @param irg  IR Graph
 */
void remove_critical_cf_edges(ir_graph *irg);

/** Places an empty basic block on critical control flow edges thereby
 * removing them.
 *
 * A critical control flow edge is an edge from a block with several
 * control exits to a block with several control entries (See Muchnic
 * p. 407).
 *
 * @param irg                     IR Graph
 * @param ignore_exception_edges  if non-zero, exception edges will be ignored
 */
void remove_critical_cf_edges_ex(ir_graph *irg, int ignore_exception_edges);

#endif
