/* Copyright (C) 1998 - 2001 by Universitaet Karlsruhe
** All rights reserved.
**
** Author: Christian Schaefer, Goetz Lindenmaier
**
** Optimizations for a whole ir graph, i.e., a procedure.
*/

/* $Id$ */

# ifndef _IRGOPT_H_
# define _IRGOPT_H_

# include "irgraph.h"

/* Applies local optimizations (see iropt.h) to all nodes in the graph. */
void local_optimize_graph (ir_graph *irg);

/* Performs dead node elimination by copying the ir graph to a new obstack.
   Further removes Bad predecesors from Blocks and the corresponding
   inputs to Phi nodes.
   Attention: the numbers assigned to nodes if the library is compiled for
   development/debugging are not conserved by copying. */
void dead_node_elimination(ir_graph *irg);

/* Inlines a method at the given call site.
   Assumes that call is a Call node in current_ir_graph and that
   the type in the Call nodes type attribute is the same as the
   type of the called graph.
   Further it assumes that all Phi nodes in a block of current_ir_graph
   are assembled in a "link" list in the link field of the corresponding
   block nodes.  Further assumes that all Proj nodes are in a "link" list
   in the nodes producing the tuple.  Conserves this feature for the old
   nodes of the graph.  This precondition can be established by a call to
   collect_phis(), see irgmod.h.
   Called_graph must be unequal to current_ir_graph.   Will not inline
   if they are equal.
   Sets visited masterflag in curren_ir_graph to max of flag in current
   and called graphs.
   Removes the call node and splits the basic block the call node
   belongs to.  Inserts a copy of the called graph between these nodes.
   It is recommended to call local_optimize_graph after inlining as this
   function leaves a set of obscure Tuple nodes, e.g. a Proj-Tuple-Jmp
   combination as control flow operation. */
void inline_method(ir_node *call, ir_graph *called_graph);

# endif /* _IRGOPT_H_ */
