/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Author: Boris Boesler
**
** traverse an ir graph
** - execute the pre function before recursion
** - execute the post function after recursion
**
** Uses current_ir_graph (from irgraph.h)!!! Set it to the proper
** graph before starting the walker.
*/

/* $Id$ */

# ifndef _IRGWALK_H_
# define _IRGWALK_H_

# include "irnode.h"

/* type of callback function for ir_graph walk */
typedef void (* irg_walk_func)(ir_node *, void *);

/* Walks over the ir graph, starting at the node given as first argument.
   Executes pre before visiting the predecessor of a node, post after.
   irg_walk uses the visited flag in irg and the nodes to determine visited
   nodes.  It executes inc_irg_visited(current_ir_graph) to generate a new
   flag.  It marks the node as visited before executing pre.
   The void* env can be used to pass status information between the
   pre and post functions.  */
void irg_walk(ir_node *node, irg_walk_func pre, irg_walk_func post, void *env);

/* Like "irg_walk", but walks over all reachable nodes in the ir
 * graph, starting at the end operation. During the walk current_ir_graph
 * is set to irg. */
void irg_walk_graph(ir_graph *irg, irg_walk_func pre, irg_walk_func post, void *env);

/* Executes irg_walk(end, pre, post, env) for all irgraphs in irprog.
   Sets current_ir_graph properly for each walk.  Conserves current
   current_ir_graph. */
void all_irg_walk(irg_walk_func pre, irg_walk_func post, void *env);


/* Walks only over Block nodes in the graph.  Has it's own visited
   flag, so that it can be interleaved with the other walker.
   If a none block is passed, starts at the block this node belongs to.
   If end is passed also visites kept alive blocks. */
void irg_block_walk(ir_node *node, irg_walk_func pre, irg_walk_func post, void *env);

/* Like "irg_block_walk", but walks over all reachable blocks in the
 * ir graph, starting at the end block. */
void irg_block_walk_graph(ir_graph *irg, irg_walk_func pre, irg_walk_func post, void *env);

/********************************************************************/
/** Walking support for interprocedural analysis                   **/
/**                                                                **/
/** @@@ Don't use, not operational yet, doesn't grok recursions!!  **/
/**                                                                **/
/** Interprocedural walking should not walk all predecessors of    **/
/** all nodes.  When leaving a procedure the walker should only    **/
/** follow the edge corresponding to the most recent entry of the  **/
/** procedure.  The following functions use an internal stack to   **/
/** remember the current call site of a procedure.                 **/
/** They also set current_ir_graph correctly.                      **/
/**                                                                **/
/** Usage example:                                                 **/
/**                                                                **/
/** void init_ip_walk ();                                          **/
/** work_on_graph(some_end_node);                                  **/
/** void finish_ip_walk();                                         **/
/**                                                                **/
/** work_on_graph(ir_node *n) {                                    **/
/**   for (i = 0; i < get_irn_arity(n); i++) {                     **/
/**     if (...) continue;                                         **/
/**     ir_node *m = get_irn_ip_pred(n, i);                        **/
/**     if !m continue;                                            **/
/**     work_on_graph(m);                                          **/
/**     return_recur(n, i);                                        **/
/**   }                                                            **/
/** }                                                              **/
/********************************************************************/

/* Allocates some necessary datastructures. */
void init_ip_walk ();
/* Frees some necessary datastructures. */
void finish_ip_walk();

/* Call for i in {0|-1 ... get_irn_arity(n)}.
   If n is a conventional node returns the same node as get_irn_n(n, i).
   If the predecessors of n are in the callee of the procedure n belongs
   to, returns get_irn_n(n, i) if this node is in the callee on the top
   of the stack, else returns NULL.
   If the predecessors of n are in a procedure called by the procedure n
   belongs to pushes the caller on the caller stack in the callee.
   Sets current_ir_graph to the graph the node returned is in. */
ir_node *get_irn_ip_pred(ir_node *n, int pos);

/* If get_irn_ip_pred() returned a node (not NULL) this must be
   called to clear up the stacks.
   Sets current_ir_graph to the graph n is in. */
void return_recur(ir_node *n, int pos);


# endif /* _IRGWALK_H_ */
