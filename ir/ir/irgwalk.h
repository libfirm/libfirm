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

/* Walks over the ir graph, starting at the node given as first argument.
   Executes pre before visiting the predecessor of a node, post after.
   irg_walk uses the visited flag in irg and the nodes to determine visited
   nodes.  It executes inc_irg_visited(current_ir_graph) to generate a new
   flag.  It marks the node as visited before executing pre.
   The void* env can be used to pass status information between the
   pre and post functions.  */
void irg_walk(ir_node *node,
	      void (pre)(ir_node*, void*), void (post)(ir_node*, void*),
	      void *env);

/* Executes irg_walk(end, pre, post, env) for all irgraphs in irprog.
   Sets current_ir_graph properly for each walk.  Conserves current
   current_ir_graph. */
void all_irg_walk(void (pre)(ir_node*, void*), void (post)(ir_node*, void*),
		  void *env);


/* Walks only over Block nodes in the graph.  Has it's own visited
   flag, so that it can be interleaved with the other walker.
   If a none block is passed, starts at the block this node belongs to.
   If end is passed also visites kept alive blocks. */
void irg_block_walk(ir_node *node,
		    void (pre)(ir_node*, void*), void (post)(ir_node*, void*),
		    void *env);


# endif /* _IRGWALK_H_ */
