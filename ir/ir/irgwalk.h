/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Author: Boris Boesler
**
** traverse an ir graph
** - execute the pre function before recursion
** - execute the post function after recursion
*/



# ifndef _IRGWALK_H_
# define _IRGWALK_H_

# include "irnode.h"

/* Walks over the ir graph, starting at the node given as first argument.
   Executes pre before visiting the predecessor of a node, post after.
   irg_walk uses the visited flag in irg and the nodes to determine visited
   nodes.  It executes inc_irg_visited(current_ir_graph) to generate a new
   flag. */
void irg_walk(ir_node *node,
	      void (pre)(ir_node*, void*), void (post)(ir_node*, void*),
	      void *env);

/* walks only over Block nodes in the graph.  Has it's own visited
   flag, so that it can be interleaved with the other walker.         */
void irg_block_walk(ir_node *node,
		    void (pre)(ir_node*, void*), void (post)(ir_node*, void*),
		    void *env);


# endif /* _IRGWALK_H_ */
