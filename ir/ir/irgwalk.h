/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
* All rights reserved.
*/

/**
 * @file irgwalk.h
 *
 * Traverse an ir graph.
 *
 * @author Boris Boesler
 *
 * Traverse an ir graph:
 * - execute the pre function before recursion
 * - execute the post function after recursion
 *
 * Uses current_ir_graph (from irgraph.h)!!! Set it to the proper
 * graph before starting the walker.
 */

/* $Id$ */

# ifndef _IRGWALK_H_
# define _IRGWALK_H_

# include "irnode.h"

/* type of callback function for ir_graph walk */
#ifndef _IRG_WALK_FUNC_TYPEDEF_
#define _IRG_WALK_FUNC_TYPEDEF_
/**
 * The type of a walk function.  Does not use the link field.
 *
 * @param node - the node that is just visited
 * @param env  - an environment pointer passed by the walk functions
 */
typedef void irg_walk_func(ir_node *node, void *env);
#endif

/** Allocates some necessary datastructures. */
void init_ip_walk(void);

/** Frees some necessary datastructures. */
void finish_ip_walk(void);

/**
 * Walks over the ir graph.
 *
 * @param node - the start node
 * @param pre  - walker function, executed before the predecessor of a node are visited
 * @param post - walker function, executed after the predecessor of a node are visited
 * @param env  - environment, passend to pre and post
 *
 * Walks over the ir graph, starting at the node given as first argument.
 * Executes pre before visiting the predecessor of a node, post after.
 * irg_walk uses the visited flag in irg and the nodes to determine visited
 * nodes.  It executes inc_irg_visited(current_ir_graph) to generate a new
 * flag.  It marks the node as visited before executing pre.
 * The void* env can be used to pass status information between the
 * pre and post functions.  Does not use the link field.
 */
void irg_walk(ir_node *node, irg_walk_func *pre, irg_walk_func *post, void *env);

/**
 * Walks over all reachable nodes in the ir graph.
 *
 * @param irg  - the irg graph
 * @param pre  - walker function, executed before the predecessor of a node are visited
 * @param post - walker function, executed after the predecessor of a node are visited
 * @param env  - environment, passend to pre and post
 *
 * Like irg_walk(), but walks over all reachable nodes in the ir
 * graph, starting at the end operation. During the walk current_ir_graph
 * is set to irg.  Does not use the link field.
 */
void irg_walk_graph(ir_graph *irg, irg_walk_func *pre, irg_walk_func *post, void *env);

/**
 * Executes irg_walk(end, pre, post, env) for all irgraphs in irprog.
 *
 * @param pre  - walker function, executed before the predecessor of a node are visited
 * @param post - walker function, executed after the predecessor of a node are visited
 * @param env  - environment, passend to pre and post
 *
 * This function executes irg_walk(end, pre, post, env) for all irgraphs in irprog.
 * Sets current_ir_graph properly for each walk.  Conserves current
 * current_ir_graph.  In interprocedural view nodes can be visited several
 * times.  Does not use the link field.
 */
void all_irg_walk(irg_walk_func *pre, irg_walk_func *post, void *env);

/**
 * Walks all irgs in interprocedural view.
 *
 * @param pre  - walker function, executed before the predecessor of a node are visited
 * @param post - walker function, executed after the predecessor of a node are visited
 * @param env  - environment, passend to pre and post
 *
 * This function walks all irgs in interprocedural view.
 * Visits each node only once.  Sets current_ir_graph properly. Does not use the link field.
 */
void cg_walk(irg_walk_func *pre, irg_walk_func *post, void *env);

/**
 * Walks only over Block nodes in the graph.
 *
 * @param node - the start node
 * @param pre  - walker function, executed before the predecessor of a node are visited
 * @param post - walker function, executed after the predecessor of a node are visited
 * @param env  - environment, passend to pre and post
 *
 * This function Walks only over Block nodes in the graph. Has it's own visited
 * flag, so that it can be interleaved with the other walker.
 * If a none block is passed, starts at the block this node belongs to.
 * If end is passed also visites kept alive blocks. Does not use the link field.
 */
void irg_block_walk(ir_node *node, irg_walk_func *pre, irg_walk_func *post, void *env);

/**
 * Walks only over reachable Block nodes in the graph.
 *
 * @param irg  - the irg graph
 * @param pre  - walker function, executed before the predecessor of a node are visited
 * @param post - walker function, executed after the predecessor of a node are visited
 * @param env  - environment, passend to pre and post
 *
 * Like irg_block_walk(), but walks over all reachable blocks in the
 * ir graph, starting at the end block. Does not use the link field.
 */
void irg_block_walk_graph(ir_graph *irg, irg_walk_func *pre, irg_walk_func *post, void *env);

/**
 * Walks over all code in const_code_irg.
 *
 * @param pre  - walker function, executed before the predecessor of a node are visited
 * @param post - walker function, executed after the predecessor of a node are visited
 * @param env  - environment, passend to pre and post
 *
 * This function walks over all code in const_code_irg.
 * Uses visited flag in const_code_irg.  Does not use the link field.
 */
void walk_const_code(irg_walk_func *pre, irg_walk_func *post, void *env);

# endif /* _IRGWALK_H_ */
