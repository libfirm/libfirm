/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Traverse an ir graph
 * @author   Boris Boesler, Goetz Lindenmaier
 */
#ifndef FIRM_IR_IRGWALK_H
#define FIRM_IR_IRGWALK_H

#include "firm_types.h"

#include "begin.h"

/**
 * @ingroup ir_graph
 * @defgroup irgwalk Traversing
 *
 *  Traverse graphs:
 *  - execute the pre function before recursion
 *  - execute the post function after recursion
 * @{
 */

/**
 * Walks over the ir graph.
 *
 * Walks over the ir graph, starting at the node given as first argument.
 * Executes pre before visiting the predecessor of a node, post after.
 * irg_walk uses the visited flag in irg and the nodes to determine visited
 * nodes.  It executes inc_irg_visited() to generate a new flag.
 * It marks the node as visited before executing pre.
 * The void* env can be used to pass status information between the
 * pre and post functions.  Does not use the link fields.
 *
 * @param node  the start node
 * @param pre   walker function, executed before the predecessor of a node are visited
 * @param post  walker function, executed after the predecessor of a node are visited
 * @param env   environment, passed to pre and post
 *
 */
FIRM_API void irg_walk(ir_node *node, irg_walk_func *pre, irg_walk_func *post,
                       void *env);

/**
 * core walker function. Does NOT call inc_irg_visited before walking.
 */
FIRM_API void irg_walk_core(ir_node *node, irg_walk_func *pre,
                            irg_walk_func *post, void *env);

/**
 * Walks over all reachable nodes in the ir graph.
 *
 * @param irg   the irg graph
 * @param pre   walker function, executed before the predecessor of a node are visited
 * @param post  walker function, executed after the predecessor of a node are visited
 * @param env   environment, passed to pre and post
 *
 * Like irg_walk(), but walks over all reachable nodes in the ir
 * graph, starting at the end operation.  Does not use the link field.
 */
FIRM_API void irg_walk_graph(ir_graph *irg, irg_walk_func *pre,
                             irg_walk_func *post, void *env);

/**
 * Walks over the ir graph.
 *
 * Walks over the ir graph, starting at the node given as first argument.
 * Executes pre before visiting the predecessor of a node, post after.
 * irg_walk uses the visited flag in irg and the nodes to determine visited
 * nodes.  It executes inc_irg_visited() to generate a new flag.
 * It marks the node as visited before executing pre.
 * The void* env can be used to pass status information between the
 * pre and post functions.  Does not use the link fields.
 * This walker also follows additional dependency egdes.
 *
 * @param node  the start node
 * @param pre   walker function, executed before the predecessor of a node are visited
 * @param post  walker function, executed after the predecessor of a node are visited
 * @param env   environment, passed to pre and post
 *
 */
FIRM_API void irg_walk_in_or_dep(ir_node *node, irg_walk_func *pre,
                                 irg_walk_func *post, void *env);

/**
 * Walks over all reachable nodes in the ir graph.
 *
 * @param irg   the irg graph
 * @param pre   walker function, executed before the predecessor of a node are visited
 * @param post  walker function, executed after the predecessor of a node are visited
 * @param env   environment, passed to pre and post
 *
 * Like irg_walk(), but walks over all reachable nodes in the ir
 * graph, starting at the end operation.  Does not use the link field.
 * This walker also follows additional dependency egdes.
 */
FIRM_API void irg_walk_in_or_dep_graph(ir_graph *irg, irg_walk_func *pre,
                                       irg_walk_func *post, void *env);

/**
 * Walks over all reachable nodes in the graph, ensuring that nodes inside
 * a basic block are visited in topological order. Nodes in different blocks
 * might get visited in an interleaved order.
 *
 * @param irg     the irg graph
 * @param walker  walker function
 * @param env     environment, passed to walker
 *
 * Does not use the link field.
 */
FIRM_API void irg_walk_topological(ir_graph *irg, irg_walk_func *walker,
                                   void *env);

/**
 * Executes irg_walk(end, pre, post, env) for all irgraphs in irprog.
 *
 * @param pre   walker function, executed before the predecessor of a node are visited
 * @param post  walker function, executed after the predecessor of a node are visited
 * @param env   environment, passed to pre and post
 *
 * This function executes irg_walk(end, pre, post, env) for all irgraphs in irprog.
 * Does not use the link field.
 */
FIRM_API void all_irg_walk(irg_walk_func *pre, irg_walk_func *post, void *env);

/** Walks only over Block nodes in the graph.
 *
 * @param node  the start node
 * @param pre   walker function, executed before the predecessor of a node are visited
 * @param post  walker function, executed after the predecessor of a node are visited
 * @param env   environment, passed to pre and post
 *
 * This function Walks only over Block nodes in the graph. Has its own visited
 * flag, so that it can be interleaved with the other walker.
 * If a non-block is passed, starts at the block this node belongs to.
 * If end is passed also visits kept alive blocks. Does not use the link field.
 */
FIRM_API void irg_block_walk(ir_node *node, irg_walk_func *pre,
                             irg_walk_func *post, void *env);

/**
 * Walks only over reachable Block nodes in the graph.
 *
 * @param irg   the irg graph
 * @param pre   walker function, executed before the predecessor of a node are visited
 * @param post  walker function, executed after the predecessor of a node are visited
 * @param env   environment, passed to pre and post
 *
 * Like irg_block_walk(), but walks over all reachable blocks in the
 * ir graph, starting at the end block. Does not use the link field.
 */
FIRM_API void irg_block_walk_graph(ir_graph *irg, irg_walk_func *pre,
                                   irg_walk_func *post, void *env);

/**
 * Walks over all code in const_code_irg.
 *
 * @param pre   walker function, executed before the predecessor of a node are visited
 * @param post  walker function, executed after the predecessor of a node are visited
 * @param env   environment, passed to pre and post
 *
 * This function walks over all code in const_code_irg.
 * Uses visited flag in const_code_irg.  Does not use the link field.
 */
FIRM_API void walk_const_code(irg_walk_func *pre, irg_walk_func *post,
                              void *env);

/**
 * Walks over reachable nodes in block-wise topological order, i.e. visit
 * all nodes in a block before going to another block, starting at the end operation.
 * Executes pre before visiting the predecessor of a node, post after.
 * irg_walk_blkwise_graph() uses the visited flag in irg and the nodes to
 * determine visited nodes.
 * It executes inc_irg_visited() to generate a new flag. It marks the node as
 * visited before executing pre.
 * The void *env can be used to pass status information between the
 * pre and post functions.  Does not use the link fields.
 *
 * @param irg   the irg graph
 * @param pre   walker function, executed before the predecessor of a node are visited
 * @param post  walker function, executed after the predecessor of a node are visited
 * @param env   environment, passed to pre and post
 */
FIRM_API void irg_walk_blkwise_graph(ir_graph *irg, irg_walk_func *pre,
                                     irg_walk_func *post, void *env);

/**
 * Walks over reachable nodes in block-wise topological order, i.e. visit
 * all nodes in a block before going to another block, starting at the end operation.
 * Visit the blocks in dominator tree top-down order.
 * Executes pre before visiting the predecessor of a node, post after.
 * irg_walk_blkwise_graph() uses the visited flag in irg and the nodes to
 * determine visited nodes.
 * It executes inc_irg_visited() to generate a new flag. It marks the node as
 * visited before executing pre.
 * The void *env can be used to pass status information between the
 * pre and post functions.  Does not use the link fields.
 *
 * @param irg   the irg graph
 * @param pre   walker function, executed before the predecessor of a node are visited
 * @param post  walker function, executed after the predecessor of a node are visited
 * @param env   environment, passed to pre and post
 */
FIRM_API void irg_walk_blkwise_dom_top_down(ir_graph *irg, irg_walk_func *pre,
                                            irg_walk_func *post, void *env);

/**
 * Additionally walk over all anchors.
 * This function visits all anchor nodes that otherwise might not been visited in a
 * walk, for instance the Bad() node.
 *
 * @param irg   the irg graph
 * @param pre   walker function, executed before the predecessor of a node are visited
 * @param post  walker function, executed after the predecessor of a node are visited
 * @param env   environment, passed to pre and post
 */
FIRM_API void irg_walk_anchors(ir_graph *irg, irg_walk_func *pre,
                               irg_walk_func *post, void *env);

/**
 * Walker function which does not increase the visited flag before walking.
 * Do not use this unless you know what you are doing.
 */
FIRM_API void irg_walk_2(ir_node *node, irg_walk_func *pre, irg_walk_func *post,
                         void *env);

/** @} */

#include "end.h"

#endif
