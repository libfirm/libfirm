/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief     Construct and access dominator tree.
 * @author    Goetz Lindenmaier
 * @date      2.2002
 * @brief     This file contains routines to construct and access dominator information.
 */
#ifndef FIRM_ANA_IRDOM_H
#define FIRM_ANA_IRDOM_H

#include "firm_types.h"

#include "begin.h"

/**
 * @ingroup irana
 * @defgroup irdom Dominance Information
 *
 *   The dominator information is stored in three fields of block nodes:
 *     - idom: a reference to the block that is the immediate dominator of
 *       this block.
 *     - dom_depth: a number giving the depth of the block in the dominator
 *       tree.
 *     - pre_num:  Number in preorder traversal.
 *
 * We generally presume (like Tarjan) that endless loops do not exist. The
 * implementation assumes a control dependency from End to loop header.
 *
 * @{
 */

/** return immediate dominator of block */
FIRM_API ir_node *get_Block_idom(const ir_node *block);

/** return immediate postdominator of a block */
FIRM_API ir_node *get_Block_ipostdom(const ir_node *block);

/** @return Depth of the given block in the dominator tree. */
int get_Block_dom_depth(const ir_node *bl);

/** @return Depth of the given block in the postdominator tree. */
int get_Block_postdom_depth(const ir_node *bl);

/**
 * Check, if a block dominates another block.
 *
 * @param a   The potential dominator block.
 * @param b   The potentially dominated block.
 *
 * @return 1, if @p a dominates @p b, else 0.
 */
FIRM_API int block_dominates(const ir_node *a, const ir_node *b);

/**
 * Check, if a block post dominates another block.
 *
 * @param a The potential post dominator block.
 * @param b The potentially post dominated block.
 *
 * @return 1, if @p a post dominates @p b, else 0.
 */
FIRM_API int block_postdominates(const ir_node *a, const ir_node *b);

/**
 * Check, if a block strictly post dominates another block, i.e. a != b.
 *
 * @param a The potential post dominator block.
 * @param b The potentially post dominated block.
 *
 * @return 1, if @p a strictly post dominates @p b, else 0.
 */
FIRM_API int block_strictly_postdominates(const ir_node *a, const ir_node *b);

/**
 * Returns the first node in the list of nodes dominated by a given block.
 *
 * Each node keeps a list of nodes which it immediately dominates. The
 * nodes are queued using the @c next pointer in the @c dom_info struct.
 * Each node keeps a head of this list using the pointer @c first in the
 * same structure.
 *
 * @param block The block for which to get the first node dominated by @c bl.
 * @return The first node dominated by @p bl.
 */
FIRM_API ir_node *get_Block_dominated_first(const ir_node *block);
/**
 * Returns the first node in the list of nodes postdominated by a given blcok.
 */
FIRM_API ir_node *get_Block_postdominated_first(const ir_node *bl);

/**
 * Returns the next node in a list of nodes which are dominated by some
 * other node.
 * @see get_Block_dominated_first().
 * @param node The previous node.
 * @return The next node in this list or NULL if it was the last.
 */
FIRM_API ir_node *get_Block_dominated_next(const ir_node *node);
/**
 * Returns the next node in a list of nodes which are postdominated by another node
 */
FIRM_API ir_node *get_Block_postdominated_next(const ir_node *node);

/**
 * Returns the deepest common dominator of two blocks.
 * @param block0  A block.
 * @param block1  Another block.
 * @return The deepest block dominating @p block0 and @p block1.
 */
FIRM_API ir_node *ir_deepest_common_dominator(ir_node *block0, ir_node *block1);

/**
 * Visit all nodes in the dominator subtree of a given node.
 * Call a pre-visitor before descending to the children and call a
 * post-visitor after returning from them.
 * @param n The node to start walking from.
 * @param pre The pre-visitor callback.
 * @param post The post-visitor callback.
 * @param env Some custom data passed to the visitors.
 */
FIRM_API void dom_tree_walk(ir_node *n, irg_walk_func *pre,
                            irg_walk_func *post, void *env);

/**
 * Visit all nodes in the post dominator subtree of a given node.
 * Call a pre-visitor before descending to the children and call a
 * post-visitor after returning from them.
 * @param n The node to start walking from.
 * @param pre The pre-visitor callback.
 * @param post The post-visitor callback.
 * @param env Some custom data passed to the visitors.
 */
FIRM_API void postdom_tree_walk(ir_node *n, irg_walk_func *pre,
                                irg_walk_func *post, void *env);

/**
 * Walk over the dominator tree of an irg starting at the root.
 * @param irg The graph.
 * @param pre A pre-visitor to call.
 * @param post A post-visitor to call.
 * @param env Some private data to give to the visitors.
 */
FIRM_API void dom_tree_walk_irg(ir_graph *irg, irg_walk_func *pre,
                                irg_walk_func *post, void *env);

/**
 * Walk over the post dominator tree of an irg starting at the root.
 * @param irg The graph.
 * @param pre A pre-visitor to call.
 * @param post A post-visitor to call.
 * @param env Some private data to give to the visitors.
 */
FIRM_API void postdom_tree_walk_irg(ir_graph *irg, irg_walk_func *pre,
                                    irg_walk_func *post, void *env);

/** Computes the dominance relation for all basic blocks of a given graph.
 *
 * Sets a flag in irg to "dom_consistent".
 * If the control flow of the graph is changed this flag must be set to
 * "dom_inconsistent".
 * Does not compute dominator information for control dead code.  Blocks
 * not reachable from Start contain the following information:
 * @code
 *   idom = NULL;
 *   dom_depth = -1;
 *   pre_num = -1;
 * @endcode
 * Also constructs outs information.  As this information is correct after
 * the run does not free the outs information.
 */
FIRM_API void compute_doms(ir_graph *irg);

/** Computes the post dominance relation for all basic blocks of a given graph.
 *
 * Sets a flag in irg to "dom_consistent".
 * If the control flow of the graph is changed this flag must be set to
 * "dom_inconsistent".
 * Does not compute post dominator information for endless lops.  Blocks
 * not reachable from End contain the following information:
 * @code
 *   idom = NULL;
 *   dom_depth = -1;
 *   pre_num = -1;
 * @endcode
 * Also constructs outs information.  As this information is correct after
 * the run does not free the outs information.
 */
FIRM_API void compute_postdoms(ir_graph *irg);

/**
 * Compute the dominance frontiers for a given graph.
 * The information is freed automatically when dominance info is freed.
 */
FIRM_API void ir_compute_dominance_frontiers(ir_graph *irg);

/**
 * Get the dominance frontier of a block.
 * @param block   The block whose dominance frontier you want.
 * @return        A list containing all blocks in the dominance frontier of
 *                @p block (as array, use ARR_LEN() to determine the size)
 */
FIRM_API ir_node **ir_get_dominance_frontier(const ir_node *block);

/** @} */

#include "end.h"

#endif
