/*
 * Project:     libFIRM
 * File name:   ir/ana/irdom.h
 * Purpose:     Construct and access dominator tree.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     2.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
* @file irdom.h
*
*   This file contains routines to construct and access dominator information.
*
*   The dominator information is stored in three fields of block nodes:
*     - idom: a reference to the block that is the immediate dominator of
*       this block.
*     - dom_depth: a number giving the depth of the block in the dominator
*       tree.
*     - pre_num:  Number in preorder traversal.
*
* @author Goetz Lindenmaier
*/


# ifndef _IRDOM_H_
# define _IRDOM_H_

# include "irgraph.h"
# include "irgwalk.h"
# include "irnode.h"


/** Accessing the dominator datastructure.
 *
 * These routines only work properly if the ir_graph is in state
 * dom_consistent or dom_inconsistent.
 *
 * If the block is not reachable from Start, returns a Bad node.
 */
ir_node *get_Block_idom(const ir_node *bl);
void set_Block_idom(ir_node *bl, ir_node *n);

int get_Block_dom_depth(const ir_node *bl);
void set_Block_dom_depth(ir_node *bl, int depth);

int get_Block_pre_num(const ir_node *bl);
void set_Block_pre_num(ir_node *bl, int num);

/**
 * Get the pre-order number of a block resulting from a dfs walk
 * over the dominator tree.
 * @param bl The block.
 * @return The pre-order number.
 */
unsigned get_Block_dom_tree_pre_num(const ir_node *bl);

/**
 * Get the largest pre-order number found in the subtree of the
 * dominator tree rooted at a given block.
 * @param bl The block.
 * @return The largest pre-order number of block's dominator subtree.
 */
unsigned get_Block_dom_max_subtree_pre_num(const ir_node *bl);

/**
 * Get the first node in the list of nodes dominated by a given block.
 *
 * Each node keeps a list of nodes which it immediately dominates. The
 * nodes are queued using the @c next pointer in the @c dom_info struct.
 * Each node keeps a head of this list using the pointer @c first in the
 * same structure.
 *
 * @param bl The block for which to get the first node dominated by @c bl.
 * @return The first node dominated by @p bl.
 */
ir_node *get_Block_dominated_first(const ir_node *bl);

/**
 * Get the next node in a list of nodes which are dominated by some
 * other node.
 * @see get_Block_dominated_first().
 * @param dom The previous node.
 * @return The next node in this list or NULL if it was the last.
 */
ir_node *get_Block_dominated_next(const ir_node *dom);

/**
 * Check, if a block dominates another block.
 * @param a The first block.
 * @param b The second block.
 * @return 1, if @p a dominates @p b, else 0.
 */
int block_dominates(const ir_node *a, const ir_node *b);

/**
 * Visit all nodes in the dominator subtree of a given node.
 * Call a pre-visitor before descending to the children and call a
 * post-visitor after returning from them.
 * @param n The node to start walking from.
 * @param pre The pre-visitor callback.
 * @param post The post-visitor callback.
 * @param env Some custom data passed to the visitors.
 */
void dom_tree_walk(ir_node *n, irg_walk_func *pre,
		irg_walk_func *post, void *env);


/**
 * Walk over the dominator tree of an irg starting at the root.
 * @param irg The graph.
 * @param pre A pre-visitor to call.
 * @param post A post-visitor to call.
 * @param env Some private data to give to the visitors.
 */
void dom_tree_walk_irg(ir_graph *irg, irg_walk_func *pre,
		irg_walk_func *post, void *env);

/* ------------ Building and Removing the dominator datasturcture ----------- */

/** Computes the dominator trees.
 *
 * Sets a flag in irg to "dom_consistent".
 * If the control flow of the graph is changed this flag must be set to
 * "dom_inconsistent".
 * Does not compute dominator information for control dead code.  Blocks
 * not reachable from Start contain the following information:
 *   idom = NULL;
 *   dom_depth = -1;
 *   pre_num = -1;
 * Also constructs outs information.  As this information is correct after
 * the run does not free the outs information.
 */
void compute_doms(ir_graph *irg);

/** Frees the dominator datastructures.  Sets the flag in irg to "dom_none". */
void free_dom_and_peace(ir_graph *irg);

#endif /* _IRDOM_H_ */
