/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief     Construct and access dominator tree.
 * @author    Goetz Lindenmaier
 * @date      2.2002
 * @version   $Id$
 * @summary
 *   This file contains routines to construct and access dominator information.
 *
 *   The dominator information is stored in three fields of block nodes:
 *     - idom: a reference to the block that is the immediate dominator of
 *       this block.
 *     - dom_depth: a number giving the depth of the block in the dominator
 *       tree.
 *     - pre_num:  Number in preorder traversal.
 */
#ifndef FIRM_ANA_IRDOM_H
#define FIRM_ANA_IRDOM_H

#include "firm_types.h"


/** Accessing the dominator data structure.
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

int get_Block_dom_pre_num(const ir_node *bl);
void set_Block_dom_pre_num(ir_node *bl, int num);

/** Accessing the post dominator data structure.
 *
 * These routines only work properly if the ir_graph is in state
 * dom_consistent or dom_inconsistent.
 *
 * If the block is not reachable from End, returns a Bad node.
 */
ir_node *get_Block_ipostdom(const ir_node *bl);
void set_Block_ipostdom(ir_node *bl, ir_node *n);

int get_Block_postdom_depth(const ir_node *bl);
void set_Block_postdom_depth(ir_node *bl, int depth);

int get_Block_postdom_pre_num(const ir_node *bl);
void set_Block_postdom_pre_num(ir_node *bl, int num);

/**
 * Get the pre-order number of a block resulting from a
 * Depth-First-Search walkover the dominator tree.
 *
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
 * Iterate over all nodes which are immediately dominated by a given
 * node.
 * @param bl   The block whose dominated blocks shall be iterated on.
 * @param curr An iterator variable of type ir_node*
 */
#define dominates_for_each(bl,curr) \
	for(curr = get_Block_dominated_first(bl); curr; \
			curr = get_Block_dominated_next(curr))

/**
 * Iterate over all nodes which are immediately post dominated by a given
 * node.
 * @param bl   The block whose post dominated blocks shall be iterated on.
 * @param curr An iterator variable of type ir_node*
 */
#define postdominates_for_each(bl,curr) \
	for(curr = get_Block_postdominated_first(bl); curr; \
			curr = get_Block_postdominated_next(curr))

/**
 * Check, if a block dominates another block.
 *
 * @param a   The potential dominator block.
 * @param b   The potentially dominated block.
 *
 * @return 1, if @p a dominates @p b, else 0.
 */
int block_dominates(const ir_node *a, const ir_node *b);

/**
 * Check, if a block strictly dominates another block, i.e. a != b.
 *
 * @param a The potential dominator block.
 * @param b The potentially dominated block.
 *
 * @return 1, if @p a strictly dominates @p b, else 0.
 */
int block_strictly_dominates(const ir_node *a, const ir_node *b);

/**
 * Returns the smallest common dominator block of two nodes.
 * @param a A node.
 * @param b Another node.
 * @return The first block dominating @p a and @p b
 */
ir_node *node_smallest_common_dominator(ir_node *a, ir_node *b);

/**
 * Returns the smallest common dominator block of all users of a node
 * BEWARE: @p irn must not be a block
 * If on or more users are Phi nodes, one can request special handling
 * with @p handle_phi = 1.  In this case the cfg predecessor block
 * corresponding to the position of the irn in the argument list of the
 * Phi is determined and treated as user.
 *
 * @param irn        A node.
 * @param handle_phi 1 if Phis should be handled different
 * @return The first block dominating all users of @irn
 */
ir_node *node_users_smallest_common_dominator(ir_node *irn, int handle_phi);

/**
 * Check, if a block post dominates another block.
 *
 * @param a The potential post dominator block.
 * @param b The potentially post dominated block.
 *
 * @return 1, if @p a post dominates @p b, else 0.
 */
int block_postdominates(const ir_node *a, const ir_node *b);

/**
 * Check, if a block strictly post dominates another block, i.e. a != b.
 *
 * @param a The potential post dominator block.
 * @param b The potentially post dominated block.
 *
 * @return 1, if @p a strictly post dominates @p b, else 0.
 */
int block_strictly_postdominates(const ir_node *a, const ir_node *b);

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
 * Visit all nodes in the post dominator subtree of a given node.
 * Call a pre-visitor before descending to the children and call a
 * post-visitor after returning from them.
 * @param n The node to start walking from.
 * @param pre The pre-visitor callback.
 * @param post The post-visitor callback.
 * @param env Some custom data passed to the visitors.
 */
void postdom_tree_walk(ir_node *n, irg_walk_func *pre,
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

/**
 * Walk over the post dominator tree of an irg starting at the root.
 * @param irg The graph.
 * @param pre A pre-visitor to call.
 * @param post A post-visitor to call.
 * @param env Some private data to give to the visitors.
 */
void postdom_tree_walk_irg(ir_graph *irg, irg_walk_func *pre,
		irg_walk_func *post, void *env);

/* ------------ Building and Removing the dominator data structure ----------- */

/** Computes the dominator trees.
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
void compute_doms(ir_graph *irg);

/** Computes the dominator trees on demand */
void assure_doms(ir_graph *irg);

/** Computes the post dominator trees.
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
void compute_postdoms(ir_graph *irg);

/** Computes the dominator trees on demand */
void assure_postdoms(ir_graph *irg);

/** Frees the dominator data structures.  Sets the flag in irg to "dom_none". */
void free_dom(ir_graph *irg);

/** Frees the post dominator data structures.  Sets the flag in irg to "dom_none". */
void free_postdom(ir_graph *irg);

#endif
