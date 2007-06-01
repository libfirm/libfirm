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
 * @brief    structure analysis
 * @author   Michael Beck
 * @date     05.04.2007
 * @version  $Id$
 */
#ifndef FIRM_ANA_STRUCTURE_H
#define FIRM_ANA_STRUCTURE_H

#include "firm_types.h"

/**
 * This enum describes the different regions constructed by the structural analysis.
 */
typedef enum ir_region_kind {
	ir_rk_Unknown,         /**< Unknown region kind. */
	ir_rk_BasicBlock,      /**< A Basic Block simply wraps a firm basic block, needed for the construction. */
	/*
	 * part0->part1->...->partn->XXX
	 */
	ir_rk_Sequence,        /**< A sequence of regions. */
	/*
	 *    part0
	 *    /  |
	 * part1 |
	 *    \  |
	 *     XXX
	 */
	ir_rk_IfThen,          /**< An if-then. */
	/*
	 *     part0
	 *     /   \
	 * part1   part2
	 *     \   /
	 *      XXX
	 */
	ir_rk_IfThenElse,      /**< An if-then-else. */
	/*
	 *      part0
	 *     /    \
	 * part1 ... partn
	 *     \    /
	 *      XXX
	 */
	ir_rk_Case,            /**< A Case like in Pascal. No fall through is allowed. */
	/*
	 *        part0
	 *     /    |    \
	 * part1->part2 partn
	 *     \         /
	 *         XXX
	 */
	ir_rk_Switch,          /**< A Switch like in C. At least one fall through exists. */
	ir_rk_Proper,          /**< A proper region. Any other DAG. */
	ir_rk_TryCatch,        /**< A TryCatch Exception handling. */
	ir_rk_TryCatchFinally, /**< A TryCatchFinally Exception handling. */
	/*
	 *  +-+
	 *  v |
	 * part0
	 */
	ir_rk_SelfLoop,        /**< A self loop. In Firm always a repeat or endless loop. */
	/*
	 * part0
	 *  | ^
	 *  v |
	 * part1
	 *  |
	 *  v
	 * XXX
	 */
	ir_rk_RepeatLoop,      /**< A Repeat loop. */
	/*
	 * part0 ---> XXX
	 *  | ^
	 *  v |
	 * part1
	 */
	ir_rk_WhileLoop,       /**< A While loop. */
	/*
	 * Arbitrary loop with single head.
	 */
	ir_rk_NaturalLoop,     /**< A natural loop. */
	ir_rk_Improper,        /**< An improper region. May contain everything. */
} ir_region_kind;

/** Returns non-zero if a region contains loops. */
#define is_loop_region(type) ((type) >= ir_rk_SelfLoop)

/**
 * Returns the link of a region.
 *
 * @param reg  the region
 */
void *get_region_link(const ir_region *reg);

/**
 * Sets the link of a region.
 *
 * @param reg   the region
 * @param data  the data
 */
void set_region_link(ir_region *reg, void *data);

/**
 * Get the immediate region of a block.
 *
 * @param block  a block node
 */
ir_region *get_block_region(const ir_node *block);

/**
 * Sets the immediate region of a block.
 *
 * @param block  a block node
 * @param reg    the region
 */
void set_block_region(ir_node *block, ir_region *reg);

/**
 * Get the immediate region of a node.
 *
 * @param n  a Firm IR node
 */
ir_region *get_irn_region(ir_node *n);

/**
 * Return non-if a given firm thing is a region.
 *
 * @param thing  a Firm object address
 */
int is_region(const void *thing);

/**
 * Return the number of predecessors in a region.
 *
 * @param reg  the region
 */
int get_region_n_preds(const ir_region *reg);

/**
 * Return the predecessor region at position pos.
 *
 * @param reg  the region
 */
ir_region *get_region_pred(const ir_region *reg, int pos);

/**
 * Set the predecessor region at position pos.
 *
 * @param reg  the region
 * @param pos  the position number
 * @param n    the new predecessor region
 */
void set_region_pred(ir_region *reg, int pos, ir_region *n);

/**
 * Return the number of successors in a region.
 *
 * @param reg  the region
 */
int get_region_n_succs(const ir_region *reg);

/**
 * Return the successor region at position pos.
 *
 * @param reg  the region
 * @param pos  the position number
 */
ir_region *get_region_succ(const ir_region *reg, int pos);

/**
 * Set the successor region at position pos.
 *
 * @param reg  the region
 * @param pos  the position number
 * @param n    the new successor region
 */
void set_region_succ(ir_region *reg, int pos, ir_region *n);

/**
 * Construct the region tree of a graph by doing
 * structural analysis.
 *
 * Uses link fields of nodes.
 *
 * @param irg  the graph
 *
 * @return the region tree
 */
ir_reg_tree *construct_region_tree(ir_graph *irg);

/**
 * Walk over the region tree.
 *
 * @param tree  the tree
 * @param pre   walker function, executed before the children of a tree node are visited
 * @param post  walker function, executed after the children of a tree node are visited
 * @param env   environment, passed to pre and post
 */
void region_tree_walk(ir_reg_tree *tree, irg_reg_walk_func *pre, irg_reg_walk_func *post, void *env);

#endif
