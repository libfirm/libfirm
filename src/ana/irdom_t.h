/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Construct and access dominator tree -- private data structures.
 * @author   Goetz Lindenmaier
 * @date     2.2002
 */
#ifndef FIRM_ANA_IRDOM_T_H
#define FIRM_ANA_IRDOM_T_H

#include "irdom.h"
#include "pmap.h"
#include "obst.h"

/** For dominator information */
typedef struct ir_dom_info {
	ir_node *idom;   /**< immediate CFG dominator */
	ir_node *next;   /**< The next node in the dominated list of @c idom. */
	ir_node *first;  /**< The first node in the list of nodes
	                      this nodes dominates immediately. */
	unsigned tree_pre_num;         /**< The pre-order number from a dfs walk
	                                    over the dominator tree. */
	unsigned max_subtree_pre_num;  /**< The largest tree pre num found in the
	                                    dominator subtree of this node. */
	int pre_num;     /**< pre-order graph-walk number */
	int dom_depth;   /**< depth in dominator-tree */
} ir_dom_info;

typedef struct ir_dom_front_info_t {
	pmap *df_map;         /**< A map, mapping every block to a list of its dominance frontier blocks. */
	struct obstack obst;  /**< An obstack holding all the frontier data. */
} ir_dom_front_info_t;

void set_Block_idom(ir_node *bl, ir_node *n);

void set_Block_dom_depth(ir_node *bl, int depth);

int get_Block_dom_pre_num(const ir_node *bl);
void set_Block_dom_pre_num(ir_node *bl, int num);

void set_Block_ipostdom(ir_node *bl, ir_node *n);

void set_Block_postdom_depth(ir_node *bl, int depth);

int get_Block_postdom_pre_num(const ir_node *bl);
void set_Block_postdom_pre_num(ir_node *bl, int num);

unsigned get_Block_dom_tree_pre_num(const ir_node *bl);
unsigned get_Block_pdom_tree_pre_num(const ir_node *bl);

unsigned get_Block_dom_max_subtree_pre_num(const ir_node *bl);
unsigned get_Block_pdom_max_subtree_pre_num(const ir_node *bl);

void ir_free_dominance_frontiers(ir_graph *irg);

/**
 * Iterate over all nodes which are immediately dominated by a given
 * node.
 * @param block  The block whose dominated blocks shall be iterated on.
 * @param curr   Name for an ir_node* iterator variable (will be declared)
 */
#define dominates_for_each(block, curr) \
	for(ir_node *curr = get_Block_dominated_first(block); curr != NULL; \
	    curr = get_Block_dominated_next(curr))

/**
 * Iterate over all nodes which are immediately post dominated by a given
 * node.
 * @param block The block whose post dominated blocks shall be iterated on.
 * @param curr  Name for an ir_node *iterator variable (will be declared)
 */
#define postdominates_for_each(block, curr) \
	for(ir_node *curr = get_Block_postdominated_first(block); curr != NULL; \
	    curr = get_Block_postdominated_next(curr))

#endif
