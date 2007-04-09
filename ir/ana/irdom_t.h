/*
 * Project:     libFIRM
 * File name:   ir/ana/irdom_t.h
 * Purpose:     Construct and access dominator tree -- private datastructures.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     2.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2007 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


/**
 * @file irdom_t.h
 *
 * Dominator information private datastructures.
 *
 * @author Goetz Lindenmaier
 *
 */
#ifndef _FIRM_IRDOM_T_H_
#define _FIRM_IRDOM_T_H_

#include "irdom.h"

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

#endif /* _FIRM_IRDOM_T_H_ */
