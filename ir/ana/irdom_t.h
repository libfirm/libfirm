/*
 * Project:     libFIRM
 * File name:   ir/ana/irdom_t.h
 * Purpose:     Construct and access dominator tree -- private datastructures.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     2.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2003 Universität Karlsruhe
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


# ifndef _IRDOM_T_H_
# define _IRDOM_T_H_

#include "irdom.h"

/** For dominator information */
typedef struct dom_info {
  struct ir_node *idom;			/**< immediate CFG dominator */
  struct ir_node *next; 		/**< The next node in the dominated
					   list of @c idom. */
  struct ir_node *first;		/**< The first node in the list of nodes
					   this nodes dominates immediately. */
  int tree_pre_num;			/**< The pre-order number from a dfs walk
					   over the dominator tree. */
  int max_subtree_pre_num;	        /**< The largest tree pre num found in the
					   dominator subtree of this node. */
  int pre_num;		      		/**< pre-order graph-walk number */
  int dom_depth;	      		/**< depth in dominator-tree */
} dom_info;

#endif /* _IRDOM_T_H_ */
