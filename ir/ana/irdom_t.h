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
 * @brief    Construct and access dominator tree -- private datastructures.
 * @author   Goetz Lindenmaier
 * @date     2.2002
 * @version  $Id$
 */
#ifndef FIRM_ANA_IRDOM_T_H
#define FIRM_ANA_IRDOM_T_H

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

#endif
