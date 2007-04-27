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
 *
 * @file strength_red.h
 *
 * Project:     libFIRM
 * File name:   ir/opt/strenth_red.h
 * Purpose:     Strength reduction.
 * Author:      Beyhan Veliev
 * Modified by:
 * Created:     22.8.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 *
 *
 *
 *
 */


# ifndef _STRENGTH_RED_H_
# define _STRENGTH_RED_H_

#include "irnode.h"
#include "irgraph.h"
#include "irloop.h"

/** Performs strength reduction for the passed graph. */
void reduce_strength(ir_graph *irg);

/* The information needed for an induction variable */
# ifndef _STRENGTH_RED_TYP_
# define _STRENGTH_RED_TYP_
typedef struct _induct_var_info {
  ir_op   *operation_code;  /**< The opcode of "op" */
  ir_node *increment;       /**< The value which increase or decrease the iteration variable. */
  ir_node *init;            /**< The start value of the iteration variable. */
  ir_node *op;              /**< The operation which increase or decrease the iteration variable. */
  ir_node *itervar_phi;     /**< The iteration variable. */
  ir_node *new_phi;         /**< The new iteration variable. */
  ir_node *new_increment;   /**< The new increment which replace the old one. */
  ir_node *new_init;        /**< The new init value of the iteration variable. */
  ir_node *new_op;          /**< The new operation that we need after replace. */
  ir_node *new_cmp;         /**< The new Cmp which replaces the old one. */
  ir_node *cmp;             /**< The Cmp which breaks the loop and compares the iteration variable with a constant. */
  ir_node *cmp_const;       /**< The other operand of Cmp. */
  ir_node *cmp_init_block;  /**< The initial block of the Cmp. */
  ir_node *reducible_node;  /**< The reducible nodes are save here. */
  int     is_reducible;     /**< To save information if anything is reducible. */
  int     phi_pred;         /**< To save the value of iteration variable predecessors. */
  int     init_pred_pos;    /**< To save the position of iteration variable start value. */
  int     op_pred_pos;      /**< To save the backedge of iteration variable. */
  ir_loop *l_itervar_phi;   /**< The loop of the induction variable */
} induct_var_info;
#endif

/** If an ir_node is an induction variable return info else return NULL. */
induct_var_info *is_induction_variable(induct_var_info *info);

#endif /* _STRENGTH_RED_H_ */
