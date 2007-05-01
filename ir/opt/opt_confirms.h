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
 * @brief   Optimizations regarding Confirm nodes.
 * @author  Michael Beck
 * @version $Id$
 */
#ifndef FIRM_OPT_CONFIRMS_H
#define FIRM_OPT_CONFIRMS_H

#include "firm_types.h"

/**a
 * @file opt_confirms.h
 *
 * Optimizations regarding Confirm nodes.
 * These optimizations are not means to be run from
 * frontends, they are called from iropt.
 */

/**
 * Check, if the value of a node is != 0.
 *
 * This is a often needed case, so we handle here Confirm
 * nodes too.
 *
 * @param n        a node representing the value
 * @param confirm  if n is confirmed to be != 0, returns
 *                 the the Confirm-node, else NULL
 */
int value_not_zero(ir_node *n, ir_node **confirm);

/**
 * Check, if the value of a node cannot represent a NULL pointer.
 *
 * - If sel_based_null_check_elim is enabled, all
 *   Sel nodes can be skipped.
 * - A SymConst(entity) is NEVER a NULL pointer
 * - A Const != NULL is NEVER a NULL pointer
 * - Confirms are evaluated
 *
 * @param n        a node representing the value
 * @param confirm  if n is confirmed to be != NULL, returns
 *                 the the Confirm-node, else NULL
 */
int value_not_null(ir_node *n, ir_node **confirm);

/**
 * Possible return values of value_classify().
 */
typedef enum _value_classify_sign {
  value_classified_unknown  = 0,   /**< could not classify */
  value_classified_positive = 1,   /**< value is positive, i.e. >= 0 */
  value_classified_negative = -1   /**< value is negative, i.e. <= 0 if
                                        no signed zero exists or < 0 else */
} value_classify_sign;

/**
 * Check, if the value of a node can be confirmed >= 0 or <= 0,
 * If the mode of the value did not honor signed zeros, else
 * check for >= 0 or < 0.
 *
 * @param n  a node representing the value
 */
value_classify_sign classify_value_sign(ir_node *n);

/**
 * Return the value of a Cmp if one or both predecessors
 * are Confirm nodes.
 *
 * @param cmp    the compare node that will be evaluated
 * @param left   the left operand of the Cmp
 * @param right  the right operand of the Cmp
 * @param pnc    the compare relation
 */
tarval *computed_value_Cmp_Confirm(ir_node *cmp, ir_node *left, ir_node *right, pn_Cmp pnc);

#endif /* FIRM_OPT_CONFIRMS_H */
