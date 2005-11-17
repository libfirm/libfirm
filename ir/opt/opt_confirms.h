/*
 * Project:     libFIRM
 * File name:   ir/opt/opt_confirms.h
 * Purpose:     Optimizations regarding Confirm nodes
 * Author:      Michael Beck
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _OPT_CONFIRMS_H_
#define _OPT_CONFIRMS_H_

#include "irnode.h"

/**
 * @file opt_confirms.h
 *
 * Optimizations regarding Confirm nodes.
 * These optimiztions are not means to be run from
 * frontends, they are called from iropt.
 */

/**
 * Possible return values of value_classify().
 */
typedef enum _value_classify {
  VALUE_UNKNOWN  = 0,   /**< could not classify */
  VALUE_POSITIVE = 1,   /**< value is positive, i.e. >= 0 */
  VALUE_NEGATIVE = -1   /**< value is negative, i.e. <= 0 if
                             no signed zero exists or < 0 else */
} value_classify;

/**
 * Check, if the value of a node is != 0.
 *
 * This is a often needed case, so we handle here Confirm
 * nodes too.
 *
 * @param n  a node representing the value
 */
int value_not_zero(ir_node *n);

/*
 * Check, if the value of a node is != NULL.
 *
 * This is a often needed case, so we handle here Confirm
 * nodes too.
 *
 * @param n  a node representing the value
 */
int value_not_null(ir_node *n);

/**
 * Check, if the value of a node can be confirmed >= 0 or <= 0,
 * If the mode of the value did not honor signed zeros, else
 * check for >= 0 or < 0.
 *
 * @param n  a node representing the value
 */
value_classify classify_value_sign(ir_node *n);

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

#endif /* _OPT_CONFIRMS_H_ */
