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
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
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
  ir_op   *operation_code;
  ir_node *increment, *init, *op, *itervar_phi, *c, *new_phi, *new_increment, *new_init;
  ir_node *new_op, *new_add, *reducible_node;
  ir_node *old_ind, *symconst, *new_cmp, *cmp_const, *cmp_init_block, *cmp;
  int      be_pos, strong_reduced;
  int      init_pred_pos, op_pred_pos, out_loop_res, phi_pred, reducible;
  ir_loop *l_itervar_phi;
}induct_var_info;
#endif

/** If an ir_node is induction variable return info else return NULL. */
induct_var_info *is_induction_variable ( induct_var_info *info);

#endif /* _STRENGTH_RED_H_ */
