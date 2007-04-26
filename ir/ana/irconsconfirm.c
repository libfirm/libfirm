/*
 * Copyrigth (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief    Construction of Confirm nodes
 * @author   Michael Beck
 * @date     6.2005
 * @version  $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "irgraph_t.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "irgmod.h"
#include "iropt_dbg.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "irprintf.h"

/**
 * Walker environment.
 */
typedef struct _env_t {
  unsigned num_confirms;  /**< number of inserted Confirm nodes */
  unsigned num_consts;    /**< number of constants placed */
  unsigned num_eq;        /**< number of equalities placed */
} env_t;

/**
 * Return the effective use block of a node and it's predecessor on
 * position pos.
 *
 * @param node  the node
 * @param pos   the position of the used input
 *
 * This handles correctly Phi nodes.
 */
static ir_node *get_effective_use_block(ir_node *node, int pos)
{
  /* the effective use of a Phi is in its predecessor block */
  if (is_Phi(node))
    return get_nodes_block(get_irn_n(node, pos));
  else
    return get_nodes_block(node);
}

/**
 * Handle a CASE-branch.
 *
 * @param block   the block which is entered by the branch
 * @param irn     the node expressing the switch value
 * @param nr      the branch label
 * @param env     statistical environment
 *
 * Branch labels are a simple case. We can replace the value
 * by a Const with the branch label.
 */
static void handle_case(ir_node *block, ir_node *irn, long nr, env_t *env)
{
  const ir_edge_t *edge, *next;
  ir_node *c = NULL;

  if (is_Bad(irn))
    return;

  for (edge = get_irn_out_edge_first(irn); edge; edge = next) {
    ir_node *succ = get_edge_src_irn(edge);
    int     pos   = get_edge_src_pos(edge);
    ir_node *blk  = get_effective_use_block(succ, pos);

    next = get_irn_out_edge_next(irn, edge);

    if (block_dominates(block, blk)) {
      /*
       * Ok, we found a user of irn that is placed
       * in a block dominated by the branch block.
       * We can replace the input with the Constant
       * branch label.
       */

      if (! c) {
        ir_mode *mode = get_irn_mode(irn);
        ir_type *tp   = get_irn_type(irn);
        tarval *tv    = new_tarval_from_long(nr, mode);
        c = new_r_Const_type(current_ir_graph, block, mode, tv, tp);
      }

      set_irn_n(succ, pos, c);
      DBG_OPT_CONFIRM_C(irn, c);
//      ir_printf("1 Replacing input %d of node %n with %n\n", pos, succ, c);

      env->num_consts += 1;
    }
  }
}  /* handle_case */

/**
 * Handle an IF-branch.
 *
 * @param block   the block which is entered by the branch
 * @param cmp     the Cmp node expressing the branch condition
 * @param pnc     the Compare relation for taking this branch
 * @param env     statistical environment
 */
static void handle_if(ir_node *block, ir_node *cmp, pn_Cmp pnc, env_t *env)
{
  ir_node *left  = get_Cmp_left(cmp);
  ir_node *right = get_Cmp_right(cmp);
  ir_op *op;
  const ir_edge_t *edge, *next;

  /* Beware of Bads */
  if (is_Bad(left) ||is_Bad(right))
    return;

  op = get_irn_op(left);

  /* Do not create Confirm nodes for Cmp(Const, Const) constructs.
     These are removed anyway */
  if (op == op_Const && is_Const(right))
    return;

  /* try to place the constant on the right side for a Confirm */
  if (op == op_Const || op == op_SymConst) {
    ir_node *t = left;

    left  = right;
    right = t;

    pnc = get_inversed_pnc(pnc);
  }

  /*
   * First case: both values are identical.
   * replace the left one by the right (potentially const) one.
   */
  if (pnc == pn_Cmp_Eq) {
    for (edge = get_irn_out_edge_first(left); edge; edge = next) {
      ir_node *succ = get_edge_src_irn(edge);
      int     pos   = get_edge_src_pos(edge);
      ir_node *blk  = get_effective_use_block(succ, pos);

      next = get_irn_out_edge_next(left, edge);
      if (block_dominates(block, blk)) {
        /*
         * Ok, we found a usage of left in a block
         * dominated by the branch block.
         * We can replace the input with right.
         */
        set_irn_n(succ, pos, right);
        DBG_OPT_CONFIRM(left, right);

//        ir_printf("2 Replacing input %d of node %n with %n\n", pos, succ, right);

        env->num_eq += 1;
      }
    }
  }
  else { /* not pn_Cmp_Eq cases */
    ir_node *c = NULL;

    for (edge = get_irn_out_edge_first(left); edge; edge = next) {
      ir_node *succ = get_edge_src_irn(edge);
      int     pos   = get_edge_src_pos(edge);
      ir_node *blk  = get_effective_use_block(succ, pos);

      next = get_irn_out_edge_next(left, edge);
      if (block_dominates(block, blk)) {
        /*
         * Ok, we found a usage of left in a block
         * dominated by the branch block.
         * We can replace the input with a Confirm(left, pnc, right).
         */
        if (! c)
          c = new_r_Confirm(current_ir_graph, block, left, right, pnc);

        pos = get_edge_src_pos(edge);
        set_irn_n(succ, pos, c);
//        ir_printf("3 Replacing input %d of node %n with %n\n", pos, succ, c);

        env->num_confirms += 1;
      }
    }
  }
}  /* handle_if */

/**
 * Pre-walker: Called for every block to insert Confirm nodes
 */
static void insert_Confirm(ir_node *block, void *env)
{
  ir_node *cond, *proj, *selector;
  ir_mode *mode;

  /*
   * we can only handle blocks with only ONE control flow
   * predecessor yet.
   */
  if (get_Block_n_cfgpreds(block) != 1)
    return;

  proj = get_Block_cfgpred(block, 0);
  if (get_irn_op(proj) != op_Proj)
    return;

  cond = get_Proj_pred(proj);
  if (get_irn_op(cond) != op_Cond)
    return;

  selector = get_Cond_selector(cond);
  mode = get_irn_mode(selector);

  if (mode == mode_b) {
    ir_node *cmp;
    pn_Cmp pnc;

    /* this should be an IF, check this */
    if (get_irn_op(selector) != op_Proj)
      return;

    cmp = get_Proj_pred(selector);
    if (get_irn_op(cmp) != op_Cmp)
      return;

    pnc = get_Proj_proj(selector);

    if (get_Proj_proj(proj) != pn_Cond_true) {
      /* it's the false branch */
      pnc = get_negated_pnc(pnc, mode);
    }
//    ir_printf("At %n using %n Confirm %=\n", block, cmp, pnc);

    handle_if(block, cmp, pnc, env);
  }
  else if (mode_is_int(mode)) {
    long proj_nr = get_Proj_proj(proj);

    /* this is a CASE, but we cannot handle the default case */
    if (proj_nr == get_Cond_defaultProj(cond))
      return;

    handle_case(block, get_Cond_selector(cond), proj_nr, env);
  }
}  /* insert_Confirm */

/*
 * Construct Confirm nodes
 */
void construct_confirms(ir_graph *irg)
{
  env_t env;
  int edges_active = edges_activated(irg);

  /* we need dominance info */
  assure_doms(irg);

  assert(get_irg_pinned(irg) == op_pin_state_pinned &&
    "Nodes must be placed to insert Confirms");

  if (! edges_active) {
    /* We need edges */
    edges_activate(irg);
  }

  env.num_confirms = 0;
  env.num_consts   = 0;
  env.num_eq       = 0;

  /* now, visit all blocks and add Confirms where possible */
  irg_block_walk_graph(irg, insert_Confirm, NULL, &env);

  if (env.num_confirms | env.num_consts | env.num_eq) {
    /* we have add nodes or changed DF edges */
    set_irg_outs_inconsistent(irg);

    /* the new nodes are not in the loop info */
    set_irg_loopinfo_inconsistent(irg);
  }

#if 0
  printf("# Confirms inserted : %u\n", env.num_confirms);
  printf("# Const replacements: %u\n", env.num_consts);
  printf("# node equalities   : %u\n", env.num_eq);
#endif

  /* deactivate edges if they where off */
  if (! edges_active)
    edges_deactivate(irg);
}  /* construct_confirms */

/**
 * Post-walker: Remove Confirm nodes
 */
static void rem_Confirm(ir_node *n, void *env) {
  if (get_irn_op(n) == op_Confirm) {
    ir_node *value = get_Confirm_value(n);
    if (value != n)
      exchange(n, value);
    else {
      /*
       * Strange: a Confirm is it's own bound. This can happen
       * in dead blocks when Phi nodes are already removed.
       */
      exchange(n, new_Bad());
    }
  }
}  /* rem_Confirm */

/*
 * Remove all Confirm nodes from a graph.
 */
void remove_confirms(ir_graph *irg) {
  irg_walk_graph(irg, NULL, rem_Confirm, NULL);
}  /* remove_confirms */
