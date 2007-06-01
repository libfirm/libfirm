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
 * @file strength_red.c
 *
 * Project:     libFIRM
 * File name:   ir/opt/strength_red.c
 * Purpose:     Make strength reduction .
 * Author:      Beyhan Veliev
 * Modified by:
 * Created:     22.8.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "strength_red_t.h"
#include "iroptimize.h"
#include "irouts.h"
#include "irnode_t.h"
#include "irgwalk.h"
#include "irloop_t.h"
#include "ircons.h"
#include "irgmod.h"
#include "irdump_t.h"
#include "irprintf.h"
#include "firmstat.h"


/** Counter for verbose information about optimization. */
static int n_reduced_expressions;
static int n_made_new_phis;

/** Detect basic iteration variables.
 *
 * The variable is represented by a subgraph as this:
 * @verbatim
 *
 *       init
 *        ^
 *        |
 *   +-- Phi
 *   |    ^
 *   |    |
 *   +-->op
 *
 * @endverbatim
 *
 * Where op is a Add or Sub node and init is loop invariant.
 *
 * @note
 * So far we only accept Phi nodes with two predecessors.
 * We could expand this to Phi nodes where all predecessors
 * are either op or loop invariant.
 *
 * @param info  After call contains the induction variable information.
 *
 * @return
 *   The info structure.
 */
induct_var_info *is_induction_variable(induct_var_info *info) {

  int i, q, r;
  int op_pred, Store_in_op, Store_in_phi;
  ir_node *cmp_pred_bl, *cond_succ_0, *cond_succ_1, *cmp_const;
  ir_node *loop_head;
  ir_node *cmp_const_block;

  info->operation_code   = NULL; /* The opcode of "op". */
  info->increment        = NULL; /* The value which increase or decrease the iteration variable. */
  info->init             = NULL; /* The start value of the iteration variable. */
  info->op               = NULL; /* The operation which increase or decrease the iteration variable. */
  info->l_itervar_phi    = NULL; /* The iteration variable. */
  info->new_phi          = NULL; /* The new iteration variable. */
  info->new_increment    = NULL; /* The new increment which replace the old one.*/
  info->new_init         = NULL; /* The new init value of the iteration variable. */
  info->new_op           = NULL; /* The new operation that we need after replace. */
  info->new_cmp          = NULL; /* The new Cmp which replaces the old one. */
  info->cmp              = NULL; /* The Cmp which breaks the loop and compares the iteration variable with a constant. */
  info->cmp_const        = NULL; /* The other operand of Cmp. */
  info->cmp_init_block   = NULL; /* The initial block of the Cmp. */
  info->reducible_node   = NULL; /* The reducible nodes are save here. */
  info->is_reducible     = 0;    /* To save information if anything is reducible. */
  info->phi_pred         = 0;    /* To save the value of iteration variable predecessors. */
  info->init_pred_pos    = -1;   /* To save the position of iteration variable start value. */
  info->op_pred_pos      = -1;   /* To save the backedge of iteration variable. */
  info->l_itervar_phi    = NULL; /* Information about loop of itervar_phi. */

  assert(get_irn_op(info->itervar_phi) == op_Phi);

  /*
   * The necessary conditions for the phi node:
   * We can handle currently Phi's with 2 predecessors, one must be a backedge.
   */
  if (get_irn_arity(info->itervar_phi) != 2 || !has_backedges(get_nodes_block(info->itervar_phi)))
    return NULL;

  for (i = 0; i < 2; ++i) {
    ir_node *pred = get_Phi_pred(info->itervar_phi, i);
    ir_op *op = get_irn_op(pred);

    /* Compute if the induction variable is added or subtracted with a constant. */
    if (op == op_Add || op == op_Sub) {
      ir_node *n_l = get_binop_left(pred);
      ir_node *n_r = get_binop_right(pred);

      if (n_l == info->itervar_phi) {
        info->operation_code = op;
        info->increment      = n_r;
        info->op_pred_pos    = i;
        info->init_pred_pos  = i ^ 1;
        break;
      }
      else if (n_r == info->itervar_phi) {
        info->operation_code = op;
        info->increment      = n_l;
        info->op_pred_pos    = i;
        info->init_pred_pos  = i ^ 1;
        break;
      }
    }
  }
  /* check if we found something */
  if (! info->operation_code)
    return NULL;

  /* Compute the position of the backedge. */
  if (is_backedge(get_nodes_block(info->itervar_phi), info->op_pred_pos)) {
    info->op   = get_Phi_pred(info->itervar_phi, info->op_pred_pos);
    info->init = get_Phi_pred(info->itervar_phi, info->init_pred_pos);
  }
  else {
    /* irregular control flow detected. */
    return NULL;
  }

  /*
   * the block of the init code should dominate the loop, else
   * we have an irregular control flow
   */
  if (get_Block_dom_depth(get_nodes_block(info->init))  >=
      get_Block_dom_depth(get_nodes_block(info->itervar_phi))) {
    return NULL;
  }

  op_pred      = get_irn_n_outs(info->op);
  Store_in_op  = 0;
  Store_in_phi = 0;

  /* Information about loop of itervar_phi. */
  info->l_itervar_phi = get_irn_loop(get_nodes_block(info->itervar_phi));


  info->phi_pred = get_irn_n_outs(info->itervar_phi);
  loop_head = get_nodes_block(info->itervar_phi);

  /*
   * This "for" searches for the Cmp successor of the
   * iter_var to reduce and marks if the iter_var have a Store
   * successor or a successor out of loop.
   */
  for (i = 0; i < info->phi_pred; i++) {
    ir_node *out = get_irn_out(info->itervar_phi, i);
    ir_op   *out_op = get_irn_op(out);

    if (out_op == op_Store)
      Store_in_phi++;
    else if (out_op == op_Cmp && !is_loop_invariant(out, loop_head)) {
      /* a Cmp can have more than one successor therefore we need this loop.*/
      for (q = get_irn_n_outs(out) - 1; q >= 0; --q) {
        ir_node *proj = get_irn_out(out, q);

        for (r = get_irn_n_outs(proj) -1; r >= 0; --r) {
          cmp_pred_bl = get_irn_out(proj, r);

          /* The wanted Cmp must be followed by a Cond successor
             not by a Mux. */
          if (get_irn_op(cmp_pred_bl) != op_Cond)
            continue;

          /* the binary Cond should have two successors */
          if (get_irn_n_outs(cmp_pred_bl) != 2)
            continue;

          cond_succ_0 = get_irn_out(cmp_pred_bl, 0);
          cond_succ_1 = get_irn_out(cmp_pred_bl, 1);

          if (is_loop_invariant(get_irn_out(cond_succ_1, 0), loop_head) ||
            is_loop_invariant(get_irn_out(cond_succ_0, 0), loop_head)) {
            if (get_Cmp_left(out) == info->itervar_phi)
              cmp_const = get_Cmp_right(out);
            else
              cmp_const = get_Cmp_left(out);
          } else
            continue;
          if (info->cmp == NULL) {
            /* A cmp is found.*/
            info->cmp       = out;
            info->cmp_const = cmp_const;
          }
          else {
            /* We have more then one cmp with our requests, that mean cmp isn't found */
            info->cmp = NULL;
            return NULL;
          }
        }
      }
    }
  }

  for (i = 0; i < op_pred; ++i) {
    ir_node *out  = get_irn_out(info->op, i);
    ir_op *out_op = get_irn_op(out);

    if (out_op == op_Store)
      Store_in_op++;
    else if (out_op == op_Cmp && !is_loop_invariant(out, loop_head)) {
      /* a Cmp can have more as one successor therefore
         I need this for loop. */
      for (q = get_irn_n_outs(out) - 1; q >= 0; --q) {
        ir_node *proj = get_irn_out(out, q);

        for (r = get_irn_n_outs(proj) -1; r >= 0; --r) {
          cmp_pred_bl = get_irn_out(proj, r);

          /* The wanted Cmp must be followed by a Cond successor. */
          if (get_irn_op(cmp_pred_bl) != op_Cond)
            continue;

          cond_succ_0 = get_irn_out(cmp_pred_bl, 0);
          cond_succ_1 = get_irn_out(cmp_pred_bl, 1);

          if (is_loop_invariant(get_irn_out(cond_succ_0, 0), loop_head) ||
            is_loop_invariant(get_irn_out(cond_succ_1, 0), loop_head)) {
            if (get_Cmp_left(out) == info->op)
              cmp_const = get_Cmp_right(out);
            else
              cmp_const = get_Cmp_left(out);
          } else
            continue;
          if (info->cmp == NULL) {
            /* A cmp is found*/
            info->cmp = out;
            info->cmp_const = cmp_const;
          }
          else {
            /* We have more then one cmp with our requests, that mean cmp isn't found*/
            info->cmp = NULL;
            return NULL;
          }
        }
      }
    }
  }

  if ((info->phi_pred == 3 && op_pred == 1 && Store_in_phi == 0 && info->cmp != NULL)  ||
      (info->phi_pred == 2 && op_pred == 2 && Store_in_op == 0 && info->cmp != NULL )  ||
      (info->phi_pred == 1 && Store_in_op == 0))
    info->is_reducible = 1;

  /* Search for loop invariant of Cmp.*/
  if (info->cmp != NULL) {
    cmp_const_block = get_nodes_block(info->cmp_const);
    if (get_Block_dom_depth(get_nodes_block(info->init)) >=
	      get_Block_dom_depth(cmp_const_block))
      info->cmp_init_block = get_nodes_block(info->init);
    else
      info->cmp_init_block = cmp_const_block;
  }
  return info;
}

/**
 * Creates a new Add node with the correct mode from its two operands.
 */
static INLINE ir_node *
my_new_r_Add(ir_graph *irg, ir_node *b, ir_node *op1, ir_node *op2) {
  ir_mode *m  = get_irn_mode(op1);
  ir_mode *m2 = get_irn_mode(op2);

  if (mode_is_reference(m2))
    m = m2;

  return new_r_Add(irg, b, op1, op2, m);
}

/**
 * Creates a new Sub node with the correct mode from its two operands.
 */
static INLINE ir_node *
my_new_r_Sub(ir_graph *irg, ir_node *b, ir_node *op1, ir_node *op2) {
  ir_mode *m  = get_irn_mode(op1);
  ir_mode *m2 = get_irn_mode(op2);

  if (mode_is_reference(m) && mode_is_reference(m2))
    m = mode_Is;        /* FIXME: may be other mode! */
  else if (mode_is_reference(m2))
    m = m2;
  return new_r_Sub(irg, b, op1, op2, m);
}

/**
 * Reduce an Add, Sub or Mul node
 *
 * @param *reduce_var  The node to reduce.
 * @param *ivi         Contains the induction variable information.
 */
static int reduce(ir_node *reduce_var, induct_var_info *ivi)
{
  ir_node *iter_varblk, *init_block, *irg_startblk, *block_init;

  /* Essential conditions for a reducible node. */
  if (get_irn_loop(get_nodes_block(reduce_var)) != ivi->l_itervar_phi)
    return 0;

  iter_varblk  = get_nodes_block(ivi->itervar_phi);
  init_block   = get_nodes_block(ivi->init);
  irg_startblk = get_irg_start_block(current_ir_graph);

  /* The "new_init" and the "new_cmp_const" must not be in the start block.*/
  if (get_Block_dom_depth(init_block) > get_Block_dom_depth(irg_startblk) &&
      init_block != iter_varblk)
    block_init = init_block;
  else
    block_init = get_nodes_block(get_Block_cfgpred(iter_varblk, ivi->init_pred_pos));

  /* To avoid that cmp is placed in the start block.*/
  if (ivi->cmp_init_block == irg_startblk)
    ivi->cmp_init_block = iter_varblk;

  if (get_irn_op(reduce_var) == op_Mul) {
    ir_node *mul_init  = NULL;
    ir_node *mul_const = NULL;

    /* Search for constant and init of strong. */
    ir_node  *mul_right = get_Mul_right(reduce_var);
    ir_node  *mul_left  = get_Mul_left(reduce_var);
    ir_op *mul_right_op = get_irn_op(mul_right);
    ir_op  *mul_left_op = get_irn_op(mul_left);

    ir_node *in[2];
    ir_node *block_inc;

    ir_node *increment_block;
    ir_node *c_block;

    n_reduced_expressions++;

    if (mul_right_op == op_Const) {
      mul_const = mul_right;
      mul_init  = mul_left;
    }
    else if (mul_left_op == op_Const) {
      mul_const = mul_left;
      mul_init  = mul_right;
    }

    if (mul_const == NULL || mul_init == NULL)
      return 0;

    increment_block = get_nodes_block(ivi->increment);
    c_block         = get_nodes_block(mul_const);

    if (get_Block_dom_depth(increment_block) >= get_Block_dom_depth(c_block))
      block_inc = increment_block;
    else
      block_inc = c_block;

    if (! ivi->is_reducible){
      int reduce_var_pred;

      /* Essential condition for the constant of strong. */
      if (get_Block_dom_depth(get_nodes_block(mul_const))  >=
          get_Block_dom_depth(get_nodes_block(ivi->itervar_phi)))
        return 0;

      n_made_new_phis++;
      if (get_opt_strength_red_verbose() && get_firm_verbosity() > 1) {
        ir_printf("The new Phi node is : %+F\n", ivi->itervar_phi);
        ir_printf("reducing operation is : %+F\n", reduce_var);
        ir_printf("in graph : %+F\n", current_ir_graph);
      }

      ivi->new_increment  = new_r_Mul (current_ir_graph, block_inc, ivi->increment, mul_const,
                                       get_irn_mode(mul_const));
      if (!(get_irn_op(mul_init) == op_Phi)){
        ivi->new_init = new_r_Mul (current_ir_graph, block_init, ivi->init, mul_const,
                                   get_irn_mode(mul_const));
        ivi->new_init = my_new_r_Add(current_ir_graph, block_init, ivi->new_init,
                                    ivi->new_increment);
      } else
        ivi->new_init = new_r_Mul (current_ir_graph, block_init, ivi->init, mul_const,
                                   get_irn_mode(mul_const));

      /* Generate a new basic induction variable. Break the data flow loop
         initially by using an Unknown node. */

      in[ivi->op_pred_pos]   = new_Unknown(get_irn_mode(ivi->new_init));

      in[ivi->init_pred_pos] = ivi->new_init;
      ivi->new_phi = new_r_Phi(current_ir_graph, get_nodes_block(ivi->itervar_phi), 2, in,
                               get_irn_mode(mul_const));
      mark_irn_visited(ivi->new_phi);

      if (ivi->operation_code == op_Add)
        ivi->new_op = my_new_r_Add(current_ir_graph, get_nodes_block(ivi->op),
                                  ivi->new_increment,ivi-> new_phi);
      else if (ivi->operation_code == op_Sub)
        ivi->new_op = my_new_r_Sub(current_ir_graph, get_nodes_block(ivi->op),ivi-> new_phi,
                                   ivi->new_increment);

      set_Phi_pred(ivi->new_phi, ivi->op_pred_pos, ivi->new_op);

      // This for search for a reducible successor of reduc_var.
      reduce_var_pred =  get_irn_n_outs(reduce_var);
      if (reduce_var_pred == 1) {
        ir_node *old_ind =get_irn_out(reduce_var, 0);
        if(get_irn_op(old_ind) == op_Add || get_irn_op(old_ind) == op_Sub ||
           get_irn_op(old_ind) == op_Mul){
          ivi->is_reducible = 1;
          ivi->reducible_node = old_ind;
        }
      }
      /* Replace the use of the strength reduced value. */
      exchange(reduce_var, ivi->new_phi);
      return 1;
    }
    else { /* ivi->is_reducible */
      if (ivi->new_phi == NULL) {
        ivi->init = new_r_Mul (current_ir_graph, block_init,
                               mul_const, ivi->init,
                               get_irn_mode(mul_const));
        if(ivi->cmp != NULL)
          ivi->cmp_const = new_r_Mul (current_ir_graph, ivi->cmp_init_block,
                                      ivi->cmp_const, mul_const, get_irn_mode(mul_const));
        ivi->increment = new_r_Mul (current_ir_graph, block_inc,
                                    ivi->increment, mul_const, get_irn_mode(mul_const));
      }
      else {
        ivi->new_init = new_r_Mul (current_ir_graph, block_init,
                                   mul_const, ivi->new_init,
                                   get_irn_mode(mul_const));
        ivi->new_increment = new_r_Mul (current_ir_graph, block_inc,
                                        ivi->new_increment, mul_const,
                                        get_irn_mode(mul_const));
      }
      if (get_opt_strength_red_verbose() && get_firm_verbosity() > 1) {
        ir_printf("\nReducing operation is : %+F\n", reduce_var);
        ir_printf("in graph : %+F\n", current_ir_graph);
      }
      return 1;
    }
  }
  else if (get_irn_op (reduce_var) == op_Add){
    ir_node *add_init  = NULL;
    ir_node *add_const = NULL;

    /* Search for constant of add. */
    ir_node  *add_right = get_Add_right(reduce_var);
    ir_node  *add_left  = get_Add_left(reduce_var);
    ir_op *add_right_op = get_irn_op(add_right);
    ir_op  *add_left_op = get_irn_op(add_left);

    ++n_reduced_expressions;

    if (add_right_op != op_Const)
      add_init = add_right;
    else if (add_left_op != op_Const)
      add_init = add_left;
    if (add_right_op == op_Const || add_right_op == op_SymConst)
      add_const = add_right;
    else if (add_left_op == op_Const || add_left_op == op_SymConst)
      add_const = add_left;
    if (add_const == NULL)
      return 0;
    if (ivi->new_phi == NULL) {
      ivi->init = my_new_r_Add(current_ir_graph, block_init,
                               add_const, ivi->init);
      if (ivi->cmp != NULL)
        ivi->cmp_const = my_new_r_Add(current_ir_graph, ivi->cmp_init_block,
                                      add_const, ivi->cmp_const);
    }
    else {
      ivi->new_init = my_new_r_Add(current_ir_graph, block_init,
                                   add_const, ivi->new_init);
    }
    if (get_opt_strength_red_verbose() && get_firm_verbosity() > 1) {
      ir_printf("\nReducing operation is : %+F\n", reduce_var);
      ir_printf("in graph : %+F\n", current_ir_graph);
    }
    return 1;
  }
  else if (get_irn_op(reduce_var) == op_Sub) {
    ir_node *sub_init  = NULL;
    ir_node *sub_const = NULL;
    ir_node  *sub_right = get_Sub_right(reduce_var);
    ir_node  *sub_left  = get_Sub_left(reduce_var);
    ir_op *sub_right_op = get_irn_op(sub_right);
    ir_op  *sub_left_op = get_irn_op(sub_left);

    ++n_reduced_expressions;

    /* Search for constant of Sub. */
    if (sub_right_op != op_Const)
      sub_init = sub_right;
    else if (sub_left_op != op_Const)
      sub_init = sub_left;
    if (sub_right_op == op_Const)
      sub_const = sub_right;
    else if (sub_left_op == op_Const)
      sub_const = sub_left;

    if (sub_const == NULL)
      return 0;

    if (ivi->new_phi == NULL) {
      ivi->init = my_new_r_Sub(current_ir_graph,  block_init,
                               ivi->init, sub_const);
      if (ivi->cmp != NULL)
        ivi->cmp_const = my_new_r_Sub(current_ir_graph, ivi->cmp_init_block,
                                      ivi->cmp_const,sub_const);
    } else
      ivi->new_init = my_new_r_Sub (current_ir_graph, block_init,
                                    ivi->new_init, sub_const);
    if (get_opt_strength_red_verbose() && get_firm_verbosity() > 1) {
      ir_printf("\nReducing operation is : %+F", reduce_var);
      ir_printf("in graph : %+F", current_ir_graph);
    }
    return 1;
  }
  return 0;
}

/**
 * Search for reducible successor of iteration variable.
 * If such successor is found it will be reduced and returned,
 * else return NULL.
 *
 * @param ivi   Contains information about the induction variable.
 * @param out   A successor of iteration variable.
 */
static ir_node *reducible(ir_node *out, induct_var_info *ivi)
{
  ir_node *reduced = NULL;
  int pred;

  for (pred = 1; pred == 1; pred = get_irn_n_outs(out)) {
    if (reduce(out, ivi))
      reduced = out;
    else
      return reduced;
    out = get_irn_out(out, 0);
  }
  return reduced;
}

/**
 * Post walker: Find a Phi node that is a iteration variable and
 * try to reduce it.
 *
 * @param itervar_phi   The iteration variable of a loop.
 * @param env           Free environment pointer.
 */
static void reduce_itervar(ir_node *itervar_phi, void *env)
{
  induct_var_info ivi;
  /* check if a iteration variable be reduced.*/
  int reduced = 0;

  if (! is_Phi(itervar_phi))
    return;

  /* A potential candidate is found.*/
  ivi.itervar_phi = itervar_phi;

  /* check if it's a induction variable.*/
  if (is_induction_variable(&ivi)) {
    int i, op_out;

    for (i = 0; i < ivi.phi_pred; i++) {
      ir_node *out = get_irn_out(ivi.itervar_phi, i);
      ir_op   *out_op = get_irn_op(out);
      /* Reduce a induction variable.*/
      if (ivi.is_reducible) {
        if (ivi.phi_pred == 3 && out != ivi.op && out != ivi.cmp) {
          ir_node *irn_reduced = reducible(out, &ivi);
          if (irn_reduced != NULL){
            reduced = 1;
            exchange(irn_reduced, ivi.itervar_phi);
          }
        }
      }
      /* Reduce a multiplication*/
      else if (out_op == op_Mul)
        if (reduce(out, &ivi) && ivi.is_reducible) {
          ir_node *reduced = reducible(ivi.reducible_node, &ivi);

          if (reduced != NULL)
            exchange(reduced, ivi.new_phi);

          ivi.is_reducible = 0;
          set_Phi_pred(ivi.new_phi, ivi.init_pred_pos, ivi.new_init);
          set_irn_mode(ivi.new_phi,get_irn_mode(ivi.new_init));
          set_irn_mode(ivi.new_op,get_irn_mode(ivi.new_phi));
        }
    }

    op_out = get_irn_n_outs(ivi.op);
    for (i = 0; i < op_out; i++){
      ir_node *out = get_irn_out(ivi.op, i);
      ir_op   *out_op = get_irn_op(out);
      /* Try to reduce the second successor of the "ivi.op"*/
      if (op_out == 2 && out != ivi.itervar_phi){
        ir_node *reduced = reducible(out, &ivi);
        if(reduced != NULL)
          exchange( reduced, ivi.op);
      }
      /* Try to reduce a multiplication, that is successor of "ivi.op".*/
      else if (out_op == op_Mul)
        if (reduce(out, &ivi) && ivi.is_reducible){
          ir_node *reduced = reducible(ivi.reducible_node, &ivi);
          if(reduced != NULL)
            exchange(reduced, ivi.new_phi);
          ivi.is_reducible = 0;
          set_Phi_pred(ivi.new_phi, ivi.init_pred_pos, ivi.new_init);
          set_irn_mode(ivi.new_phi,get_irn_mode(ivi.new_init));
          set_irn_mode(ivi.new_op,get_irn_mode(ivi.new_phi));
        }
    }
    /* Set some predecessors and modes after reduce.*/
    if (ivi.is_reducible && reduced) {
      if(get_irn_op(ivi.op) == op_Add)
        if(get_Add_left(ivi.op) == ivi.itervar_phi)
          set_Add_right(ivi.op, ivi.increment);
        else
          set_Add_left(ivi.op, ivi.increment);
      else if(get_Sub_left(ivi.op) == ivi.itervar_phi)
        set_Sub_right(ivi.op, ivi.increment);
      else
        set_Sub_right(ivi.op, ivi.increment);
      set_Phi_pred(ivi.itervar_phi, ivi.init_pred_pos, ivi.init);
      set_irn_mode(ivi.itervar_phi, get_irn_mode(ivi.init));
      set_irn_mode(ivi.op, get_irn_mode(ivi.itervar_phi));
      if (ivi.cmp != NULL){
        set_irn_mode(ivi.cmp_const, get_irn_mode(ivi.itervar_phi));
        if(get_Cmp_left(ivi.cmp) == ivi.itervar_phi)
          set_Cmp_right(ivi.cmp, ivi.cmp_const);
        else
          set_Cmp_left(ivi.cmp, ivi.cmp_const);
      }
    }
  }
}

/* Performs strength reduction for the passed graph. */
void reduce_strength(ir_graph *irg) {
  ir_graph *rem = current_ir_graph;
  int modifiyed = 1;

  if (!get_optimize() || !get_opt_strength_red()) return;

  current_ir_graph = irg;

  n_reduced_expressions = 0;
  n_made_new_phis = 0;
  /* -- Precompute some information -- */
  /* Call algorithm that computes the backedges */
  construct_cf_backedges(irg);
  /* Call algorithm that computes the dominator trees. */
  assure_doms(irg);
  /* Call algorithm that computes the out edges */
  assure_irg_outs(irg);

  /* -- Search expressions that can be optimized -- */
  irg_walk_graph(irg, NULL, reduce_itervar, NULL);

  if (get_opt_strength_red_verbose()) {
    printf ("\n %d made new_phis und  ", n_made_new_phis);
    printf("reduced %d iteration variables "
           "in \n graph %s.%s.\n", n_reduced_expressions,
       get_type_name(get_entity_owner(get_irg_entity(irg))),
       get_entity_name(get_irg_entity(irg)));
  }

  if (modifiyed) {
    set_irg_outs_inconsistent(irg);
    set_irg_loopinfo_inconsistent(irg);
  }

  current_ir_graph = rem;
}
