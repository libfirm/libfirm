/**
 *
 * @file irsimpeltype.c
 *
 * Project:     libFIRM
 * File name:   ir/opt/strength_red.c
 * Purpose:     Make strength reduction .
 * Author:      Beyhan Veliev
 * Modified by:
 * Created:     22.8.2003
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 *
 *
 *
 */


# include "strength_red.h"

# include "irnode_t.h"
# include "irgwalk.h"
# include "irloop_t.h"
# include "ircons.h"
# include "irgmod.h"
# include "irdump.h"




/* The information needed for a induction variable*/
struct induct_var_info {
  ir_op   *operation_code;
  ir_node *increment, *init, *op;
  int      be_pos;
  int      init_pred_pos;
  int      op_pred_pos;
};
static struct induct_var_info ivi;


/** Detect basic iteration variables.
 *
 * The variable ir represented by a subgraph as this:
 *
 *       init
 *       /|\
 *        |
 *   |-- Phi
 *   |   /|\
 *   |    |
 *   |-->op
 *
 * Where op is a Add or Sub, and init is loop invariant.
 * @@@ So far we only accept Phi nodes with two predecessors.
 * We could expand this to Phi nodes where all preds are either
 * op or loop invariant.
 *
 * @param n A phi node.
 */
static struct induct_var_info *is_induction_variable (ir_node *n) {

  ir_node  *phi_pred_0, *phi_pred_1, *add_r, *add_l, *sub_r, *sub_l ;
  ir_op    *phi_pred_0_op, *phi_pred_1_op;
  struct   induct_var_info *info;

  info = &ivi;
  info->operation_code = NULL;
  info->increment = NULL;
  info->init = NULL;
  info->op = NULL;
  info->be_pos = -1;
  info->init_pred_pos = -1;
  info->op_pred_pos = -1;

  assert(get_irn_op(n) == op_Phi);

  /* The necessary conditions for the phi node. */
  if (get_irn_arity(n) != 2             ||
      !has_backedges(get_nodes_block(n))  )
    return NULL;

  /* The predecessors  of the phi node. */
  phi_pred_0 = get_Phi_pred(n, 0);
  phi_pred_1 = get_Phi_pred(n, 1);

  /*The operation of the predecessors. */
  phi_pred_0_op = get_irn_op(get_Phi_pred(n, 0));
  phi_pred_1_op = get_irn_op(get_Phi_pred(n, 1));

  /*Compute if the induction variable is added or substracted wiht a constant . */
  if (phi_pred_0_op == op_Add){
    info->operation_code = op_Add;
    add_l = get_Add_left(phi_pred_0);
    add_r = get_Add_right(phi_pred_0);
    info->op_pred_pos = 0;
    if (add_l == n){
      info->increment = add_r;
    } else if (add_r == n){
      info->increment = add_l;
    } else return NULL;
  } else if (phi_pred_1_op == op_Add){
    info->operation_code = op_Add ;
    add_l = get_Add_left(phi_pred_1);
    add_r = get_Add_right(phi_pred_1);
    info->op_pred_pos = 1;
    if (add_l == n){
      info->increment = add_r;
    } else if (add_r == n){
      info->increment = add_l;
    } else return NULL;
  } else if (phi_pred_0_op == op_Sub){
    info->operation_code = op_Sub;
    sub_r = get_Sub_right(phi_pred_0);
    sub_l = get_Sub_left(phi_pred_0);
    info->op_pred_pos = 0;
    if (sub_l == n){
      info->increment = sub_r;
    } else if (sub_r == n){
      info->increment = sub_l;
    } else return NULL;
  } else if (phi_pred_1_op == op_Sub){
    info->operation_code = op_Sub;
    sub_r = get_Sub_right(phi_pred_1);
    sub_l = get_Sub_left(phi_pred_1);
    info->op_pred_pos = 1;
    if (sub_l == n){
      info->increment = sub_r;
    } else return NULL;
  } else
    return NULL;

  /*Compute the position of the backedge. */
  if (is_backedge(get_nodes_block(n), 0)){
    info->be_pos = 0;
    info->init_pred_pos = 1;
    info->op = get_Phi_pred(n, 0);
    info->init = get_Phi_pred(n, 1);
  } else if (is_backedge(get_nodes_block(n), 1)){
    info->be_pos = 1;
    info->init_pred_pos = 0;
    info->op = get_Phi_pred(n, 1);
    info->init = get_Phi_pred(n, 0);
  }

  if (info->be_pos == 0) {
    if (get_Block_dom_depth(get_nodes_block(phi_pred_1))  >=
	get_Block_dom_depth(get_nodes_block(n))) {
      return NULL;
    }
  } else if (get_Block_dom_depth(get_nodes_block(phi_pred_0))  >=
	     get_Block_dom_depth(get_nodes_block(n))) return NULL;

  if (get_Block_dom_depth(get_nodes_block(info->increment))  >=
      get_Block_dom_depth(get_nodes_block(n))) return NULL;

  return info;
}

/**
 * Reduce a node.
 *
 * @param *srong   The node to be reduce.
 * @param *env     Free environment pointer.
 *
 * The node for reduce mus be in a loop whit *phi and *add.The *phi node muss
 * have 2 predecessors a Const and a Add node. The predecessors of Add node muss * be *phi and a Const node. The nodes a, b, c  muss be Const with dom_depth <   * phi.
 */

void reduce_a_node(ir_node *strong, void *env) {
  ir_node *phi, *l, *r, *c;
  ir_op *op_strong;

  // This "if" finds the node for reduce.

  op_strong = get_irn_op(strong);
  if (op_strong == op_Mul/* || op_strong == op_Div */) {

    l = get_binop_left (strong);
    r = get_binop_right(strong);

    ir_loop *l_strong = get_irn_loop(get_nodes_block(strong));

    // This "if" finds the Phi predecessors for the node that must be reduced.
    if ((get_irn_op(l) == op_Phi)           &&
	is_induction_variable(l) != NULL    &&
	(get_irn_loop(get_nodes_block(l)) == l_strong)) {
      phi = l;
      c = r;
    } else if ((get_irn_op(r) == op_Phi)           &&
	       is_induction_variable(r) != NULL    &&
	       (get_irn_loop(get_nodes_block(r)) == l_strong)) {
      phi = r;
      c = l;
    } else return;

    if (get_Block_dom_depth(get_nodes_block(c))  >=
	get_Block_dom_depth(get_nodes_block(phi))) return;

#if 1
    printf("Reducing node: "); DDMN(strong);
    printf("  iter var is  "); DDMN(ivi.op);
    printf("  in graph     "); DDMG(current_ir_graph);
#endif

    ir_node *inc, *init, *new_phi, *in[2], *new_op, *block_init, *block_inc;
    ir_node *init_block      = get_nodes_block(ivi.init);
    ir_node *increment_block = get_nodes_block(ivi.increment);
    ir_node *c_block         = get_nodes_block(c) ;

    if (get_Block_dom_depth(increment_block) >= get_Block_dom_depth(c_block))
      block_inc = increment_block;
    else
      block_inc = c_block;

    if (get_Block_dom_depth(init_block) >= get_Block_dom_depth(c_block))
      block_init = init_block;
    else
      block_init = c_block;

    /* Compute new loop invariant increment and initialization values. */
    if (op_strong == op_Mul) {
      inc  = new_r_Mul (current_ir_graph, block_inc, ivi.increment, c, get_irn_mode(c));
      init = new_r_Mul (current_ir_graph, block_init, ivi.init, c, get_irn_mode(ivi.init));
    } else if (op_strong == op_Div) {
      inc =  new_r_Div (current_ir_graph, block_inc, get_irg_initial_mem(get_irn_irg(strong)),
			ivi.increment, c);
      init = new_r_Div (current_ir_graph, block_init, get_irg_initial_mem(get_irn_irg(strong)),
                        ivi.init, c);
    }


    /* Generate a new basic induction variable. Break the data flow loop
       initially by using an Unknown node. */
    in[ivi.op_pred_pos]   = new_Unknown(get_irn_mode(init));
    in[ivi.init_pred_pos] = init;
    new_phi = new_r_Phi(current_ir_graph, get_nodes_block(phi), 2, in, get_irn_mode(init));

    if (ivi.operation_code == op_Add)
      new_op = new_r_Add(current_ir_graph, get_nodes_block(ivi.op), inc, new_phi,
			 get_irn_mode(inc));
    else if (ivi.operation_code == op_Sub)
      new_op = new_r_Sub(current_ir_graph, get_nodes_block(ivi.op), new_phi, inc,
			 get_irn_mode(inc));
    set_Phi_pred(new_phi, ivi.op_pred_pos, new_op);

    /* Replace the use of the strength reduced value. */
    exchange(strong, new_phi);

  } else return;
}


/* Performs strength reduction for the passed graph. */
void reduce_strength(ir_graph *irg) {

 if (!get_optimize() || !get_opt_strength_red()) return;

  /* -- Precompute some information -- */
  /* Call algorithm that computes the backedges */
  construct_cf_backedges(irg);
  /* Call algorithm that computes the dominator trees. */
  compute_doms(irg);

  /* -- Search expressions that can be optimized -- */
  irg_walk_graph(irg, NULL, reduce_a_node, NULL);
}
