/*
 * Project:     libFIRM
 * File name:   testprograms/irr_loop_example.c
 * Purpose:     Test Phi construction with irregular control flow.
 * Author:      Christian Schaefer, Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

# include <stdio.h>
# include <string.h>

# include "irvrfy.h"
# include "irdump.h"
# include "firm.h"

/**
 *  This file constructs a control flow of following shape:
 *
 * method loop1:
 *
 *         firstBlock
 *          /      \
 *         /        \
 *       |/_        _\|
 *            ---->
 * LoopBlock1       LoopBlock2
 *            <----
 *        \              /
 *         \            /
 * 	   _\|        |/_
 *           nextBlock
 *
 * method loop2:
 *
 *         scndBlock  <---------- firstBlock
 *          /      \                  \
 *         /        \                  \
 *       |/_        _\|                _\|
 *            ---->              ---->
 * LoopBlock1       LoopBlock2 1       LoopBlock3
 *            <----              <----
 *        \                             /
 *         \                           /
 * 	   _\|                        /
 *           nextBlock   <---------- /
 *
 *
 **/

int main(int argc, char **argv)
{
  ir_graph *irg;        /* this variable contains the irgraph */
  type     *owner;      /* the class in which this method is defined */
  type     *proc_tp;  /* typeinformation for the method main */
  entity   *ent;        /* represents this method as entity of owner */
  ir_node  *expr, *c1, *c2, *c3, *c4, *c5, *cond, *f, *t, *f2, *loopBlock1, *f_l1, *t_l1,
           *loopBlock2, *f_l2, *t_l2, *loopBlock3, *f_l3, *t_l3, *x;
  int       i;


  /* init library */
  init_firm (NULL);
  set_opt_constant_folding (0);  /* so that the stupid tests are not optimized. */
  set_opt_cse(1);
  set_opt_dead_node_elimination(1);
  turn_off_edge_labels();

  /* FIRM was designed for oo languages where all methods belong to a class.
   * For imperative languages like C we view a file as a large class containing
   * all functions as methods in this file.
   * Therefore we define a class "empty" according to the file name
   * with a method main as an entity.
   */
#define CLASSNAME "IRR_LOOP"
#define METHOD_TP_NAME "METH_TP_NOARG_NORES"
#define NRARGS 0
#define NRES 0
#define NUM_OF_LOCAL_VARS 0

  owner = new_type_class (new_id_from_str (CLASSNAME));
  printf("\nCreating testprogram: %s...\n", CLASSNAME);
  proc_tp = new_type_method(new_id_from_str(METHOD_TP_NAME), NRARGS, NRES);

  /* --- method loop1 ------------------------------------------------------ */

#define METHODNAME "loop1"

  ent = new_entity (owner, new_id_from_str (METHODNAME), proc_tp);
  get_entity_ld_name(ent); /* To enforce name mangling for vcg graph name */

  irg = new_ir_graph (ent, NUM_OF_LOCAL_VARS);

  /* to make three conditionals  */
  expr = new_Const (mode_Is, new_tarval_from_long (0, mode_Is));
  c1 = new_Const (mode_Is, new_tarval_from_long (1, mode_Is));
  c2 = new_Const (mode_Is, new_tarval_from_long (2, mode_Is));
  c3 = new_Const (mode_Is, new_tarval_from_long (2, mode_Is));

  cond = new_Cond(new_Proj(new_Cmp(expr, c1), mode_b, Eq));
  f = new_Proj(cond, mode_X, 0);
  t = new_Proj(cond, mode_X, 1);
  mature_immBlock(get_irg_current_block(irg));

  loopBlock1 = new_immBlock();
  add_immBlock_pred(loopBlock1, t);
  cond = new_Cond(new_Proj(new_Cmp(expr, c2), mode_b, Eq));
  f_l1 = new_Proj(cond, mode_X, 0);
  t_l1 = new_Proj(cond, mode_X, 1);

  loopBlock2 = new_immBlock();
  add_immBlock_pred(loopBlock2, f);
  cond = new_Cond(new_Proj(new_Cmp(expr, c3), mode_b, Eq));
  f_l2 = new_Proj(cond, mode_X, 0);
  t_l2 = new_Proj(cond, mode_X, 1);

  add_immBlock_pred(loopBlock1, t_l2);
  add_immBlock_pred(loopBlock2, t_l1);
  mature_immBlock(loopBlock1);
  mature_immBlock(loopBlock2);

  new_immBlock();
  add_immBlock_pred(get_irg_current_block(irg), f_l2);
  add_immBlock_pred(get_irg_current_block(irg), f_l1);
  x = new_Return (get_store(), 0, NULL);
  mature_immBlock (get_irg_current_block(irg));

  add_immBlock_pred (get_irg_end_block(irg), x);
  mature_immBlock (get_irg_end_block(irg));

  finalize_cons (irg);

  /* --- method loop2 ------------------------------------------------------ */

#undef METHODNAME
#define METHODNAME "loop2"

  ent = new_entity (owner, new_id_from_str (METHODNAME), proc_tp);
  get_entity_ld_name(ent); /* To enforce name mangling for vcg graph name */

  irg = new_ir_graph (ent, NUM_OF_LOCAL_VARS);

  /* to make several conditionals  */
  expr = new_Const (mode_Is, new_tarval_from_long (0, mode_Is));
  c1   = new_Const (mode_Is, new_tarval_from_long (1, mode_Is));
  c2   = new_Const (mode_Is, new_tarval_from_long (2, mode_Is));
  c3   = new_Const (mode_Is, new_tarval_from_long (3, mode_Is));
  c4   = new_Const (mode_Is, new_tarval_from_long (4, mode_Is));
  c5   = new_Const (mode_Is, new_tarval_from_long (5, mode_Is));

  cond = new_Cond(new_Proj(new_Cmp(expr, c1), mode_b, Eq));
  f2 = new_Proj(cond, mode_X, 0);
  t = new_Proj(cond, mode_X, 1);
  mature_immBlock(get_irg_current_block(irg));

  new_immBlock();
  add_immBlock_pred(get_irg_current_block(irg), t);
  cond = new_Cond(new_Proj(new_Cmp(expr, c5), mode_b, Eq));
  f = new_Proj(cond, mode_X, 0);
  t = new_Proj(cond, mode_X, 1);
  mature_immBlock(get_irg_current_block(irg));

  loopBlock1 = new_immBlock();
  add_immBlock_pred(loopBlock1, t);
  cond = new_Cond(new_Proj(new_Cmp(expr, c2), mode_b, Eq));
  f_l1 = new_Proj(cond, mode_X, 0);
  t_l1 = new_Proj(cond, mode_X, 1);

  loopBlock2 = new_immBlock();
  add_immBlock_pred(loopBlock2, f);
  cond = new_Cond(new_Proj(new_Cmp(expr, c3), mode_b, Eq));
  f_l2 = new_Proj(cond, mode_X, 0);
  t_l2 = new_Proj(cond, mode_X, 1);

  loopBlock3 = new_immBlock();
  add_immBlock_pred(loopBlock3, f2);
  cond = new_Cond(new_Proj(new_Cmp(expr, c4), mode_b, Eq));
  f_l3 = new_Proj(cond, mode_X, 0);
  t_l3 = new_Proj(cond, mode_X, 1);

  add_immBlock_pred(loopBlock1, t_l2);
  add_immBlock_pred(loopBlock2, t_l1);
  add_immBlock_pred(loopBlock3, f_l2);
  add_immBlock_pred(loopBlock2, t_l3);
  mature_immBlock(loopBlock1);
  mature_immBlock(loopBlock2);
  mature_immBlock(loopBlock3);

  new_immBlock();
  add_immBlock_pred(get_irg_current_block(irg), f_l1);
  add_immBlock_pred(get_irg_current_block(irg), f_l3);
  x = new_Return (get_store(), 0, NULL);
  mature_immBlock (get_irg_current_block(irg));

  add_immBlock_pred (get_irg_end_block(irg), x);
  mature_immBlock (get_irg_end_block(irg));

  finalize_cons (irg);

  /* --- more ...  ------------------------------------------------------ */

  printf("Optimizing ...\n");
  //dead_node_elimination(irg);

  for (i = 0; i < get_irp_n_irgs(); ++i) {
    current_ir_graph = get_irp_irg(i);
    /* verify the graph */
    irg_vrfy(current_ir_graph);
    construct_cf_backedges(current_ir_graph);
    dump_loop_tree(current_ir_graph, "");

    printf("Dumping the graph and a control flow graph.\n");
    dump_ir_block_graph(current_ir_graph, "");
    dump_cfg(current_ir_graph, "");
    printf("Use xvcg to view these graphs:\n");
    printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  }
  /* Error for loop2 */
  compute_execution_frequency(get_irp_irg(0), 10, 0.001);
  dump_interval_graph(get_irp_irg(0), "");

  return (0);
}
