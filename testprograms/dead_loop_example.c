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

#include <stdio.h>
#include <string.h>

#include <libfirm/firm.h>

/**
 *  This file constructs a control flow with an unreachable
 *  loop _and_ an unreachable endless loop.  This looks like:
 *
 *    LoopBlock2			  LoopBlock2'
 *     |    /|\				   |    /|\
 *     |     |               		   |     |
 *    \|/    |     			  \|/    |
 *    LoopBlock1    StartBlock 	          LoopBlock1'
 *        \              /
 *         \            /
 * 	   _\|        |/_
 *          ReturnBlock
 *              |
 *              |
 *             \|/
 *           nextBlock
 *
 *
 **/

int main(void)
{
  ir_graph *irg;        /* this variable contains the irgraph */
  ir_type     *prim_t_int;
  ir_type     *owner;      /* the class in which this method is defined */
  ir_type     *proc_main;  /* typeinformation for the method main */
  ir_entity   *ent;        /* represents this method as ir_entity of owner */
  ir_node  *returnBlock, *loopBlock1, *loopBlock2, *x, *c1, *c2, *t, *f;


  /* init library */
  init_firm(NULL);
  /*set_opt_normalize(0); */
  set_opt_constant_folding(0);  /* so that the stupid tests are not optimized. */
  set_opt_cse(1);

  /* FIRM was designed for oo languages where all methods belong to a class.
   * For imperative languages like C we view a file as a large class containing
   * all functions as methods in this file.
   * Therefore we define a class "empty" according to the file name
   * with a method main as an ir_entity.
   */
#define CLASSNAME "DEAD_LOOP"
#define METHODNAME "main"
#define NRARGS 1
#define NRES 0
  printf("\nCreating an IR graph: %s...\n", CLASSNAME);

  prim_t_int = new_type_primitive(new_id_from_str("int"), mode_Is);

  owner = new_type_class(new_id_from_str(CLASSNAME));
  proc_main = new_type_method(new_id_from_str(METHODNAME), NRARGS, NRES);
  set_method_param_type(proc_main, 0, prim_t_int);
  ent = new_entity(owner, new_id_from_str(METHODNAME), proc_main);
  get_entity_ld_name(ent); /* To enforce name mangling for vcg graph name */

#define NUM_OF_LOCAL_VARS 0

  irg = new_ir_graph(ent, NUM_OF_LOCAL_VARS);

  returnBlock = get_irg_current_block(irg);

  /* Make some real stupid stuff: a data loop (without Phi). */
  {
   ir_node *a, *b, *c, *in[2];
   add_immBlock_pred(get_cur_block(), new_Bad());
   a = new_Const(mode_Is, new_tarval_from_long(1, mode_Is));
   b = new_Const(mode_Is, new_tarval_from_long(2, mode_Is));
   c = new_Add(a, b, mode_Is);
   b = new_Sub(c, b, mode_Is);
   in[0] = b;
   in[1] = new_Bad();
   a = new_Phi(2, in, mode_Is);
   set_Add_left(c, a);
   /* add_End_keepalive(get_irg_end(irg), a); */
   set_nodes_block(c, new_Bad());
   set_nodes_block(a, new_Bad());
  }

  /* Make the unreachable loop */
  loopBlock1 = new_immBlock();
  loopBlock2 = new_immBlock();
  x = new_Jmp();
  add_immBlock_pred(loopBlock1, x);
  mature_immBlock(loopBlock1);

  set_cur_block(loopBlock1);
  c1 = new_Const(mode_Is, new_tarval_from_long(1, mode_Is));
  c2 = new_Proj(get_irg_args(irg), mode_Is, 0);
  x =  new_Cond(new_Proj(new_Cmp(c1, c2), mode_b, pn_Cmp_Eq));
  f = new_Proj(x, mode_X, pn_Cond_false);
  t = new_Proj(x, mode_X, pn_Cond_true);
  add_immBlock_pred(loopBlock2, t);
  add_immBlock_pred(returnBlock, f);
  mature_immBlock(loopBlock2);

  /* Make the unreachable, endless loop */
  loopBlock1 = new_immBlock();
  loopBlock2 = new_immBlock();
  x = new_Jmp();
  add_immBlock_pred(loopBlock1, x);
  mature_immBlock(loopBlock1);

  set_cur_block(loopBlock1);
  x = new_Jmp();
  add_immBlock_pred(loopBlock2, x);
  add_End_keepalive(get_irg_end(irg), loopBlock1);
  mature_immBlock(loopBlock2);

  /* Make the return block */
  set_cur_block(returnBlock);
  x = new_Return(get_store(), 0, NULL);
  mature_immBlock(get_irg_current_block(irg));

  add_immBlock_pred(get_irg_end_block(irg), x);
  mature_immBlock(get_irg_end_block(irg));

  irg_finalize_cons(irg);

#if 0
  printf("Optimizing ...\n");
  dead_node_elimination(irg);
#endif

  /* verify the graph */
  irg_vrfy(irg);

  printf("Dumping the graph and a control flow graph.\n");
  turn_off_edge_labels();
  dump_keepalive_edges(1);
  dump_consts_local(0);
  dump_ir_graph(irg, "-cfg");
  dump_ir_block_graph(irg, "-cfg");
  dump_cfg(irg, "-cfg");

  printf("Running analyses.\n");
  compute_irg_outs(irg);
  compute_doms(irg);
  construct_backedges(irg);

  printf("Dumping the graph with analyses information.\n");

  dump_out_edges(0);
  dump_dominator_information(0);
  dump_loop_information(0);
  dump_backedge_information(1);

  dump_ir_graph(irg, "-ana");
  dump_ir_block_graph(irg, "-anablocks");
  dump_cfg(irg, "-ana");
  dump_loop_tree(irg, "-ana");

  printf("Optimizing.\n");
  optimize_cf(current_ir_graph);
  local_optimize_graph(current_ir_graph);

  printf("Dumping the optimized graph.\n");
  dump_ir_graph(irg, "-opt");
  dump_ir_block_graph(irg, "-opt");
  dump_cfg(irg, "-opt");
  dump_loop_tree(irg, "-opt");

  printf("Use ycomp to view these graphs:\n");
  printf("ycomp GRAPHNAME\n\n");

  return 0;
}
