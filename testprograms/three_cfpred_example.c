/*
 * Project:     libFIRM
 * File name:   testprograms/three_cfpred_example.c
 * Purpose:     Construct a block with more than two predecessors.
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
 *  This file constructs a control flow of following shape:
 *
 *
 *       firstCondBlock
 *          /     \
 *         /       \
 *       |/_       _\|
 *     Block1    scnCondBlock
 *        |       |        |
 *        |       |        |
 *    |      \ /      \ /
 *        |     Block2   Block3
 *         \      |       /
 *          \     |      /
 *          _\|  \ /   |/_
 *            nextBlock
 *
 *
 *   This is a program as, e.g.,
 *
 *   if () then
 *     { Jmp label1; } //  happens anyways
 *   else
 *     { Jmp label1; } //  happens anyways
 * label1:
 *   return();
 *   Jmp label1;
 *
 **/

int main(void)
{
  const char *suffix = "";
  ir_type     *prim_t_int;
  ir_graph *irg;          /* this variable contains the irgraph */
  ir_type *owner;      /* the class in which this method is defined */
  ir_type *proc_main; /* ir_type information for the method main */
  ir_entity *ent;            /* represents this method as ir_entity of owner */
  ir_node *c1, *c2, *cond, *f, *t, *endBlock, *Block1, *jmp,
          *scndCondBlock, *Block2, *Block3, *x;

  /* init library */
  init_firm(NULL);

  set_optimize(1);

  /*** Make basic ir_type information for primitive ir_type int. ***/
  prim_t_int = new_type_primitive(new_id_from_chars("int", 3), mode_Is);

  /* FIRM was designed for oo languages where all methods belong to a class.
   * For imperative languages like C we view a file as a large class containing
   * all functions as methods in this file.
   * This class now is automatically generated.
   */
#define METHODNAME "THREE_CFPRED_EXAMPLE_main"
#define NRARGS 1
#define NRES 1
  printf("\nCreating an IR graph: THREE_CFPRED_EXAMPLE ...\n");

  owner = get_glob_type();
  proc_main = new_type_method(new_id_from_chars(METHODNAME, strlen(METHODNAME)),
                  NRARGS, NRES);
  set_method_param_type(proc_main, 0, prim_t_int);
  set_method_res_type(proc_main, 0, prim_t_int);

  ent = new_entity(owner,
                   new_id_from_chars(METHODNAME, strlen(METHODNAME)),
                   proc_main);

#define NUM_OF_LOCAL_VARS 2

  irg = new_ir_graph(ent, NUM_OF_LOCAL_VARS);

  /* to make a condition  */
  c1 = new_Const(mode_Is, new_tarval_from_long(1, mode_Is));
  c2 = new_Proj(get_irg_args(irg), mode_Is, 0);
  set_value(1, c2);

  cond = new_Cond(new_Proj(new_Cmp(c1, c2), mode_b, pn_Cmp_Eq));
  set_value(0, new_Const(mode_Is, new_tarval_from_long(6, mode_Is)));
  f = new_Proj(cond, mode_X, pn_Cond_false);
  t = new_Proj(cond, mode_X, pn_Cond_true);
  mature_immBlock(get_irg_current_block(irg));

  /* end block to add jmps */
  endBlock = new_immBlock();

  /* Block 1 */
  Block1 = new_immBlock();
  add_immBlock_pred(Block1, t);
  mature_immBlock(Block1);
  set_value(0, new_Const(mode_Is, new_tarval_from_long(5, mode_Is)));
  jmp = new_Jmp();
  add_immBlock_pred(endBlock, jmp);

  /* scndCondBlock */
  scndCondBlock = new_immBlock();
  add_immBlock_pred(scndCondBlock, f);
  mature_immBlock(scndCondBlock);
  c1 = new_Const(mode_Is, new_tarval_from_long(3, mode_Is));
  cond = new_Cond(new_Proj(new_Cmp(c1, get_value(1, mode_Is)), mode_b, pn_Cmp_Eq));
  f = new_Proj(cond, mode_X, pn_Cond_false);
  t = new_Proj(cond, mode_X, pn_Cond_true);
  mature_immBlock(get_irg_current_block(irg));

  /* Block 2 */
  Block2 = new_immBlock();
  add_immBlock_pred(Block2, f);
  mature_immBlock(Block2);
  jmp = new_Jmp();
  add_immBlock_pred(endBlock, jmp);

  /* Block 3 */
  Block3 = new_immBlock();
  add_immBlock_pred(Block3, t);
  mature_immBlock(Block3);
  jmp = new_Jmp();
  add_immBlock_pred(endBlock, jmp);

  /* finish the end Block */
  set_cur_block(endBlock);
  {
    ir_node *in[1];
    in[0] = get_value(0, mode_Is);
    x = new_Return(get_store(), 1, in);
  }
  mature_immBlock(get_irg_current_block(irg));

  /* finish the Block with the end node */
  add_immBlock_pred(get_irg_end_block(irg), x);
  mature_immBlock(get_irg_end_block(irg));

  /* verify the graph */
  irg_vrfy(irg);
  irg_finalize_cons(irg);

  printf("Optimizing ...\n");
  dead_node_elimination(irg);

  printf("Dumping the graph and a control flow graph.\n");
  dump_ir_block_graph(irg, suffix);
  dump_cfg(irg, suffix);
  printf("Use ycomp to view these graphs:\n");
  printf("ycomp THREE_CFPRED_EXAMPLE_main.vcg\n\n");
  printf("ycomp THREE_CFPRED_EXAMPLE_main-cfg.vcg\n\n");

  return(0);
}
