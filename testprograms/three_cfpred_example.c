/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Christian Schaefer, Goetz Lindenmaier
**
** testprogram.
*/

# include <stdio.h>

# include "irdump.h"
# include "firm.h"

/*
 *   a dead block / unreachable code.
 */

/**
***  This file constructs a control flow of following shape:
***
***
***       firstCondBlock
***          /     \
***         /       \
***       |/_       _\|
***     Block1    scnCondBlock
***        |       |        |
*** 	   |       |        |
*** 	   |      \ /      \ /
***        |     Block2   Block3
***         \      |       /
***	     \     |      /
***	     _\|  \ /   |/_
***            nextBlock
***
***
***   This is a program as, e.g.,
***
***   if () then
***     { Jmp label1; } // happens anyways
***   else
***     { Jmp label1; } // happens anyways
*** label1:
***   return();
***   Jmp label1;
***
**/

int main(int argc, char **argv)
{
  ir_graph *irg;          /* this variable contains the irgraph */
  type_class *owner;      /* the class in which this method is defined */
  type_method *proc_main; /* type information for the method main */
  entity *ent;            /* represents this method as entity of owner */
  ir_node *c1, *c2, *cond, *f, *t, *endBlock, *Block1, *jmp,
          *scndCondBlock, *Block2, *Block3, *x;

  /* init library */
  init_firm ();

  set_optimize(1);

  /* FIRM was designed for oo languages where all methods belong to a class.
   * For imperative languages like C we view a file as a large class containing
   * all functions as methods in this file.
   * This class now is automatically generated.
   */
#define METHODNAME "main"
#define NRARGS 1
#define NRES 1
  printf("creating an IR graph: ...\n");

  owner = get_glob_type();
  proc_main = new_type_method(id_from_str(METHODNAME, strlen(METHODNAME)),
			      NRARGS, NRES);
  /** @@@ setting of arg/res types misses **/
  ent = new_entity ((type *)owner,
                    id_from_str (METHODNAME, strlen(METHODNAME)),
                    (type *)proc_main);

#define NUM_OF_LOCAL_VARS 2

  irg = new_ir_graph (ent, NUM_OF_LOCAL_VARS);

  /* to make a condition  */
  c1 = new_Const (mode_i, tarval_from_long (mode_i, 1));
  c2 = new_Proj (get_irg_args(irg), mode_i, 0);
  set_value(1, c2);

  cond = new_Cond(new_Proj(new_Cmp(c1, c2), mode_b, Eq));
  set_value(0, new_Const (mode_i, tarval_from_long (mode_i, 6)));
  f = new_Proj(cond, mode_X, 0);
  t = new_Proj(cond, mode_X, 1);
  mature_block(irg->current_block);

  /* end block to add jmps */
  endBlock = new_Block();

  /* Block 1 */
  Block1 = new_Block();
  add_in_edge(Block1, t);
  mature_block(Block1);
  set_value(0, new_Const (mode_i, tarval_from_long (mode_i, 5)));
  jmp = new_Jmp();
  add_in_edge(endBlock, jmp);

  /* scndCondBlock */
  scndCondBlock = new_Block();
  add_in_edge(scndCondBlock, f);
  mature_block(scndCondBlock);
  c1 = new_Const (mode_i, tarval_from_long (mode_i, 3));
  cond = new_Cond(new_Proj(new_Cmp(c1, get_value(1, mode_i)), mode_b, Eq));
  f = new_Proj(cond, mode_X, 0);
  t = new_Proj(cond, mode_X, 1);
  mature_block(irg->current_block);

  /* Block 2 */
  Block2 = new_Block();
  add_in_edge(Block2, f);
  mature_block(Block2);
  jmp = new_Jmp();
  add_in_edge(endBlock, jmp);

  /* Block 3 */
  Block3 = new_Block();
  add_in_edge(Block3, t);
  mature_block(Block3);
  jmp = new_Jmp();
  add_in_edge(endBlock, jmp);

  /* finish the end Block */
  switch_block(endBlock);
  {
    ir_node *in[1];
    in[0] = get_value(0, mode_i);
    x = new_Return (get_store(), 1, in);
  }
  mature_block (irg->current_block);

  /* finish the Block with the end node */
  add_in_edge (irg->end_block, x);
  mature_block (irg->end_block);

  printf("\nDone building the graph.\n");

  /* verify the graph */
  irg_vrfy(irg);

  dead_node_elimination(irg);

  printf("Dumping the graph and a control flow graph.\n");
  dump_ir_block_graph (irg);
  dump_cfg (irg);
  printf("use xvcg to view these graphs:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n");

  return (0);
}
