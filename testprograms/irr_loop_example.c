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
 *   an irreducible loop.
 */

/**
***  This file constructs a control flow of following shape:
***
***
***         firstBlock
***          /      \
***         /        \
***       |/_        _\|
***            ---->
*** LoopBlock1       LoopBlock2
***            <----
***        \              /
*** 	    \            /
*** 	    _\|        |/_
***           nextBlock
***
***
**/

int main(int argc, char **argv)
{
  ir_graph *irg;          /* this variable contains the irgraph */
  type_class *owner;      /* the class in which this method is defined */
  type_method *proc_main; /* typeinformation for the method main */
  entity *ent;            /* represents this method as entity of owner */
  ir_node *expr, *c1, *c2, *c3, *cond, *f, *t, *loopBlock1, *f_l1, *t_l1,
          *loopBlock2, *f_l2, *t_l2, *x;


  /* init library */
  init_firm ();
  set_opt_constant_folding (1);

  /* FIRM was designed for oo languages where all methods belong to a class.
   * For imperative languages like C we view a file as a large class containing
   * all functions as methods in this file.
   * Therefore we define a class "empty" according to the file name
   * with a method main as an entity.
   */
#define CLASSNAME "IRR_LOOP"
#define METHODNAME "main"
#define NRARGS 0
#define NRES 0
  printf("creating an IR graph: %s...\n", CLASSNAME);

  owner = new_type_class (id_from_str (CLASSNAME, strlen(CLASSNAME)));
  proc_main = new_type_method(id_from_str(METHODNAME, strlen(METHODNAME)),
                              NRARGS, NRES);
  ent = new_entity ((type *)owner,
                    id_from_str (METHODNAME, strlen(METHODNAME)),
                    (type *)proc_main);

#define NUM_OF_LOCAL_VARS 0

  irg = new_ir_graph (ent, NUM_OF_LOCAL_VARS);

  /* two make three conditionals  */
  expr = new_Const (mode_i, tarval_from_long (mode_i, 0));
  c1 = new_Const (mode_i, tarval_from_long (mode_i, 1));
  c2 = new_Const (mode_i, tarval_from_long (mode_i, 2));
  c3 = new_Const (mode_i, tarval_from_long (mode_i, 2));

  cond = new_Cond(new_Proj(new_Cmp(expr, c1), mode_b, Eq));
  f = new_Proj(cond, mode_X, 0);
  t = new_Proj(cond, mode_X, 1);
  mature_block(irg->current_block);

  loopBlock1 = new_Block();
  add_in_edge(loopBlock1, t);
  cond = new_Cond(new_Proj(new_Cmp(expr, c2), mode_b, Eq));
  f_l1 = new_Proj(cond, mode_X, 0);
  t_l1 = new_Proj(cond, mode_X, 1);

  loopBlock2 = new_Block();
  add_in_edge(loopBlock2, f);
  cond = new_Cond(new_Proj(new_Cmp(expr, c3), mode_b, Eq));
  f_l2 = new_Proj(cond, mode_X, 0);
  t_l2 = new_Proj(cond, mode_X, 1);

  add_in_edge(loopBlock1, t_l2);
  add_in_edge(loopBlock2, t_l1);
  mature_block(loopBlock1);
  mature_block(loopBlock2);

  new_Block();
  add_in_edge(irg->current_block, f_l2);
  add_in_edge(irg->current_block, f_l1);
  {
    ir_node *in[0];
    x = new_Return (get_store(), 1, in);
  }
  mature_block (irg->current_block);

  add_in_edge (irg->end_block, x);
  mature_block (irg->end_block);

  /* verify the graph */
  irg_vrfy(irg);

  dead_node_elimination(irg);

  printf("\nDone building the graph..\n");
  printf("Dumping the graph and a control flow graph.\n");
  dump_ir_block_graph (irg);
  dump_cfg (irg);

  printf("use xvcg to view these graphs:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n");

  return (0);
}
