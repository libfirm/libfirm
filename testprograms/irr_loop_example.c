/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Christian Schaefer, Goetz Lindenmaier
**
** testprogram.
*/

# include <stdio.h>
# include <string.h>

# include "irvrfy.h"
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
  ir_graph *irg;        /* this variable contains the irgraph */
  type     *owner;      /* the class in which this method is defined */
  type     *proc_main;  /* typeinformation for the method main */
  entity   *ent;        /* represents this method as entity of owner */
  ir_node  *expr, *c1, *c2, *c3, *cond, *f, *t, *loopBlock1, *f_l1, *t_l1,
           *loopBlock2, *f_l2, *t_l2, *x;


  /* init library */
  init_firm ();
  set_opt_constant_folding (0);  /* so that the stupid tests are not optimized. */
  set_opt_cse(1);
  set_opt_dead_node_elimination(1);

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
  printf("\nCreating an IR graph: %s...\n", CLASSNAME);

  owner = new_type_class (id_from_str (CLASSNAME, strlen(CLASSNAME)));
  proc_main = new_type_method(id_from_str(METHODNAME, strlen(METHODNAME)),
                              NRARGS, NRES);
  ent = new_entity (owner,
                    id_from_str (METHODNAME, strlen(METHODNAME)),
                    proc_main);

#define NUM_OF_LOCAL_VARS 0

  irg = new_ir_graph (ent, NUM_OF_LOCAL_VARS);

  /* to make three conditionals  */
  expr = new_Const (mode_Is, tarval_from_long (mode_Is, 0));
  c1 = new_Const (mode_Is, tarval_from_long (mode_Is, 1));
  c2 = new_Const (mode_Is, tarval_from_long (mode_Is, 2));
  c3 = new_Const (mode_Is, tarval_from_long (mode_Is, 2));

  cond = new_Cond(new_Proj(new_Cmp(expr, c1), mode_b, Eq));
  f = new_Proj(cond, mode_X, 0);
  t = new_Proj(cond, mode_X, 1);
  mature_block(get_irg_current_block(irg));

  loopBlock1 = new_immBlock();
  add_in_edge(loopBlock1, t);
  cond = new_Cond(new_Proj(new_Cmp(expr, c2), mode_b, Eq));
  f_l1 = new_Proj(cond, mode_X, 0);
  t_l1 = new_Proj(cond, mode_X, 1);

  loopBlock2 = new_immBlock();
  add_in_edge(loopBlock2, f);
  cond = new_Cond(new_Proj(new_Cmp(expr, c3), mode_b, Eq));
  f_l2 = new_Proj(cond, mode_X, 0);
  t_l2 = new_Proj(cond, mode_X, 1);

  add_in_edge(loopBlock1, t_l2);
  add_in_edge(loopBlock2, t_l1);
  mature_block(loopBlock1);
  mature_block(loopBlock2);

  new_immBlock();
  add_in_edge(get_irg_current_block(irg), f_l2);
  add_in_edge(get_irg_current_block(irg), f_l1);
  {
    ir_node *in[0];
    x = new_Return (get_store(), 0, in);
  }
  mature_block (get_irg_current_block(irg));

  add_in_edge (get_irg_end_block(irg), x);
  mature_block (get_irg_end_block(irg));

  finalize_cons (irg);

  printf("Optimizing ...\n");
  dead_node_elimination(irg);

  /* verify the graph */
  irg_vrfy(irg);

  printf("Dumping the graph and a control flow graph.\n");
  dump_ir_block_graph (irg);
  dump_cfg (irg);
  printf("Use xvcg to view these graphs:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  return (0);
}
