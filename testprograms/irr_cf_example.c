/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
* All rights reserved.
*
* Authors: Christian Schaefer, Goetz Lindenmaier
*
* testprogram.
*/

# include <stdio.h>
# include <string.h>

# include "irvrfy.h"
# include "irdump.h"
# include "firm.h"

/*
 *  irregular control flow
 */

/**
*  This file constructs a control flow of following shape:
*
* StartBlock
*     |
*    \|/
*   Block --->  Block
*     |           |
*    \|/         \|/
*   Block --->  Block
*     |           |
*    \|/         \|/
*   Block --->  Block
*                 |
*                \|/
*              EndBlock
*
*   This is a program as, e.g.,
*
*   switch (expr){
*     case 1:
*     case 2:
*       break;
*     default:
*   }
*   return
**/

int main(int argc, char **argv)
{
  ir_graph *irg;        /* this variable contains the irgraph */
  type     *owner;      /* the class in which this method is defined */
  type     *proc_main;  /* typeinformation for the method main */
  entity   *ent;        /* represents this method as entity of owner */
  ir_node  *expr, *c1, *c2, *cond, *f, *t, *jmp, *x;

  printf("\nCreating an IR graph: IRR_CF...\n");

  /* init library */
  init_firm ();
  set_opt_constant_folding(0); /* so that stupid test are not evaluated. */

  /* FIRM was designed for oo languages where all methods belong to a class.
   * For imperative languages like C we view a file as a large class containing
   * all functions as methods in this file.
   * Therefore we define a class "empty" according to the file name
   * with a method main as an entity.
   */
#define CLASSNAME "IRREGULAR_CF"
#define METHODNAME "main"
#define NRARGS 0
#define NRES 0

  owner = new_type_class (id_from_str (CLASSNAME, strlen(CLASSNAME)));
  proc_main = new_type_method(id_from_str(METHODNAME, strlen(METHODNAME)),
                              NRARGS, NRES);
  ent = new_entity ((type *)owner,
                    id_from_str (METHODNAME, strlen(METHODNAME)),
                    (type *)proc_main);
  get_entity_ld_name(ent);
#define NUM_OF_LOCAL_VARS 0

  irg = new_ir_graph (ent, NUM_OF_LOCAL_VARS);

  /* two make two conditionals that represent a switch */
  expr = new_Const (mode_Is, tarval_from_long (mode_Is, 0));
  c1 = new_Const (mode_Is, tarval_from_long (mode_Is, 1));
  c2 = new_Const (mode_Is, tarval_from_long (mode_Is, 2));

  cond = new_Cond(new_Proj(new_Cmp(expr, c1), mode_b, Eq));
  f = new_Proj(cond, mode_X, 0);
  t = new_Proj(cond, mode_X, 1);
  mature_block(get_irg_current_block(irg));

  new_immBlock();
  add_in_edge(get_irg_current_block(irg), t);
  jmp = new_Jmp();
  mature_block(get_irg_current_block(irg));

  new_immBlock();
  add_in_edge(get_irg_current_block(irg), f);
  cond = new_Cond(new_Proj(new_Cmp(expr, c2), mode_b, Eq));
  f = new_Proj(cond, mode_X, 0);
  t = new_Proj(cond, mode_X, 1);
  mature_block(get_irg_current_block(irg));

  new_immBlock();
  add_in_edge(get_irg_current_block(irg), t);
  add_in_edge(get_irg_current_block(irg), jmp);
  jmp = new_Jmp();
  mature_block(get_irg_current_block(irg));

  new_immBlock();
  add_in_edge(get_irg_current_block(irg), f);
  t = new_Jmp();
  mature_block(get_irg_current_block(irg));

  new_immBlock();
  add_in_edge(get_irg_current_block(irg), t);
  add_in_edge(get_irg_current_block(irg), jmp);
  {
    ir_node *in[0]; /* this is the array containing the return parameters */
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
