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
 *   a dead block / unreachable code.
 */

/**
***  This file constructs a control flow of following shape:
***
***
***         firstBlock
***          /   \
***         /     \
***       |/_     _\|
***     Block1    Block2   deadBlock
***        \       |       /
*** 	    \      |      /
*** 	    _\|   \ /   |/_
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
  type *owner;      /* the class in which this method is defined */
  type *proc_main; /* type information for the method main */
  type     *prim_t_int;
  entity *ent;            /* represents this method as entity of owner */
  ir_node *c1, *c2, *cond, *f, *t, *endBlock, *Block1, *jmp, *Block2,
          *deadBlock, *x;

  /* init library */
  init_firm ();

  /*** Make basic type information for primitive type int. ***/
  prim_t_int = new_type_primitive(id_from_str ("int", 3), mode_Is);

  /* FIRM was designed for oo languages where all methods belong to a class.
   * For imperative languages like C we view a file as a large class containing
   * all functions as methods in this file.
   * Therefore we define a class "empty" according to the file name
   * with a method main as an entity.
   */
#define CLASSNAME "DEAD_BLOCK"
#define METHODNAME "main"
#define NRARGS 0
#define NRES 1
  printf("\nCreating an IR graph: %s...\n", CLASSNAME);

  owner = new_type_class (id_from_str (CLASSNAME, strlen(CLASSNAME)));
  proc_main = new_type_method(id_from_str(METHODNAME, strlen(METHODNAME)),
                              NRARGS, NRES);
  set_method_res_type(proc_main, 0, prim_t_int);
  ent = new_entity (owner,
                    id_from_str (METHODNAME, strlen(METHODNAME)),
                    proc_main);

#define NUM_OF_LOCAL_VARS 1

  irg = new_ir_graph (ent, NUM_OF_LOCAL_VARS);

  /* to make a condition  */
  c1 = new_Const (mode_Is, tarval_from_long (mode_Is, 1));
  c2 = new_Const (mode_Is, tarval_from_long (mode_Is, 2));
  set_value(0, c2);

  cond = new_Cond(new_Proj(new_Cmp(c1, c2), mode_b, Eq));
  f = new_Proj(cond, mode_X, 0);
  t = new_Proj(cond, mode_X, 1);
  mature_block(get_irg_current_block(irg));

  /* end block to add jmps */
  endBlock = new_immBlock();

  /* Block 1 */
  Block1 = new_immBlock();
  add_in_edge(Block1, t);
  mature_block(Block1);
  jmp = new_Jmp();
  add_in_edge(endBlock, jmp);

  /* Block 2 */
  Block2 = new_immBlock();
  add_in_edge(Block2, f);
  mature_block(Block2);
  jmp = new_Jmp();
  add_in_edge(endBlock, jmp);

  /* dead Block */
  deadBlock = new_immBlock();
  mature_block(deadBlock);
  jmp = new_Jmp();
  add_in_edge(endBlock, jmp);

  /* finish end block */
  switch_block(endBlock);
  {
    ir_node *in[1];
    in[0] = get_value(0, mode_Is);
    get_store();
    x = new_Return (get_store(), 1, in);
  }
  mature_block (get_irg_current_block(irg));

  add_in_edge (get_irg_end_block(irg), x);
  mature_block (get_irg_end_block(irg));

  finalize_cons (irg);

  printf("Optimizing ...\n");
  local_optimize_graph (irg);
  dead_node_elimination (irg);

  /* verify the graph */
  irg_vrfy(irg);

  printf("Dumping the graph and a control flow graph.\n");
  dump_ir_block_graph (irg);
  dump_cfg (irg);
  printf("Use xvcg to view these graphs:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  return (0);
}
