/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Christian Schaefer, Goetz Lindenmaier
**
** testprogram.
*/

#include <stdio.h>

# include "irdump.h"
# include "firm.h"

/*
 * das leere FIRM Programm
 */

/**
***  This file constructs the ir for the following pseudo-program:
***
***  main() {
***    int a = 0;
***    int b = 1;
***
***    if (a > 2)
***      { a = b; }
***    else
***      { b = 2; }
***
***    return a, b;
**/

int main(int argc, char **argv)
{
  ir_graph *irg;          /* this variable contains the irgraph */
  type_class *owner;      /* the class in which this method is defined */
  entity *ent;            /* represents this method as entity of owner */
  ir_node *x, *x_then, *x_else, *c0, *c1, *c2, *cmpGt, *f, *t, *b;

  printf("creating an IR graph: IF_ELSE_EXAMPLE...\n");

  /* init library */
  init_firm ();

  /* FIRM was designed for oo languages where all methods belong to a class.
   * For imperative languages like C we view a file as a large class containing
   * all functions as methods in this file.
   * Therefore we define a class "IF_ELSE_EXAMPLE" with a method main as an
   * entity.
   */
#define CLASSNAME "IF_ELSE_EXAMPLE"
#define ENTITYNAME "main"

  owner = new_type_class (id_from_str (CLASSNAME, strlen(CLASSNAME)));
  ent = new_entity ((type *)owner, id_from_str (ENTITYNAME, strlen(ENTITYNAME)), NULL);


  /* Generates the basic graph for the method represented by entity ent, that
   * is, generates start and end blocks and nodes and a first, initial block.
   * The constructor needs to know how many local variables the method has.
   */
#define NUM_OF_LOCAL_VARS 2

  irg = new_ir_graph (ent, NUM_OF_LOCAL_VARS);

  /* Generate two constants */
  c0 = new_Const (mode_i, tarval_from_long (mode_i, 0));
  c1 = new_Const (mode_i, tarval_from_long (mode_i, 1));

  /* set a and b to constants */
  set_value (0, c0);  /* this (0) is variable a */
  set_value (1, c1);  /* this (1) is variable b */

  /* the expression that evaluates the condition */
  c2 = new_Const (mode_i, tarval_from_long (mode_i, 2));
  cmpGt = new_Proj(new_Cmp(get_value(0, mode_i), c2), mode_b, Gt);

  /* the conditional branch */
  x = new_Cond (cmpGt);
  f = new_Proj (x, mode_X, 0); /* if condition is false */
  t = new_Proj (x, mode_X, 1); /* if condition is true */

  mature_block (irg->current_block);

  /* generate and fill the then block */
  b = new_Block ();
  add_in_edge (b, t);
  set_value (0, get_value(1, mode_i));
  mature_block (b);
  x_then = new_Jmp ();

  /* generate and fill the else block */
  b = new_Block ();
  add_in_edge (b, f);
  set_value (1, new_Const (mode_i, tarval_from_long (mode_i, 2)));
  mature_block (b);
  x_else = new_Jmp ();

  /* generate the join block and add all cfg edges */
  b = new_Block ();
  add_in_edge (b, x_then);
  add_in_edge (b, x_else);


  /* Generate the return node into current region. */
  {
    ir_node *in[2]; /* this is the array containing the return parameters */
    in[0] = get_value(0, mode_i);
    in[1] = get_value(1, mode_i);
    x = new_Return (get_store(), 2, in);
  }
  /* Now generate all instructions for this block and all its predecessor blocks
   * so we can mature it. */
  mature_block (irg->current_block);

  /* This adds the in edge of the end block which originates at the return statement.
   * The return node passes controlflow to the end block.  */
  add_in_edge (irg->end_block, x);
  /* Now we can mature the end block as all it's predecessors are known. */
  mature_block (irg->end_block);

  /* verify the graph */
  vrfy_graph(irg);

  printf("\nDone building the graph.  Dumping it.\n");
  dump_ir_block_graph (irg);

  printf("use xvcg to view this graph:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n");

  return (0);
}
