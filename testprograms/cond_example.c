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

/**
***  This file constructs the ir for the following pseudo-program:
***
***  main(int a) {
***    if ((a > 2) && (a < 10))
***      { a = 1; }
***
***    return a;
**/

int main(int argc, char **argv)
{
  ir_graph *irg;          /* this variable contains the irgraph */
  type_class *owner;      /* the class in which this method is defined */
  entity *ent;            /* represents this method as entity of owner */
  ir_node *x, *x_then, *arg1, *c2, *c10, *cmpGt, *cmpLt, *and, *f, *t, *b;

  printf("creating an IR graph: COND_EXAMPLE...\n");

  /* init library */
  init_firm ();

  /* FIRM was designed for oo languages where all methods belong to a class.
   * For imperative languages like C we view a file as a large class containing
   * all functions as methods in this file.
   * Therefore we define a class "COND_EXAMPLE" with a method main as an
   * entity.
   */
#define CLASSNAME "COND_EXAMPLE"
#define ENTITYNAME "main"

  owner = new_type_class (id_from_str (CLASSNAME, strlen(CLASSNAME)));
  ent = new_entity ((type *)owner, id_from_str (ENTITYNAME, strlen(ENTITYNAME)), NULL);


  /* Generates the basic graph for the method represented by entity ent, that
   * is, generates start and end blocks and nodes and a first, initial block.
   * The constructor needs to know how many local variables the method has.
   */
#define NUM_OF_LOCAL_VARS 1

  irg = new_ir_graph (ent, NUM_OF_LOCAL_VARS);

  /* get the first argument a of method main - see irgraph.h */
  arg1 = new_Proj(irg->args, mode_i, 0);

  /* arg1 as first first local variable - makes things simple */
  set_value(0, arg1);

  /* the expression that evaluates the condition */
  /* cmpGt = a > 2 */
  c2 = new_Const (mode_i, tarval_from_long (mode_i, 2));
  cmpGt = new_Proj(new_Cmp(get_value(0, mode_i), c2), mode_b, Gt);
  cmpGt = new_Conv(cmpGt, mode_i);

  /* cmpLt = a < 10 */
  c10 = new_Const (mode_i, tarval_from_long (mode_i, 10));
  cmpLt = new_Proj(new_Cmp(get_value(0, mode_i), c10), mode_b, Lt);
  cmpLt = new_Conv(cmpLt, mode_i);

  /* cmpGt && cmpLt */
  and = new_And(cmpGt, cmpLt, mode_i);
  /* compare result and 0 because we have no cast from integer to bool */
  and = new_Cmp(and, new_Const (mode_i, tarval_from_long (mode_i, 0)));
  and = new_Proj(and, mode_b, Ne);

  /* the conditional branch */
  x = new_Cond (and);
  f = new_Proj (x, mode_X, 0); /* if condition is false */
  t = new_Proj (x, mode_X, 1); /* if condition is true */

  mature_block (irg->current_block);

  /* generate and fill the then block */
  b = new_Block ();
  add_in_edge (b, t);
  set_value (0, new_Const (mode_i, tarval_from_long (mode_i, 1)));
  mature_block (b);
  x_then = new_Jmp ();

  /* generate the fall through block and add all cfg edges */
  b = new_Block ();
  add_in_edge (b, x_then);
  add_in_edge (b, f);


  /* Generate the return node into current region. */
  {
    ir_node *in[1]; /* this is the array containing the return parameters */
    in[0] = get_value(0, mode_i);
    x = new_Return (get_store(), 1, in);
  }
  /* Now generate all instructions for this block and all its predecessor blocks
   * so we can mature it. */
  mature_block (irg->current_block);

  /* This adds the in edge of the end block which originates at the return statement.
   * The return node passes controlflow to the end block.  */
  add_in_edge (irg->end_block, x);
  /* Now we can mature the end block as all it's predecessors are known. */
  mature_block (irg->end_block);

  printf("\nDone building the graph.  Dumping it.\n");
  dump_ir_block_graph (irg);

  printf("use xvcg to view this graph:\n");
  printf("/ben/trapp/bin/i486/xvcg GRAPHNAME\n");

  return (0);


}
