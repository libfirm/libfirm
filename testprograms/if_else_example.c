/*
 * Project:     libFIRM
 * File name:   testprograms/if_else_example.c
 * Purpose:     Shows construction of if ... else control flow.
 *              Tests Phi construction.
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
# include "firm.h"
# include "irdump.h"

/**
 *  This file constructs the ir for the following pseudo-program:
 *
 *  main() {
 *    int a = 0;
 *    int b = 1;
 *
 *    if (a > 2)
 *      { a = b; }
 *    else
 *      { b = 2; }
 *
 *    return a, b;
 */

int main(int argc, char **argv)
{
  type     *prim_t_int;
  ir_graph *irg;       /* this variable contains the irgraph */
  type     *owner;     /* the class in which this method is defined */
  type     *method;    /* the type of this method */
  entity   *ent;       /* represents this method as entity of owner */
  ir_node  *x, *x_then, *x_else, *c0, *c1, *c2, *cmpGt, *f, *t, *b;

  printf("\nCreating an IR graph: IF_ELSE_EXAMPLE...\n");

  /* init library */
  init_firm (NULL);

  /*** Make basic type information for primitive type int. ***/
  prim_t_int = new_type_primitive(new_id_from_chars ("int", 3), mode_Is);

  /* FIRM was designed for oo languages where all methods belong to a class.
   * For imperative languages like C we view a file as a large class containing
   * all functions as methods in this file.
   * Therefore we define a class "IF_ELSE_EXAMPLE" with a method main as an
   * entity.
   */
#define ENTITYNAME "IF_ELSE_EXAMPLE_main"

  owner = get_glob_type();
  method = new_type_method (new_id_from_chars(ENTITYNAME, strlen(ENTITYNAME)), 0, 2);
  set_method_res_type(method, 0, prim_t_int);
  set_method_res_type(method, 1, prim_t_int);

  ent = new_entity (owner, new_id_from_chars (ENTITYNAME,
		    strlen(ENTITYNAME)), method);

  /* Generates the basic graph for the method represented by entity ent, that
   * is, generates start and end blocks and nodes and a first, initial block.
   * The constructor needs to know how many local variables the method has.
   */
#define NUM_OF_LOCAL_VARS 2

  irg = new_ir_graph (ent, NUM_OF_LOCAL_VARS);

  /* Generate two constants */
  c0 = new_Const (mode_Is, new_tarval_from_long (0, mode_Is));
  c1 = new_Const (mode_Is, new_tarval_from_long (1, mode_Is));

  /* set a and b to constants */
  set_value (0, c0);  /* this (0) is variable a */
  set_value (1, c1);  /* this (1) is variable b */

  /* the expression that evaluates the condition */
  c2 = new_Const (mode_Is, new_tarval_from_long (2, mode_Is));
  cmpGt = new_Proj(new_Cmp(get_value(0, mode_Is), c2), mode_b, pn_Cmp_Gt);

  /* the conditional branch */
  x = new_Cond (cmpGt);
  f = new_Proj (x, mode_X, 0); /* if condition is false */
  t = new_Proj (x, mode_X, 1); /* if condition is true */

  mature_immBlock (get_irg_current_block(irg));

  /* generate and fill the then block */
  b = new_immBlock ();
  add_immBlock_pred (b, t);
  set_value (0, get_value(1, mode_Is));
  mature_immBlock (b);
  x_then = new_Jmp ();

  /* generate and fill the else block */
  b = new_immBlock ();
  add_immBlock_pred (b, f);
  set_value (1, new_Const (mode_Is, new_tarval_from_long (2, mode_Is)));
  mature_immBlock (b);
  x_else = new_Jmp ();

  /* generate the join block and add all cfg edges */
  b = new_immBlock ();
  add_immBlock_pred (b, x_then);
  add_immBlock_pred (b, x_else);

  /* Generate the return node into current region. */
  {
    ir_node *in[2]; /* this is the array containing the return parameters */
    in[0] = get_value(0, mode_Is);
    in[1] = get_value(1, mode_Is);
    x = new_Return (get_store(), 2, in);
  }
  /* Now generate all instructions for this block and all its predecessor
     blocks so we can mature it. */
  mature_immBlock (get_irg_current_block(irg));

  /* This adds the in edge of the end block which originates at the
     return statement.  The return node passes control flow to the
     end block.  */
  add_immBlock_pred (get_irg_end_block(irg), x);
  /* Now we can mature the end block as all it's predecessors are known. */
  mature_immBlock (get_irg_end_block(irg));

  finalize_cons (irg);

  printf("Optimizing ...\n");
  local_optimize_graph(irg);
  dead_node_elimination(irg);

  /* verify the graph */
  irg_vrfy(irg);
  finalize_cons (irg);

  printf("Done building the graph.  Dumping it.\n");
  dump_ir_block_graph (irg, 0);
  printf("use xvcg to view this graph:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  return (0);
}
