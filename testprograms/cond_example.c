/*
 * Project:     libFIRM
 * File name:   testprograms/cond_example.c
 * Purpose:     Shows how to represent boolean expressions.
 * Author:      Christian Schaefer, Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


#include <stdio.h>
# include <string.h>

# include "irvrfy.h"
# include "irdump.h"
# include "firm.h"

/**
*  This file constructs the ir for the following pseudo-program:
*
*  main(int a) {
*    if ((a > 2) && (a < 10))
*      { a = 1; }
*
*    return a;
**/

int main(int argc, char **argv)
{
  type     *prim_t_int;
  ir_graph *irg;       /* this variable contains the irgraph */
  type     *owner;     /* the class in which this method is defined */
  type     *method;    /* the type of this method */
  entity   *ent;       /* represents this method as entity of owner */
  ir_node  *x, *x_then, *arg1, *c2, *c10, *cmpGt, *cmpLt, *and, *f, *t, *b;

  printf("\nCreating an IR graph: COND_EXAMPLE...\n");

  /* init library */
  init_firm (NULL);

  /*** Make basic type information for primitive type int. ***/
  prim_t_int = new_type_primitive(new_id_from_chars ("int", 3), mode_Is);

  /* FIRM was designed for oo languages where all methods belong to a class.
   * For imperative languages like C we view a file as a large class containing
   * all functions as methods in this file.
   * Therefore we define a class "COND_EXAMPLE" with a method main as an
   * entity.
   */
#define CLASSNAME "COND_EXAMPLE"
#define ENTITYNAME "main"

  owner = new_type_class (new_id_from_chars (CLASSNAME, strlen(CLASSNAME)));
  method = new_type_method (new_id_from_chars("main", 4), 1, 1);
  set_method_param_type(method, 0, prim_t_int);
  set_method_res_type(method, 0, prim_t_int);
  ent = new_entity (owner, new_id_from_chars (ENTITYNAME, strlen(ENTITYNAME)), method);
  get_entity_ld_name(ent);


  /* Generates the basic graph for the method represented by entity ent, that
   * is, generates start and end blocks and nodes and a first, initial block.
   * The constructor needs to know how many local variables the method has.
   */
#define NUM_OF_LOCAL_VARS 1

  irg = new_ir_graph (ent, NUM_OF_LOCAL_VARS);

  /* get the first argument a of method main - see irgraph.h */
  arg1 = new_Proj(get_irg_args(irg), mode_Is, 0);

  /* arg1 as first first local variable - makes things simple */
  set_value(0, arg1);

  /* the expression that evaluates the condition */
  /* cmpGt = a > 2 */
  c2 = new_Const (mode_Is, new_tarval_from_long (2, mode_Is));
  cmpGt = new_Proj(new_Cmp(get_value(0, mode_Is), c2), mode_b, Gt);
  cmpGt = new_Conv(cmpGt, mode_Is);

  /* cmpLt = a < 10 */
  c10 = new_Const (mode_Is, new_tarval_from_long (10, mode_Is));
  cmpLt = new_Proj(new_Cmp(get_value(0, mode_Is), c10), mode_b, Lt);
  cmpLt = new_Conv(cmpLt, mode_Is);

  /* cmpGt && cmpLt */
  and = new_And(cmpGt, cmpLt, mode_Is);
  /* compare result and 0 because we have no cast from integer to bool */
  and = new_Cmp(and, new_Const (mode_Is, new_tarval_from_long (0, mode_Is)));
  and = new_Proj(and, mode_b, Ne);

  /* the conditional branch */
  x = new_Cond (and);
  f = new_Proj (x, mode_X, 0); /* if condition is false */
  t = new_Proj (x, mode_X, 1); /* if condition is true */

  mature_immBlock (get_irg_current_block(irg));

  /* generate and fill the then block */
  b = new_immBlock ();
  add_immBlock_pred (b, t);
  set_value (0, new_Const (mode_Is, new_tarval_from_long (1, mode_Is)));
  mature_immBlock (b);
  x_then = new_Jmp ();

  /* generate the fall through block and add all cfg edges */
  b = new_immBlock ();
  add_immBlock_pred (b, x_then);
  add_immBlock_pred (b, f);


  /* Generate the return node into current region. */
  {
    ir_node *in[1]; /* this is the array containing the return parameters */
    in[0] = get_value(0, mode_Is);
    x = new_Return (get_store(), 1, in);
  }
  /* Now generate all instructions for this block and all its predecessor blocks
   * so we can mature it. */
  mature_immBlock (get_irg_current_block(irg));

  /* This adds the in edge of the end block which originates at the
     return statement. The return node passes controlflow to the end block.*/
  add_immBlock_pred (get_irg_end_block(irg), x);
  /* Now we can mature the end block as all it's predecessors are known. */
  mature_immBlock (get_irg_end_block(irg));

  finalize_cons (irg);

  printf("Optimizing ...\n");
  dead_node_elimination(irg);

  /* verify the graph */
  irg_vrfy(irg);

  printf("Done building the graph.  Dumping it.\n");
  dump_ir_block_graph (irg, 0);
  printf("Use xvcg to view this graph:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  return (0);
}
