/*
 * Project:     libFIRM
 * File name:   testprograms/exception.c
 * Purpose:     Shows construction of exceptions.
 *              Tests Phi construction.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
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
*    int a = 5;  // val 0
*    int b = 6;  // val 1
*    int c = 7;  // val 2
*    int d;      // val 3
*
*    try {
*      d = a/0;
*      c = 8;
*      d = b/0
*    } catch () { return c }
*
*    return d;
**/

int main(int argc, char **argv)
{
  type     *prim_t_int;
  ir_graph *irg;       /* this variable contains the irgraph */
  type     *owner;     /* the class in which this method is defined */
  type     *method;    /* the type of this method */
  entity   *ent;       /* represents this method as entity of owner */
  ir_node  *x, *catch_block, *block, *zero, *a, *b, *c, *d;

  printf("\nCreating an IR graph: EXCEPTION...\n");

  /* init library */
  init_firm (NULL);

  /*** Make basic type information for primitive type int. ***/
  prim_t_int = new_type_primitive(new_id_from_str ("int"), mode_Is);

  /* FIRM was designed for oo languages where all methods belong to a class.
   * For imperative languages like C we view a file as a large class containing
   * all functions as methods in this file.
   * Therefore we define a class "IF_ELSE_EXAMPLE" with a method main as an
   * entity.
   */
#define ENTITYNAME "EXCEPTION_main"

  owner = get_glob_type();
  method = new_type_method (new_id_from_str(ENTITYNAME), 0, 1);
  set_method_res_type(method, 0, prim_t_int);

  ent = new_entity (owner, new_id_from_str (ENTITYNAME), method);

  /* Generates the basic graph for the method represented by entity ent, that
   * is, generates start and end blocks and nodes and a first, initial block.
   * The constructor needs to know how many local variables the method has.
   */
#define NUM_OF_LOCAL_VARS 4

  irg = new_ir_graph (ent, NUM_OF_LOCAL_VARS);

  /* Initialize a, b, c. */
  zero = new_Const (mode_Is, new_tarval_from_long (0, mode_Is));
  a = new_Const (mode_Is, new_tarval_from_long (5, mode_Is));
  b = new_Const (mode_Is, new_tarval_from_long (6, mode_Is));
  c = new_Const (mode_Is, new_tarval_from_long (7, mode_Is));

  /* set a and b to constants */
  set_value (0, a);  /* this (0) is variable a */
  set_value (1, b);  /* this (1) is variable b */
  set_value (2, c);  /* this (2) is variable c */

  block = get_cur_block();
  catch_block = new_immBlock();
  set_cur_block(block);

  /* d = a / 0 */
  d = new_Div(get_store(), get_value(0, mode_Is), zero);
  set_store(new_Proj(d, mode_M, pn_Div_M));
  x = new_Proj(d, mode_X, pn_Div_X_except);
  add_immBlock_pred(catch_block, x);
  d = new_Proj(d, mode_Is, pn_Div_res);
  set_value(3, d);  /* this (3) is variable d */

  /* c = 8 */
  c = new_Const (mode_Is, new_tarval_from_long (8, mode_Is));
  set_value (2, c);  /* this (2) is variable c */

  /* d = b / 0 */
  d = new_Div(get_store(), get_value(1, mode_Is), zero);
  set_store(new_Proj(d, mode_M, pn_Div_M));
  x = new_Proj(d, mode_X, pn_Div_X_except);
  add_immBlock_pred(catch_block, x);
  d = new_Proj(d, mode_Is, pn_Div_res);
  set_value(3, d);  /* this (3) is variable d */

  /* return d */
  d = get_value(3, mode_Is);
  x = new_Return (get_store(), 1, &d);
  mature_immBlock(get_cur_block());
  add_immBlock_pred(get_irg_end_block(current_ir_graph), x);

  /* return c */
  set_cur_block(catch_block);
  c = get_value(2, mode_Is);
  x = new_Return (get_store(), 1, &c);
  mature_immBlock(get_cur_block());
  add_immBlock_pred(get_irg_end_block(current_ir_graph), x);

  /* Now we can mature the end block as all it's predecessors are known. */
  mature_immBlock (get_irg_end_block(irg));

  irg_finalize_cons (irg);

  /* verify the graph */
  irg_vrfy(irg);

  printf("Done building the graph.  Dumping it.\n");
  dump_ir_block_graph (irg, "");
  dump_ir_graph (irg, "");
  printf("use xvcg to view this graph:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  return (0);
}
