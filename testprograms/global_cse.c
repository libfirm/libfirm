/*
 * Project:     libFIRM
 * File name:   testprograms/global_cse.c
 * Purpose:     Test global cse.
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
# include "irdump.h"
# include "firm.h"

/**
*  This file constructs the ir for the following pseudo-program:
*
*  int main(int a) {
*    int b = 2;
*    if ( a == b ) {
*       a := a - 3;
*    } else {
*       a := a - 3;
*       a := a + 5;
*    }
*    return a;
*  }
**/

int
main(void)
{
  ir_graph *irg;
  type *owner;
  entity *ent;
  type *proc_main; /* type information for the method main */
  type *typ;
  ir_node *x, *r, *t, *f, *a, *cmp;
  int a_pos, b_pos;

  printf("\nCreating an IR graph: GLOBAL_CSE_EXAMPLE...\n");

  init_firm (NULL);

  set_optimize(1);
  set_opt_constant_folding(1);
  set_opt_cse(1);
  set_opt_global_cse(1);
  set_opt_dead_node_elimination (1);

#define CLASSNAME "GLOBAL_CSE_EXAMPLE"
#define METHODNAME "GLOBAL_CSE_EXAMPLE_main"
#define NRARGS 1
#define NRES 1

  /** Type information for the procedure **/

  owner = get_glob_type();
  /* Type information for the procedure */
  proc_main = new_type_method(new_id_from_chars(METHODNAME, strlen(METHODNAME)),
			      NRARGS, NRES);
  /* The entity for the procedure */
  ent = new_entity (owner,
                    new_id_from_chars (METHODNAME, strlen(METHODNAME)),
                    proc_main);
  /* The type int.  This type is necessary to model the result and parameters
     the procedure. */
#define PRIM_NAME "int"
  typ = new_type_primitive(new_id_from_chars(PRIM_NAME, strlen(PRIM_NAME)), mode_Is);
  /* The parameter and result types of the procedure. */
  set_method_param_type(proc_main, 0, typ);
  set_method_res_type(proc_main, 0, typ);

  /** The code of the procedure **/

  /* Generates start and end blocks and nodes, and a first, initial block */
#define NRLOCS 2
  irg = new_ir_graph (ent, NRLOCS);

  /* The value position used for: */
  a_pos = 0;
  b_pos = 1;

  /* Get the procedure parameter and assign it to the parameter variable
     a. */
  set_value (a_pos, new_Proj (get_irg_args(irg), mode_Is, 0));
  /* Generate the constant and assign it to b. The assignment is resovled to a
     dataflow edge. */
  set_value (b_pos, new_Const (mode_Is, new_tarval_from_long (2, mode_Is)));
  /* We know all predecessors of the block and all set_values and set_stores are
     preformed.   We can mature the block.  */
  mature_immBlock (get_irg_current_block(irg));

  /* Generate a conditional branch */
  cmp = new_Cmp(get_value(a_pos, mode_Is), get_value(b_pos, mode_Is)); /*
  cmp = new_Cmp(new_Const (mode_Is, new_tarval_from_long (2, mode_Is)),
                new_Const (mode_Is, new_tarval_from_long (2, mode_Is)));*/
  x = new_Cond (new_Proj(cmp, mode_b, pn_Cmp_Eq));
  f = new_Proj (x, mode_X, 0);
  t = new_Proj (x, mode_X, 1);

  /* generate and fill the then block */
  r = new_immBlock ();
  add_immBlock_pred (r, t);
  a = new_Sub(get_value(a_pos, mode_Is),
              new_Const (mode_Is, new_tarval_from_long (3, mode_Is)),
  	      mode_Is);
  set_value (a_pos, a);

  mature_immBlock (r);
  t = new_Jmp ();

  /* generate the else block */
  r = new_immBlock ();
  add_immBlock_pred (r, f);
  a = new_Sub(get_value(a_pos, mode_Is),
              new_Const (mode_Is, new_tarval_from_long (3, mode_Is)),
  	      mode_Is);
  a = new_Add(a, new_Const (mode_Is, new_tarval_from_long (5, mode_Is)), mode_Is);
  set_value (a_pos, a);

  mature_immBlock (r);
  f = new_Jmp ();

  /* generate the fall through block and add all cfg edges */
  r = new_immBlock ();
  add_immBlock_pred (r, f);
  add_immBlock_pred (r, t);
  mature_immBlock (r);
  /* The Return statement */
  {
     ir_node *in[1], *store ;
     in[0] = get_value (a_pos, mode_Is);
     store = get_store();

     x = new_Return (store, 1, in);
  }

  /* finalize the end block generated in new_ir_graph() */
  add_immBlock_pred (get_irg_end_block(irg), x);
  mature_immBlock (get_irg_end_block(irg));

  /* verify the graph */
  irg_vrfy(irg);
  irg_finalize_cons (irg);

  printf("Optimizing ...\n");
  local_optimize_graph(irg);
  dead_node_elimination(irg);

  /* output the vcg file */
  printf("Done building the graph.  Dumping it.\n");
  dump_ir_block_graph (irg, 0);
  printf("use xvcg to view this graph:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  return (0);
}
