/* Copyright (C) 2001 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Goetz Lindenmaier
**
** testprogram.
*/

# include "irdump.h"
# include "firm.h"

/**
***  This file constructs the ir for the following pseudo-program:
***
***  main(int a) {        // pos 0
***    int b = 1;         // pos 1
***    int h;             // pos 2
***
***    while (0 == 2) loop {   // 0 == 0 will abort libfirm!!
***      h = a;
***      a = b;
***      b = h;
***    }
***
***    return a-b;
***  }
**/

int
main(void)
{
  type_primitive *prim_t_int;
  ir_graph *irg;
  type_class *owner;
  type_method *proc_main;
  entity *ent;
  ir_node *b, *x, *r, *t, *f;

  printf("\nCreating an IR graph: WHILE_EXAMPLE...\n");

  init_firm ();
  //turn_of_edge_labels();

  set_optimize(1);
  set_opt_constant_folding(1);
  set_opt_cse(1);
  set_opt_dead_node_elimination (1);

  prim_t_int = new_type_primitive(id_from_str ("int", 3), mode_i);

#define METHODNAME "main"
#define NRARGS 1
#define NRES 0

  proc_main = new_type_method(id_from_str(METHODNAME, strlen(METHODNAME)),
                              NRARGS, NRES);
  set_method_param_type(proc_main, 0, (type *)prim_t_int);

  owner = new_type_class (id_from_str ("WHILE_EXAMPLE", 16));
  ent = new_entity ((type *)owner, id_from_str ("main", 4), (type *)proc_main);

  /* Generates start and end blocks and nodes and a first, initial block */
  irg = new_ir_graph (ent, 4);

  /* Generate two constants */
  set_value (0, new_Proj(get_irg_args(irg), mode_I, 0));
  set_value (1, new_Const (mode_I, tarval_from_long (mode_I, 1)));
  x = new_Jmp();
  mature_block (get_irg_current_block(irg));

  /* generate a block for the loop header and the conditional branch */
  r = new_immBlock ();
  add_in_edge (r, x);
  x = new_Cond (new_Proj(new_Cmp(new_Const (mode_I, tarval_from_long (mode_i, 0)),
				 new_Const (mode_I, tarval_from_long (mode_i, 2))),
                         mode_b, Eq));
  f = new_Proj (x, mode_X, 0);
  t = new_Proj (x, mode_X, 1);

  /* generate the block for the loop body */
  b = new_immBlock ();
  add_in_edge (b, t);
  x = new_Jmp ();
  add_in_edge (r, x);

  /* the code in the loop body,
     as we are dealing with local variables only the dataflow edges
     are manipulated */
  set_value (2, get_value (0, mode_I));
  set_value (0, get_value (1, mode_I));
  set_value (1, get_value (2, mode_I));
  mature_block (b);
  mature_block (r);

  /* generate the return block */
  r = new_immBlock ();
  add_in_edge (r, f);
  mature_block (r);

  {
     ir_node *in[1];
     in[0] = new_Sub (get_value (0, mode_I), get_value (1, mode_I), mode_I);

     x = new_Return (get_store (), 1, in);
  }

  /* finalize the end block generated in new_ir_graph() */
  add_in_edge (get_irg_end_block(irg), x);
  mature_block (get_irg_end_block(irg));

  printf("Optimizing ...\n");

  local_optimize_graph(irg),
  dead_node_elimination(irg);

  /* verify the graph */
  irg_vrfy(irg);

  /* output the vcg file */
  printf("Done building the graph.  Dumping it.\n");
  dump_ir_block_graph (irg);
  printf("Use xvcg to view this graph:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  return (0);
}
