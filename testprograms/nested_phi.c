/*
 * Project:     libFIRM
 * File name:   testprograms/while_example.c
 * Purpose:     Construct a loop.
 * Author:      Goetz Lindenmaier
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
 *  main(int a, int b) { //  pos 0, pos 1
 *    int c = 1;         //  pos 2
 *    int d = 2;         //  pos 3
 *
 *    while (a < c) {
 *      while (a < d) {
 *      }
 *    }
 *
 *    return a-b;
 *  }
 */

#define a_pos 0
#define b_pos 1
#define c_pos 2
#define d_pos 3

int
main(void)
{
  type *prim_t_int;
  ir_graph *irg;
  type *owner;
  type *proc_main;
  entity *ent;
  ir_node *h1, *b1, *h2, *b2, *x, *r, *t1, *f1, *t2, *f2;

  printf("\nCreating an IR graph: NESTED_PHI...\n");

  init_firm (NULL);
  //  set_opt_normalize (0);
  set_optimize(1);
  set_opt_constant_folding(1);
  set_opt_cse(1);
  set_opt_dead_node_elimination (1);

  prim_t_int = new_type_primitive(id_from_str ("int", 3), mode_Is);

#define METHODNAME "main_tp"
#define NRARGS 1
#define NRES 1

  proc_main = new_type_method(id_from_str(METHODNAME, strlen(METHODNAME)),
                              NRARGS, NRES);
  set_method_param_type(proc_main, 0, prim_t_int);
  set_method_res_type(proc_main, 0, prim_t_int);

  owner = new_type_class (new_id_from_str ("NESTED_PHI"));
  ent = new_entity (owner, new_id_from_str ("main"), proc_main);

  /* Generates start and end blocks and nodes and a first, initial block */
  irg = new_ir_graph (ent, 4);

  /* Generate two values */
  set_value (a_pos, new_Proj(get_irg_args(irg), mode_Is, 0));
  set_value (b_pos, new_Proj(get_irg_args(irg), mode_Is, 0));
  set_value (c_pos, new_Const (mode_Is, new_tarval_from_long (1, mode_Is)));
  set_value (d_pos, new_Const (mode_Is, new_tarval_from_long (2, mode_Is)));

  /* a block for the outer loop header and the conditional branch */
  h1 = get_irg_current_block(irg);
  x = new_Cond (new_Proj(new_Cmp(get_value(a_pos, mode_Is), get_value(c_pos, mode_Is)),
                         mode_b, Le));
  f1 = new_Proj (x, mode_X, 0);
  t1 = new_Proj (x, mode_X, 1);

  /* generate the block for the loop body */
  b1 = new_immBlock ();
  add_in_edge (b1, t1);

  /* The loop body is the head of the inner loop */
  h2 = b1;
  x = new_Cond (new_Proj(new_Cmp(get_value(a_pos, mode_Is), get_value(d_pos, mode_Is)),
                         mode_b, Le));
  f2 = new_Proj (x, mode_X, 0);
  t2 = new_Proj (x, mode_X, 1);
  add_in_edge(h1, f2);
  mature_block(h1);

  /* The inner loop body */
  b2 = new_immBlock ();
  add_in_edge (b2, t2);
  mature_block(b2);
  x = new_Jmp();
  add_in_edge (h2, x);
  mature_block(h2);

  /* generate the return block */
  r = new_immBlock ();
  add_in_edge (r, f1);
  mature_block (r);

  {
     ir_node *in[1];
     in[0] = new_Sub (get_value (a_pos, mode_Is), get_value (b_pos, mode_Is), mode_Is);

     x = new_Return (get_store (), 1, in);
  }

  /* finalize the end block generated in new_ir_graph() */
  add_in_edge (get_irg_end_block(irg), x);
  mature_block (get_irg_end_block(irg));

  irg_finalize_cons (irg);

  printf("Optimizing ...\n");

  //local_optimize_graph(irg),
  //dead_node_elimination(irg);

  /* verify the graph */
  irg_vrfy(irg);

  /* output the vcg file */
  printf("Done building the graph.  Dumping it.\n");
  turn_off_edge_labels();
  dump_all_types();
  dump_ir_block_graph (irg);
  printf("Use xvcg to view this graph:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  return (0);
}
