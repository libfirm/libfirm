/*
 * Project:     libFIRM
 * File name:   testprograms/endless_loop.c
 * Purpose:     Representation of an endless loop.
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
*  VAR_A is some extern variable.
*
*  main(int a) {        //  pos 0
*    int b = 1;         //  pos 1
*    int h;             //  pos 2
*
*    while (0 == 0) loop {
*      h = a;
*      a = b;
*      b = h;
*      VAR_A = b;
*    }
*
*    return a-b;
*  }
**/

int
main(void)
{
  type *prim_t_int;
  ir_graph *irg;
  type *owner;
  type *proc_main;
  entity *ent;
  ir_node *b, *x, *r, *t, *f;

  printf("\nCreating an IR graph: ENDLESS_LOOP_EXAMPLE...\n");

  init_firm (NULL);

  set_optimize(1);
  set_opt_constant_folding(1);
  set_opt_cse(1);
  set_opt_global_cse(0);
  set_opt_dead_node_elimination (1);

  prim_t_int = new_type_primitive(new_id_from_chars ("int", 3), mode_Is);

#define METHODNAME "main_tp"
#define NRARGS 1
#define NRES 1

  proc_main = new_type_method(new_id_from_chars(METHODNAME, strlen(METHODNAME)),
                              NRARGS, NRES);
  set_method_param_type(proc_main, 0, prim_t_int);
  set_method_res_type(proc_main, 0, prim_t_int);


  owner = new_type_class (new_id_from_chars ("ENDLESS_LOOP_EXAMPLE", 20));
  ent = new_entity (owner, new_id_from_chars ("main", strlen("main")), proc_main);
  get_entity_ld_name(ent); /* force name mangling */

  /* Generates start and end blocks and nodes and a first, initial block */
  irg = new_ir_graph (ent, 4);

  /* Generate two values */
  set_value (0, new_Proj(get_irg_args(irg), mode_Is, 0));
  set_value (1, new_Const (mode_Is, new_tarval_from_long (1, mode_Is)));

  x = new_Jmp();
  mature_immBlock (get_irg_current_block(irg));

  /* generate a block for the loop header and the conditional branch */
  r = new_immBlock ();
  add_immBlock_pred (r, x);
  x = new_Cond (new_Proj(new_Cmp(new_Const (mode_Is, new_tarval_from_long (0, mode_Is)),
                 new_Const (mode_Is, new_tarval_from_long (0, mode_Is))),
             mode_b, pn_Cmp_Eq));
  f = new_Proj (x, mode_X, 0);
  t = new_Proj (x, mode_X, 1);

  /* generate the block for the loop body */
  b = new_immBlock ();
  add_immBlock_pred (b, t);
  x = new_Jmp ();
  add_immBlock_pred (r, x);

  /* The code in the loop body,
     as we are dealing with local variables only the dataflow edges
     are manipulated. */
  set_value (2, get_value (0, mode_Is));
  set_value (0, get_value (1, mode_Is));
  set_value (1, get_value (2, mode_Is));

  /* set VAR_A to constant value */
  set_store (new_Proj (new_Store (get_store (),
                                  new_simpleSel(
                                    get_store(),
                                    get_irg_globals(irg),
                                    new_entity(get_glob_type(),new_id_from_chars("VAR_A",6),prim_t_int)),
                                  get_value(1, mode_Is)),
                       mode_M, 0));

  mature_immBlock (b);
  mature_immBlock (r);

  /* generate the return block */
  r = new_immBlock ();
  add_immBlock_pred (r, f);
  mature_immBlock (r);

  {
     ir_node *in[1];
     in[0] = new_Sub (get_value (0, mode_Is), get_value (1, mode_Is), mode_Is);

     x = new_Return (get_store (), 1, in);
  }

  /* finalize the end block generated in new_ir_graph() */
  add_immBlock_pred (get_irg_end_block(irg), x);
  mature_immBlock (get_irg_end_block(irg));

  finalize_cons (irg);

  printf("Optimizing ...\n");

  dead_node_elimination(irg);
  local_optimize_graph(irg);

  /* verify the graph */
  irg_vrfy(irg);

  /* output the vcg file */
  printf("Done building the graph.  Dumping it.\n");
  /* turn_of_edge_labels(); */
  dump_keepalive_edges(true);
  char *dump_file_suffix = "";
  dump_all_types(dump_file_suffix);
  dump_ir_block_graph (irg, dump_file_suffix);
  printf("Use xvcg to view this graph:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  return (0);
}
