/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Christian Schaefer, Goetz Lindenmaier
**
** testprogram.
*/

# include "irdump.h"
# include "firm.h"

/**
***  This file constructs the ir for the following pseudo-program:
***
***  Doesn't work for some reason!!!!
***
***  int main(int a) {
***    int b = 2;
***    if ( a == b )
***      { a := a - 2; }
***
***    return a;
***  }
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

  printf("\nCreating an IR graph: IF_EXAMPLE...\n");

  init_firm ();

#define CLASSNAME "IF_EXAMPLE"
#define METHODNAME "main"
#define NRARGS 1
#define NRES 1

  owner = get_glob_type();
  proc_main = new_type_method(id_from_str(METHODNAME, strlen(METHODNAME)),
			      NRARGS, NRES);
  ent = new_entity (owner,
                    id_from_str (METHODNAME, strlen(METHODNAME)),
                    proc_main);

#define RES_NAME "int"
  typ = new_type_primitive(id_from_str(RES_NAME, strlen(RES_NAME)), mode_i);
  set_method_param_type(proc_main, 0, typ);
  set_method_res_type(proc_main, 0, typ);

  /* Generates start and end blocks and nodes and a first, initial block */
  irg = new_ir_graph (ent, 2);

  /* The value position used for a: */
  a_pos = 0;
  b_pos = 1;

  /* Generate the constant */
  set_value (a_pos, new_Proj (get_irg_args(irg), mode_i, 0));
  /*set_value (a_pos, new_Const (mode_i, tarval_from_long (mode_i, 0)));*/
  set_value (b_pos, new_Const (mode_i, tarval_from_long (mode_i, 2)));
  mature_block (get_irg_current_block(irg));

  /* Generate a conditional branch */
  cmp = new_Cmp(get_value(a_pos, mode_i), get_value(b_pos, mode_i));
  x = new_Cond (new_Proj(cmp, mode_b, Eq));
  f = new_Proj (x, mode_X, 0);
  t = new_Proj (x, mode_X, 1);

  /* generate and fill the then block */
  r = new_immBlock ();
  add_in_edge (r, t);
  a = new_Sub(get_value(a_pos, mode_i),
              new_Const (mode_i, tarval_from_long (mode_i, 2)),
  	      mode_i);
  set_value (a_pos, a);

  mature_block (r);
  x = new_Jmp ();

  /* generate the fall through block and add all cfg edges */
  r = new_immBlock ();
  add_in_edge (r, f);
  add_in_edge (r, x);
  mature_block (r);
  {
     ir_node *in[1], *store ;
     in[0] = get_value (a_pos, mode_i);
     store = get_store();

     x = new_Return (store, 1, in);
  }

  /* finalize the end block generated in new_ir_graph() */
  add_in_edge (get_irg_end_block(irg), x);
  mature_block (get_irg_end_block(irg));


  printf("Optimizing ...\n");
  local_optimize_graph(irg);
  dead_node_elimination(irg);

  /* verify the graph */
  irg_vrfy(irg);

  /* output the vcg file */
  printf("Done building the graph.  Dumping it.\n");
  dump_ir_block_graph (irg);
  printf("use xvcg to view this graph:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  return (0);
}
