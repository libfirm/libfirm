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
***  main() {
***    int a = 0;
***    int b = 2;
***    if ( a == a )  // a constant condition would be eliminated,
***                   // a realistic condition would no more be a small example
***      { a := a - 2; }
***
***    // return a, bla; // das ist hier kein korrekter graph
***  }
**/

int
main(void)
{
  ir_graph *irg;
  type_class *owner;
  entity *ent;
  type_method *proc_main; /* typeinformation for the method main */
  type_primitive *typ;
  ir_node *x, *r, *t, *f, *a;
  int a_pos, b_pos;
  FILE *outfile;

  printf("creating an IR graph: IF_EXAMPLE...\n");

  init_firm ();

#define CLASSNAME "IF_EXAMPLE"
#define METHODNAME "main"
#define NRARGS 0
#define NRES 2

  owner = new_type_class (id_from_str (CLASSNAME, strlen(CLASSNAME)));
  proc_main = new_type_method(id_from_str(METHODNAME, strlen(METHODNAME)), NRARGS, NRES);
  ent = new_entity ((type *)owner,
                    id_from_str (METHODNAME, strlen(METHODNAME)),
                    (type *)proc_main);

#define RES_NAME "res1"
  typ = new_type_primitive(ID_FROM_STR(RES_NAME, strlen(RES_NAME)), mode_i);
  set_method_res_type(proc_main, 0, (type*)typ);
#undef RES_NAME

#define RES_NAME "res2"
  typ = new_type_primitive(ID_FROM_STR(RES_NAME, strlen(RES_NAME)), mode_i);
  set_method_res_type(proc_main, 1, (type*)typ);
#undef RES_NAME


  /* Generates start and end blocks and nodes and a first, initial block */
  irg = new_ir_graph (ent, 1);

  /* The value position used for a: */
  a_pos = 0;
  b_pos = 1;

  /* Generate the constant */
  set_value (a_pos, new_Const (mode_i, tarval_from_long (mode_i, 0)));
  set_value (b_pos, new_Const (mode_i, tarval_from_long (mode_i, 2)));
  mature_block (irg->current_block);

  /* Generate a conditional branch */
  x = new_Cond (new_Proj(new_Cmp(get_value(a_pos, mode_i),
                                 get_value(a_pos, mode_i)),
                         mode_b, Eq));
  f = new_Proj (x, mode_X, 0);
  t = new_Proj (x, mode_X, 1);

  /* generate and fill the then block */
  r = new_Block ();
  add_in_edge (r, t);
  // mature_block (r);
  a = new_Sub(get_value(a_pos, mode_i),
              new_Const (mode_i, tarval_from_long (mode_i, 2)),
  	      mode_i);
  set_value (a_pos, a);

  mature_block (r);
  x = new_Jmp ();

  /* generate the fall through block and add all cfg edges */
  r = new_Block ();
  add_in_edge (r, f);
  add_in_edge (r, x);
  mature_block (r);
  {
     ir_node *in[3], *store ;
     in[0] = get_value (a_pos, mode_i);
     in[1] = a;
     in[2] = get_value (b_pos, mode_i);
     store = get_store();

     x = new_Return (store, 2, in);
  }

  /* finalize the end block generated in new_ir_graph() */
  add_in_edge (irg->end_block, x);
  mature_block (irg->end_block);

  /* verify the graph */
  vrfy_graph(irg);

  /* output the vcg file */
  printf("\nDone building the graph.  Dumping it.\n");

  dump_ir_block_graph (irg);

  printf("use xvcg to view this graph:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n");

  return (0);
}
