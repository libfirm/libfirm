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
***  main() {
***    int a = 0;
***    int b = 1;
***
***    if (0 == 0)
***      { a = 2; }
***
***    while (0 == 0) loop {
***      h = a;
***      a = b;
***      b = h;
***    }
***
***    return a-b;
**/

int
main(void)
{
  ir_graph *irg;
  type_class *owner;
  entity *ent;
  ir_node *b, *x, *r, *t, *f;

  printf("creating an IR graph: IF_WHILE_EXAMPLE...\n");

  init_firm ();

  owner = new_type_class (id_from_str ("IF_WHILE_EXAMPLE", 16));
  ent = new_entity ((type *)owner, id_from_str ("main", 4), NULL);

  /* Generates start and end blocks and nodes and a first, initial block */
  irg = new_ir_graph (ent, 4);

  /* Generate two constants */
  set_value (0, new_Const (mode_I, tarval_from_long (mode_i, 0)));
  set_value (1, new_Const (mode_I, tarval_from_long (mode_i, 1)));
  mature_block (irg->current_block);

  /* Generate a conditional branch */
  x = new_Cond (new_Proj(new_Cmp(new_Const (mode_I, tarval_from_long (mode_i, 0)),
                                 new_Const (mode_I, tarval_from_long (mode_i, 0))),
                         mode_b, Eq));
  f = new_Proj (x, mode_X, 0);
  t = new_Proj (x, mode_X, 1);

  /* generate and fill the then block */
  r = new_Block ();
  add_in_edge (r, t);
  set_value (0, new_Const (mode_I, tarval_from_long (mode_i, 2)));
  mature_block (r);
  x = new_Jmp ();

  /* generate the fall through block and add all cfg edges */
  r = new_Block ();
  add_in_edge (r, f);
  add_in_edge (r, x);
  mature_block (r);
  x = new_Jmp ();

  /* generate a block for the loop header and the conditional branch */
  r = new_Block ();
  add_in_edge (r, x);
  x = new_Cond (new_Proj(new_Cmp(new_Const (mode_I, tarval_from_long (mode_i, 0)),
                                 new_Const (mode_I, tarval_from_long (mode_i, 0))),
                         mode_b, Eq));
  f = new_Proj (x, mode_X, 0);
  t = new_Proj (x, mode_X, 1);

  /* generate the block for the loop body */
  b = new_Block ();
  add_in_edge (b,t);
  x = new_Jmp ();
  add_in_edge (r, x);
  mature_block (r);

  /* the code in the loop body,
     as we are dealing with local variable only the dataflow edges are manipulated */
  set_value (2, get_value (0, mode_I));
  set_value (0, get_value (1, mode_I));
  set_value (1, get_value (2, mode_I));
  mature_block (b);

  /* generate the return block */
  r = new_Block ();
  add_in_edge (r, f);
  mature_block (r);

  {
     ir_node *in[1];
     in[0] = new_Sub (get_value (0, mode_I), get_value (1, mode_I), mode_I);

     x = new_Return (get_store (), 1, in);
  }

  /* finalize the end block generated in new_ir_graph() */
  add_in_edge (irg->end_block, x);
  mature_block (irg->end_block);

  /* output the vcg file */
  printf("\nDone building the graph.  Dumping it.\n");
  dump_ir_block_graph (irg);

  printf("use xvcg to view this graph:\n");
  printf("/ben/trapp/bin/i486/xvcg GRAPHNAME\n");

  return (0);
}
