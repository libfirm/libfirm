/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Christian Schaefer, Goetz Lindenmaier
**
** testprogram
*/

# include "irdump.h"
# include "firm.h"

/**
***  This file constructs the ir for the following pseudo-program:
***
***  main() {
***    int c, d;
***
***    c = 5 + 7;
***    d = 7 + 5;
***
***    return (c, d);
***  }
**/

int
main(void)
{
  ir_graph *irg;
  type_class *owner;
  type_method *method;    /* the type of this method */
  entity *ent;
  ir_node *a, *b, *c, *d, *x;

  printf("\nCreating an IR graph: CONST_EVAL_EXAMPLE...\n");

  init_firm ();

  /* Try both optimizations: */
  set_opt_constant_folding(1);
  set_opt_cse(1);
  set_opt_dead_node_elimination (1);

  owner = new_type_class (id_from_str ("CONST_EVAL_EXAMPLE", 18));
  method = new_type_method (id_from_str("main", 4), 0, 2);
  ent = new_entity ((type *)owner, id_from_str ("main", 4), (type *)method);

  irg = new_ir_graph (ent, 4);

  a = new_Const (mode_i, tarval_from_long (mode_i, 7));
  b = new_Const (mode_i, tarval_from_long (mode_i, 5));

  x = new_Jmp ();
  mature_block (get_irg_current_block(irg));

  /*  To test const eval on DivMod
  c = new_DivMod(get_store(), a, b);
  set_store(new_Proj(c, mode_M, 0));
  d = new_Proj(c, mode_i, 3);
  c = new_Proj(c, mode_i, 2);
  */

  c = new_Add (new_Const (mode_i, tarval_from_long (mode_i, 5)),
	       new_Const (mode_i, tarval_from_long (mode_i, 7)),
	       mode_i);
  d = new_Add (new_Const (mode_i, tarval_from_long (mode_i, 7)),
	       new_Const (mode_i, tarval_from_long (mode_i, 5)),
	       mode_i);

  {
     ir_node *in[2];
     in[0] = c;
     in[1] = d;

     x = new_Return (get_store (), 2, in);
  }

  add_in_edge (get_irg_end_block(irg), x);
  mature_block (get_irg_end_block(irg));

  printf("Optimizing ...\n");
  dead_node_elimination(irg);

  printf("Done building the graph.  Dumping it.\n");
  /* verify the graph */
  irg_vrfy(irg);

  dump_ir_block_graph (irg);

  printf("use xvcg to view this graph:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  return (0);
}
