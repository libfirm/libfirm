/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Christian Schaefer, Goetz Lindenmaier
**
** testprogram.
*/

# include "irdump.h"
# include "firm.h"

/**  This example demonstrates the use of memory edges.
***  This file constructs the ir for the following pseudo-program:
***
***  int VAR_A
***  int VAR_B
***
***  main() {
***
***    VAR_A = 0
***    VAR_B = 1
***
***    repeat {
***      h = VAR_A;
***      VAR_A = VAR_B;
***      VAR_B = h;
***    } until (0 == h)
***
***    return (VAR_A)
***  }
***
***
***  A better example would be the following program:
***  (name e.g.: memory-imp_example.c as it models imperative concepts.)
***
***  In this program a local variable is dereferenced.  It has
***  to be modeled as an entity of the stack so that a pointer to it is available.
***  It is also an example where an analysis could find out that the
***  pointer is never actually used.
***
***  main () {
***    int a;
***    int *p;
***
***    a = 2;
***    p = &a;
***    return (*p);
***  }
***
**/

int
main(void)
{
  ir_graph *irg;
  type_class *owner;
  type_method *method;    /* the type of this method */
  entity *ent;
  ir_node *a, *b, *x, *y, *r;

  printf("\nCreating an IR graph: MEMORY_EXAMPLE...\n");

  init_firm ();

  set_opt_dead_node_elimination (1);

  /* a class to get started with, containing the main procedure */
  owner = new_type_class (id_from_str ("MEMORY_EXAMPLE", 14));
  method = new_type_method (id_from_str("main", 4), 0, 2);
  ent = new_entity ((type *)owner, id_from_str ("main", 4), (type *)method);

  /* Generates start and end blocks and nodes and a first, initial block */
  irg = new_ir_graph (ent, 4);

  /* generate two constant pointers to string constants */
  /* this simulates two global variables, a and b point to these variables */
  a = new_Const (mode_p, tarval_p_from_str ("VAR_A"));
  b = new_Const (mode_p, tarval_p_from_str ("VAR_B"));

  /* set VAR_A and VAR_B to constant values */
  set_store (new_Proj (new_Store (get_store (), a,
		     	          new_Const (mode_I, tarval_from_long (mode_i, 0))),
                       mode_M, 0));

  set_store (new_Proj (new_Store (get_store (), b,
			          new_Const (mode_I, tarval_from_long (mode_i, 1))),
                       mode_M, 0));

  /* finish this first block */
  x = new_Jmp ();
  mature_block (irg->current_block);

  /* a loop body */
  r = new_Block ();
  add_in_edge (r, x);

  /* exchange the content of the two variables. Exceptions not cached. */
  /* load the value and make it's effects visible. */
  x = new_Load (get_store (), a);
    set_store (new_Proj (x, mode_M, 0));
    x = new_Proj(x, mode_I, 2);
  /* the same again: load the value and make it's effects visible. */
  y = new_Load (get_store (), b);
    set_store (new_Proj (y, mode_M, 0));
    y = new_Proj(y, mode_I, 2);
  /* store the exchanged values. */
  set_store (new_Proj (new_Store (get_store (), a, y), mode_M, 0));
  set_store (new_Proj (new_Store (get_store (), b, x), mode_M, 0));

  /* test the condition */
  x = new_Cond (
        new_Proj (
          new_Cmp (
            new_Const (mode_I, tarval_from_long (mode_i, 0)),
            x),
          mode_b, Gt));

  /* build the cfg of the loop */
  add_in_edge (r, new_Proj (x, mode_X, 0));
  x = new_Proj (x, mode_X, 1);
  mature_block(r);

  /* generate the block the loop exits to */
  r = new_Block ();
  add_in_edge (r, x);

  /* generate the return block and return the content of VAR_A */
  {
     ir_node *in[1];
     x = new_Load (get_store (), a);
     in[0] = new_Proj (x, mode_I, 2);

     x = new_Return (new_Proj(x, mode_M, 0), 1, in);
  }
  mature_block (r);
  add_in_edge (irg->end_block, x);
  mature_block (irg->end_block);

  printf("Optimizing ...\n");
  dead_node_elimination(irg);

  /* verify the graph */
  irg_vrfy(irg);

  printf("Done building the graph.  Dumping it.\n");
  dump_ir_block_graph (irg);
  printf("Use xvcg to view this graph:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  return (0);
}
