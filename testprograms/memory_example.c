/*
 * Project:     libFIRM
 * File name:   testprograms/memory_example.c
 * Purpose:     Illustrate memory edges.
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
*  int VAR_A
*  int VAR_B
*
*  main() {
*
*    VAR_A = 0
*    VAR_B = 1
*
*    repeat {
*      h = VAR_A;
*      VAR_A = VAR_B;
*      VAR_B = h;
*    } until (0 == h)
*
*    return (VAR_A)
*  }
*
*
*  A better example would be the following program:
*  (name e.g.: memory-imp_example.c as it models imperative concepts.)
*
*  In this program a local variable is dereferenced.  It has
*  to be modeled as an entity of the stack so that a pointer to it is available.
*  It is also an example where an analysis could find out that the
*  pointer is never actually used.
*
*  main () {
*    int a;
*    int *p;
*
*    a = 2;
*    p = &a;
*    return (*p);
*  }
*
**/

int
main(void)
{
  ir_graph *irg;
  type     *owner;
  type     *method;    /* the type of this method */
  type     *prim_t_int;
  entity   *ent;
  ir_node  *a, *b, *x, *y, *r;

  printf("\nCreating an IR graph: MEMORY_EXAMPLE...\n");

  init_firm (NULL);

  set_opt_dead_node_elimination (1);

  /*** Make basic type information for primitive type int. ***/
  prim_t_int = new_type_primitive(new_id_from_chars ("int", 3), mode_Iu);

  /* a class to get started with, containing the main procedure */
  owner = new_type_class (new_id_from_chars ("MEMORY_EXAMPLE", 14));
  method = new_type_method (new_id_from_chars("main", 4), 0, 1);
  set_method_res_type(method, 0, prim_t_int);
  ent = new_entity (owner, new_id_from_chars ("main", 4), method);
  get_entity_ld_name(ent); /* To enforce name mangling for vcg graph name */

  /* Generates start and end blocks and nodes and a first, initial block */
  irg = new_ir_graph (ent, 4);

  /* create two global variables, a and b point to these variables */
  a = new_simpleSel(
              get_store(),
              get_irg_globals(irg),
              new_entity(get_glob_type(),new_id_from_chars("VAR_A",6),prim_t_int));
  b = new_simpleSel(
              get_store(),
              get_irg_globals(irg),
              new_entity(get_glob_type(),new_id_from_chars("VAR_B",6),prim_t_int));
   /* set VAR_A and VAR_B to constant values */
  set_store (new_Proj (new_Store (get_store (), a,
		     	          new_Const (mode_Iu, new_tarval_from_long (0, mode_Is))),
                       mode_M, 0));

  set_store (new_Proj (new_Store (get_store (), b,
			          new_Const (mode_Iu, new_tarval_from_long (1, mode_Is))),
                       mode_M, 0));

  /* finish this first block */
  x = new_Jmp ();
  mature_immBlock (get_irg_current_block(irg));

  /* a loop body */
  r = new_immBlock ();
  add_immBlock_pred (r, x);

  /* exchange the content of the two variables. Exceptions not cached. */
  /* load the value and make it's effects visible. */
  x = new_Load (get_store (), a, mode_Iu);
    set_store (new_Proj (x, mode_M, 0));
    x = new_Proj(x, mode_Iu, 2);
  /* the same again: load the value and make it's effects visible. */
  y = new_Load (get_store (), b, mode_Iu);
    set_store (new_Proj (y, mode_M, 0));
    y = new_Proj(y, mode_Iu, 2);
  /* store the exchanged values. */
  set_store (new_Proj (new_Store (get_store (), a, y), mode_M, 0));
  set_store (new_Proj (new_Store (get_store (), b, x), mode_M, 0));

  /* test the condition */
  x = new_Cond (
        new_Proj (
          new_Cmp (
            new_Const (mode_Iu, new_tarval_from_long (0, mode_Is)),
            x),
          mode_b, Gt));

  /* build the cfg of the loop */
  add_immBlock_pred (r, new_Proj (x, mode_X, 0));
  x = new_Proj (x, mode_X, 1);
  mature_immBlock(r);

  /* generate the block the loop exits to */
  r = new_immBlock ();
  add_immBlock_pred (r, x);

  /* generate the return block and return the content of VAR_A */
  {
     ir_node *in[1];
     x = new_Load (get_store (), a, mode_Iu);
     in[0] = new_Proj (x, mode_Iu, 2);

     x = new_Return (new_Proj(x, mode_M, 0), 1, in);
  }
  mature_immBlock (r);
  add_immBlock_pred (get_irg_end_block(irg), x);
  mature_immBlock (get_irg_end_block(irg));

  finalize_cons (irg);

  printf("Optimizing ...\n");
  dead_node_elimination(irg);

  /* verify the graph */
  irg_vrfy(irg);

  printf("Done building the graph.  Dumping it.\n");
  dump_ir_block_graph (irg, 0);
  printf("Use xvcg to view this graph:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  return (0);
}
