/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Christian Schaefer, Goetz Lindenmaier
**
** testprogram.
*/

#include <stdio.h>

# include "irdump.h"
# include "firm.h"

/*
 * das leere FIRM Programm
 */

/**
***  This file constructs the ir for the following pseudo-program:
***
***  main() {
***    return;
***  }
**/

int main(int argc, char **argv)
{
  ir_graph *irg;          /* this variable contains the irgraph */
  type_class *owner;      /* the class in which this method is defined */
  type_method *proc_main; /* type information for the method main */
  entity *ent;            /* represents this method as entity of owner */
  ir_node *x;

  printf("creating an IR graph: EMPTY...\n");

  /* init library */
  init_firm ();

  /* Don't optimize anything */
  set_optimize(0);

  /* FIRM was designed for oo languages where all methods belong to a class.
   * For imperative languages like C we view a file as a large class containing
   * all functions as methods in this file.
   * This class now is automatically generated.
   */
#define METHODNAME "main"
#define NRARGS 0
#define NRES 0

  owner = get_glob_type();
  proc_main = new_type_method(id_from_str(METHODNAME, strlen(METHODNAME)),
                              NRARGS, NRES);
  ent = new_entity ((type *)owner,
                    id_from_str (METHODNAME, strlen(METHODNAME)),
                    (type *)proc_main);

  /* Generates the basic graph for the method represented by entity ent, that
   * is, generates start and end blocks and nodes and a first, initial block.
   * The constructor needs to know how many local variables the method has.
   */
#define NUM_OF_LOCAL_VARS 0

  irg = new_ir_graph (ent, NUM_OF_LOCAL_VARS);


  /* The constructor new_ir_graph() generated a region to place nodes in.
   * This region is accessible via the attribut current_block of irg and
   * it is not matured.
   * Generate the return node into this region. The Return node is needed to
   * return at least the store. */
  {
    ir_node *in[0]; /* this is the array containing the return parameters */
    x = new_Return (get_store(), 0, in);
  }
  /* Now generate all instructions for this block and all its predecessor blocks
   * so we can mature it. */
  mature_block (irg->current_block);

  /* This adds the in edge of the end block which originates at the return statement.
   * The return node passes controlflow to the end block.  */
  add_in_edge (irg->end_block, x);
  /* Now we can mature the end block as all it's predecessors are known. */
  mature_block (irg->end_block);

  /* verify the graph */
  irg_vrfy(irg);
  dead_node_elimination(irg);

  printf("\nDone building the graph.  Dumping it.\n");
  dump_ir_block_graph (irg);

  printf("use xvcg to view this graph:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n");

  return (0);
}
