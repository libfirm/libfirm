/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Goetz Lindenmaier
**
** testprogram.
**
*/

#include <stdio.h>

# include "irdump.h"
# include "firm.h"

/*
 * das leere FIRM Programm
 */

/**
***  This program shows how to build ir for global variables.
***  It constructs the ir for the following pseudo-program:
***
***  int i;
***
***  main() {
***    i = 2;
***    return;
***  }
**/

int main(int argc, char **argv)
{
  ir_graph *irg;          /* this variable contains the irgraph */
  type_class *owner;      /* the class in which this method is defined */
  type_method *proc_main; /* type information for the method main */
  type_primitive *prim_t_int;  /* describes int type defined by the language */
  entity *main_ent;       /* represents this method as entity of owner */
  entity *i_ent;          /* the entity representing the global variable i */
  ir_node *x, *i_ptr, *store;

  printf("creating an IR graph: GLOBAL_VAR ...\n");

  /* init library */
  init_firm ();

  /* make basic type information for primitive type int.
     In Sather primitive types are represented by a class.
     This is the modeling appropriate for other languages.
     Mode_i says that all integers shall be implemented as a
     32 bit integer value.  */
  prim_t_int = new_type_primitive(id_from_str ("int", 3), mode_i);

  /* FIRM was designed for oo languages where all methods belong to a class.
   * For imperative languages like C we view a file or compilation unit as
   * a large class containing all functions as methods in this file.
   * This class is automatically generated and can be obtained by get_glob_type().
   */
#define METHODNAME "main"
#define NRARGS 0
#define NRES 0

  /* Main is an entity of this global class. */
  owner = get_glob_type();
  proc_main = new_type_method(id_from_str(METHODNAME, strlen(METHODNAME)),
                              NRARGS, NRES);
  main_ent = new_entity ((type *)owner,
			 id_from_str (METHODNAME, strlen(METHODNAME)),
			 (type *)proc_main);

  /* Generates the basic graph for the method represented by entity main_ent, that
   * is, generates start and end blocks and nodes and a first, initial block.
   * The constructor needs to know how many local variables the method has.
   */
#define NUM_OF_LOCAL_VARS 0

  /* Generate the entities for the global variables. */
  i_ent = new_entity ((type *)get_glob_type(),
		      id_from_str ("i", strlen("i")),
		      (type *)prim_t_int);

  irg = new_ir_graph (main_ent, NUM_OF_LOCAL_VARS);

  /* The constructor new_ir_graph() generated a region to place nodes in.
   * This region is accessible via the attribut current_block of irg and
   * it is not matured.
   * Generate the assignment to i and the return node into this region.
   * The Return node is needed to return at least the store. */
  i_ptr = new_simpleSel(get_store(), get_irg_globals(irg), i_ent);

  store = new_Store (get_store(), i_ptr,
		     new_Const(mode_i, tarval_from_long (mode_i, 2)));
  set_store(new_Proj(store, mode_M, 0));

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

  printf("\nDone building the graph.  Dumping it.\n");
  dump_ir_block_graph (irg);
  dump_ir_graph_w_types (irg);

  printf("use xvcg to view this graph:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n");

  return (0);
}
