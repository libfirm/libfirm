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

/**
***  This file constructs the ir for the following pseudo-program:
***
***  void f(char *);
***
***  main() {
***    f("Hello world !");
***  }
**/

int main(int argc, char **argv)
{
  ir_graph *irg;          /* this variable contains the irgraph */
  type_class *owner;      /* the class in which this method is defined */
  entity *ent;            /* represents this method as entity of owner */
  ir_node *x, *const_str, *proc_ptr, *call;

  printf("creating an IR graph: CALL_STR_EXAMPLE...\n");

  /* init library */
  init_firm ();

  /* FIRM was designed for oo languages where all methods belong to a class.
   * For imperative languages like C we view a file as a large class containing
   * all functions as methods in this file.
   * Therefore we define a class "CALL_STR_EXAMPLE" with a method main as
   * an entity.
   */
#define CLASSNAME "CALL_STR_EXAMPLE"
#define ENTITYNAME "main"

  owner = new_type_class (id_from_str (CLASSNAME, strlen(CLASSNAME)));
  ent = new_entity ((type *)owner, id_from_str (ENTITYNAME, strlen(ENTITYNAME)), NULL);


  /* Generates the basic graph for the method represented by entity ent, that
   * is, generates start and end blocks and nodes and a first, initial block.
   * The constructor needs to know how many local variables the method has.
   */
#define NUM_OF_LOCAL_VARS 0

  irg = new_ir_graph (ent, NUM_OF_LOCAL_VARS);

  /* the string is enterd in the constant table. const_str is a pointer to the string */
  const_str = new_Const (mode_p, tarval_p_from_str ("Hello world!"));

  /* get the pointer to the procedure from the class type */
  /* this is how a pointer to be fixed by the linker is represented after
     lowering a Sel node. */
#define FUNCTIONNAME "f"
  proc_ptr = new_Const (mode_p, tarval_p_from_str (FUNCTIONNAME));

  /* call procedure set_a, first built array with parameters */
  {
    ir_node *in[1];
    in[0] = const_str;
    call = new_Call(get_store(), proc_ptr, 1, in, NULL);
  }
  /* make the possible change of call to memory visible */
  set_store(new_Proj(call, mode_M, 0));


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

  printf("\nDone building the graph.  Dumping it.\n");
  dump_ir_block_graph (irg);

  printf("use xvcg to view this graph:\n");
  printf("/ben/trapp/bin/i486/xvcg GRAPHNAME\n");

  return (0);


}
