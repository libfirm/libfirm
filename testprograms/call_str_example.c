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
  ir_graph *irg;         /* this variable contains the irgraph */
  type     *owner;       /* the class in which this method is defined */
  type     *proc_main;   /* type information for the method main */
  type     *proc_called; /* type information for called method f */
  entity   *ent;         /* represents this method as entity of owner */
  ir_node  *x, *const_str, *proc_ptr, *call;

  printf("\nCreating an IR graph: CALL_STR_EXAMPLE...\n");

  /* init library */
  init_firm ();

  /* FIRM was designed for oo languages where all methods belong to a class.
   * For imperative languages like C we view a program as a large class containing
   * all functions of the program as methods in this class.  This class is
   * automatically generated.
   * We use the same name for the method type as for the method entity.
   */
#define METHODNAME "main"
#define NRARGS 0
#define NRES 0
  owner = get_glob_type();
  proc_main = new_type_method(id_from_str(METHODNAME, strlen(METHODNAME)),
                              NRARGS, NRES);

  /* Make type information for called method which also belongs to the
     global type. */
#define F_METHODNAME "f"
#define F_NRARGS 1
#define F_NRES 0
  owner = get_glob_type();
  proc_called = new_type_method(id_from_str(F_METHODNAME, strlen(F_METHODNAME)),
                              F_NRARGS, F_NRES);

  /* Make the entity for main needed for a correct  ir_graph.  */
#define ENTITYNAME "main"
  ent = new_entity (owner, id_from_str (ENTITYNAME, strlen(ENTITYNAME)),
                    proc_main);

  /* Generates the basic graph for the method represented by entity ent, that
   * is, generates start and end blocks and nodes and a first, initial block.
   * The constructor needs to know how many local variables the method has.
   */
#define NUM_OF_LOCAL_VARS 0
  irg = new_ir_graph (ent, NUM_OF_LOCAL_VARS);

  /* the string is entered in the constant table. const_str is a pointer to the string */
  const_str = new_Const (mode_p, tarval_p_from_str ("Hello world!"));

  /* get the pointer to the procedure from the class type */
  /* this is how a pointer to be fixed by the linker is represented after
     lowering a Sel node. */
#define FUNCTIONNAME "f"
  proc_ptr = new_SymConst ((type_or_id_p)id_from_str (FUNCTIONNAME, strlen(FUNCTIONNAME)),
			   linkage_ptr_info);

  /* call procedure set_a, first built array with parameters */
  {
    ir_node *in[1];
    in[0] = const_str;
    call = new_Call(get_store(), proc_ptr, 1, in, proc_called);
  }
  /* make the possible changes by the called method to memory visible */
  set_store(new_Proj(call, mode_M, 0));

  /* Make the return node returning the memory. */
  {
    ir_node *in[0]; /* this is the array containing the return parameters */
    x = new_Return (get_store(), 0, in);
  }
  /* Now we generated all instructions for this block and all its predecessor blocks
   * so we can mature it. */
  mature_block (get_irg_current_block(irg));

  /* This adds the in edge of the end block which originates at the return statement.
   * The return node passes controlflow to the end block.  */
  add_in_edge (get_irg_end_block(irg), x);
  /* Now we can mature the end block as all it's predecessors are known. */
  mature_block (get_irg_end_block(irg));

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
