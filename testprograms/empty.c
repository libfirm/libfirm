/*
 * Project:     libFIRM
 * File name:   testprograms/empty.c
 * Purpose:     The smallest possible firm graph.
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
*  An empty Firm program.
*
*  This file constructs the ir for the following pseudo-program:
*
*  main() {
*    return;
*  }
*
*
**/

int main(int argc, char **argv)
{
  ir_graph *irg;        /* this variable contains the irgraph */
  type     *owner;      /* the class in which this method is defined */
  type     *proc_main;  /* type information for the method main */
  entity   *ent;        /* represents this method as entity of owner */
  ir_node  *x;          /* to build control flow */

  printf("\nCreating an IR graph: EMPTY...\n");

  /* init library */
  init_firm (NULL);

  /** Build type information for the procedure. **/

  /* FIRM was designed for oo languages where all methods belong to a class.
   * For imperative languages like C we view a file as a large class containing
   * all functions in this file as methods.
   * This clas is generated automatically.
   */
  owner = get_glob_type();

#define METHODNAME "EMPTY_main"
#define NRARGS 0
#define NRES 0
  /* The type of the method */
  proc_main = new_type_method(new_id_from_chars(METHODNAME, strlen(METHODNAME)),
                              NRARGS, NRES);
  /* An entity representing the method.  Owner of the entity is the global class
     type mentioned above. */
  ent = new_entity ((type *)owner,
                    new_id_from_chars (METHODNAME, strlen(METHODNAME)),
                    (type *)proc_main);

  /** Build code for the procedure. **/

  /* Generates the basic graph for the method represented by entity ent, that
   * is, generates start and end blocks and nodes and a first, initial block.
   * The constructor needs to know the number of local variables (including
   * the arguments) in the method.
   */
#define NUM_OF_LOCAL_VARS 0
  irg = new_ir_graph (ent, NUM_OF_LOCAL_VARS);

  /* The constructor new_ir_graph() generated a region to place nodes in.
   * This region is accessible via the attribut current_block of irg and
   * it is not matured.
   * Generate the return node into this region. The Return node is needed to
   * return at least the memory. */
    x = new_Return (get_store(), 0, NULL);
  /* Now we generated all instructions for this block and all its predecessor
   * blocks so we can mature it.  (There are not too much.) */
  mature_immBlock (get_irg_current_block(irg));

  /* This adds the in edge of the end block which originates at the return statement.
   * The return node passes controlflow to the end block.  */
  add_immBlock_pred (get_irg_end_block(irg), x);
  /* Now we can mature the end block as all it's predecessors are known. */
  mature_immBlock (get_irg_end_block(irg));

  /* Verify the graph.  Finds some very bad errors in the graph. */
  irg_vrfy(irg);
  irg_finalize_cons (irg);

  printf("Done building the graph.  Dumping it.\n");
  dump_ir_block_graph (irg, 0);

  printf("use xvcg to view this graph:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  return (0);
}
