/*
 * Project:     libFIRM
 * File name:   testprograms/
 * Purpose:
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

# include <stdio.h>
# include <string.h>

# include "irvrfy.h"
# include "firm.h"
# include "irdump.h"


/** This file constructs the ir to test a problem with code placement.
 *
 * Global cse and removal of dead code can produce nodes, that depend
 * on live nodes, but originally were placed in dead code.  The
 * dominator analyses can not compute dominator information for
 * them, or their block is the Bad node.
 *
 * We must place this nodes in the code, as they could be reached
 * through a live node.
 *
 * This program constructs a graph that can result from the program below
 * by dead node elimination and global cse.
 *
 *  class SomeClass {}
 *  long main() {
 *    long a = 0;
 *    int  b = sizeof(SomeClass);
 *
 *    if (a > 2)
 *      { a = (long) b; }
 *    else
 *      { a = (long) b; }
 *
 *    return a;
 *  }
 *
 *  The Conv node for the cast to long is constructed twice, in each
 *  if case once.  Global cse visits the Conv in the block that will
 *  turn dead first, i.e., it adds this node in the hash table.  The
 *  assignment in the alive block is replaced by this node.  In a next
 *  step the dead code is removed, only straight control flow remains.
 *  This results in a Conv node that is not placed in an existing node,
 *  but needed by the program.
 *
 *  Code placememnt (place early) tries to place the node in the Start
 *  block, which is illegal.  Actually, place late should move the block
 *  out of the Start block.
 */

#define PROGNAME "PLACE_WITH_DEAD"

int main(int argc, char **argv)
{
  type     *prim_t_int;
  ir_graph *irg;       /* this variable contains the irgraph */
  type     *owner;     /* the class in which this method is defined */
  type     *method;    /* the type of this method */
  entity   *ent;       /* represents this method as entity of owner */
  ir_node  *a, *b, *x;
  symconst_symbol sym;

  printf("\nCreating an IR graph: " PROGNAME "...\n");

  /* init library */
  init_firm (NULL);

  /* Make basic type information for primitive type int. */
  prim_t_int = new_type_primitive(new_id_from_chars ("int", 3), mode_Ls);

  /* Make the method type and entity */
  owner = get_glob_type();
  method = new_type_method (new_id_from_str(PROGNAME"_main_tp"), 0, 1);
  set_method_res_type(method, 0, prim_t_int);
  ent = new_entity (owner, new_id_from_str (PROGNAME"_main"), method);

  /* The ir graph */
#define NUM_OF_LOCAL_VARS 2
  irg = new_ir_graph (ent, NUM_OF_LOCAL_VARS);

  /* Generate the two constants. A SymConst can not be constant evaluated. */
  sym.type_p = new_type_class(new_id_from_str("SomeClass"));
  a = new_Const (mode_Is, new_tarval_from_long (0, mode_Is));
  b = new_SymConst (sym, symconst_size);

  /* Generate the Conv with Bad as block */
  a = new_Conv(b, mode_Ls);
  set_nodes_block(a, new_Bad());

  /* Generate the return. */
  x = new_Return (get_store(), 1, &a);

  /* Finish the graph. */
  mature_immBlock (get_irg_current_block(irg));
  add_immBlock_pred (get_irg_end_block(irg), x);
  mature_immBlock (get_irg_end_block(irg));
  finalize_cons (irg);
  irg_vrfy(irg);

  printf("Done building the graph.  Dumping it.\n");
  dump_ir_block_graph (irg, 0);
  dump_ir_graph (irg, 0);

  printf("Code placement ...\n");
  set_opt_global_cse(1);	/* need this option for code placement */
  place_code(irg);

  dump_ir_block_graph (irg, "-placed");
  dump_ir_graph (irg, "-placed");
  irg_vrfy(irg);

  printf("use xvcg to view this graph:\n");
  printf("/ben/goetz/bin/xvcg GRAPHNAME\n\n");

  return (0);
}
