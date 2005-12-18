/*
 * Project:     libFIRM
 * File name:   ir/ir/irtools.c
 * Purpose:     Some often needed tool-functions
 * Author:      Michael Beck
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <stdlib.h>
#include "irnode_t.h"
#include "irbackedge_t.h"
#include "irtools.h"

/* the famous clear_link implementation. */
void firm_clear_link(ir_node *n, void *env) {
  set_irn_link(n, NULL);
}

/**
 * Copies a node to a new irg. The Ins of the new node point to
 * the predecessors on the old irg.  n->link points to the new node.
 *
 * @param n    The node to be copied
 * @param irg  the new irg
 *
 * Does NOT copy standard nodes like Start, End etc that are fixed
 * in an irg. Instead, the corresponding nodes of the new irg are returned.
 * Note further, that the new nodes have no block.
 */
void
copy_irn_to_irg(ir_node *n, ir_graph *irg)
{
  ir_op *op = get_irn_op(n);
  ir_graph *old_irg;
  ir_node *nn = NULL;

  /* do not copy standard nodes */
  if (op == op_Bad)
    nn = get_irg_bad(irg);
  else if (op == op_NoMem)
    n = get_irg_no_mem(irg);
  else if (op == op_Block) {
    old_irg = get_irn_irg(n);

    if (n == get_irg_start_block(old_irg))
      nn = get_irg_start_block(irg);
    else if (n == get_irg_end_block(old_irg))
      nn = get_irg_end_block(irg);
  }
  else if (op == op_Start)
    nn = get_irg_start(irg);
  else if (op == op_End)
    nn = get_irg_end(irg);
  else if (op == op_Proj) {
    old_irg = get_irn_irg(n);

    if (n == get_irg_cstore(old_irg))
      nn = get_irg_cstore(irg);
    else if (n == get_irg_frame(old_irg))
      nn = get_irg_frame(irg);
    else if (n == get_irg_globals(old_irg))
      nn = get_irg_globals(irg);
    else if (n == get_irg_initial_mem(old_irg))
      nn = get_irg_initial_mem(irg);
    else if (n == get_irg_args(old_irg))
      nn = get_irg_args(irg);
  }

  if (nn) {
    set_irn_link(n, nn);
    return;
  }

  nn = new_ir_node(get_irn_dbg_info(n),
         irg,
         NULL,            /* no block yet, will be set later */
         op,
         get_irn_mode(n),
         get_irn_arity(n),
         get_irn_in(n));


  /* Copy the attributes.  These might point to additional data.  If this
     was allocated on the old obstack the pointers now are dangling.  This
     frees e.g. the memory of the graph_arr allocated in new_immBlock. */
  copy_node_attr(n, nn);
  new_backedge_info(nn);
  set_irn_link(n, nn);

  /* fix the irg for blocks */
  if (is_Block(nn))
    nn->attr.block.irg = irg;
}
