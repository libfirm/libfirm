/*
 * Project:     libFIRM
 * File name:   ir/ir/irgmod.h
 * Purpose:     Support for ir graph modification.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include "irvrfy.h"
# include "irflag.h"
# include "irgwalk.h"
# include "irnode_t.h"
# include "irgraph_t.h"
# include "irgmod.h"
# include "array.h"
# include "ircons.h"

/* Turns a node into a "useless" Tuple.  The Tuple just forms a tuple
   from several inputs.
   This is useful if a node returning a tuple is removed, but the Projs
   extracting values from the tuple are not available. */
void
turn_into_tuple (ir_node *node, int arity)
{
  assert(node);
  set_irn_op(node, op_Tuple);
  if (get_irn_arity(node) == arity) {
    /* keep old array */
  } else {
    /* Allocate new array, don't free old in_array, it's on the obstack. */
    ir_node *block = get_nodes_Block(node);
    node->in = NEW_ARR_D (ir_node *, current_ir_graph->obst, arity+1);
    set_nodes_Block(node, block);
  }
}

/* Insert irnode `new' in place of irnode `old'
   Since `new' may be bigger than `old' replace `old'
   by an op_Id which is smaller than everything */
INLINE void
exchange (ir_node *old, ir_node *nw)
{
  ir_node *block = old->in[0];

  old->op = op_Id;
  old->in = NEW_ARR_D (ir_node *, current_ir_graph->obst, 2);
  old->in[0] = block;
  old->in[1] = nw;
}

/**********************************************************************/
/*  Functionality for collect_phis                                     */
/**********************************************************************/

static void
clear_link (ir_node *n, void *env) {
  set_irn_link(n, NULL);
}

static void
collect (ir_node *n, void *env) {
  ir_node *pred;
  if (get_irn_op(n) == op_Phi) {
    set_irn_link(n, get_irn_link(get_nodes_Block(n)));
    set_irn_link(get_nodes_Block(n), n);
  }
  if (get_irn_op(n) == op_Proj) {
    pred = n;
    while (get_irn_op(pred) == op_Proj)
      pred = get_Proj_pred(pred);
    set_irn_link(n, get_irn_link(pred));
    set_irn_link(pred, n);
  }
}

void collect_phiprojs(ir_graph *irg) {
  ir_graph *rem;

  /* Remember external state of current_ir_graph. */
  rem = current_ir_graph;
  current_ir_graph = irg;

  irg_walk(get_irg_end(current_ir_graph), clear_link, collect, NULL);

  current_ir_graph = rem;
}


/**********************************************************************/
/*  Funcionality for part_block                                       */
/**********************************************************************/

/* Moves node and all predecessors of node from from_bl to to_bl.
   Does not move predecessors of Phi nodes (or block nodes). */

static void move (ir_node *node, ir_node *from_bl, ir_node *to_bl) {
  int i;
  ir_node *proj, *pred;

  /* move this node */
  set_nodes_Block(node, to_bl);

  /* move its projs */
  if (get_irn_mode(node) == mode_T) {
    proj = get_irn_link(node);
    while (proj) {
      if (get_nodes_Block(proj) == from_bl)
	set_nodes_Block(proj, to_bl);
      proj = get_irn_link(proj);
    }
  }

  /* recursion ... */
  if (get_irn_op(node) == op_Phi) return;

  for (i = 0; i < get_irn_arity(node); i++) {
    pred = get_irn_n(node, i);
    if (get_nodes_Block(pred) == from_bl)
      move(pred, from_bl, to_bl);
  }
}

void part_block(ir_node *node) {
  ir_node *new_block;
  ir_node *old_block;
  ir_node *phi;

  /* Turn off optimizations so that blocks are not merged again. */
  int rem_opt = get_optimize();
  set_optimize(0);

  /* Transform the control flow */
  old_block = get_nodes_Block(node);
  new_block = new_Block(get_Block_n_cfgpreds(old_block),
			get_Block_cfgpred_arr(old_block));
  set_irg_current_block(current_ir_graph, new_block);
  {
    ir_node *in[1];
    in[0] = new_Jmp();
    set_irn_in(old_block, 1, in);
    irn_vrfy_irg(old_block, current_ir_graph);
  }

  /* move node and its predecessors to new_block */
  move(node, old_block, new_block);

  /* move Phi nodes to new_block */
  phi = get_irn_link(old_block);
  set_irn_link(new_block, phi);
  set_irn_link(old_block, NULL);
  while (phi) {
    if(get_nodes_Block(phi) == old_block);   /* @@@ inlinening chokes on phis that don't
					       obey this condition.  How do they get into
					       the list??? Example: InterfaceIII */
      set_nodes_Block(phi, new_block);
    phi = get_irn_link(phi);
  }

  set_optimize(rem_opt);
}
