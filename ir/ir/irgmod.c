/*
 * Project:     libFIRM
 * File name:   ir/ir/irgmod.c
 * Purpose:     Support for ir graph modification.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

# include "irvrfy.h"
# include "irflag_t.h"
# include "irgwalk.h"
# include "irnode_t.h"
# include "irgraph_t.h"
# include "irgmod.h"
# include "array.h"
# include "ircons.h"
# include "irhooks.h"
# include "iredges_t.h"

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
    ir_node *block = get_nodes_block(node);
    node->in = NEW_ARR_D (ir_node *, current_ir_graph->obst, arity+1);
		edges_invalidate(node, current_ir_graph);
    set_nodes_block(node, block);
  }
}

/* Insert irnode `new' in place of irnode `old'
   Since `new' may be bigger than `old' replace `old'
   by an op_Id which is smaller than everything */
void
exchange (ir_node *old, ir_node *nw)
{
	/*
	 * If new outs are on, we can skip the id node creation and reroute
	 * the edges from the old node to the new directly.
	 */
	if (edges_activated(current_ir_graph)) {
		edges_reroute(old, nw, current_ir_graph);
	}
  else {
    /* Else, do it the old-fashioned way. */

		ir_graph *irg = get_irn_irg (old);
		ir_node *block;

		assert(old != nw);
		assert (irg);
		assert(get_irn_op(old)->opar != oparity_dynamic);

		hook_turn_into_id(old);

		block = old->in[0];
		if (!block) {
			block = is_Block(nw) ? nw : get_nodes_block(nw);

			if (!block) {
				DDMN(old);
				DDMN(nw);
				assert(0 && "cannot find legal block for id");
			}
		}

		old->op = op_Id;
		old->in = NEW_ARR_D (ir_node *, irg->obst, 2);
		old->in[0] = block;
		old->in[1] = nw;
	}
}

/*--------------------------------------------------------------------*/
/*  Functionality for collect_phis                                    */
/*--------------------------------------------------------------------*/

static void
clear_link (ir_node *n, void *env) {
  set_irn_link(n, NULL);
}

static void
collect (ir_node *n, void *env) {
  ir_node *pred;
  if (get_irn_op(n) == op_Phi) {
    set_irn_link(n, get_irn_link(get_nodes_block(n)));
    set_irn_link(get_nodes_block(n), n);
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


/*--------------------------------------------------------------------*/
/*  Functionality for part_block                                      */
/*--------------------------------------------------------------------*/

/**
 * Moves node and all predecessors of node from from_bl to to_bl.
 * Does not move predecessors of Phi nodes (or block nodes).
 */
static void move (ir_node *node, ir_node *from_bl, ir_node *to_bl) {
  int i, arity;
  ir_node *proj, *pred;

  /* move this node */
  set_nodes_block(node, to_bl);

  /* move its projs */
  if (get_irn_mode(node) == mode_T) {
    proj = get_irn_link(node);
    while (proj) {
      if (get_nodes_block(proj) == from_bl)
	set_nodes_block(proj, to_bl);
      proj = get_irn_link(proj);
    }
  }

  /* recursion ... */
  if (get_irn_op(node) == op_Phi) return;

  arity = get_irn_arity(node);
  for (i = 0; i < arity; i++) {
    pred = get_irn_n(node, i);
    if (get_nodes_block(pred) == from_bl)
      move(pred, from_bl, to_bl);
  }
}

void part_block(ir_node *node) {
  ir_node *new_block;
  ir_node *old_block;
  ir_node *phi;

  /* Turn off optimizations so that blocks are not merged again. */
  int rem_opt = get_opt_optimize();
  set_optimize(0);

  /* Transform the control flow */
  old_block = get_nodes_block(node);
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
    if(get_nodes_block(phi) == old_block);   /* @@@ inlinening chokes on phis that don't
                           obey this condition.  How do they get into
                           the list??? Example: InterfaceIII */
      set_nodes_block(phi, new_block);
    phi = get_irn_link(phi);
  }

  set_optimize(rem_opt);
}
