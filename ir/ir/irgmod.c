/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief    Support for ir graph modification.
 * @author   Martin Trapp, Christian Schaefer, Goetz Lindenmaier
 * @version  $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "irvrfy.h"
#include "irflag_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irgmod.h"
#include "array.h"
#include "ircons.h"
#include "irhooks.h"
#include "iredges_t.h"
#include "irtools.h"

/**
 * Turns a node into a "useless" Tuple.  The Tuple just forms a tuple
 * from several inputs.
 * This is useful if a node returning a tuple is removed, but the Projs
 * extracting values from the tuple are not available.
 */
void turn_into_tuple(ir_node *node, int arity) {
	assert(node);
	set_irn_op(node, op_Tuple);
	if (get_irn_arity(node) == arity) {
		/* keep old array */
	} else {
		/* don't use get_nodes_block here, we allow turn_into_tuple for unpinned nodes */
		ir_node *block = get_irn_n(node, -1);
		/* Allocate new array, don't free old in_array, it's on the obstack. */
		edges_node_deleted(node, current_ir_graph);
		node->in = NEW_ARR_D(ir_node *, current_ir_graph->obst, arity+1);
		/* clear the new in array, else edge_notify tries to delete garbage */
		memset(node->in, 0, (arity+1) * sizeof(node->in[0]));
		set_irn_n(node, -1, block);
	}
}

/**
 * Insert irnode `new' in place of irnode `old'
 * Since `new' may be bigger than `old' replace `old'
 * by an op_Id which is smaller than everything.
 */
void exchange(ir_node *old, ir_node *nw) {
	ir_graph *irg;

	assert(old && nw);
	assert(old != nw && "Exchanging node with itself is not allowed");

	irg = get_irn_irg(old);
	assert(irg == get_irn_irg(nw) && "New node must be in same irg as old node");

	hook_replace(old, nw);

	/*
	 * If new outs are on, we can skip the id node creation and reroute
	 * the edges from the old node to the new directly.
	 */
	if (edges_activated(irg)) {
		/* copy all dependencies from old to new */
		add_irn_deps(nw, old);

		edges_reroute(old, nw, irg);
		edges_reroute_kind(old, nw, EDGE_KIND_DEP, irg);
		edges_node_deleted(old, irg);
		old->op = op_Bad;
	} else {
		/* Else, do it the old-fashioned way. */
		ir_node *block;

		assert(get_irn_op(old)->opar != oparity_dynamic);

		hook_turn_into_id(old);

		block = old->in[0];
		if (! block) {
			block = is_Block(nw) ? nw : get_nodes_block(nw);

			if (! block) {
				DDMN(old);
				DDMN(nw);
				assert(0 && "cannot find legal block for id");
			}
		}

		old->op    = op_Id;
		old->in    = NEW_ARR_D (ir_node *, irg->obst, 2);
		old->in[0] = block;
		old->in[1] = nw;
	}
}

/*--------------------------------------------------------------------*/
/*  Functionality for collect_phis                                    */
/*--------------------------------------------------------------------*/

/**
 * Walker: links all Phi nodes to their Blocks and
 *         all Proj nodes to there predecessors
 */
static void collect(ir_node *n, void *env) {
	ir_node *pred;

	if (is_Phi(n)) {
		set_irn_link(n, get_irn_link(get_nodes_block(n)));
		set_irn_link(get_nodes_block(n), n);
	} else if (is_Proj(n)) {
		pred = n;
		do {
			pred = get_Proj_pred(pred);
		} while (is_Proj(pred));

		set_irn_link(n, get_irn_link(pred));
		set_irn_link(pred, n);
	}
}

void collect_phiprojs(ir_graph *irg) {
	irg_walk_graph(irg, firm_clear_link, collect, NULL);
}


/*--------------------------------------------------------------------*/
/*  Functionality for part_block                                      */
/*--------------------------------------------------------------------*/

/**
 * Moves node and all predecessors of node from from_bl to to_bl.
 * Does not move predecessors of Phi nodes (or block nodes).
 */
static void move(ir_node *node, ir_node *from_bl, ir_node *to_bl) {
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
		ir_node *jmp = new_Jmp();
		set_irn_in(old_block, 1, &jmp);
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
