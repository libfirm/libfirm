/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
#include "error.h"

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
		ir_graph *irg = get_irn_irg(node);
		ir_node *block = get_nodes_block(node);
		edges_node_deleted(node, irg);
		/* Allocate new array, don't free old in_array, it's on the obstack. */
		node->in = NEW_ARR_D(ir_node *, irg->obst, arity+1);
		/* clear the new in array, else edge_notify tries to delete garbage */
		memset(node->in, 0, (arity+1) * sizeof(node->in[0]));
		/* set the block back */
		set_nodes_block(node, block);
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
				panic("cannot find legal block for id");
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
 * Walker: links all Phi nodes to their Blocks lists,
 *         all Proj nodes to there predecessors and all
 *         partBlocks to there MacroBlock header.
 */
static void collect(ir_node *n, void *env) {
	ir_node *pred;
	(void) env;

	if (is_Phi(n)) {
		ir_node *block = get_nodes_block(n);
		set_Phi_next(n, get_Block_phis(block));
		set_Block_phis(block, n);
	} else if (is_Proj(n)) {
		pred = n;
		do {
			pred = get_Proj_pred(pred);
		} while (is_Proj(pred));

		set_irn_link(n, get_irn_link(pred));
		set_irn_link(pred, n);
	} else if (is_Block(n)) {
		ir_node *mbh = get_Block_MacroBlock(n);

		if (mbh != n) {
			set_irn_link(n, get_irn_link(mbh));
			set_irn_link(mbh, n);
		}
	}
}

/**
 * clear all links, including the Phi list of blocks and Phi nodes.
 */
static void clear_links(ir_node *n, void *env) {
	(void) env;

	set_irn_link(n, NULL);
	if (is_Block(n))
		set_Block_phis(n, NULL);
	else if (is_Phi(n))
		set_Phi_next(n, NULL);
}

void collect_phiprojs(ir_graph *irg) {
	irg_walk_graph(irg, clear_links, collect, NULL);
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

	/* move this node */
	set_nodes_block(node, to_bl);

	/* move its Projs */
	if (get_irn_mode(node) == mode_T) {
		ir_node *proj = get_irn_link(node);
		while (proj) {
			if (get_nodes_block(proj) == from_bl)
				set_nodes_block(proj, to_bl);
			proj = get_irn_link(proj);
		}
	}

	/* recursion ... */
	if (is_Phi(node))
		return;

	arity = get_irn_arity(node);
	for (i = 0; i < arity; i++) {
		ir_node *pred = get_irn_n(node, i);
		if (get_nodes_block(pred) == from_bl)
			move(pred, from_bl, to_bl);
	}
}

void part_block(ir_node *node) {
	ir_node *new_block;
	ir_node *old_block;
	ir_node *phi;
	ir_node *mbh;

	/* Turn off optimizations so that blocks are not merged again. */
	int rem_opt = get_opt_optimize();
	set_optimize(0);

	/* Transform the control flow */
	old_block = get_nodes_block(node);
	mbh       = get_Block_MacroBlock(old_block);
	new_block = new_Block(get_Block_n_cfgpreds(old_block),
	                      get_Block_cfgpred_arr(old_block));
	set_irn_n(new_block, -1, mbh);
	set_irg_current_block(current_ir_graph, new_block);
	{
		ir_node *jmp = new_Jmp();
		set_irn_in(old_block, 1, &jmp);
		irn_vrfy_irg(old_block, current_ir_graph);
	}

	/* move node and its predecessors to new_block */
	move(node, old_block, new_block);

	/* move Phi nodes to new_block */
	phi = get_Block_phis(old_block);
	set_Block_phis(new_block, phi);
	set_Block_phis(old_block, NULL);
	while (phi) {
		set_nodes_block(phi, new_block);
		phi = get_Phi_next(phi);
	}

	/* rewire partBlocks */
	if (mbh != old_block) {
		ir_node *next, *block = get_irn_link(mbh);

		set_irn_link(mbh, NULL);
		set_irn_link(old_block, NULL);

		/* note that we must splice the list of partBlock here */
		for (; block != NULL; block = next) {
			ir_node *curr = block;
			assert(is_Block(curr));

			next = get_irn_link(block);

			for (;;) {
				if (curr == old_block) {
					/* old_block dominates the block, so old_block will be
					   the new macro block header */
					set_irn_n(block, -1, old_block);
					set_irn_link(block, get_irn_link(old_block));
					set_irn_link(old_block, block);
					break;
				}
				if (curr == mbh) {
					/* leave it in the mbh */
					set_irn_link(block, get_irn_link(mbh));
					set_irn_link(mbh, block);
					break;
				}

				assert(get_Block_n_cfgpreds(curr) == 1);
				curr = get_Block_cfgpred_block(curr, 0);
			}
		}
	}

	set_optimize(rem_opt);
}

/* kill a node by setting its predecessors to Bad and finally exchange the node by Bad itself. */
void kill_node(ir_node *node) {
	ir_graph *irg = get_irn_irg(node);
	ir_node *bad = get_irg_bad(irg);
	int i;

	for (i = get_irn_arity(node) - 1; i >= -1; --i) {
		set_irn_n(node, i, bad);
	}
	exchange(node, bad);
}
