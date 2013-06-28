/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Support for ir graph modification.
 * @author   Martin Trapp, Christian Schaefer, Goetz Lindenmaier
 */
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

void turn_into_tuple(ir_node *const node, int const arity, ir_node *const *const in)
{
	set_irn_in(node, arity, in);
	set_irn_op(node, op_Tuple);
}

void exchange(ir_node *old, ir_node *nw)
{
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

		edges_reroute(old, nw);
		edges_reroute_kind(old, nw, EDGE_KIND_DEP);
		edges_node_deleted(old);
		/* noone is allowed to reference this node anymore */
		set_irn_op(old, op_Deleted);
	} else {
		/* Else, do it the old-fashioned way. */
		ir_node *block;

		hook_turn_into_id(old);

		block = old->in[0];
		if (! block) {
			block = is_Block(nw) ? nw : get_nodes_block(nw);

			if (! block) {
				panic("cannot find legal block for id");
			}
		}

		if (get_irn_op(old)->opar == oparity_dynamic) {
			DEL_ARR_F(get_irn_in(old));
		}

		old->op    = op_Id;
		old->in    = NEW_ARR_D(ir_node*, get_irg_obstack(irg), 2);
		old->in[0] = block;
		old->in[1] = nw;
	}

	/* update irg flags */
	clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUTS
	                   | IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO);
}

/**
 * Walker: links all Phi nodes to their Blocks lists,
 *         all Proj nodes to there predecessors.
 */
static void collect_phiprojs_walker(ir_node *n, void *env)
{
	ir_node *pred;
	(void) env;

	if (is_Phi(n)) {
		ir_node *block = get_nodes_block(n);
		add_Block_phi(block, n);
	} else if (is_Proj(n)) {
		pred = n;
		do {
			pred = get_Proj_pred(pred);
		} while (is_Proj(pred));

		set_irn_link(n, get_irn_link(pred));
		set_irn_link(pred, n);
	}
}

void collect_phiprojs(ir_graph *irg)
{
	assert((ir_resources_reserved(irg) & (IR_RESOURCE_IRN_LINK|IR_RESOURCE_PHI_LIST)) ==
		(IR_RESOURCE_IRN_LINK|IR_RESOURCE_PHI_LIST));
	irg_walk_graph(irg, firm_clear_node_and_phi_links, collect_phiprojs_walker, NULL);
}

/**
 * Moves node and all predecessors of node from from_bl to to_bl.
 * Does not move predecessors of Phi nodes (or block nodes).
 */
static void move(ir_node *node, ir_node *from_bl, ir_node *to_bl)
{
	int i, arity;

	/* move this node */
	set_nodes_block(node, to_bl);

	/* move its Projs */
	if (get_irn_mode(node) == mode_T) {
		ir_node *proj = (ir_node*)get_irn_link(node);
		while (proj) {
			if (get_nodes_block(proj) == from_bl)
				set_nodes_block(proj, to_bl);
			proj = (ir_node*)get_irn_link(proj);
		}
	}

	if (is_Phi(node))
		return;

	/* recursion ... */
	arity = get_irn_arity(node);
	for (i = 0; i < arity; i++) {
		ir_node *pred = get_irn_n(node, i);
		if (get_nodes_block(pred) == from_bl)
			move(pred, from_bl, to_bl);
	}
}

static void move_projs(const ir_node *node, ir_node *to_bl)
{
	if (get_irn_mode(node) != mode_T)
		return;

	foreach_out_edge(node, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		if (!is_Proj(proj))
			continue;
		set_nodes_block(proj, to_bl);
		move_projs(proj, to_bl);
	}
}

/**
 * Moves node and all predecessors of node from from_bl to to_bl.
 * Does not move predecessors of Phi nodes (or block nodes).
 */
static void move_edges(ir_node *node, ir_node *from_bl, ir_node *to_bl)
{
	int i, arity;

	/* move this node */
	set_nodes_block(node, to_bl);

	/* move its Projs */
	move_projs(node, to_bl);

	/* We must not move predecessors of Phi nodes, even if they are in
	 * from_bl. (because these are values from an earlier loop iteration
	 * which are not predecessors of node here)
	 */
	if (is_Phi(node))
		return;

	/* recursion ... */
	arity = get_irn_arity(node);
	for (i = 0; i < arity; i++) {
		ir_node *pred = get_irn_n(node, i);
		if (get_nodes_block(pred) == from_bl)
			move_edges(pred, from_bl, to_bl);
	}
}

void part_block(ir_node *node)
{
	ir_graph *irg = get_irn_irg(node);
	ir_node  *new_block, *old_block;
	ir_node  *phi, *jmp;

	/* Turn off optimizations so that blocks are not merged again. */
	int rem_opt = get_opt_optimize();
	set_optimize(0);

	/* Transform the control flow */
	old_block = get_nodes_block(node);
	new_block = new_r_Block(irg, get_Block_n_cfgpreds(old_block),
	                        get_Block_cfgpred_arr(old_block));

	/* create a jump from new_block to old_block, which is now the lower one */
	jmp = new_r_Jmp(new_block);
	set_irn_in(old_block, 1, &jmp);

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

	set_optimize(rem_opt);
}

ir_node *part_block_edges(ir_node *node)
{
	ir_graph *irg       = get_irn_irg(node);
	ir_node  *old_block = get_nodes_block(node);
	ir_node  *new_block = new_r_Block(irg, get_Block_n_cfgpreds(old_block), get_Block_cfgpred_arr(old_block));

	/* old_block has no predecessors anymore for now */
	set_irn_in(old_block, 0, NULL);

	/* move node and its predecessors to new_block */
	move_edges(node, old_block, new_block);

	/* move Phi nodes to new_block */
	foreach_out_edge_safe(old_block, edge) {
		ir_node *phi = get_edge_src_irn(edge);
		if (!is_Phi(phi))
			continue;
		set_nodes_block(phi, new_block);
	}

	return old_block;
}

void kill_node(ir_node *node)
{
	ir_graph *irg = get_irn_irg(node);

	if (edges_activated(irg)) {
		edges_node_deleted(node);
	}
	/* noone is allowed to reference this node anymore */
	set_irn_op(node, op_Deleted);
}

ir_node *duplicate_subgraph(dbg_info *dbg, ir_node *n, ir_node *block)
{
	ir_graph *irg  = get_irn_irg(block);
	ir_mode  *mode = get_irn_mode(n);
	switch (get_irn_opcode(n)) {
	case iro_Const:
		return new_rd_Const(dbg, irg, get_Const_tarval(n));
	case iro_SymConst:
		return new_rd_SymConst(dbg, irg, mode,
		                       get_SymConst_symbol(n), get_SymConst_kind(n));
	case iro_Add:
		return new_rd_Add(dbg, block,
		                  duplicate_subgraph(dbg, get_Add_left(n), block),
		                  duplicate_subgraph(dbg, get_Add_right(n), block),
		                  mode);
	case iro_Sub:
		return new_rd_Sub(dbg, block,
		                  duplicate_subgraph(dbg, get_Sub_left(n), block),
		                  duplicate_subgraph(dbg, get_Sub_right(n), block),
		                  mode);
	case iro_Mul:
		return new_rd_Mul(dbg, block,
		                  duplicate_subgraph(dbg, get_Mul_left(n), block),
		                  duplicate_subgraph(dbg, get_Mul_right(n), block),
		                  mode);
	case iro_And:
		return new_rd_And(dbg, block,
		                  duplicate_subgraph(dbg, get_And_left(n), block),
		                  duplicate_subgraph(dbg, get_And_right(n), block),
		                  mode);
	case iro_Or:
		return new_rd_Or(dbg, block,
		                 duplicate_subgraph(dbg, get_Or_left(n), block),
		                 duplicate_subgraph(dbg, get_Or_right(n), block),
		                 mode);
	case iro_Eor:
		return new_rd_Eor(dbg, block,
		                  duplicate_subgraph(dbg, get_Eor_left(n), block),
		                  duplicate_subgraph(dbg, get_Eor_right(n), block),
		                  mode);
	case iro_Conv:
		return new_rd_Conv(dbg, block,
		                   duplicate_subgraph(dbg, get_Conv_op(n), block),
		                   mode);
	case iro_Minus:
		return new_rd_Minus(dbg, block,
		                    duplicate_subgraph(dbg, get_Minus_op(n), block),
		                    mode);
	case iro_Not:
		return new_rd_Not(dbg, block,
		                  duplicate_subgraph(dbg, get_Not_op(n), block), mode);
	case iro_Unknown:
		return new_r_Unknown(irg, mode);
	default:
		break;
	}
	panic("opcode invalid or not implemented %+F", n);
}
