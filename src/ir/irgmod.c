/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Support for ir graph modification.
 * @author   Martin Trapp, Christian Schaefer, Goetz Lindenmaier
 */
#include "irgmod.h"

#include "array.h"
#include "ircons.h"
#include "iredges_t.h"
#include "irflag_t.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irhooks.h"
#include "irnode_t.h"
#include "irtools.h"
#include "panic.h"

void turn_into_tuple(ir_node *const node, int const arity,
                     ir_node *const *const in)
{
	assert(get_irn_mode(node) == mode_T);

	hook_replace(node, node);

	set_irn_in(node, arity, in);
	set_irn_op(node, op_Tuple);

	/* update irg flags */
	clear_irg_properties(get_irn_irg(node), IR_GRAPH_PROPERTY_NO_TUPLES);
}

void exchange(ir_node *old, ir_node *nw)
{
	assert(old != NULL && nw != NULL);
	assert(old != nw);

	ir_graph *irg = get_irn_irg(old);
	assert(irg == get_irn_irg(nw));
#ifndef NDEBUG
	/* When replacing a PhiM node, it must not be held by a keep-alive edge.
	 * => Keep-alive edges are not normal users and should not move along when
	 * exchanging. */
	if (is_Phi(old) && get_Phi_loop(old) && !(is_Phi(nw) && get_Phi_loop(nw))
	    && !is_Bad(nw)) {
		ir_node *end = get_irg_end(irg);
		foreach_irn_in(end, i, kept) {
			assert(kept != old);
		}
	}
#endif

	hook_replace(old, nw);

	/* If new outs are on, we can skip the id node creation and reroute
	 * the edges from the old node to the new directly. */
	if (edges_activated(irg)) {
		edges_reroute(old, nw);
		edges_node_deleted(old);
		/* noone is allowed to reference this node anymore */
		set_irn_op(old, op_Deleted);
	} else {
		/* Else, do it the old-fashioned way. */
		ir_node *block = old->in[0];
		if (block == NULL) {
			block = get_block(nw);
			if (block == NULL) {
				panic("cannot find legal block for id");
			}
		}

		if (is_irn_dynamic(old))
			DEL_ARR_F(old->in);

		old->op    = op_Id;
		old->in    = NEW_ARR_D(ir_node*, get_irg_obstack(irg), 2);
		old->in[0] = block;
		old->in[1] = nw;
	}

	/* update irg flags */
	clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUTS
	                        | IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO);
}

static void collect_new_start_block_node_(ir_node *node)
{
	/* special handling for the Start node: it needs to be at the beginning
	 * of the list of Projs, and in the start_block node list. Do not put it
	 * into the start block node list. */
	if (is_Start(node))
		return;
	/* misuse link field of End node to collect all start-block nodes */
	ir_node *end = get_irg_end(get_irn_irg(node));
	set_irn_link(node, get_irn_link(end));
	set_irn_link(end, node);
}

void collect_new_start_block_node(ir_node *node)
{
	assert(is_irn_start_block_placed(node));
	/* alreayd in list? */
	if (get_irn_link(node) != NULL)
		return;
	collect_new_start_block_node_(node);
}

void collect_new_phi_node(ir_node *node)
{
	ir_node *block = get_nodes_block(node);
	add_Block_phi(block, node);
}

/**
 * Walker: links all Phi nodes to their Blocks lists,
 *         all Proj nodes to their predecessors.
 */
static void collect_nodes(ir_node *node, void *env)
{
	(void)env;

	if (is_Phi(node)) {
		collect_new_phi_node(node);
	} else if (is_Proj(node)) {
		ir_node *pred = node;
		do {
			pred = get_Proj_pred(pred);
		} while (is_Proj(pred));

		assert(!is_irn_start_block_placed(pred) || is_Start(pred));
		set_irn_link(node, get_irn_link(pred));
		set_irn_link(pred, node);
	} else if (is_irn_start_block_placed(node)) {
		collect_new_start_block_node_(node);
	}
}

static void init_links(ir_node *node, void *data)
{
	(void)data;
	if (is_End(node))
		return;
	firm_clear_node_and_phi_links(node, data);
}

void collect_phiprojs_and_start_block_nodes(ir_graph *irg)
{
	assert((ir_resources_reserved(irg) & (IR_RESOURCE_IRN_LINK|IR_RESOURCE_PHI_LIST)) ==
		(IR_RESOURCE_IRN_LINK|IR_RESOURCE_PHI_LIST));
	ir_node *end = get_irg_end(irg);
	set_irn_link(end, end);
	irg_walk_anchors(irg, init_links, collect_nodes, NULL);
}

static void move_node(ir_node *node, ir_node *to_bl)
{
	set_nodes_block(node, to_bl);
	/* if no mode_T node, do not move Projs. Note that BadT shouldn't have
	 * any Projs as well and is part of the start_block list and therefore
	 * doesn't have a valid Proj list */
	if (get_irn_mode(node) != mode_T || is_Bad(node))
		return;

	for (ir_node *proj = (ir_node*)get_irn_link(node);
	     proj != NULL; proj = (ir_node*)get_irn_link(proj)) {
		set_nodes_block(proj, to_bl);
	}
}

/**
 * Moves node and all predecessors of node from from_bl to to_bl.
 * Does not move predecessors of Phi nodes (or block nodes).
 */
static void move(ir_node *node, ir_node *from_bl, ir_node *to_bl)
{
	move_node(node, to_bl);

	if (is_Phi(node))
		return;

	/* recursion ... */
	foreach_irn_in(node, i, pred) {
		if (get_nodes_block(pred) == from_bl)
			move(pred, from_bl, to_bl);
	}
}

static void move_node_edges(ir_node *node, ir_node *to_bl)
{
	set_nodes_block(node, to_bl);
	if (get_irn_mode(node) != mode_T)
		return;

	foreach_out_edge(node, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		if (!is_Proj(proj))
			continue;
		move_node_edges(proj, to_bl);
	}
}

/**
 * Moves node and all predecessors of node from from_bl to to_bl.
 * Does not move predecessors of Phi nodes (or block nodes).
 */
static void move_edges(ir_node *node, ir_node *from_bl, ir_node *to_bl)
{
	move_node_edges(node, to_bl);

	/* We must not move predecessors of Phi nodes, even if they are in
	 * from_bl. (because these are values from an earlier loop iteration
	 * which are not predecessors of node here) */
	if (is_Phi(node))
		return;

	/* recursion ... */
	foreach_irn_in(node, i, pred) {
		if (get_nodes_block(pred) == from_bl)
			move_edges(pred, from_bl, to_bl);
	}
}

static void update_startblock(ir_node *old_block, ir_node *new_block)
{
	ir_graph *irg = get_irn_irg(old_block);
	set_irg_start_block(irg, new_block);
	/* move constants around */
	ir_node *end = get_irg_end(irg);
	for (ir_node *cnst = get_irn_link(end); cnst != end;
	     cnst = (ir_node*)get_irn_link(cnst)) {
		move_node(cnst, new_block);
	}
	ir_node *start = get_irg_start(irg);
	move_node(start, new_block);
}

void part_block(ir_node *node)
{
	/* Turn off optimizations so that blocks are not merged again. */
	int rem_opt = get_optimize();
	set_optimize(0);

	/* Transform the control flow */
	ir_node  *old_block = get_nodes_block(node);
	ir_graph *irg       = get_irn_irg(node);
	ir_node  *new_block = new_r_Block(irg, get_Block_n_cfgpreds(old_block),
	                                  get_Block_cfgpred_arr(old_block));

	/* create a jump from new_block to old_block, which is now the lower one */
	ir_node *jmp = new_r_Jmp(new_block);
	set_irn_in(old_block, 1, &jmp);

	/* move node and its predecessors to new_block */
	move(node, old_block, new_block);

	/* move Phi nodes to new_block */
	ir_node *phi = get_Block_phis(old_block);
	set_Block_phis(new_block, phi);
	set_Block_phis(old_block, NULL);
	while (phi) {
		set_nodes_block(phi, new_block);
		phi = get_Phi_next(phi);
	}

	if (old_block == get_irg_start_block(irg))
		update_startblock(old_block, new_block);

	set_optimize(rem_opt);
}

ir_node *part_block_edges(ir_node *node)
{
	ir_node  *old_block  = get_nodes_block(node);
	int       n_cfgpreds = get_Block_n_cfgpreds(old_block);
	ir_node **cfgpreds   = get_Block_cfgpred_arr(old_block);
	ir_graph *irg        = get_irn_irg(node);
	ir_node  *new_block  = new_r_Block(irg, n_cfgpreds, cfgpreds);

	/* old_block has no predecessors anymore for now */
	set_irn_in(old_block, 0, NULL);

	/* move node and its predecessors to new_block */
	move_edges(node, old_block, new_block);

	/* move Phi nodes and constants to new_block */
	foreach_out_edge_safe(old_block, edge) {
		ir_node *blnode = get_edge_src_irn(edge);
		ir_node *skip   = skip_Proj(skip_Proj(blnode));
		if (is_Phi(skip) || is_irn_start_block_placed(skip))
			set_nodes_block(blnode, new_block);
	}

	if (old_block == get_irg_start_block(irg))
		set_irg_start_block(irg, new_block);

	return old_block;
}

void kill_node(ir_node *node)
{
	hook_replace(node, NULL);

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
	case iro_Address:
		return new_rd_Address(dbg, irg, get_Address_entity(n));
	case iro_Align:
		return new_rd_Align(dbg, irg, mode, get_Align_type(n));
	case iro_Const:
		return new_rd_Const(dbg, irg, get_Const_tarval(n));
	case iro_Offset:
		return new_rd_Offset(dbg, irg, mode, get_Offset_entity(n));
	case iro_Size:
		return new_rd_Size(dbg, irg, mode, get_Size_type(n));
	case iro_Add:
		return new_rd_Add(dbg, block,
		                  duplicate_subgraph(dbg, get_Add_left(n), block),
		                  duplicate_subgraph(dbg, get_Add_right(n), block));
	case iro_Sub:
		return new_rd_Sub(dbg, block,
		                  duplicate_subgraph(dbg, get_Sub_left(n), block),
		                  duplicate_subgraph(dbg, get_Sub_right(n), block));
	case iro_Mul:
		return new_rd_Mul(dbg, block,
		                  duplicate_subgraph(dbg, get_Mul_left(n), block),
		                  duplicate_subgraph(dbg, get_Mul_right(n), block));
	case iro_And:
		return new_rd_And(dbg, block,
		                  duplicate_subgraph(dbg, get_And_left(n), block),
		                  duplicate_subgraph(dbg, get_And_right(n), block));
	case iro_Or:
		return new_rd_Or(dbg, block,
		                 duplicate_subgraph(dbg, get_Or_left(n), block),
		                 duplicate_subgraph(dbg, get_Or_right(n), block));
	case iro_Eor:
		return new_rd_Eor(dbg, block,
		                  duplicate_subgraph(dbg, get_Eor_left(n), block),
		                  duplicate_subgraph(dbg, get_Eor_right(n), block));
	case iro_Conv:
		return new_rd_Conv(dbg, block,
		                   duplicate_subgraph(dbg, get_Conv_op(n), block),
		                   mode);
	case iro_Minus:
		return new_rd_Minus(dbg, block,
		                    duplicate_subgraph(dbg, get_Minus_op(n), block));
	case iro_Not:
		return new_rd_Not(dbg, block,
		                  duplicate_subgraph(dbg, get_Not_op(n), block));
	case iro_NoMem:
		return get_irg_no_mem(irg);
	case iro_Member: {
		ir_node   *ptr    = duplicate_subgraph(dbg, get_Member_ptr(n), block);
		ir_entity *entity = get_Member_entity(n);
		return new_rd_Member(dbg, block, ptr, entity);
	}
	case iro_Sel: {
		ir_node *ptr   = duplicate_subgraph(dbg, get_Sel_ptr(n), block);
		ir_node *index = duplicate_subgraph(dbg, get_Sel_index(n), block);
		return new_rd_Sel(dbg, block, ptr, index, get_Sel_type(n));
	}
	case iro_Unknown:
		return new_r_Unknown(irg, mode);
	default:
		break;
	}
	panic("opcode invalid or not implemented %+F", n);
}
