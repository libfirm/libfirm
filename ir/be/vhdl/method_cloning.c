/*
 * This file is part of libFirm.
 * Copyright (C) 2019 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Graph cloning.
 * @author  Johannes Bucher, Daniel Biester
 */

#include "method_cloning.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irnode_t.h"

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
static void copy_irn_to_irg(ir_node *n, ir_graph *irg)
{
	/* do not copy standard nodes */
	ir_node *nn = NULL;
	switch (get_irn_opcode(n)) {
		case iro_NoMem:
			nn = get_irg_no_mem(irg);
			break;

		case iro_Block: {
			ir_graph *old_irg = get_irn_irg(n);
			if (n == get_irg_start_block(old_irg))
				nn = get_irg_start_block(irg);
			else if (n == get_irg_end_block(old_irg))
				nn = get_irg_end_block(irg);
			break;
		}

		case iro_Member: {
			ir_graph *const old_irg = get_irn_irg(n);
			ir_node  *const old_ptr = get_Member_ptr(n);
			if (old_ptr == get_irg_frame(old_irg)) {
				dbg_info  *const dbgi  = get_irn_dbg_info(n);
				ir_node   *const block = get_irg_start_block(irg);
				ir_entity *const ent   = get_entity_link(get_Member_entity(n));
				nn = new_rd_Member(dbgi, block, old_ptr, ent);
			}
			break;
		}

		case iro_Start:
			nn = get_irg_start(irg);
			break;

		case iro_End:
			nn = get_irg_end(irg);
			break;

		case iro_Proj: {
			ir_graph *old_irg = get_irn_irg(n);
			if (n == get_irg_frame(old_irg))
				nn = get_irg_frame(irg);
			else if (n == get_irg_initial_mem(old_irg))
				nn = get_irg_initial_mem(irg);
			else if (n == get_irg_args(old_irg))
				nn = get_irg_args(irg);
			break;
		}
	}

	if (nn) {
		set_irn_link(n, nn);
		return;
	}

	nn = new_ir_node(get_irn_dbg_info(n),
	                 irg,
	                 NULL,            /* no block yet, will be set later */
	                 get_irn_op(n),
	                 get_irn_mode(n),
	                 get_irn_arity(n),
	                 get_irn_in(n));


	/* Copy the attributes.  These might point to additional data.  If this
	   was allocated on the old obstack the pointers now are dangling.  This
	   frees e.g. the memory of the graph_arr allocated in new_immBlock. */
	copy_node_attr(irg, n, nn);
	set_irn_link(n, nn);
}

static inline ir_node *get_irn_copy(ir_node *const irn)
{
	return (ir_node*)get_irn_link(irn);
}

/**
 * Pre-Walker: Copies blocks and nodes from the original method graph
 * to the cloned graph. Fixes the argument projection numbers for
 * all arguments behind the removed one.
 *
 * @param irn  A node from the original method graph.
 * @param env  The clone graph.
 */
static void copy_nodes(ir_node *irn, void *env)
{
	ir_graph *const clone_irg = (ir_graph*)env;

	copy_irn_to_irg(irn, clone_irg);
}

/**
 * Post-walker: Set the predecessors of the copied nodes.
 * The copied nodes are set as link of their original nodes. The links of
 * "irn" predecessors are the predecessors of copied node.
 */
static void set_preds(ir_node *irn, void *env)
{
	ir_graph *const clone_irg = (ir_graph*)env;

	/* Arg is the method argument, that we have replaced by a constant.*/
	ir_node *const arg = (ir_node*)get_irg_link(clone_irg);
	if (arg == irn)
		return;

	ir_node  *const irn_copy = get_irn_copy(irn);

	if (is_Block(irn)) {
		ir_graph *const irg       = get_irn_irg(irn);
		ir_node  *const end_block = get_irg_end_block(irg);
		for (int i = get_Block_n_cfgpreds(irn); i-- > 0;) {
			ir_node *const pred = get_Block_cfgpred(irn, i);
			/* "End" block must be handled extra, because it is not matured. */
			if (end_block == irn)
				add_immBlock_pred(get_irg_end_block(clone_irg), get_irn_copy(pred));
			else
				set_Block_cfgpred(irn_copy, i, get_irn_copy(pred));
		}
	} else {
		/* First we set the block our copy if it is not a block.*/
		set_nodes_block(irn_copy, get_irn_copy(get_nodes_block(irn)));
		if (is_End(irn)) {
			/* Handle the keep-alives. This must be done separately, because
			 * the End node was NOT copied */
			for (int i = 0, n = get_End_n_keepalives(irn); i < n; ++i)
				add_End_keepalive(irn_copy, get_irn_copy(get_End_keepalive(irn, i)));
		} else {
			foreach_irn_in_r(irn, i, pred) {
							set_irn_n(irn_copy, i, get_irn_copy(pred));
						}
		}
	}
}


static void clone_frame(ir_graph *const src_irg, ir_graph *const dst_irg)
{
	ir_type *const src_frame = get_irg_frame_type(src_irg);
	ir_type *const dst_frame = get_irg_frame_type(dst_irg);
	for (size_t i = 0, n = get_compound_n_members(src_frame); i != n; ++i) {
		ir_entity *const src_ent = get_compound_member(src_frame, i);
		ident     *const name    = get_entity_name(src_ent);
		ir_entity *const dst_ent = clone_entity(src_ent, name, dst_frame);
		set_entity_link(src_ent, dst_ent);
	}
}

/**
 * Create a new graph for the clone of the method,
 * that we want to clone.
 *
 * @param new The entity of the method that we clone
 */
static void create_clone_proc_irg(ir_entity *new)
{
	ir_graph *const method_irg = get_entity_linktime_irg(new);
	ir_reserve_resources(method_irg, IR_RESOURCE_IRN_LINK);

	/* We create the skeleton of the clone irg.*/
	ir_graph *const clone_irg  = new_ir_graph(new, 0);
	clone_frame(method_irg, clone_irg);

	/* We copy the blocks and nodes, that must be in
	the clone graph and set their predecessors. */
	irg_walk_graph(method_irg, copy_nodes, set_preds, clone_irg);

	/* The "cloned" graph must be matured. */
	irg_finalize_cons(clone_irg);
	ir_free_resources(method_irg, IR_RESOURCE_IRN_LINK);
}

/**
 * Make a clone of a method.
 *
 * @param irg ir_graph to clone
 */
ir_entity *clone_method(ir_graph *irg)
{
	/* We get a new ident for our clone method.*/
	ir_entity *ent = get_irg_entity(irg);
	ident     *const clone_ident = id_unique(get_entity_ident(ent));
	/* We get our entity for the clone method. */
	ir_type   *const owner       = get_entity_owner(ent);
	ir_entity *const new_entity  = clone_entity(ent, clone_ident, owner);

	/* a cloned entity is always local */
	set_entity_visibility(new_entity, ir_visibility_local);

	/* We need now a new ir_graph for our clone method. */
	create_clone_proc_irg(new_entity);
	//TODO check properties of new graph
	return new_entity;

}
