/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Various irnode constructors. Automatic construction of SSA
 *          representation.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Boris Boesler
 *          Michael Beck, Matthias Braun
 */
#include "ircons_t.h"

#include "array.h"
#include "irbackedge_t.h"
#include "iredges_t.h"
#include "irflag_t.h"
#include "irflag_t.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgraph_t.h"
#include "irhooks.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "irop_t.h"
#include "iropt_t.h"
#include "irprog_t.h"
#include "irverify.h"
#include "util.h"

/**
 * Language dependent variable initialization callback.
 */
static uninitialized_local_variable_func_t *default_initialize_local_variable = NULL;

ir_node *new_rd_Const_long(dbg_info *db, ir_graph *irg, ir_mode *mode,
                           long value)
{
	return new_rd_Const(db, irg, new_tarval_from_long(value, mode));
}

ir_node *new_r_Const_long(ir_graph *irg, ir_mode *mode, long value)
{
	return new_rd_Const_long(NULL, irg, mode, value);
}

/** Creates a Phi node with 0 predecessors. */
static inline ir_node *new_rd_Phi0(dbg_info *dbgi, ir_node *block,
                                   ir_mode *mode, int pos)
{
	ir_graph *irg = get_irn_irg(block);
	ir_node  *res = new_ir_node(dbgi, irg, block, op_Phi, mode, 0, NULL);
	res->attr.phi.u.pos = pos;
	verify_new_node(res);
	return res;
}

static ir_node *get_r_value_internal(ir_node *block, int pos, ir_mode *mode);

static void try_remove_unnecessary_phi(ir_node *phi)
{
	ir_node *phi_value = NULL;

	/* See if all inputs are either pointing to a single value or
	 * are self references. */
	bool have_self_loop = false;
	foreach_irn_in(phi, i, in) {
		if (in == phi) {
			have_self_loop = true;
			continue;
		}
		if (in == phi_value)
			continue;
		/** found a different value from the one we already found, can't remove
		 * the phi (yet) */
		if (phi_value != NULL)
			return;
		phi_value = in;
	}
	if (phi_value == NULL)
		return;

	/* Do not remove PhiM with self-loops as potentially endless loops are
	 * observable, see comment in equivalent_node_Phi() to learn more. */
	if (have_self_loop && get_irn_mode(phi) == mode_M)
		return;

	/* if we're here then all phi inputs have been either phi_value
	 * or self-references, we can replace the phi by phi_value. */
	exchange(phi, phi_value);

	/* recursively check phi_value, because it could be that we were the last
	 * phi-node in a loop-body. Then our argument is an unnecessary phi in
	 * the loop header which can be eliminated now */
	if (is_Phi(phi_value)) {
		try_remove_unnecessary_phi(phi_value);
	}
}

/**
 * Computes the predecessors for the real phi node, and then
 * allocates and returns this node.  The routine called to allocate the
 * node might optimize it away and return a real value.
 * This function must be called with an in-array of proper size.
 */
static ir_node *set_phi_arguments(ir_node *phi, int pos)
{
	ir_node  *block        = get_nodes_block(phi);
	ir_graph *irg          = get_irn_irg(block);
	int       arity        = get_irn_arity(block);
	ir_node **in           = ALLOCAN(ir_node*, arity);
	ir_mode  *mode         = get_irn_mode(phi);

	/* This loop goes to all predecessor blocks of the block the Phi node
	 * is in and there finds the operands of the Phi node by calling
	 * get_r_value_internal.  */
	for (int i = 0; i < arity; ++i) {
		ir_node *cfgpred = get_Block_cfgpred_block(block, i);
		ir_node *value;
		if (cfgpred == NULL) {
			value = new_r_Bad(irg, mode);
		} else {
			value = get_r_value_internal(cfgpred, pos, mode);
		}
		in[i] = value;
	}

	phi->attr.phi.u.backedge = new_backedge_arr(get_irg_obstack(irg), arity);
	set_irn_in(phi, arity, in);

	verify_new_node(phi);

	try_remove_unnecessary_phi(phi);

	/* To solve the problem of (potentially) endless loops being observable
	 * behaviour we add a keep-alive edge too all PhiM nodes. */
	if (mode == mode_M && !is_Id(phi)) {
		phi->attr.phi.loop = true;
		keep_alive(phi);
	}

	return phi;
}

/**
 * This function returns the last definition of a value.  In case
 * this value was last defined in a previous block, Phi nodes are
 * inserted.  If the part of the firm graph containing the definition
 * is not yet constructed, a dummy Phi node is returned.
 *
 * @param block   the current block
 * @param pos     the value number of the value searched
 * @param mode    the mode of this value (needed for Phi construction)
 */
static ir_node *get_r_value_internal(ir_node *block, int pos, ir_mode *mode)
{
	ir_node *res = block->attr.block.graph_arr[pos];
	if (res != NULL)
		return res;

	/* in a matured block we can immediately determine the phi arguments */
	if (get_Block_matured(block)) {
		ir_graph *const irg   = get_irn_irg(block);
		int       const arity = get_irn_arity(block);
		/* no predecessors: use unknown value */
		if (arity == 0) {
			if (block == get_irg_start_block(irg)) {
				if (default_initialize_local_variable != NULL) {
					ir_node *rem = get_r_cur_block(irg);
					set_r_cur_block(irg, block);
					res = default_initialize_local_variable(irg, mode, pos - 1);
					set_r_cur_block(irg, rem);
				} else {
					res = new_r_Unknown(irg, mode);
				}
			} else {
				goto bad; /* unreachable block, use Bad */
			}
		/* one predecessor just use its value */
		} else if (arity == 1) {
			ir_node *cfgpred = get_Block_cfgpred(block, 0);
			if (is_Bad(cfgpred)) {
bad:
				res = new_r_Bad(irg, mode);
			} else {
				ir_node *cfgpred_block = get_nodes_block(cfgpred);
				res = get_r_value_internal(cfgpred_block, pos, mode);
			}
		} else {
			/* multiple predecessors construct Phi */
			res = new_rd_Phi0(NULL, block, mode, pos);
			/* enter phi0 into our variable value table to break cycles
			 * arising from set_phi_arguments */
			block->attr.block.graph_arr[pos] = res;
			res = set_phi_arguments(res, pos);
		}
	} else {
		/* in case of immature block we have to keep a Phi0 */
		res = new_rd_Phi0(NULL, block, mode, pos);
		/* enqueue phi so we can set arguments once the block matures */
		res->attr.phi.next     = block->attr.block.phis;
		block->attr.block.phis = res;
	}
	block->attr.block.graph_arr[pos] = res;
	return res;
}

void mature_immBlock(ir_node *block)
{
	if (get_Block_matured(block))
		return;

	set_Block_matured(block, 1);

	/* Create final in-array for the block. */
	ir_graph *const irg = get_irn_irg(block);
	if (block->attr.block.dynamic_ins) {
		/* Attach a Bad predecessor if there is no other. This is necessary to
		 * fulfill the invariant that all nodes can be found through reverse
		 * edges from the start block. */
		ir_node             **new_in;
		struct obstack *const obst    = get_irg_obstack(irg);
		size_t                n_preds = ARR_LEN(block->in) - 1;
		if (n_preds == 0) {
			n_preds   = 1;
			new_in    = NEW_ARR_D(ir_node*, obst, 2);
			new_in[0] = NULL;
			new_in[1] = new_r_Bad(irg, mode_X);
		} else {
			new_in = DUP_ARR_D(ir_node*, obst, block->in);
		}
		DEL_ARR_F(block->in);
		block->in                     = new_in;
		block->attr.block.backedge    = new_backedge_arr(obst, n_preds);
		block->attr.block.dynamic_ins = false;
	}

	/* Traverse a chain of Phi nodes attached to this block and mature these,
	 * too. */
	for (ir_node *phi = block->attr.block.phis; phi != NULL;) {
		ir_node *const next      = phi->attr.phi.next;
		int      const pos       = phi->attr.phi.u.pos;
		ir_node *const new_value = set_phi_arguments(phi, pos);
		if (block->attr.block.graph_arr[pos] == phi)
			block->attr.block.graph_arr[pos] = new_value;
		phi = next;
	}

	/* Now, as the block is a finished Firm node, we can optimize it.
	 * Since other nodes have been allocated since the block was created we can
	 * not free the node on the obstack.  Therefore we have to call
	 * optimize_in_place().
	 * Unfortunately the optimization does not change a lot, as all allocated
	 * nodes refer to the unoptimized node.
	 * We can call optimize_in_place_2(), as global cse has no effect on blocks.
	 */
	verify_new_node(block);
	optimize_in_place_2(block);
}

ir_node *new_d_Const_long(dbg_info *db, ir_mode *mode, long value)
{
	assert(irg_is_constrained(current_ir_graph, IR_GRAPH_CONSTRAINT_CONSTRUCTION));
	return new_rd_Const_long(db, current_ir_graph, mode, value);
}

ir_node *new_rd_DivRL(dbg_info *dbgi, ir_node *block, ir_node * irn_mem, ir_node * irn_left, ir_node * irn_right, int pinned)
{
	ir_graph *const irg = get_irn_irg(block);
	ir_node  *const in[] = { irn_mem, irn_left, irn_right };
	ir_node        *res  = new_ir_node(dbgi, irg, block, op_Div, mode_T, ARRAY_SIZE(in), in);
	res->attr.div.resmode       = get_irn_mode(irn_left);
	res->attr.div.no_remainder  = 1;
	res->attr.div.exc.pinned    = pinned;
	verify_new_node(res);
	res = optimize_node(res);
	return res;
}

ir_node *new_r_DivRL(ir_node *block, ir_node * irn_mem, ir_node * irn_left, ir_node * irn_right, int pinned)
{
	return new_rd_DivRL(NULL, block, irn_mem, irn_left, irn_right, pinned);
}

ir_node *new_d_DivRL(dbg_info *dbgi, ir_node * irn_mem, ir_node * irn_left, ir_node * irn_right, int pinned)
{
	assert(irg_is_constrained(current_ir_graph, IR_GRAPH_CONSTRAINT_CONSTRUCTION));
	return new_rd_DivRL(dbgi, current_ir_graph->current_block, irn_mem, irn_left, irn_right, pinned);
}

ir_node *new_DivRL(ir_node * irn_mem, ir_node * irn_left, ir_node * irn_right, int pinned)
{
	return new_d_DivRL(NULL, irn_mem, irn_left, irn_right, pinned);
}

ir_node *new_rd_Phi_loop(dbg_info *db, ir_node *block, int arity,
                             ir_node *in[])
{
	ir_graph *const irg = get_irn_irg(block);
	ir_node *res = new_ir_node(db, irg, block, op_Phi, mode_M, arity, in);
	res->attr.phi.u.backedge = new_backedge_arr(get_irg_obstack(irg), arity);
	res->attr.phi.loop = true;
	verify_new_node(res);
	ir_node *optimized = optimize_node(res);
	if (optimized == res)
		keep_alive(optimized);
	return optimized;
}

ir_node *new_r_Phi_loop(ir_node *block, int arity, ir_node *in[])
{
	return new_rd_Phi_loop(NULL, block, arity, in);
}

ir_node *new_d_Phi_loop(dbg_info *db, int arity, ir_node *in[])
{
	assert(irg_is_constrained(current_ir_graph, IR_GRAPH_CONSTRAINT_CONSTRUCTION));
	return new_rd_Phi_loop(db, current_ir_graph->current_block, arity, in);
}

ir_node *new_Phi_loop(int arity, ir_node *in[])
{
	assert(irg_is_constrained(current_ir_graph, IR_GRAPH_CONSTRAINT_CONSTRUCTION));
	return new_rd_Phi_loop(NULL, current_ir_graph->current_block, arity, in);
}

ir_node *new_rd_immBlock(dbg_info *dbgi, ir_graph *irg)
{
	assert(irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_CONSTRUCTION));
	/* creates a new dynamic in-array as length of in is -1 */
	ir_node *const res = new_ir_node(dbgi, irg, NULL, op_Block, mode_BB, -1, NULL);

	set_Block_matured(res, 0);
	block_attr *const b = &res->attr.block;
	b->dynamic_ins = true;
	b->backedge    = NULL;
	b->entity      = NULL;

	set_Block_block_visited(res, 0);

	/* Create and initialize array for Phi-node construction. */
	res->attr.block.graph_arr = NEW_ARR_DZ(ir_node*, get_irg_obstack(irg), irg->n_loc);

	/* Immature block may not be optimized! */
	verify_new_node(res);

	return res;
}

ir_node *new_r_immBlock(ir_graph *irg)
{
	return new_rd_immBlock(NULL, irg);
}

ir_node *new_d_immBlock(dbg_info *dbgi)
{
	return new_rd_immBlock(dbgi, current_ir_graph);
}

ir_node *new_immBlock(void)
{
	return new_d_immBlock(NULL);
}

void add_immBlock_pred(ir_node *block, ir_node *jmp)
{
	assert(is_Block(block) && "Error: Must be a Block");
	assert(!get_Block_matured(block) && "Error: Block already matured!\n");
	assert(jmp->kind == k_ir_node);

	ARR_APP1(ir_node *, block->in, jmp);
}

void set_cur_block(ir_node *target)
{
	set_r_cur_block(current_ir_graph, target);
}

void set_r_cur_block(ir_graph *irg, ir_node *target)
{
	assert(irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_CONSTRUCTION));
	assert(target == NULL || is_Block(target));
	assert(target == NULL || get_irn_irg(target) == irg);
	irg->current_block = target;
}

ir_node *get_r_cur_block(ir_graph *irg)
{
	assert(irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_CONSTRUCTION));
	return irg->current_block;
}

ir_node *(get_cur_block)(void)
{
	return get_r_cur_block(current_ir_graph);
}

ir_node *get_r_value(ir_graph *irg, int pos, ir_mode *mode)
{
	assert(irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_CONSTRUCTION));
	assert(pos >= 0);

	return get_r_value_internal(irg->current_block, pos + 1, mode);
}

ir_node *get_value(int pos, ir_mode *mode)
{
	return get_r_value(current_ir_graph, pos, mode);
}

/**
 * helper function for guess_mode: recursively look for a definition for
 * local variable @p pos, returns its mode if found.
 */
static ir_mode *guess_recursively(ir_node *block, int pos)
{
	if (irn_visited_else_mark(block))
		return NULL;

	/* already have a definition -> we can simply look at its mode */
	ir_node *const value = block->attr.block.graph_arr[pos];
	if (value != NULL)
		return get_irn_mode(value);

	/* now we try to guess, by looking at the predecessor blocks */
	for (int i = 0, n_preds = get_irn_arity(block); i < n_preds; ++i) {
		ir_node *const pred_block = get_Block_cfgpred_block(block, i);
		if (pred_block == NULL)
			continue;
		ir_mode *const mode = guess_recursively(pred_block, pos);
		if (mode != NULL)
			return mode;
	}

	/* no way to guess */
	return NULL;
}

ir_mode *ir_r_guess_mode(ir_graph *irg, int pos)
{
	/* already have a definition -> we can simply look at its mode */
	ir_node *const block = irg->current_block;
	ir_node *const value = block->attr.block.graph_arr[pos+1];
	if (value != NULL)
		return get_irn_mode(value);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(irg);
	ir_mode *const mode = guess_recursively(block, pos + 1);
	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);

	return mode;
}

ir_mode *ir_guess_mode(int pos)
{
	return ir_r_guess_mode(current_ir_graph, pos);
}

void set_r_value(ir_graph *irg, int pos, ir_node *value)
{
	assert(irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_CONSTRUCTION));
	assert(pos >= 0);
	assert(pos + 1 < irg->n_loc);
	assert(value->kind == k_ir_node);
	irg->current_block->attr.block.graph_arr[pos + 1] = value;
}

void set_value(int pos, ir_node *value)
{
	set_r_value(current_ir_graph, pos, value);
}

ir_node *get_r_store(ir_graph *irg)
{
	assert(irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_CONSTRUCTION));
	return get_r_value_internal(irg->current_block, 0, mode_M);
}

ir_node *get_store(void)
{
	return get_r_store(current_ir_graph);
}

void set_r_store(ir_graph *const irg, ir_node *store)
{
	assert(irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_CONSTRUCTION));
	assert(get_irn_mode(store) == mode_M && "storing non-memory node");
	irg->current_block->attr.block.graph_arr[0] = store;
}

void set_store(ir_node *store)
{
	set_r_store(current_ir_graph, store);
}

void keep_alive(ir_node *ka)
{
	ir_graph *irg = get_irn_irg(ka);
	add_End_keepalive(get_irg_end(irg), ka);
}

void ir_set_uninitialized_local_variable_func(
		uninitialized_local_variable_func_t *func)
{
	default_initialize_local_variable = func;
}

void irg_finalize_cons(ir_graph *irg)
{
	ir_node *end_block = get_irg_end_block(irg);
	mature_immBlock(end_block);

	clear_irg_constraints(irg, IR_GRAPH_CONSTRAINT_CONSTRUCTION);
}

ir_node *new_Const_long(ir_mode *mode, long value)
{
	return new_d_Const_long(NULL, mode, value);
}

ir_node *new_r_Anchor(ir_graph *irg)
{
	ir_node *const res = new_ir_node(NULL, irg, NULL, op_Anchor, mode_ANY, 0, NULL);

	/* hack to get get_irn_irg in set_irn_in() working */
	res->in[0] = res;

	/* we can't have NULL inputs so reference ourselves for now */
	ir_node *in[n_Anchor_max + 1];
	for (size_t i = 0; i < ARRAY_SIZE(in); ++i) {
		in[i] = res;
	}
	set_irn_in(res, ARRAY_SIZE(in), in);

	return res;
}

ir_node *new_r_Block_noopt(ir_graph *irg, int arity, ir_node *in[])
{
	ir_node *res = new_ir_node(NULL, irg, NULL, op_Block, mode_BB, arity, in);
	res->attr.block.backedge = new_backedge_arr(get_irg_obstack(irg), arity);
	set_Block_matured(res, 1);
	/* Create and initialize array for Phi-node construction. */
	if (irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_CONSTRUCTION)) {
		res->attr.block.graph_arr = NEW_ARR_DZ(ir_node*, get_irg_obstack(irg), irg->n_loc);
	}
	verify_new_node(res);
	return res;
}

void (verify_new_node)(ir_node *node)
{
	verify_new_node_(node);
}

ir_node *new_rd_Const_null(dbg_info *const dbgi, ir_graph *const irg, ir_mode *const mode)
{
	return new_rd_Const(dbgi, irg, get_mode_null(mode));
}

ir_node *new_r_Const_null(ir_graph *const irg, ir_mode *const mode)
{
	return new_r_Const(irg, get_mode_null(mode));
}

ir_node *new_rd_Const_one(dbg_info *const dbgi, ir_graph *const irg, ir_mode *const mode)
{
	return new_rd_Const(dbgi, irg, get_mode_one(mode));
}

ir_node *new_r_Const_one(ir_graph *const irg, ir_mode *const mode)
{
	return new_r_Const(irg, get_mode_one(mode));
}
