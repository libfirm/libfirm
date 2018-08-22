/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   If conversion
 * @author  Christoph Mallon
 */
#include "cdep_t.h"
#include "debug.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "iroptimize.h"
#include "irtools.h"
#include "pdeq.h"
#include "target_t.h"
#include <assert.h>
#include <stdbool.h>

/**
 * Environment for if-conversion.
 */
typedef struct walker_env {
	arch_allow_ifconv_func allow_ifconv;
	bool                   changed; /**< Set if the graph was changed. */
} walker_env;

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/**
 * Returns non-zero if a Block can be emptied.
 *
 * @param block  the block
 */
static bool can_empty_block(ir_node *block)
{
	return get_Block_mark(block) == 0;
}

/**
 * Find the ProjX node leading from block dependency to block start.
 *
 * @param start       a block that is control depended on dependency
 * @param dependency  the block that decides whether start is executed
 *
 * @return a ProjX node that represent the decision control flow or
 *         NULL is start is not dependent at all or a block on the way
 *         cannot be emptied
 */
static ir_node *walk_to_projx(ir_node *start, const ir_node *dependency)
{
	/* No need to find the conditional block if this block cannot be emptied and
	 * therefore not moved */
	if (!can_empty_block(start)) return NULL;

	foreach_irn_in(start, i, pred) {
		ir_node *pred_block = get_nodes_block(skip_Proj(pred));

		if (pred_block == dependency) {
			if (is_Proj(pred)) {
				assert(get_irn_mode(pred) == mode_X);
				/* we found it */
				return pred;
			}
			/* Not a Proj? Should not happen. */
			return NULL;
		}

		if (is_Proj(pred)) {
			assert(get_irn_mode(pred) == mode_X);
			/* another Proj but not from the control block */
			return NULL;
		}

		if (is_cdep_on(pred_block, dependency)) {
			return walk_to_projx(pred_block, dependency);
		}
	}
	return NULL;
}


/**
 * Recursively copies/moves the DAG starting at node to the i-th predecessor
 * block of src_block
 * - if node isn't in the src_block, recursion ends and node is returned
 * - if node is a Phi in the src_block, the i-th predecessor of this Phi is
 *   returned and recursion ends
 * - if node transitively does not depend on a Phi in the source block,
 *   moves node to the immediate dominator of the source block
 * otherwise returns a copy of the passed node created in the i-th predecessor of
 * src_block.
 *
 * @param node       a root of a DAG
 * @param src_block  the block of the DAG
 * @param i          the position of the predecessor the DAG
 *                   is moved to
 *
 * @return  the root of the copied/moved DAG
 */
static ir_node *copy_or_move(ir_node *node, ir_node *src_block, int i)
{
	if (get_nodes_block(node) != src_block) {
		/* already outside src_block, do not copy */
		DB((dbg, LEVEL_1, "copy_or_move: %+F outside src_block %+F\n", node, src_block));
		return node;
	}
	if (is_Phi(node)) {
		/* move through the Phi to the i-th predecessor */
		DB((dbg, LEVEL_1, "copy_or_move: %+F is Phi, returning argument %+F\n", node, get_irn_n(node, i)));
		return get_irn_n(node, i);
	}

	bool can_move = true;

	unsigned n = get_irn_arity(node);
	ir_node **new_preds = ALLOCAN(ir_node*, n);

	foreach_irn_in_r(node, j, pred) {
		ir_node *copy_pred = copy_or_move(pred, src_block, i);
		can_move &= copy_pred == pred;
		new_preds[j] = copy_pred;
	}

	if (can_move) {
		ir_node *idom = get_Block_idom(src_block);
		set_nodes_block(node, idom);

		DB((dbg, LEVEL_1, "copy_or_move: moving %+F to idom %+F\n", node, idom));
		return node;
	} else {
		ir_node *copy      = exact_copy(node);
		ir_node *dst_block = get_Block_cfgpred_block(src_block, i);
		set_nodes_block(copy, dst_block);
		set_irn_in(copy, n, new_preds);

		DB((dbg, LEVEL_1, "copy_or_move: copying %+F to predecessor %+F, copy is %+F\n", node, dst_block, copy));
		return copy;
	}
}


/**
 * Remove predecessors i and j (i < j) from a node and
 * add an additional predecessor new_pred.
 *
 * @param node      the node whose inputs are changed
 * @param i         the first index to remove
 * @param j         the second index to remove
 * @param new_pred  a node that is added as a new input to node
 */
static void rewire(ir_node *node, int i, int j, ir_node *new_pred)
{
	assert(i < j);
	int arity = get_irn_arity(node);
	ir_node **ins = ALLOCAN(ir_node*, arity - 1);
	int k;

	int l = 0;
	for (k = 0; k < i;     ++k) ins[l++] = get_irn_n(node, k);
	for (++k;   k < j;     ++k) ins[l++] = get_irn_n(node, k);
	for (++k;   k < arity; ++k) ins[l++] = get_irn_n(node, k);
	ins[l++] = new_pred;
	assert(l == arity - 1);
	set_irn_in(node, l, ins);
}


/**
 * Remove the j-th predecessor from the i-th predecessor of block and add it to block
 */
static void split_block(ir_node *block, int i, int j)
{
	ir_node  *pred_block = get_Block_cfgpred_block(block, i);
	int       arity      = get_Block_n_cfgpreds(block);
	ir_node **ins        = ALLOCAN(ir_node*, arity + 1);

	DB((dbg, LEVEL_1, "Splitting predecessor %d of predecessor %d of %+F\n", j, i, block));

	for (ir_node *phi = get_Block_phis(block); phi != NULL; phi = get_Phi_next(phi)) {
		ir_node *copy = copy_or_move(get_irn_n(phi, i), pred_block, j);

		int k;
		for (k = 0; k < i; ++k) ins[k] = get_irn_n(phi, k);
		ins[k++] = copy;
		for (; k < arity; ++k) ins[k] = get_irn_n(phi, k);
		ins[k++] = get_irn_n(phi, i);
		set_irn_in(phi, k, ins);
	}

	int k;
	for (k = 0; k < i; ++k) ins[k] = get_Block_cfgpred(block, k);
	ins[k++] = get_irn_n(pred_block, j);
	for (; k < arity; ++k) ins[k] = get_Block_cfgpred(block, k);
	ins[k++] = get_Block_cfgpred(block, i);
	set_irn_in(block, k, ins);

	int       new_pred_arity = get_irn_arity(pred_block) - 1;
	ir_node **pred_ins       = ALLOCAN(ir_node*, new_pred_arity);

	for (ir_node *next, *phi = get_Block_phis(pred_block); phi != NULL; phi = next) {
		next = get_Phi_next(phi);
		for (k = 0; k != j;              ++k) pred_ins[k] = get_irn_n(phi, k);
		for (;      k != new_pred_arity; ++k) pred_ins[k] = get_irn_n(phi, k + 1);
		if (k == 1) {
			if (get_Phi_loop(phi))
				remove_keep_alive(phi);
			exchange(phi, pred_ins[0]);
		} else {
			set_irn_in(phi, k, pred_ins);
		}
	}

	for (k = 0; k != j;              ++k) pred_ins[k] = get_irn_n(pred_block, k);
	for (;      k != new_pred_arity; ++k) pred_ins[k] = get_irn_n(pred_block, k + 1);
	if (k == 1) {
		exchange(pred_block, get_nodes_block(pred_ins[0]));
	} else {
		set_irn_in(pred_block, k, pred_ins);
	}
}


static void prepare_path(ir_node *block, int i, const ir_node *dependency)
{
	ir_node *pred = get_Block_cfgpred_block(block, i);

	DB((dbg, LEVEL_1, "Preparing predecessor %d of %+F\n", i, block));

	/* Optimize blocks with only one predecessor. */
	while (get_irn_arity(pred) == 1) {
		for (ir_node *next, *phi = get_Block_phis(pred); phi != NULL; phi = next) {
			next = get_Phi_next(phi);

			ir_node *operand = get_irn_n(phi, 0);
			if (get_Phi_loop(phi))
				remove_keep_alive(phi);
			exchange(phi, operand);
		}

		ir_node *pred_pred = get_Block_cfgpred(pred, 0);
		if (!is_Jmp(pred_pred))
			break;

		ir_node *pred_pred_block = get_nodes_block(pred_pred);
		exchange(pred, pred_pred_block);
		pred = pred_pred_block;
	}

	int pred_arity = get_irn_arity(pred);
	for (int j = 0; j < pred_arity; ++j) {
		ir_node *pred_pred = get_Block_cfgpred_block(pred, j);

		if (pred_pred != dependency && is_cdep_on(pred_pred, dependency)) {
			prepare_path(pred, j, dependency);
			split_block(block, i, j);
			break;
		}
	}
}

/**
 * Block walker: Search for diamonds and do the if conversion.
 */
static void if_conv_walker(ir_node *block, void *ctx)
{
	walker_env *env = (walker_env*)ctx;

	/* We might have replaced this block already. */
	if (!is_Block(block))
		return;

	/* Bail out, if there are no Phis at all */
	if (get_Block_phis(block) == NULL) return;

restart:;
	int arity = get_irn_arity(block);
	for (int i = 0; i < arity; ++i) {
		ir_node *pred0 = get_Block_cfgpred_block(block, i);
		if (pred0 == block) continue;

		for (ir_cdep *cdep = find_cdep(pred0); cdep != NULL; cdep = get_cdep_next(cdep)) {
			const ir_node *dependency = get_cdep_node(cdep);
			ir_node       *projx0     = walk_to_projx(pred0, dependency);

			if (projx0 == NULL) continue;

			ir_node *cond = get_Proj_pred(projx0);
			if (! is_Cond(cond))
				continue;

			for (int j = i + 1; j < arity; ++j) {
				ir_node *pred1 = get_Block_cfgpred_block(block, j);
				if (pred1 == block) continue;

				if (!is_cdep_on(pred1, dependency)) continue;

				ir_node *projx1 = walk_to_projx(pred1, dependency);

				if (projx1 == NULL) continue;

				ir_node *sel       = get_Cond_selector(cond);
				ir_node *phi       = get_Block_phis(block);
				bool     supported = true;
				bool     negated   = get_Proj_num(projx0) == pn_Cond_false;
				for (ir_node *p = phi; p != NULL; p = get_Phi_next(p)) {
					ir_node *mux_false;
					ir_node *mux_true;
					if (negated) {
						mux_true  = get_Phi_pred(p, j);
						mux_false = get_Phi_pred(p, i);
					} else {
						mux_true  = get_Phi_pred(p, i);
						mux_false = get_Phi_pred(p, j);
					}
					if (mux_true == mux_false)
						continue;
					ir_mode *mode = get_irn_mode(mux_true);
					if (mode == mode_M
						|| !env->allow_ifconv(sel, mux_false, mux_true)) {
						supported = false;
						break;
					}
				}
				if (!supported)
					continue;

				DB((dbg, LEVEL_1, "Found Cond %+F with proj %+F and %+F\n",
					cond, projx0, projx1
				));

				/* remove critical edges */
				env->changed = true;
				prepare_path(block, i, dependency);
				prepare_path(block, j, dependency);
				arity = get_irn_arity(block);

				ir_node *const mux_block = get_nodes_block(cond);
				do { /* generate Mux nodes in mux_block for Phis in block */
					ir_node *val_i = get_irn_n(phi, i);
					ir_node *val_j = get_irn_n(phi, j);
					ir_node *mux;

					if (val_i == val_j) {
						mux = val_i;
						DB((dbg, LEVEL_2,  "Generating no Mux for %+F, because both values are equal\n", phi));
					} else {
						ir_node *t, *f;

						/* Something is very fishy if two predecessors of a PhiM point into
						 * one block, but not at the same memory node
						 */
						assert(get_irn_mode(phi) != mode_M);
						if (negated) {
							t = val_j;
							f = val_i;
						} else {
							t = val_i;
							f = val_j;
						}

						dbg_info *const dbgi = get_irn_dbg_info(phi);
						mux = new_rd_Mux(dbgi, mux_block, sel, f, t);
						DB((dbg, LEVEL_2, "Generating %+F for %+F\n", mux, phi));
					}

					ir_node *next_phi = get_Phi_next(phi);

					if (arity == 2) {
						if (get_Phi_loop(phi))
							remove_keep_alive(phi);
						exchange(phi, mux);
					} else {
						rewire(phi, i, j, mux);
					}
					phi = next_phi;
				} while (phi != NULL);

				/* move mux operands into mux_block */
				exchange(get_Block_cfgpred_block(block, i), mux_block);
				exchange(get_Block_cfgpred_block(block, j), mux_block);

				if (arity == 2) {
					unsigned mark;
					DB((dbg, LEVEL_1,  "Welding block %+F to %+F\n", block, mux_block));
					mark =  get_Block_mark(mux_block) | get_Block_mark(block);
					/* mark both block just to be sure, should be enough to mark mux_block */
					set_Block_mark(mux_block, mark);
					exchange(block, mux_block);
					return;
				} else {
					rewire(block, i, j, new_r_Jmp(mux_block));
					goto restart;
				}
			}
		}
	}
}

/**
 * Block walker: clear block marks and Phi lists.
 */
static void init_block_link(ir_node *block, void *env)
{
	(void)env;
	set_Block_mark(block, 0);
	set_Block_phis(block, NULL);
}


/**
 * Daisy-chain all Phis in a block.
 * If a non-movable node is encountered set the has_pinned flag in its block.
 */
static void collect_phis(ir_node *node, void *env)
{
	(void)env;

	if (is_Phi(node)) {
		ir_node *block = get_nodes_block(node);

		add_Block_phi(block, node);
	} else {
		if (!is_Block(node) && get_irn_pinned(node)) {
			/*
			 * Ignore control flow nodes (except Raise), these will be removed.
			 */
			if (!is_cfop(node) && !is_Raise(node)) {
				ir_node *block = get_nodes_block(node);

				DB((dbg, LEVEL_2, "Node %+F in block %+F is unmovable\n", node, block));
				set_Block_mark(block, 1);
			}
		}
	}
}

static void fill_waitq(ir_node *node, void *env) {
	deq_t *waitq = (deq_t*)env;
	deq_push_pointer_right(waitq, node);
}

void opt_if_conv_cb(ir_graph *irg, arch_allow_ifconv_func callback)
{
	walker_env  env   = { .allow_ifconv = callback, .changed = false };
	deq_t waitq;
	deq_init(&waitq);

	assure_irg_properties(irg,
		IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES
		| IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE
		| IR_GRAPH_PROPERTY_NO_BADS
		| IR_GRAPH_PROPERTY_ONE_RETURN
		| IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);

	FIRM_DBG_REGISTER(dbg, "firm.opt.ifconv");

	DB((dbg, LEVEL_1, "Running if-conversion on %+F\n", irg));

	compute_cdep(irg);

	ir_reserve_resources(irg, IR_RESOURCE_BLOCK_MARK | IR_RESOURCE_PHI_LIST);

	irg_block_walk_graph(irg, init_block_link, fill_waitq, &waitq);
	irg_walk_graph(irg, collect_phis, NULL, NULL);

	/* Disable local optimizations to avoid the creation of
	 * new Phi nodes that are not tracked by the if conversion. */
	int rem_opt = get_optimize();
	set_optimize(0);

	while (!deq_empty(&waitq)) {
		ir_node *n = deq_pop_pointer_left(ir_node, &waitq);
		if_conv_walker(n, &env);
	}
	deq_free(&waitq);

	set_optimize(rem_opt);

	ir_free_resources(irg, IR_RESOURCE_BLOCK_MARK | IR_RESOURCE_PHI_LIST);

	if (env.changed) {
		local_optimize_graph(irg);
	}

	free_cdep(irg);

	confirm_irg_properties(irg,
		IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES
		| IR_GRAPH_PROPERTY_ONE_RETURN);
}

void opt_if_conv(ir_graph *irg)
{
	opt_if_conv_cb(irg, ir_target.allow_ifconv);
}
