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

/*
 * @file    ir/opt/ifconv.c
 * @brief   If conversion
 * @author  Christoph Mallon
 * @version $Id$
 */

#include "config.h"

#include <assert.h>
#include "iroptimize.h"
#include "obst.h"
#include "irnode_t.h"
#include "cdep.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irtools.h"
#include "array_t.h"

// debug
#include "irdump.h"
#include "debug.h"

/**
 * Environment for if-conversion.
 */
typedef struct walker_env {
	const ir_settings_if_conv_t *params; /**< Conversion parameter. */
	int                         changed; /**< Set if the graph was changed. */
} walker_env;

DEBUG_ONLY(static firm_dbg_module_t *dbg);

/**
 * Default callback for Mux creation: allows every Mux to be created.
 */
static int default_allow_ifconv(ir_node *sel, ir_node* phi_list, int i, int j)
{
	(void) sel;
	(void) phi_list;
	(void) i;
	(void) j;
	return 1;
}

/**
 * Default options.
 */
static const ir_settings_if_conv_t default_info = {
	0,    /* doesn't matter for Mux */
	default_allow_ifconv
};

/**
 * Returns non-zero if a Block can be emptied.
 *
 * @param block  the block
 */
static int can_empty_block(ir_node *block) {
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
static ir_node* walk_to_projx(ir_node* start, const ir_node* dependency)
{
	int arity;
	int i;

	/* No need to find the conditional block if this block cannot be emptied and
	 * therefore not moved */
	if (!can_empty_block(start)) return NULL;

	arity = get_irn_arity(start);
	for (i = 0; i < arity; ++i) {
		ir_node* pred = get_irn_n(start, i);
		ir_node* pred_block = get_nodes_block(skip_Proj(pred));

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
 * Recursively copies the DAG starting at node to the i-th predecessor
 * block of src_block
 * - if node isn't in the src_block, recursion ends and node is returned
 * - if node is a Phi in the src_block, the i-th predecessor of this Phi is
 *   returned and recursion ends
 * otherwise returns a copy of the passed node created in the i-th predecessor of
 * src_block.
 *
 * @param node       a root of a DAG
 * @param src_block  the block of the DAG
 * @param i          the position of the predecessor the DAG
 *                   is moved to
 *
 * @return  the root of the copied DAG
 */
static ir_node* copy_to(ir_node* node, ir_node* src_block, int i)
{
	ir_node* dst_block;
	ir_node* copy;
	int j;

	if (get_nodes_block(node) != src_block) {
		/* already outside src_block, do not copy */
		return node;
	}
	if (is_Phi(node)) {
		/* move through the Phi to the i-th predecessor */
		return get_irn_n(node, i);
	}

	/* else really need a copy */
	copy = exact_copy(node);
	dst_block = get_nodes_block(get_irn_n(src_block, i));
	set_nodes_block(copy, dst_block);

	DB((dbg, LEVEL_1, "Copying node %+F to block %+F, copy is %+F\n",
		node, dst_block, copy));

	/* move recursively all predecessors */
	for (j = get_irn_arity(node) - 1; j >= 0; --j) {
		set_irn_n(copy, j, copy_to(get_irn_n(node, j), src_block, i));
		DB((dbg, LEVEL_2, "-- pred %d is %+F\n", j, get_irn_n(copy, j)));
	}
	return copy;
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
static void rewire(ir_node* node, int i, int j, ir_node* new_pred)
{
	int arity = get_irn_arity(node);
	ir_node **ins;
	int k;
	int l;

	NEW_ARR_A(ir_node *, ins, arity - 1);

	l = 0;
	for (k = 0; k < i;     ++k) ins[l++] = get_irn_n(node, k);
	for (++k;   k < j;     ++k) ins[l++] = get_irn_n(node, k);
	for (++k;   k < arity; ++k) ins[l++] = get_irn_n(node, k);
	ins[l++] = new_pred;
	assert(l == arity - 1);
	set_irn_in(node, l, ins);
}


/**
 * Remove the j-th predecessors from the i-th predecessor of block and add it to block
 */
static void split_block(ir_node* block, int i, int j)
{
	ir_node* pred_block = get_nodes_block(get_irn_n(block, i));
	int arity = get_irn_arity(block);
	int new_pred_arity;
	ir_node *phi, *next;
	ir_node **ins;
	ir_node **pred_ins;
	int k;

	DB((dbg, LEVEL_1, "Splitting predecessor %d of predecessor %d of %+F\n", j, i, block));

	NEW_ARR_A(ir_node*, ins, arity + 1);

	for (phi = get_Block_phis(block); phi != NULL; phi = get_Phi_next(phi)) {
		ir_node* copy = copy_to(get_irn_n(phi, i), pred_block, j);

		for (k = 0; k < i; ++k) ins[k] = get_irn_n(phi, k);
		ins[k++] = copy;
		for (; k < arity; ++k) ins[k] = get_irn_n(phi, k);
		ins[k] = get_irn_n(phi, i);
		assert(k == arity);
		set_irn_in(phi, arity + 1, ins);
	}

	for (k = 0; k < i; ++k) ins[k] = get_irn_n(block, k);
	ins[k++] = get_irn_n(pred_block, j);
	for (; k < arity; ++k) ins[k] = get_irn_n(block, k);
	ins[k] = get_irn_n(block, i);
	assert(k == arity);
	set_irn_in(block, arity + 1, ins);

	new_pred_arity = get_irn_arity(pred_block) - 1;
	NEW_ARR_A(ir_node*, pred_ins, new_pred_arity);

	for (phi = get_Block_phis(pred_block); phi != NULL; phi = next) {
		for (k = 0; k < j; ++k) pred_ins[k] = get_irn_n(phi, k);
		for (; k < new_pred_arity; ++k) pred_ins[k] = get_irn_n(phi, k + 1);
		assert(k == new_pred_arity);
		next = get_Phi_next(phi);
		if (new_pred_arity > 1) {
			set_irn_in(phi, new_pred_arity, pred_ins);
		} else {
			exchange(phi, pred_ins[0]);
		}
	}

	for (k = 0; k < j; ++k) pred_ins[k] = get_irn_n(pred_block, k);
	for (; k < new_pred_arity; ++k) pred_ins[k] = get_irn_n(pred_block, k + 1);
	assert(k == new_pred_arity);
	if (new_pred_arity > 1) {
		set_irn_in(pred_block, new_pred_arity, pred_ins);
	} else {
		exchange(pred_block, get_nodes_block(pred_ins[0]));
	}
}


static void prepare_path(ir_node* block, int i, const ir_node* dependency)
{
	ir_node* pred = get_nodes_block(get_irn_n(block, i));
	int pred_arity;
	int j;

	DB((dbg, LEVEL_1, "Preparing predecessor %d of %+F\n", i, block));

	pred_arity = get_irn_arity(pred);
	for (j = 0; j < pred_arity; ++j) {
		ir_node* pred_pred = get_nodes_block(get_irn_n(pred, j));

		if (is_cdep_on(pred_pred, dependency)) {
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
	walker_env *env = ctx;
	int arity;
	int i;

	/* Bail out, if there are no Phis at all */
	if (get_Block_phis(block) == NULL) return;

restart:
	arity = get_irn_arity(block);
	for (i = 0; i < arity; ++i) {
		ir_node* pred0;
		ir_cdep* cdep;

		pred0 = get_Block_cfgpred_block(block, i);
		for (cdep = find_cdep(pred0); cdep != NULL; cdep = cdep->next) {
			const ir_node* dependency = cdep->node;
			ir_node* projx0 = walk_to_projx(pred0, dependency);
			ir_node* cond;
			int j;

			if (projx0 == NULL) continue;

			cond = get_Proj_pred(projx0);
			if (! is_Cond(cond))
				continue;

			/* We only handle boolean decisions, no switches */
			if (get_irn_mode(get_Cond_selector(cond)) != mode_b) continue;

			for (j = i + 1; j < arity; ++j) {
				ir_node* projx1;
				ir_node* sel;
				ir_node* mux_block;
				ir_node* phi;
				ir_node* pred1;
				dbg_info* cond_dbg;

				pred1 = get_Block_cfgpred_block(block, j);

				if (!is_cdep_on(pred1, dependency)) continue;

				projx1 = walk_to_projx(pred1, dependency);

				if (projx1 == NULL) continue;

				phi = get_Block_phis(block);
				if (!env->params->allow_ifconv(get_Cond_selector(cond), phi, i, j))
					continue;

				DB((dbg, LEVEL_1, "Found Cond %+F with proj %+F and %+F\n",
					cond, projx0, projx1
				));

				env->changed = 1;
				prepare_path(block, i, dependency);
				prepare_path(block, j, dependency);
				arity = get_irn_arity(block);

				sel = get_Cond_selector(cond);

				mux_block = get_nodes_block(cond);
				cond_dbg = get_irn_dbg_info(cond);
				do {
					ir_node* val_i = get_irn_n(phi, i);
					ir_node* val_j = get_irn_n(phi, j);
					ir_node* mux;
					ir_node* next_phi;

					if (val_i == val_j) {
						mux = val_i;
						DB((dbg, LEVEL_2,  "Generating no Mux, because both values are equal\n"));
					} else {
						ir_node *t, *f;

						/* Something is very fishy if two predecessors of a PhiM point into
						 * one block, but not at the same memory node
						 */
						assert(get_irn_mode(phi) != mode_M);
						if (get_Proj_proj(projx0) == pn_Cond_true) {
							t = val_i;
							f = val_j;
						} else {
							t = val_j;
							f = val_i;
						}

						mux = new_rd_Mux(cond_dbg, mux_block, sel, f, t, get_irn_mode(phi));
						DB((dbg, LEVEL_2, "Generating %+F for %+F\n", mux, phi));
					}

					next_phi = get_Phi_next(phi);

					if (arity == 2) {
						exchange(phi, mux);
					} else {
						rewire(phi, i, j, mux);
					}
					phi = next_phi;
				} while (phi != NULL);

				exchange(get_nodes_block(get_irn_n(block, i)), mux_block);
				exchange(get_nodes_block(get_irn_n(block, j)), mux_block);

				if (arity == 2) {
					unsigned mark;
#if 1
					DB((dbg, LEVEL_1,  "Welding block %+F and %+F\n", block, mux_block));
					/* copy the block-info from the Mux-block to the block before merging */

					mark =  get_Block_mark(mux_block) | get_Block_mark(block);
					set_Block_mark(block, mark);
					set_Block_phis(block, get_Block_phis(mux_block));

					set_irn_in(block, get_irn_arity(mux_block), get_irn_in(mux_block) + 1);
					exchange_cdep(mux_block, block);
					exchange(mux_block, block);
#else
					DB((dbg, LEVEL_1,  "Welding block %+F to %+F\n", block, mux_block));
					mark =  get_Block_mark(mux_block) | get_Block_mark(block);
					/* mark both block just to be sure, should be enough to mark mux_block */
					set_Block_mark(mux_block, mark);
					exchange(block, mux_block);
#endif
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
static void collect_phis(ir_node *node, void *env) {
	(void) env;

	if (is_Phi(node)) {
		ir_node *block = get_nodes_block(node);

		add_Block_phi(block, node);
	} else {
		if (is_no_Block(node) && get_irn_pinned(node) == op_pin_state_pinned) {
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

void opt_if_conv(ir_graph *irg, const ir_settings_if_conv_t *params)
{
	walker_env env;

	/* get the parameters */
	env.params  = (params != NULL ? params : &default_info);
	env.changed = 0;

	FIRM_DBG_REGISTER(dbg, "firm.opt.ifconv");

	DB((dbg, LEVEL_1, "Running if-conversion on %+F\n", irg));

	normalize_one_return(irg);
	remove_critical_cf_edges(irg);

	compute_cdep(irg);

	ir_reserve_resources(irg, IR_RESOURCE_BLOCK_MARK | IR_RESOURCE_PHI_LIST);

	irg_block_walk_graph(irg, init_block_link, NULL, NULL);
	irg_walk_graph(irg, collect_phis, NULL, NULL);
	irg_block_walk_graph(irg, NULL, if_conv_walker, &env);

	ir_free_resources(irg, IR_RESOURCE_BLOCK_MARK | IR_RESOURCE_PHI_LIST);

	if (env.changed) {
		local_optimize_graph(irg);

		/* graph has changed, invalidate analysis info */
		set_irg_outs_inconsistent(irg);
		set_irg_extblk_inconsistent(irg);
		set_irg_loopinfo_inconsistent(irg);
		set_irg_doms_inconsistent(irg);
	}

	free_cdep(irg);
}
