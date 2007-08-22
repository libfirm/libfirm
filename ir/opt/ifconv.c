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

/*
 * @file    ir/opt/ifconv.c
 * @brief   If conversion
 * @author  Christoph Mallon
 * @version $Id$
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <assert.h>
#include "iroptimize.h"
#include "obst.h"
#include "irnode_t.h"
#include "cdep.h"
#include "ircons.h"
#include "irdom.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irtools.h"
#include "array.h"
#include "xmalloc.h"

// debug
#include "irdump.h"
#include "debug.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg);

/** allow every Psi to be created. */
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
	0,    /* doesn't matter for Psi */
	default_allow_ifconv
};

/**
 * Additional block info.
 */
typedef struct block_info {
	ir_node *phi;   /**< head of the Phi list */
	int has_pinned; /**< set if the block contains instructions that cannot be moved */
} block_info;


static INLINE block_info* get_block_blockinfo(const ir_node* block)
{
	return get_irn_link(block);
}


/**
 * Returns non-zero if a Block can be emptied.
 */
static int can_empty_block(ir_node *block)
{
	return !get_block_blockinfo(block)->has_pinned;
}


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
		ir_node* pred_block = get_nodes_block(pred);

		if (pred_block == dependency) {
			if (is_Proj(pred)) {
				assert(get_irn_mode(pred) == mode_X);
				return pred;
			}
			return NULL;
		}

		if (is_Proj(pred)) {
			assert(get_irn_mode(pred) == mode_X);
			return NULL;
		}

		if (is_cdep_on(pred_block, dependency)) {
			return walk_to_projx(pred_block, dependency);
		}
	}
	return NULL;
}


/**
 * Copies the DAG starting at node to the ith predecessor block of src_block
 * -if the node isn't in the src_block, this is a nop and the node is returned
 * -if the node is a phi in the src_block, the ith predecessor of the phi is
 *   returned
 * otherwise returns the copy of the passed node
 */
static ir_node* copy_to(ir_node* node, ir_node* src_block, int i)
{
	ir_node* dst_block;
	ir_node* copy;
	int arity;
	int j;

	if (get_nodes_block(node) != src_block) return node;
	if (get_irn_op(node) == op_Phi) return get_irn_n(node, i);

	copy = exact_copy(node);
	dst_block = get_nodes_block(get_irn_n(src_block, i));
	set_nodes_block(copy, dst_block);

	DB((dbg, LEVEL_1, "Copying node %+F to block %+F, copy is %+F\n",
		node, dst_block, copy));

	arity = get_irn_arity(node);
	for (j = 0; j < arity; ++j) {
		set_irn_n(copy, j, copy_to(get_irn_n(node, j), src_block, i));
		DB((dbg, LEVEL_2, "-- pred %d is %+F\n", j, get_irn_n(copy, j)));
	}
	return copy;
}


/**
 * Remove predecessors i and j from node and add predecessor new_pred
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
 * Remove the jth predecessors from the ith predecessor of block and add it to block
 */
static void split_block(ir_node* block, int i, int j)
{
	ir_node* pred_block = get_nodes_block(get_irn_n(block, i));
	int arity = get_irn_arity(block);
	int new_pred_arity;
	ir_node* phi;
	ir_node **ins;
	ir_node **pred_ins;
	int k;

	DB((dbg, LEVEL_1, "Splitting predecessor %d of predecessor %d of %+F\n", j, i, block));

	NEW_ARR_A(ir_node*, ins, arity + 1);

	for (phi = get_block_blockinfo(block)->phi; phi != NULL; phi = get_irn_link(phi)) {
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

	for (phi = get_block_blockinfo(pred_block)->phi; phi != NULL; phi = get_irn_link(phi)) {
		for (k = 0; k < j; ++k) pred_ins[k] = get_irn_n(phi, k);
		for (; k < new_pred_arity; ++k) pred_ins[k] = get_irn_n(phi, k + 1);
		assert(k == new_pred_arity);
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


static void if_conv_walker(ir_node* block, void* env)
{
	ir_settings_if_conv_t* opt_info = env;
	int arity;
	int i;

	/* Bail out, if there are no Phis at all */
	if (get_block_blockinfo(block)->phi == NULL) return;

restart:
	arity = get_irn_arity(block);
	for (i = 0; i < arity; ++i) {
		ir_node* pred0;
		cdep* cdep;

		pred0 = get_nodes_block(get_irn_n(block, i));
		for (cdep = find_cdep(pred0); cdep != NULL; cdep = cdep->next) {
			const ir_node* dependency = cdep->node;
			ir_node* projx0 = walk_to_projx(pred0, dependency);
			ir_node* cond;
			int j;

			if (projx0 == NULL) continue;

			cond = get_Proj_pred(projx0);
			if (get_irn_op(cond) != op_Cond) continue;

			/* We only handle boolean decisions, no switches */
			if (get_irn_mode(get_Cond_selector(cond)) != mode_b) continue;

			for (j = i + 1; j < arity; ++j) {
				ir_node* projx1;
				ir_node* conds[1];
				ir_node* psi_block;
				ir_node* phi;
				ir_node* pred1;
				dbg_info* cond_dbg;

				pred1 = get_nodes_block(get_irn_n(block, j));

				if (!is_cdep_on(pred1, dependency)) continue;

				projx1 = walk_to_projx(pred1, dependency);

				if (projx1 == NULL) continue;

				phi = get_block_blockinfo(block)->phi;
				if (!opt_info->allow_ifconv(get_Cond_selector(cond), phi, i, j)) continue;

				DB((dbg, LEVEL_1, "Found Cond %+F with proj %+F and %+F\n",
					cond, projx0, projx1
				));

				prepare_path(block, i, dependency);
				prepare_path(block, j, dependency);
				arity = get_irn_arity(block);

				conds[0] = get_Cond_selector(cond);

				psi_block = get_nodes_block(cond);
				cond_dbg = get_irn_dbg_info(cond);
				do {
					ir_node* val_i = get_irn_n(phi, i);
					ir_node* val_j = get_irn_n(phi, j);
					ir_node* psi;
					ir_node* next_phi;

					if (val_i == val_j) {
						psi = val_i;
						DB((dbg, LEVEL_2,  "Generating no psi, because both values are equal\n"));
					} else {
						ir_node* vals[2];

						/* Something is very fishy if two predecessors of a PhiM point into
						 * one block, but not at the same memory node
						 */
						assert(get_irn_mode(phi) != mode_M);
						if (get_Proj_proj(projx0) == pn_Cond_true) {
							vals[0] = val_i;
							vals[1] = val_j;
						} else {
							vals[0] = val_j;
							vals[1] = val_i;
						}

						psi = new_rd_Psi(cond_dbg, current_ir_graph, psi_block, 1, conds, vals, get_irn_mode(phi));
						DB((dbg, LEVEL_2, "Generating %+F for %+F\n", psi, phi));
					}

					next_phi = get_irn_link(phi);

					if (arity == 2) {
						exchange(phi, psi);
					} else {
						rewire(phi, i, j, psi);
					}

					phi = next_phi;
				} while (phi != NULL);

				exchange(get_nodes_block(get_irn_n(block, i)), psi_block);
				exchange(get_nodes_block(get_irn_n(block, j)), psi_block);

				if (arity == 2) {
#if 1
					DB((dbg, LEVEL_1,  "Welding block %+F and %+F\n", block, psi_block));
					/* copy the block-info from the Psi-block to the block before merging */
					get_block_blockinfo(psi_block)->has_pinned |= get_block_blockinfo(block)->has_pinned;
					set_irn_link(block, get_irn_link(psi_block));

					set_irn_in(block, get_irn_arity(psi_block), get_irn_in(psi_block) + 1);
					exchange_cdep(psi_block, block);
					exchange(psi_block, block);
#else
					DB((dbg, LEVEL_1,  "Welding block %+F to %+F\n", block, psi_block));
					get_block_blockinfo(psi_block)->has_pinned |=	get_block_blockinfo(block)->has_pinned;
					exchange(block, psi_block);
#endif
					return;
				} else {
					rewire(block, i, j, new_r_Jmp(current_ir_graph, psi_block));
					goto restart;
				}
			}
		}
	}
}

/**
 * Block walker: add additional data
 */
static void init_block_link(ir_node *block, void *env)
{
	struct obstack *obst = env;
	block_info *bi = obstack_alloc(obst, sizeof(*bi));

	bi->phi = NULL;
	bi->has_pinned = 0;
	set_irn_link(block, bi);
}


/**
 * Daisy-chain all phis in a block
 * If a non-movable node is encountered set the has_pinned flag
 */
static void collect_phis(ir_node *node, void *env)
{
	(void) env;

	if (is_Phi(node)) {
		ir_node *block = get_nodes_block(node);
		block_info *bi = get_block_blockinfo(block);

		set_irn_link(node, bi->phi);
		bi->phi = node;
	} else {
		if (is_no_Block(node) && get_irn_pinned(node) == op_pin_state_pinned) {
			/*
			 * Ignore control flow nodes, these will be removed.
			 * This ignores Raise. That is surely bad. FIXME.
			 */
			if (!is_cfop(node)) {
				ir_node *block = get_nodes_block(node);
				block_info *bi = get_block_blockinfo(block);

				DB((dbg, LEVEL_2, "Node %+F in block %+F is unmovable\n", node, block));
				bi->has_pinned = 1;
			}
		}
	}
}

static void optimise_psis_0(ir_node* psi, void* env)
{
	ir_node* t;
	ir_node* f;

	(void) env;

	if (!is_Psi(psi)) return;

	t = get_Psi_val(psi, 0);
	f = get_Psi_default(psi);

	DB((dbg, LEVEL_3, "Simplify %+F T=%+F F=%+F\n", psi, t, f));

	if (is_Unknown(t)) {
		DB((dbg, LEVEL_3, "Replace Psi with unknown operand by %+F\n", f));
		exchange(psi, f);
		return;
	}
	if (is_Unknown(f)) {
		DB((dbg, LEVEL_3, "Replace Psi with unknown operand by %+F\n", t));
		exchange(psi, t);
		return;
	}

	if (is_Psi(t)) {
		ir_graph* irg   = current_ir_graph;
		ir_node*  block = get_nodes_block(psi);
		ir_mode*  mode  = get_irn_mode(psi);
		ir_node*  c0    = get_Psi_cond(psi, 0);
		ir_node*  c1    = get_Psi_cond(t, 0);
		ir_node*  t1    = get_Psi_val(t, 0);
		ir_node*  f1    = get_Psi_default(t);
		if (f == f1) {
			/* Psi(c0, Psi(c1, x, y), y) -> typical if (c0 && c1) x else y */
			ir_node* and_    = new_r_And(irg, block, c0, c1, mode_b);
			ir_node* vals[2] = { t1, f1 };
			ir_node* new_psi = new_r_Psi(irg, block, 1, &and_, vals, mode);
			exchange(psi, new_psi);
		} else if (f == t1) {
			/* Psi(c0, Psi(c1, x, y), x) */
			ir_node* not_c1 = new_r_Not(irg, block, c1, mode_b);
			ir_node* and_   = new_r_And(irg, block, c0, not_c1, mode_b);
			ir_node* vals[2] = { f1, t1 };
			ir_node* new_psi = new_r_Psi(irg, block, 1, &and_, vals, mode);
			exchange(psi, new_psi);
		}
	} else if (is_Psi(f)) {
		ir_graph* irg   = current_ir_graph;
		ir_node*  block = get_nodes_block(psi);
		ir_mode*  mode  = get_irn_mode(psi);
		ir_node*  c0    = get_Psi_cond(psi, 0);
		ir_node*  c1    = get_Psi_cond(f, 0);
		ir_node*  t1    = get_Psi_val(f, 0);
		ir_node*  f1    = get_Psi_default(f);
		if (t == t1) {
			/* Psi(c0, x, Psi(c1, x, y)) -> typical if (c0 || c1) x else y */
			ir_node* or_     = new_r_Or(irg, block, c0, c1, mode_b);
			ir_node* vals[2] = { t1, f1 };
			ir_node* new_psi = new_r_Psi(irg, block, 1, &or_, vals, mode);
			exchange(psi, new_psi);
		} else if (t == f1) {
			/* Psi(c0, x, Psi(c1, y, x)) */
			ir_node* not_c1  = new_r_Not(irg, block, c1, mode_b);
			ir_node* or_     = new_r_Or(irg, block, c0, not_c1, mode_b);
			ir_node* vals[2] = { f1, t1 };
			ir_node* new_psi = new_r_Psi(irg, block, 1, &or_, vals, mode);
			exchange(psi, new_psi);
		}
	}
}


static void optimise_psis_1(ir_node* psi, void* env)
{
	ir_node* t;
	ir_node* f;
	ir_mode* mode;

	(void) env;

	if (!is_Psi(psi)) return;

	t = get_Psi_val(psi, 0);
	f = get_Psi_default(psi);

	DB((dbg, LEVEL_3, "Simplify %+F T=%+F F=%+F\n", psi, t, f));

	mode = get_irn_mode(psi);

	if (is_Const(t) && is_Const(f) && (mode_is_int(mode))) {
		ir_node* block = get_nodes_block(psi);
		ir_node* c     = get_Psi_cond(psi, 0);
		tarval* tv_t = get_Const_tarval(t);
		tarval* tv_f = get_Const_tarval(f);
		if (tarval_is_one(tv_t) && tarval_is_null(tv_f)) {
			ir_node* conv  = new_r_Conv(current_ir_graph, block, c, mode);
			exchange(psi, conv);
		} else if (tarval_is_null(tv_t) && tarval_is_one(tv_f)) {
			ir_node* not_  = new_r_Not(current_ir_graph, block, c, mode_b);
			ir_node* conv  = new_r_Conv(current_ir_graph, block, not_, mode);
			exchange(psi, conv);
		}
	}
}


void opt_if_conv(ir_graph *irg, const ir_settings_if_conv_t *params)
{
	struct obstack obst;
	ir_settings_if_conv_t p;

	/* get the parameters */
	p = (params != NULL ? *params : default_info);

	FIRM_DBG_REGISTER(dbg, "firm.opt.ifconv");

	DB((dbg, LEVEL_1, "Running if-conversion on %+F\n", irg));

	normalize_one_return(irg);
	remove_critical_cf_edges(irg);

	compute_cdep(irg);
	assure_doms(irg);

	obstack_init(&obst);
	irg_block_walk_graph(irg, init_block_link, NULL, &obst);
	irg_walk_graph(irg, collect_phis, NULL, NULL);
	irg_block_walk_graph(irg, NULL, if_conv_walker, &p);

	local_optimize_graph(irg);

	irg_walk_graph(irg, NULL, optimise_psis_0, NULL);
#if 1
	irg_walk_graph(irg, NULL, optimise_psis_1, NULL);
#endif

	obstack_free(&obst, NULL);

	free_dom(irg);
	free_cdep(irg);
}
