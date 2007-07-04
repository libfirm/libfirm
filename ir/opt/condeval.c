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
 * @brief   Partial condition evaluation
 * @date    10. Sep. 2006
 * @author  Christoph Mallon, Matthias Braun
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "iroptimize.h"

#include <assert.h>
#include "array.h"
#include "debug.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irnode.h"
#include "irnode_t.h"
#include "iredges.h"
#include "iredges_t.h"
#include "irtools.h"
#include "irgraph.h"
#include "tv.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg);

/**
 * Add the new predecessor x to node node, which is either a Block or a Phi
 */
static void add_pred(ir_node* node, ir_node* x)
{
	ir_node** ins;
	int n;
	int i;

	assert(is_Block(node) || is_Phi(node));

	n = get_irn_arity(node);
	NEW_ARR_A(ir_node*, ins, n + 1);
	for (i = 0; i < n; i++)
		ins[i] = get_irn_n(node, i);
	ins[n] = x;
	set_irn_in(node, n + 1, ins);
}

static ir_node *search_def_and_create_phis(ir_node *block, ir_mode *mode)
{
	int i;
	int n_cfgpreds;
	ir_graph *irg;
	ir_node *phi;
	ir_node **in;

	// This is needed because we create bads sometimes
	if(is_Bad(block))
		return new_Bad();

	// already processed this block?
	if(irn_visited(block)) {
		ir_node *value = (ir_node*) get_irn_link(block);
		return value;
	}

	// blocks with only 1 pred need no phi
	n_cfgpreds = get_Block_n_cfgpreds(block);
	if(n_cfgpreds == 1) {
		ir_node *pred_block = get_Block_cfgpred_block(block, 0);
		ir_node *value = search_def_and_create_phis(pred_block, mode);

		set_irn_link(block, value);
		mark_irn_visited(block);
		return value;
	}

	// create a new phi
	NEW_ARR_A(ir_node*, in, n_cfgpreds);
	for(i = 0; i < n_cfgpreds; ++i)
		in[i] = new_Unknown(mode);

	irg = get_irn_irg(block);
	phi = new_r_Phi(irg, block, n_cfgpreds, in, mode);
	set_irn_link(block, phi);
	mark_irn_visited(block);

	// set phi preds
	for(i = 0; i < n_cfgpreds; ++i) {
		ir_node *pred_block = get_Block_cfgpred_block(block, i);
		ir_node *pred_val = search_def_and_create_phis(pred_block, mode);

		set_irn_n(phi, i, pred_val);
	}

	return phi;
}

/**
 * Given a set of values this function constructs SSA-form for the users of the
 * first value (the users are determined through the out-edges of the value).
 * Uses the irn_visited flags. Works without using the dominance tree.
 */
static void construct_ssa(ir_node * const *blocks, ir_node * const *vals, int n_vals)
{
	int i;
	ir_graph *irg;
	ir_mode *mode;
	const ir_edge_t *edge;
	const ir_edge_t *next;
	ir_node *value;

	assert(n_vals > 0);

	irg = get_irn_irg(vals[0]);
	inc_irg_visited(irg);

	mode = get_irn_mode(vals[0]);
	for(i = 0; i < n_vals; ++i) {
		ir_node *value = vals[i];
		ir_node *value_block = blocks[i];

		assert(get_irn_mode(value) == mode);

		set_irn_link(value_block, value);
		mark_irn_visited(value_block);
	}

	// Only fix the users of the first, i.e. the original node
	value = vals[0];

	// this can happen when fixing phi preds, we mustn't fix the users
	if(get_nodes_block(value) != blocks[0]) return;

	foreach_out_edge_safe(value, edge, next) {
		ir_node *user = get_edge_src_irn(edge);
		int j = get_edge_src_pos(edge);
		ir_node *user_block = get_nodes_block(user);
		ir_node *newval;

		// ignore keeps
		if(get_irn_op(user) == op_End)
			continue;

		DB((dbg, LEVEL_3, ">>> Fixing user %+F (pred %d == %+F)\n", user, j, get_irn_n(user, j)));

		if(is_Phi(user)) {
			ir_node *pred_block = get_Block_cfgpred_block(user_block, j);
			newval = search_def_and_create_phis(pred_block, mode);
		} else {
			newval = search_def_and_create_phis(user_block, mode);
		}

		// don't fix newly created phis from the SSA construction
		if (newval != user) {
			DB((dbg, LEVEL_4, ">>>> Setting input %d of %+F to %+F\n", j, user, newval));
			set_irn_n(user, j, newval);
		}
	}
}

static void split_critical_edge(ir_node *block, int pos) {
	ir_graph *irg = get_irn_irg(block);
	ir_node *in[1];
	ir_node *new_block;
	ir_node *new_jmp;

	in[0] = get_Block_cfgpred(block, pos);
	new_block = new_r_Block(irg, 1, in);
	new_jmp = new_r_Jmp(irg, new_block);
	set_Block_cfgpred(block, pos, new_jmp);
}

typedef struct _condeval_env_t {
	ir_node *true_block;
	pn_Cmp pnc;
	ir_node *cnst;
	unsigned long visited_nr;

	ir_node *cnst_pred;	/**< the block before the constant */
	int cnst_pos;       /**< the pos to the constant block (needed to kill that edge later) */
} condeval_env_t;

static void copy_and_fix(ir_node *block, ir_node *copy_block, int j, const condeval_env_t *env) {
	const ir_edge_t *edge;

	/* Look at all nodes in the cond_block and copy them into pred */
	foreach_out_edge(block, edge) {
		ir_node *node = get_edge_src_irn(edge);
		ir_node *copy;
		ir_mode *mode = get_irn_mode(node);

		/* ignore control flow */
		if (mode == mode_X || is_Cond(node))
			continue;
		/* we may not copy mode_b nodes, because this could produce phi with mode_bs which can't
		   be handled in all backends. Instead we duplicate the node and move it to it's users */
		if (mode == mode_b) {
			const ir_edge_t *edge, *next;
			ir_node *pred;
			int      pn;

			assert(is_Proj(node));

			pred = get_Proj_pred(node);
			pn   = get_Proj_proj(node);

			foreach_out_edge_safe(node, edge, next) {
				ir_node *cmp_copy;
				ir_node *user       = get_edge_src_irn(edge);
				int pos             = get_edge_src_pos(edge);
				ir_node *user_block = get_nodes_block(user);

				if(user_block == block)
					continue;

				cmp_copy = exact_copy(pred);
				set_nodes_block(cmp_copy, user_block);
				copy = new_r_Proj(current_ir_graph, user_block, cmp_copy, mode_b, pn);
				set_irn_n(user, pos, copy);
			}
			continue;
		}

		/* we can evaluate Phis right now, all other nodes get copied */
		if (is_Phi(node)) {
			copy = get_Phi_pred(node, j);
		} else {
			copy = exact_copy(node);
			set_nodes_block(copy, copy_block);
		}

		set_irn_link(node, copy);
		set_irn_visited(node, env->visited_nr);

		/* we might hit values in blocks that have already been processed by a
		 * recursive find_phi_with_const call */
		assert(get_irn_visited(copy) <= env->visited_nr);
		if(get_irn_visited(copy) >= env->visited_nr) {
			ir_node *prev_copy = get_irn_link(copy);
			if(prev_copy != NULL)
				set_irn_link(node, prev_copy);
		}
	}

	/* fix data-flow (and reconstruct SSA if needed) */
	foreach_out_edge(block, edge) {
		ir_node *vals[2];
		ir_node *blocks[2];
		ir_node *node = get_edge_src_irn(edge);
		ir_mode *mode = get_irn_mode(node);

		if (mode == mode_X || is_Cond(node))
			continue;
		if (mode == mode_b)
			continue;

		DB((dbg, LEVEL_2, ">> Fixing users of %+F\n", node));

		blocks[0] = block;
		vals[0] = node;
		blocks[1] = copy_block;
		vals[1] = get_irn_link(node);
		construct_ssa(blocks, vals, 2);
	}
}

static int eval_cmp(pn_Cmp pnc, tarval *tv1, tarval *tv2) {
	pn_Cmp cmp_result = tarval_cmp(tv1, tv2);

	// does the compare evaluate to true?
	if(cmp_result == pn_Cmp_False)
		return 0;
	if((cmp_result & pnc) != cmp_result)
		return 0;

	return 1;
}

static ir_node *find_phi_with_const(ir_node *jump, ir_node *value, condeval_env_t *env) {
	ir_node *block = get_nodes_block(jump);

	if(irn_visited(value))
		return NULL;
	mark_irn_visited(value);

	if(is_Const(value)) {
		tarval *tv_const = get_Const_tarval(env->cnst);
		tarval *tv = get_Const_tarval(value);

		if(!eval_cmp(env->pnc, tv, tv_const))
			return NULL;

		DB((
			dbg, LEVEL_1,
			"> Found condition evaluation candidate %+F->%+F\n",
			env->true_block, block
		));

		// adjust true_block to point directly towards our jump
		add_pred(env->true_block, jump);

		split_critical_edge(env->true_block, 0);

		// we need a bigger visited nr when going back
		env->visited_nr++;

		return block;
	}

	if(is_Phi(value)) {
		int i, arity;

		// the phi has to be in the same block as the jump
		if(get_nodes_block(value) != block)
			return NULL;

		arity = get_irn_arity(value);
		for(i = 0; i < arity; ++i) {
			ir_node *copy_block;
			ir_node *phi_pred = get_Phi_pred(value, i);
			ir_node *cfgpred = get_Block_cfgpred(block, i);

			copy_block = find_phi_with_const(cfgpred, phi_pred, env);
			if(copy_block == NULL)
				continue;

			/* copy duplicated nodes in copy_block and fix SSA */
			copy_and_fix(block, copy_block, i, env);

			if(copy_block == get_nodes_block(cfgpred)) {
				env->cnst_pred = block;
				env->cnst_pos = i;
			}

			// return now as we can't process more possibilities in 1 run
			return copy_block;
		}
	}

	return NULL;
}


static int cond_eval_cmp(ir_node* proj_b, ir_node* block, ir_node* projx, ir_node* cond)
{
	ir_graph *irg = current_ir_graph;
	ir_node  *cmp = get_Proj_pred(proj_b);
	ir_node  *left;
	ir_node  *right;
	ir_node  *cond_block;
	ir_node  *copy_block;
	pn_Cmp    pnc;

	assert(is_Cmp(cmp));

	left  = get_Cmp_left(cmp);
	right = get_Cmp_right(cmp);
	assert(get_irn_mode(left) == get_irn_mode(right));

	pnc = get_Proj_proj(proj_b);
	/* we assume that the cond_block is the true case */
	if (get_Proj_proj(projx) == pn_Cond_false) {
		pnc = get_negated_pnc(pnc, get_irn_mode(left));
	}

	/* we assume that the constant is on the right side, swap left/right
	 * if needed */
	if(is_Const(left)) {
		ir_node *t = left;
		left = right;
		right = t;

		pnc = get_inversed_pnc(pnc);
	}

	if(!is_Const(right))
		return 0;

	cond_block = get_nodes_block(cond);

	// special case: comparing a constant with a constant
	if(is_Const(left)) {
		tarval *tv1 = get_Const_tarval(left);
		tarval *tv2 = get_Const_tarval(right);
		ir_node *pred;
		if(eval_cmp(pnc, tv1, tv2)) {
			pred = new_r_Jmp(irg, cond_block);
		} else {
			pred = new_Bad();
		}
		set_Block_cfgpred(block, 0, pred);
	} else {
		condeval_env_t env;

		if(get_nodes_block(left) != cond_block)
			return 0;

		// (recursively) look if a pred of a phi is a constant
		env.true_block = block;
		env.pnc = pnc;
		env.cnst = right;
		inc_irg_visited(current_ir_graph);
		env.visited_nr = get_irg_visited(irg);

		copy_block = find_phi_with_const(projx, left, &env);

		if (copy_block == NULL)
			return 0;

		/* we have to remove the edge towards the pred as the pred now
		 * jumps into the true_block. We also have to shorten phis
		 * in our block because of this */
		const ir_edge_t *edge, *next;
		ir_node* bad = new_Bad();
		size_t cnst_pos = env.cnst_pos;

		/* shorten phis */
		foreach_out_edge_safe(env.cnst_pred, edge, next) {
			ir_node *node = get_edge_src_irn(edge);

			if(is_Phi(node))
				set_Phi_pred(node, cnst_pos, bad);
		}

		set_Block_cfgpred(env.cnst_pred, cnst_pos, bad);
	}
	/* the graph is changed now */
	return 1;
}


/**
 * Block-walker: searches for the following construct
 *
 *  Const or Phi with constants
 *           |
 *          Cmp
 *           |
 *         Cond
 *          /
 *       ProjX
 *        /
 *     Block
 */
static void cond_eval(ir_node* block, void* data)
{
	int *changed = data;
	ir_node *pred;
	ir_node *projx;
	ir_node *cond;

	if(get_Block_n_cfgpreds(block) != 1)
		return;

	projx = get_Block_cfgpred(block, 0);
	if (!is_Proj(projx))
		return;
	assert(get_irn_mode(projx) == mode_X);

	cond = get_Proj_pred(projx);
	if (!is_Cond(cond))
		return;

	pred = get_Cond_selector(cond);
	// TODO handle switches
	if (get_irn_mode(pred) != mode_b)
		return;
	if (!is_Proj(pred))
		return;

	if (cond_eval_cmp(pred, block, projx, cond))
		*changed = 1;
}

void opt_cond_eval(ir_graph* irg)
{
	int changed, rerun;

	FIRM_DBG_REGISTER(dbg, "firm.opt.condeval");

	DB((dbg, LEVEL_1, "===> Performing condition evaluation on %+F\n", irg));

	remove_critical_cf_edges(irg);
	normalize_proj_nodes(irg);

	edges_assure(irg);
	set_using_irn_link(irg);
	set_using_visited(irg);

	changed = 0;
	do {
		rerun = 0;
		irg_block_walk_graph(irg, cond_eval, NULL, &rerun);
		changed |= rerun;
	} while (rerun);

	if (changed) {
		/* control flow changed, some blocks may become dead */
		set_irg_outs_inconsistent(irg);
		set_irg_doms_inconsistent(irg);
		set_irg_extblk_inconsistent(irg);
		set_irg_loopinfo_inconsistent(irg);
	}

	clear_using_visited(irg);
	clear_using_irn_link(irg);
}
