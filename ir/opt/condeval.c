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

//#define AVOID_PHIB

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

	irg = get_irn_irg(block);
	assert(block != get_irg_start_block(irg));

	// blocks with only 1 pred need no phi
	n_cfgpreds = get_Block_n_cfgpreds(block);
	if(n_cfgpreds == 1) {
		ir_node *pred_block = get_Block_cfgpred_block(block, 0);
		ir_node *value      = search_def_and_create_phis(pred_block, mode);

		set_irn_link(block, value);
		mark_irn_visited(block);
		return value;
	}

	// create a new phi
	NEW_ARR_A(ir_node*, in, n_cfgpreds);
	for(i = 0; i < n_cfgpreds; ++i)
		in[i] = new_Unknown(mode);

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

	assert(n_vals == 2);

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

	foreach_out_edge_safe(value, edge, next) {
		ir_node *user = get_edge_src_irn(edge);
		int j = get_edge_src_pos(edge);
		ir_node *user_block = get_nodes_block(user);
		ir_node *newval;

		// ignore keeps
		if(get_irn_op(user) == op_End)
			continue;

		if (user_block == blocks[1])
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

typedef struct condeval_env_t {
	ir_node       *true_block;
	pn_Cmp         pnc;
	ir_node       *cnst;
	tarval        *tv;
	unsigned long  visited_nr;

	ir_node       *cnst_pred;   /**< the block before the constant */
	int            cnst_pos;    /**< the pos to the constant block (needed to
	                                  kill that edge later) */
} condeval_env_t;

static ir_node *copy_and_fix_node(const condeval_env_t *env, ir_node *block,
                                  ir_node *copy_block, int j, ir_node *node) {
	int      i, arity;
	ir_node *copy;

	/* we can evaluate Phis right now, all other nodes get copied */
	if (is_Phi(node)) {
		copy = get_Phi_pred(node, j);
		/* we might have to evaluate a phi-cascades */
		if(get_irn_visited(copy) >= env->visited_nr) {
			copy = get_irn_link(copy);
		}
	} else {
		copy = exact_copy(node);
		set_nodes_block(copy, copy_block);

		assert(get_irn_mode(copy) != mode_X);

		arity = get_irn_arity(copy);
		for(i = 0; i < arity; ++i) {
			ir_node *pred     = get_irn_n(copy, i);
			ir_node *new_pred;

			if(get_nodes_block(pred) != block)
				continue;

			if(get_irn_visited(pred) >= env->visited_nr) {
				new_pred = get_irn_link(pred);
			} else {
				new_pred = copy_and_fix_node(env, block, copy_block, j, pred);
			}
			set_irn_n(copy, i, new_pred);
		}
	}

	set_irn_link(node, copy);
	set_irn_visited(node, env->visited_nr);

	return copy;
}

static void copy_and_fix(const condeval_env_t *env, ir_node *block,
                         ir_node *copy_block, int j) {
	const ir_edge_t *edge;

	/* Look at all nodes in the cond_block and copy them into pred */
	foreach_out_edge(block, edge) {
		ir_node *node = get_edge_src_irn(edge);
		ir_node *copy;
		ir_mode *mode = get_irn_mode(node);

		/* ignore control flow */
		if (mode == mode_X || is_Cond(node))
			continue;
#ifdef AVOID_PHIB
		/* we may not copy mode_b nodes, because this could produce phi with
		 * mode_bs which can't be handled in all backends. Instead we duplicate
		 * the node and move it to it's users */
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
#endif

		copy = copy_and_fix_node(env, block, copy_block, j, node);

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
#ifdef AVOID_PHIB
		if (mode == mode_b)
			continue;
#endif

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

static ir_node *find_const(condeval_env_t *env, ir_node *jump, ir_node *value)
{
	ir_node *block = get_nodes_block(jump);

	if(irn_visited(value))
		return NULL;
	mark_irn_visited(value);

	if(is_Const(value)) {
		tarval *tv_const = get_Const_tarval(env->cnst);
		tarval *tv       = get_Const_tarval(value);

		if(!eval_cmp(env->pnc, tv, tv_const)) {
			return NULL;
		}

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

		/* the phi has to be in the same block as the jump */
		if(get_nodes_block(value) != block) {
			return NULL;
		}

		arity = get_irn_arity(value);
		for(i = 0; i < arity; ++i) {
			ir_node *copy_block;
			ir_node *phi_pred = get_Phi_pred(value, i);
			ir_node *cfgpred  = get_Block_cfgpred(block, i);

			copy_block = find_const(env, cfgpred, phi_pred);
			if(copy_block == NULL)
				continue;

			/* copy duplicated nodes in copy_block and fix SSA */
			copy_and_fix(env, block, copy_block, i);

			if(copy_block == get_nodes_block(cfgpred)) {
				env->cnst_pred = block;
				env->cnst_pos  = i;
			}

			/* return now as we can't process more possibilities in 1 run */
			return copy_block;
		}
	}

	return NULL;
}

static ir_node *find_candidate(condeval_env_t *env, ir_node *jump,
                               ir_node *value)
{
	ir_node *block = get_nodes_block(jump);

	if(irn_visited(value)) {
		return NULL;
	}
	mark_irn_visited(value);

	if(is_Const(value)) {
		tarval *tv = get_Const_tarval(value);

		if(tv != env->tv)
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
			ir_node *cfgpred  = get_Block_cfgpred(block, i);

			copy_block = find_candidate(env, cfgpred, phi_pred);
			if(copy_block == NULL)
				continue;

			/* copy duplicated nodes in copy_block and fix SSA */
			copy_and_fix(env, block, copy_block, i);

			if(copy_block == get_nodes_block(cfgpred)) {
				env->cnst_pred = block;
				env->cnst_pos  = i;
			}

			// return now as we can't process more possibilities in 1 run
			return copy_block;
		}
	}
	if(is_Proj(value)) {
		ir_node *left;
		ir_node *right;
		int      pnc;
		ir_node *cmp = get_Proj_pred(value);
		if(!is_Cmp(cmp))
			return NULL;

		left  = get_Cmp_left(cmp);
		right = get_Cmp_right(cmp);
		pnc   = get_Proj_proj(value);

		/* we assume that the constant is on the right side, swap left/right
		 * if needed */
		if(is_Const(left)) {
			ir_node *t = left;
			left       = right;
			right      = t;

			pnc        = get_inversed_pnc(pnc);
		}

		if(!is_Const(right))
			return 0;

		if(get_nodes_block(left) != block) {
			return 0;
		}

		/* negate condition when we're looking for the false block */
		if(env->tv == get_tarval_b_false()) {
			pnc = get_negated_pnc(pnc, get_irn_mode(right));
		}

		// (recursively) look if a pred of a phi is a constant
		env->pnc  = pnc;
		env->cnst = right;

		return find_const(env, jump, left);
	}

	return NULL;
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
	condeval_env_t env;
	int *changed = data;
	ir_node *selector;
	ir_node *projx;
	ir_node *cond;
	ir_node *copy_block;
	int      selector_evaluated;
	const ir_edge_t *edge, *next;
	ir_node* bad;
	size_t   cnst_pos;

	if(get_Block_n_cfgpreds(block) != 1)
		return;

	projx = get_Block_cfgpred(block, 0);
	if (!is_Proj(projx))
		return;
	assert(get_irn_mode(projx) == mode_X);

	cond = get_Proj_pred(projx);
	if (!is_Cond(cond))
		return;

	selector = get_Cond_selector(cond);
	// TODO handle switch Conds
	if (get_irn_mode(selector) != mode_b)
		return;

	/* handle cases that can be immediately evalutated */
	selector_evaluated = -1;
	if(is_Proj(selector)) {
		ir_node *cmp = get_Proj_pred(selector);
		if(is_Cmp(cmp)) {
			ir_node *left  = get_Cmp_left(cmp);
			ir_node *right = get_Cmp_right(cmp);
			if(is_Const(left) && is_Const(right)) {
				int     pnc      = get_Proj_proj(selector);
				tarval *tv_left  = get_Const_tarval(left);
				tarval *tv_right = get_Const_tarval(right);

				selector_evaluated = eval_cmp(pnc, tv_left, tv_right);
			}
		}
	} else if(is_Const(selector)) {
		tarval *tv = get_Const_tarval(selector);
		if(tv == get_tarval_b_true()) {
			selector_evaluated = 1;
		} else {
			assert(tv == get_tarval_b_false());
			selector_evaluated = 0;
		}
	}

	env.cnst_pred = NULL;
	if (get_Proj_proj(projx) == pn_Cond_false) {
		env.tv = get_tarval_b_false();
		if(selector_evaluated >= 0)
			selector_evaluated = !selector_evaluated;
	} else {
		env.tv = get_tarval_b_true();
	}

	if(selector_evaluated == 0) {
		bad = new_Bad();
		exchange(projx, bad);
		*changed = 1;
		return;
	} else if(selector_evaluated == 1) {
		dbg_info *dbgi = get_irn_dbg_info(selector);
		ir_node  *jmp  = new_rd_Jmp(dbgi, current_ir_graph, get_nodes_block(projx));
		exchange(projx, jmp);
		*changed = 1;
		return;
	}

	// (recursively) look if a pred of a phi is a constant
	env.true_block = block;
	inc_irg_visited(current_ir_graph);
	env.visited_nr = get_irg_visited(current_ir_graph);

	copy_block = find_candidate(&env, projx, selector);
	if (copy_block == NULL)
		return;

	/* we have to remove the edge towards the pred as the pred now
	 * jumps into the true_block. We also have to shorten phis
	 * in our block because of this */
	bad      = new_Bad();
	cnst_pos = env.cnst_pos;

	/* shorten phis */
	foreach_out_edge_safe(env.cnst_pred, edge, next) {
		ir_node *node = get_edge_src_irn(edge);

		if(is_Phi(node))
			set_Phi_pred(node, cnst_pos, bad);
	}

	set_Block_cfgpred(env.cnst_pred, cnst_pos, bad);

	/* the graph is changed now */
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
