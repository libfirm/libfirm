/*
 * Project:     libFIRM
 * File name:   ir/opt/cfopt.c
 * Purpose:     Partial condition evaluation
 * Author:      Christoph Mallon, Matthias Braun
 * Created:     10. Sep. 2006
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2006 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <assert.h>
#include "array.h"
#include "condeval.h"
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

/**
 * Remove predecessor j from node, which is either a Block or a Phi
 * returns true if only one predecessor is left
 */
static int remove_pred(ir_node* node, int j)
{
	int n;

	assert(is_Block(node) || is_Phi(node));

	n = get_irn_arity(node);
	if (n == 2) {
		ir_node* pred = get_irn_n(node, 1 - j);

		if (is_Block(node)) {
			pred = get_nodes_block(pred);
			edges_reroute(node, pred, current_ir_graph);
		} else {
			exchange(node, pred);
		}
		return 1;
	} else {
		ir_node** ins;
		int i;

		NEW_ARR_A(ir_node*, ins, n - 1);
		for (i = 0; i < j; i++)
			ins[i] = get_irn_n(node, i);
		for (i++; i < n; i++)
			ins[i - 1] = get_irn_n(node, i);

		set_irn_in(node, n - 1, ins);
		return 0;
	}
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
	in = alloca(sizeof(in[0]) * n_cfgpreds);
	for(i = 0; i < n_cfgpreds; ++i)
		in[i] = new_Unknown(mode);

	irg = get_irn_irg(block);
	phi = new_r_Phi(irg, block, n_cfgpreds, in, mode);
	set_irn_link(block, phi);
	mark_irn_visited(block);

	// set phi preds
	for(i = 0; i < n_cfgpreds; ++i) {
		ir_node *pred_block = get_Block_cfgpred_block(block, i);
		ir_node *pred_val;

		pred_val = search_def_and_create_phis(pred_block, mode);
		set_irn_n(phi, i, pred_val);
	}

	return phi;
}

/**
 * Given a set of values this function constructs SSA-form for all users of the
 * values (the user are determined through the out-edges of the values). Uses
 * the irn_visited flags. Works without using the dominance tree.
 */
static void construct_ssa(ir_node * const *blocks, ir_node * const *vals, int n_vals)
{
	int i;
	ir_graph *irg;
	ir_mode *mode;

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

	for(i = 0; i < n_vals; ++i) {
		const ir_edge_t *edge, *next;
		ir_node *value = vals[i];

		// this can happen when fixing phi preds, we mustn't fix the users
		if(get_nodes_block(value) != blocks[i]) {
			continue;
		}

		foreach_out_edge_safe(value, edge, next) {
			ir_node *user = get_edge_src_irn(edge);
			int j = get_edge_src_pos(edge);
			ir_node *user_block = get_nodes_block(user);
			ir_node *newval;

			// ignore keeps
			if(get_irn_op(user) == op_End)
				continue;

			if(is_Phi(user)) {
				ir_node *pred_block = get_Block_cfgpred_block(user_block, j);
				newval = search_def_and_create_phis(pred_block, mode);
			} else {
				newval = search_def_and_create_phis(user_block, mode);
			}

			// don't fix newly created phis from the SSA construction
			if(newval != user)
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

		/* ignore control flow */
		if (get_irn_mode(node) == mode_X)
			continue;

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

		if (get_irn_mode(node) == mode_X)
			continue;

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


/**
 * Block-walker: searchs for the following construct
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
	ir_graph *irg = current_ir_graph;
	ir_node *copy_block;
	ir_node *pred;
	ir_node *projx;
	ir_node *cond;
	ir_node *cmp;
	ir_node *left;
	ir_node *right;
	ir_node *cond_block;
	pn_Cmp pnc;

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
	pnc = get_Proj_proj(pred);

	cmp = get_Proj_pred(pred);
	assert(is_Cmp(cmp));

	left  = get_Cmp_left(cmp);
	right = get_Cmp_right(cmp);
	assert(get_irn_mode(left) == get_irn_mode(right));

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
		return;

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
		*changed = 1;
		set_irg_doms_inconsistent(irg);
		set_irg_extblk_inconsistent(irg);
		set_irg_loopinfo_inconsistent(irg);
	} else {
		if(get_nodes_block(left) != cond_block)
			return;

		// (recursively) look if a pred of a phi is a constant
		env.true_block = block;
		env.pnc = pnc;
		env.cnst = right;
		inc_irg_visited(current_ir_graph);
		env.visited_nr = get_irg_visited(irg);

		copy_block = find_phi_with_const(projx, left, &env);

		if(copy_block != NULL) {
			/* we have to remove the edge towards the pred as the pred now
			 * jumps into the true_block. We also have to shorten phis
			 * in our block because of this */
			const ir_edge_t *edge, *next;

			/* shorten phis */
			foreach_out_edge_safe(env.cnst_pred, edge, next) {
				ir_node *node = get_edge_src_irn(edge);

				if(is_Phi(node))
					remove_pred(node, env.cnst_pos);
			}

			remove_pred(env.cnst_pred, env.cnst_pos);

			// the graph is changed now
			*changed = 1;
			set_irg_doms_inconsistent(irg);
			set_irg_extblk_inconsistent(irg);
			set_irg_loopinfo_inconsistent(irg);
		}
	}
}

void opt_cond_eval(ir_graph* irg)
{
	int changed;

	FIRM_DBG_REGISTER(dbg, "firm.opt.condeval");
	//firm_dbg_set_mask(dbg, SET_LEVEL_5);

	DB((dbg, LEVEL_1, "===> Performing condition evaluation on %+F\n", irg));

	edges_assure(irg);
	remove_critical_cf_edges(irg);

	normalize_proj_nodes(irg);

	do {
		changed = 0;
		irg_block_walk_graph(irg, cond_eval, NULL, &changed);
	} while(changed);
}
