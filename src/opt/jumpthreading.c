/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Path-Sensitive Jump Threading
 * @date    10. Sep. 2006
 * @author  Christoph Mallon, Matthias Braun
 */
#include "array.h"
#include "debug.h"
#include "ircons.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "iropt_dbg.h"
#include "iroptimize.h"
#include "iroptimize.h"
#include "irtools.h"
#include "tv.h"
#include "vrp.h"
#include <assert.h>
#include <stdbool.h>

#undef AVOID_PHIB

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/**
 * Add the new predecessor x to node node, which is either a Block or a Phi
 */
static void add_pred(ir_node *node, ir_node *x)
{
	int        const n   = get_Block_n_cfgpreds(node);
	ir_node  **const ins = ALLOCAN(ir_node*, n+1);
	foreach_irn_in(node, i, pred) {
		ins[i] = pred;
	}
	ins[n] = x;
	set_irn_in(node, n + 1, ins);
}

static ir_node *ssa_second_def;
static ir_node *ssa_second_def_block;

static ir_node *search_def_and_create_phis(ir_node *block, ir_mode *mode,
                                           bool first)
{
	assert(is_Block(block));

	/* the other defs can't be marked for cases where a user of the original
	 * value is in the same block as the alternative definition.
	 * In this case we mustn't use the alternative definition.
	 * So we keep a flag that indicated whether we walked at least 1 block
	 * away and may use the alternative definition */
	if (block == ssa_second_def_block && !first)
		return ssa_second_def;

	/* already processed this block? */
	if (irn_visited(block)) {
		ir_node *value = (ir_node*) get_irn_link(block);
		return value;
	}

	ir_graph *irg = get_irn_irg(block);
	assert(block != get_irg_start_block(irg));

	/* a Block with only 1 predecessor needs no Phi */
	int n_cfgpreds = get_Block_n_cfgpreds(block);
	if (n_cfgpreds == 1) {
		ir_node *pred_block = get_Block_cfgpred_block(block, 0);
		ir_node *value;
		if (pred_block == NULL) {
			ir_graph *irg = get_irn_irg(block);
			value = new_r_Bad(irg, mode);
		} else {
			value = search_def_and_create_phis(pred_block, mode, false);
		}
		set_irn_link(block, value);
		mark_irn_visited(block);
		return value;
	}

	/* create a new Phi */
	ir_node **in    = ALLOCAN(ir_node*, n_cfgpreds);
	ir_node  *dummy = new_r_Dummy(irg, mode);
	for (int i = 0; i < n_cfgpreds; ++i) {
		in[i] = dummy;
	}

	/* we might have created a potential endless loop, and need a PhiLoop */
	ir_node *phi = mode == mode_M ? new_r_Phi_loop(block, n_cfgpreds, in)
	                              : new_r_Phi(block, n_cfgpreds, in, mode);
	set_irn_link(block, phi);
	mark_irn_visited(block);

	/* set Phi predecessors */
	for (int i = 0; i < n_cfgpreds; ++i) {
		ir_node *pred_block = get_Block_cfgpred_block(block, i);
		ir_node *pred_val;
		if (pred_block == NULL) {
			ir_graph *irg = get_irn_irg(block);
			pred_val = new_r_Bad(irg, mode);
		} else {
			pred_val = search_def_and_create_phis(pred_block, mode, false);
		}
		set_irn_n(phi, i, pred_val);
	}

	return phi;
}

/**
 * Given a set of values this function constructs SSA-form for the users of the
 * first value (the users are determined through the out-edges of the value).
 * Uses the irn_visited flags. Works without using the dominance tree.
 */
static void construct_ssa(ir_node *orig_block, ir_node *orig_val,
                          ir_node *second_block, ir_node *second_val)
{
	/* no need to do anything */
	if (orig_val == second_val && !(is_Phi(orig_val) && get_Phi_loop(orig_val)))
		return;

	ir_graph *irg = get_irn_irg(orig_val);
	inc_irg_visited(irg);

	ir_mode *mode = get_irn_mode(orig_val);
	set_irn_link(orig_block, orig_val);
	mark_irn_visited(orig_block);

	if (orig_val == second_val) {
		/* In the loop-phi case setting a 2nd def is wrong */
		ssa_second_def_block = NULL;
	} else {
		ssa_second_def_block = second_block;
		ssa_second_def       = second_val;
	}

	/* Only fix the users of the first, i.e. the original node */
	foreach_out_edge_safe(orig_val, edge) {
		ir_node *user = get_edge_src_irn(edge);
		/* ignore keeps */
		if (is_End(user))
			continue;

		int j = get_edge_src_pos(edge);
		DB((dbg, LEVEL_3, ">>> Fixing user %+F (pred %d == %+F)\n", user, j,
		    get_irn_n(user, j)));

		ir_node *user_block = get_nodes_block(user);
		ir_node *newval;
		if (is_Phi(user)) {
			ir_node *pred_block = get_Block_cfgpred_block(user_block, j);
			if (pred_block == NULL) {
				ir_graph *irg = get_irn_irg(user_block);
				newval = new_r_Bad(irg, mode);
			} else {
				newval = search_def_and_create_phis(pred_block, mode, true);
			}
		} else {
			newval = search_def_and_create_phis(user_block, mode, true);
		}

		/* don't fix newly created Phis from the SSA construction */
		if (newval != user) {
			DB((dbg, LEVEL_4, ">>>> Setting input %d of %+F to %+F\n", j, user,
			    newval));
			set_irn_n(user, j, newval);
			if (is_Phi(user) && get_irn_mode(user) == mode_M && !get_Phi_loop(user)) {
				set_Phi_loop(user, true);
				keep_alive(user);
				keep_alive(user_block);
			}
		}
	}
}

/**
 * jumpthreading produces critical edges, e.g. B-C:
 *     A         A
 *  \ /       \  |
 *   B    =>   B |
 *  / \       / \|
 *     C         C
 *
 * By splitting this critical edge more threadings might be possible.
 */
static void split_critical_edge(ir_node *block, int pos)
{
	ir_graph *irg       = get_irn_irg(block);
	ir_node  *in[]      = { get_Block_cfgpred(block, pos) };
	ir_node  *new_block = new_r_Block(irg, ARRAY_SIZE(in), in);
	ir_node  *new_jmp   = new_r_Jmp(new_block);
	set_Block_cfgpred(block, pos, new_jmp);
}

typedef struct jumpthreading_env_t {
	ir_node      *true_block;  /**< Block we try to thread into */
	ir_node      *cmp;         /**< The Compare node that might be partial
	                                evaluated */
	ir_relation   relation;    /**< The Compare mode of the Compare node. */
	ir_node      *cnst;
	ir_tarval    *tv;
	ir_visited_t  visited_nr;
	ir_node      *cnst_pred;   /**< the block before the constant */
	int           cnst_pos;    /**< the pos to the constant block (needed to
	                                kill that edge later) */
} jumpthreading_env_t;

static ir_node *copy_and_fix_node(const jumpthreading_env_t *env,
                                  ir_node *block, ir_node *copy_block, int j,
                                  ir_node *node)
{
	/* we can evaluate Phis right now, all other nodes get copied */
	ir_node *copy;
	if (is_Phi(node)) {
		copy = get_Phi_pred(node, j);
		/* we might have to evaluate a Phi-cascade */
		if (get_irn_visited(copy) >= env->visited_nr)
			copy = (ir_node*)get_irn_link(copy);
	} else {
		copy = exact_copy(node);
		set_nodes_block(copy, copy_block);
		assert(get_irn_mode(copy) != mode_X);

		foreach_irn_in(copy, i, pred) {
			if (get_nodes_block(pred) != block)
				continue;

			ir_node *new_pred;
			if (get_irn_visited(pred) >= env->visited_nr) {
				new_pred = (ir_node*)get_irn_link(pred);
			} else {
				new_pred = copy_and_fix_node(env, block, copy_block, j, pred);
			}
			DB((dbg, LEVEL_2, ">> Set Pred of %+F to %+F\n", copy, new_pred));
			set_irn_n(copy, i, new_pred);
		}
	}

	set_irn_link(node, copy);
	set_irn_visited(node, env->visited_nr);
	return copy;
}

static void copy_and_fix(const jumpthreading_env_t *env, ir_node *block,
                         ir_node *copy_block, int j)
{
	/* Look at all nodes in the cond_block and copy them into copy_block */
	foreach_out_edge(block, edge) {
		ir_node *node = get_edge_src_irn(edge);
		if (is_End(node)) {
			/* edge is a Keep edge. If the end block is unreachable via normal
			 * control flow, we must maintain end's reachability with Keeps. */
			keep_alive(copy_block);
			continue;
		}
		/* ignore control flow */
		ir_mode *mode = get_irn_mode(node);
		if (mode == mode_X || is_Cond(node) || is_Switch(node))
			continue;
#ifdef AVOID_PHIB
		/* we may not copy mode_b nodes, because this could produce Phi with
		 * mode_bs which can't be handled in all backends. Instead we duplicate
		 * the node and move it to its users */
		if (mode == mode_b) {
			ir_node *const pred = get_Proj_pred(node);
			unsigned const pn   = get_Proj_num(node);

			foreach_out_edge_safe(node, edge) {
				ir_node *user       = get_edge_src_irn(edge);
				ir_node *user_block = get_nodes_block(user);
				if (user_block == block)
					continue;

				int      pos      = get_edge_src_pos(edge);
				ir_node *cmp_copy = exact_copy(pred);
				set_nodes_block(cmp_copy, user_block);
				ir_node *copy = new_r_Proj(cmp_copy, mode_b, pn);
				set_irn_n(user, pos, copy);
			}
			continue;
		}
#endif
		ir_node *copy = copy_and_fix_node(env, block, copy_block, j, node);

		/* we might hit values in blocks that have already been processed by a
		 * recursive find_const_or_confirm() call */
		assert(get_irn_visited(copy) <= env->visited_nr);
		if (get_irn_visited(copy) >= env->visited_nr) {
			ir_node *prev_copy = (ir_node*)get_irn_link(copy);
			if (prev_copy != NULL)
				set_irn_link(node, prev_copy);
		}
	}

	/* fix data-flow (and reconstruct SSA if needed) */
	foreach_out_edge(block, edge) {
		ir_node *node = get_edge_src_irn(edge);
		ir_mode *mode = get_irn_mode(node);
		if (mode == mode_X || is_Cond(node) || is_Switch(node))
			continue;
#ifdef AVOID_PHIB
		if (mode == mode_b)
			continue;
#endif

		ir_node *copy_node = (ir_node*)get_irn_link(node);
		DB((dbg, LEVEL_2, ">> Fixing users of %+F (copy %+F)\n", node, copy_node));
		construct_ssa(block, node, copy_block, copy_node);
	}

	/* make sure copied PhiM nodes are kept alive if old nodes were */
	ir_graph *irg = get_irn_irg(block);
	ir_node  *end = get_irg_end(irg);
	for (int i = 0, arity = get_End_n_keepalives(end); i < arity; ++i) {
		ir_node *keep = get_End_keepalive(end, i);
		if (get_irn_visited(keep) < env->visited_nr || is_Block(keep))
			continue;
		ir_node *copy = get_irn_link(keep);
		/* exact copy does not reproduce the keep alive edges */
		if (is_Phi(copy) && get_Phi_loop(copy))
			add_End_keepalive(end, copy);
	}
}

/**
 * Returns whether the cmp evaluates to true or false, or can't be evaluated!
 *
 * @param relation  the compare mode of the Compare
 * @param tv_left   the left tarval
 * @param tv_right  the right tarval
 * @returns 1: true, 0: false, -1: can't evaluate
 */
static int eval_cmp_tv(ir_relation relation, ir_tarval *tv_left,
                       ir_tarval *tv_right)
{
	/* does the compare evaluate to true? */
	ir_relation cmp_result = tarval_cmp(tv_left, tv_right);
	if (cmp_result == ir_relation_false)
		return -1;
	if ((cmp_result & relation) != 0)
		return 1;

	return 0;
}

/**
 * Returns whether the cmp evaluates to true or false, or can't be evaluated!
 *
 * @param env      the environment
 * @param cand     the candidate node, either a Const or a Confirm
 * @returns 1: true, 0: false, -1: can't evaluate
 */
static int eval_cmp(jumpthreading_env_t *env, ir_node *cand)
{
	if (is_Const(cand)) {
		ir_tarval *tv_cand = get_Const_tarval(cand);
		ir_tarval *tv_cmp  = get_Const_tarval(env->cnst);
		return eval_cmp_tv(env->relation, tv_cand, tv_cmp);
	} else {
		assert(is_Confirm(cand));
		ir_tarval *res = computed_value_Cmp_Confirm(cand, env->cnst,
		                                            env->relation);
		if (!tarval_is_constant(res))
			return -1;
		return res == tarval_b_true;
	}
}

/**
 * Check for Const or Confirm with Const.
 */
static bool is_Const_or_Confirm(const ir_node *node)
{
	if (is_Confirm(node))
		node = get_Confirm_bound(node);
	return is_Const(node);
}

/**
 * get the tarval of a Const or Confirm with
 */
static ir_tarval *get_Const_or_Confirm_tarval(const ir_node *node)
{
	if (is_Confirm(node))
		node = get_Confirm_bound(node);
	return get_Const_tarval(node);
}

static ir_node *find_const_or_confirm(jumpthreading_env_t *env, ir_node *jump,
                                      ir_node *value)
{
	if (irn_visited_else_mark(value))
		return NULL;

	ir_node *block = get_nodes_block(jump);
	if (is_Const_or_Confirm(value)) {
		int evaluated = eval_cmp(env, value);
		if (evaluated < 0)
			return NULL;
		/* maybe we could evaluate the condition completely without any
		 * partial tracking along paths. */
		assert(get_Block_n_cfgpreds(env->true_block) == 1);
		if (block == get_Block_cfgpred_block(env->true_block, 0)) {
			if (evaluated == 0) {
				ir_graph *irg = get_irn_irg(block);
				ir_node  *bad = new_r_Bad(irg, mode_X);
				exchange(jump, bad);
			} else if (evaluated == 1) {
				dbg_info *dbgi = get_irn_dbg_info(skip_Proj(jump));
				ir_node  *jmp  = new_rd_Jmp(dbgi, get_nodes_block(jump));
				exchange(jump, jmp);
			}
			/* we need a bigger visited nr when going back */
			env->visited_nr++;
			return block;
		}
		if (evaluated <= 0)
			return NULL;

		DB((dbg, LEVEL_1, "> Found jump threading candidate %+F->%+F\n",
			block, env->true_block));

		/* adjust true_block to point directly towards our jump */
		add_pred(env->true_block, jump);

		split_critical_edge(env->true_block, 0);

		/* we need a bigger visited nr when going back */
		env->visited_nr++;
		return block;
	}

	if (is_Phi(value)) {
		assert(get_irn_arity(value) > 1);

		/* the Phi has to be in the same Block as the Jmp */
		if (get_nodes_block(value) != block)
			return NULL;

		foreach_irn_in(value, i, phi_pred) {
			ir_node *cfgpred    = get_Block_cfgpred(block, i);
			ir_node *copy_block = find_const_or_confirm(env, cfgpred, phi_pred);
			if (copy_block == NULL)
				continue;

			/* copy duplicated nodes in copy_block and fix SSA */
			copy_and_fix(env, block, copy_block, i);
			if (copy_block == get_nodes_block(cfgpred)) {
				env->cnst_pred = block;
				env->cnst_pos  = i;
			}

			/* return now as we can't process more possibilities in 1 run */
			return copy_block;
		}
	}

	return NULL;
}

static ir_node *find_candidate(jumpthreading_env_t *env, ir_node *jump,
                               ir_node *value)
{
	if (irn_visited_else_mark(value))
		return NULL;

	ir_node *block = get_nodes_block(jump);
	if (is_Const_or_Confirm(value)) {
		ir_tarval *tv = get_Const_or_Confirm_tarval(value);
		if (tv != env->tv)
			return NULL;

		DB((dbg, LEVEL_1, "> Found jump threading candidate %+F->%+F\n",
			block, env->true_block));

		/* adjust true_block to point directly towards our jump */
		add_pred(env->true_block, jump);

		split_critical_edge(env->true_block, 0);

		/* we need a bigger visited nr when going back */
		env->visited_nr++;
		return block;
	}
	if (is_Phi(value)) {
		/* the Phi has to be in the same Block as the Jmp */
		if (get_nodes_block(value) != block)
			return NULL;

		foreach_irn_in(value, i, phi_pred) {
			ir_node *cfgpred    = get_Block_cfgpred(block, i);
			ir_node *copy_block = find_candidate(env, cfgpred, phi_pred);
			if (copy_block == NULL)
				continue;

			/* copy duplicated nodes in copy_block and fix SSA */
			copy_and_fix(env, block, copy_block, i);
			if (copy_block == get_nodes_block(cfgpred)) {
				env->cnst_pred = block;
				env->cnst_pos  = i;
			}

			/* return now as we can't process more possibilities in 1 run */
			return copy_block;
		}
	}
	if (is_Cmp(value)) {
		ir_node    *cmp      = value;
		ir_node    *left     = get_Cmp_left(cmp);
		ir_node    *right    = get_Cmp_right(cmp);
		ir_relation relation = get_Cmp_relation(cmp);

		/* we assume that the constant is on the right side, swap left/right
		 * if needed */
		if (is_Const(left)) {
			ir_node *t = left;
			left       = right;
			right      = t;

			relation   = get_inversed_relation(relation);
		}

		if (!is_Const(right))
			return NULL;
		if (get_nodes_block(left) != block)
			return NULL;
		/* negate condition when we're looking for the false block */
		if (env->tv == tarval_b_false)
			relation = get_negated_relation(relation);

		/* (recursively) look if a pred of a Phi is a constant or a Confirm */
		env->cmp      = cmp;
		env->relation = relation;
		env->cnst     = right;
		return find_const_or_confirm(env, jump, left);
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
static void thread_jumps(ir_node* block, void* data)
{
	bool *changed = (bool*)data;

	/* we do not deal with Phis, so restrict this to exactly one cfgpred */
	if (get_Block_n_cfgpreds(block) != 1)
		return;

	ir_node *projx = get_Block_cfgpred(block, 0);
	if (!is_Proj(projx))
		return;

	ir_node *cond = get_Proj_pred(projx);
	/* TODO handle switch Conds */
	if (!is_Cond(cond))
		return;

	/* (recursively) look if a pred of a Phi is a constant or a Confirm */
	ir_node  *selector = get_Cond_selector(cond);
	ir_graph *irg      = get_irn_irg(block);
	if (is_Const(selector)) {
		const ir_tarval *tv = get_Const_tarval(selector);
		assert(tv == tarval_b_false || tv == tarval_b_true);
		ir_node    *const cond_block = get_nodes_block(cond);
		ir_node    *const jmp        = new_r_Jmp(cond_block);
		ir_node    *const bad        = new_r_Bad(irg, mode_X);
		const bool        is_true    = tv == tarval_b_true;
		ir_node *const in[] = {
			[pn_Cond_false] = is_true ? bad : jmp,
			[pn_Cond_true]  = is_true ? jmp : bad,
		};
		turn_into_tuple(cond, ARRAY_SIZE(in), in);
		*changed = true;
		return;
	}
	inc_irg_visited(irg);
	jumpthreading_env_t env;
	env.cnst_pred  = NULL;
	env.tv         = get_Proj_num(projx) == pn_Cond_false
	                 ? tarval_b_false : tarval_b_true;
	env.true_block = block;
	env.visited_nr = get_irg_visited(irg);

	ir_node *copy_block = find_candidate(&env, projx, selector);
	if (copy_block == NULL)
		return;

	if (copy_block != get_nodes_block(cond)) {
		/* We might thread the condition block of an infinite loop,
		 * such that there is no path to End anymore. */
		keep_alive(block);

		/* we have to remove the edge towards the pred as the pred now
		 * jumps into the true_block. We also have to shorten Phis
		 * in our block because of this */
		ir_node *badX     = new_r_Bad(irg, mode_X);
		int      cnst_pos = env.cnst_pos;

		/* shorten Phis */
		foreach_out_edge_safe(env.cnst_pred, edge) {
			ir_node *node = get_edge_src_irn(edge);

			if (is_Phi(node)) {
				ir_node *bad = new_r_Bad(irg, get_irn_mode(node));
				set_Phi_pred(node, cnst_pos, bad);
			}
		}

		set_Block_cfgpred(env.cnst_pred, cnst_pos, badX);
	}

	/* the graph is changed now */
	*changed = true;
}

void opt_jumpthreading(ir_graph* irg)
{
	assure_irg_properties(irg,
		IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE
		| IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES
		| IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES);

	FIRM_DBG_REGISTER(dbg, "firm.opt.jumpthreading");

	DB((dbg, LEVEL_1, "===> Performing jumpthreading on %+F\n", irg));

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_IRN_VISITED);

	bool changed = false;
	bool rerun;
	do {
		rerun = false;
		irg_block_walk_graph(irg, thread_jumps, NULL, &rerun);
		changed |= rerun;
	} while (rerun);

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_IRN_VISITED);

	if (changed) {
		/* we tend to produce a lot of duplicated keep edges, remove them */
		remove_End_Bads_and_doublets(get_irg_end(irg));
		confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);
	} else {
		confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_ALL);
	}
}
