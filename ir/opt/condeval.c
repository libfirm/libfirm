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
	for (i = 0; i < n; i++) ins[i] = get_irn_n(node, i);
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
			ir_fprintf(stderr, "Exchaning %+F with %+F\n", node, pred);
			//set_irn_in(node, get_irn_arity(pred), get_irn_in(pred) + 1);
			//exchange(node, pred);
			edges_reroute(node, pred, current_ir_graph);
		} else {
			exchange(node, pred);
		}
		return 1;
	} else {
		ir_node** ins;
		int i;

		NEW_ARR_A(ir_node*, ins, n - 1);
		for (i = 0; i < j; i++) ins[i]     = get_irn_n(node, i);
		for (i++;   i < n; i++) ins[i - 1] = get_irn_n(node, i);
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

	assert(!is_Bad(block));

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
 * values (the user are determined through the out-edges of the values).
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


#if 0
/**
 * Compare node compares two Const nodes
 * Returns true if this direction is taken
 */
static int handle_const_const(ir_node* cnst_left, cnst_right, pn_Cmp pnc)
{
	// TODO
}
#endif


/**
 *
 */
static int handle_phi_const(ir_node* block, ir_node* cond_block, ir_node* phi, ir_node* cnst, pn_Cmp pnc)
{
	int changed = 0;
	ir_graph *irg;
	tarval* tv_cnst;
	int n_phi;
	int j;

	tv_cnst = get_Const_tarval(cnst);
	n_phi = get_Phi_n_preds(phi);
	for (j = 0; j < n_phi; j++) {
		const ir_edge_t *edge, *next;
		ir_node *pred;
		ir_node *pred_block;
		tarval *tv_phi;
		pn_Cmp cmp_val;

		pred = get_Phi_pred(phi, j);
		// TODO handle Phi cascades
		if (!is_Const(pred))
			continue;

		tv_phi = get_Const_tarval(pred);

		cmp_val = tarval_cmp(tv_phi, tv_cnst);
		if (cmp_val == pn_Cmp_False) continue;
		if ((cmp_val & pnc) != cmp_val) continue;

		DB((
			dbg, LEVEL_1,
			"> Found condition evaluation candidate %+F->%+F predecessor %d\n",
			block, cond_block, j
		));

		add_pred(block, get_Block_cfgpred(cond_block, j));
		// split critical edges
		/*if(get_irn_arity(block) == 2)*/ {
			ir_node *in[1];
			ir_node *new_block;
			ir_node *new_jmp;

			in[0] = get_Block_cfgpred(block, 0);
			new_block = new_r_Block(current_ir_graph, 1, in);
			new_jmp = new_r_Jmp(current_ir_graph, new_block);
			set_Block_cfgpred(block, 0, new_jmp);
		}

		pred_block = get_Block_cfgpred_block(cond_block, j);

		/* Look at all nodes in the cond_block and copy them into pred */
		foreach_out_edge(cond_block, edge) {
			ir_node *node = get_edge_src_irn(edge);
			ir_node *copy;

			// ignore these as SSA construction would fail on ProjX
			if (is_Proj(node) && get_irn_mode(node) == mode_X)
				continue;

			// we can evaluate Phis right now, all other nodes get copied
			if (is_Phi(node)) {
				copy = get_Phi_pred(node, j);
			} else {
				copy = exact_copy(node);
				set_nodes_block(copy, pred_block);
			}

			set_irn_link(node, copy);
		}

		/* fix data-flow (and reconstruct SSA if needed) */
		foreach_out_edge(cond_block, edge) {
			ir_node *vals[2];
			ir_node *blocks[2];

			ir_node *node = get_edge_src_irn(edge);
			assert(get_nodes_block(node) == cond_block);
			if (is_Proj(node) && get_irn_mode(node) == mode_X)
				continue;

			blocks[0] = cond_block;
			vals[0] = node;
			blocks[1] = pred_block;
			vals[1] = get_irn_link(node);
			construct_ssa(blocks, vals, 2);
		}

		/* shorten phis */
		foreach_out_edge_safe(cond_block, edge, next) {
			ir_node *node = get_edge_src_irn(edge);

			if(is_Phi(node))
				remove_pred(node, j);
		}

		changed = 1;
		remove_pred(cond_block, j);
#if 0
		if (remove_pred(cond_block, j))
			break;
		// the phi got shortened
		n_phi--;
		j--;
#endif
		break;
	}

	if (changed) {
		irg = get_irn_irg(block);
		set_irg_doms_inconsistent(irg);
		set_irg_extblk_inconsistent(irg);
		set_irg_loopinfo_inconsistent(irg);
	}
	return changed;
}


/**
 * Block-walker:
 */
static void cond_eval(ir_node* block, void* env)
{
	int n_block;
	int i;

	n_block = get_Block_n_cfgpreds(block);
	for (i = 0; i < n_block; i++) {
		ir_node* pred;
		ir_node* projx;
		ir_node* cond;
		ir_node* cmp;
		ir_node* left;
		ir_node* right;
		ir_node* cond_block;
		pn_Cmp pnc;

		pred = get_Block_cfgpred(block, i);
		if (!is_Proj(pred)) continue;
		projx = pred;

		pred = get_Proj_pred(projx);
		if (!is_Cond(pred)) continue;
		cond = pred;

		pred = get_Cond_selector(cond);
		assert(is_Proj(pred));
		// TODO handle switches
		if (get_irn_mode(pred) != mode_b) continue;
		pnc = get_Proj_proj(pred);

		cmp = get_Proj_pred(pred);
		assert(is_Cmp(cmp));

		left  = get_Cmp_left(cmp);
		right = get_Cmp_right(cmp);
		assert(get_irn_mode(left) == get_irn_mode(right));

		if (get_Proj_proj(projx) == 0) {
			pnc = get_negated_pnc(pnc, get_irn_mode(left));
		}

#if 0 // TODO implement
		if (is_Const(left) && is_Const(right)) {
			if (!handle_const_const()) {
				n_block--;
				i--;
			}
			continue;
		}
#endif
		cond_block = get_nodes_block(cond);
		if (is_Phi(left) && is_Const(right)) {
			if (get_nodes_block(left) != cond_block) continue;
			handle_phi_const(block, cond_block, left, right, pnc);
			continue;
		}
		if (is_Const(left) && is_Phi(right)) {
			if (get_nodes_block(right) != cond_block) continue;
			handle_phi_const(block, cond_block, right, left, get_inversed_pnc(pnc));
			continue;
		}
#if 0
		if (is_Phi(left) && is_Phi(right)) {
			// TODO implement
		}
#endif
	}
}


void opt_cond_eval(ir_graph* irg)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.condeval");
	firm_dbg_set_mask(dbg, SET_LEVEL_5);

	DB((dbg, LEVEL_1, "===> Performing condition evaluation on %+F\n", irg));

	if(edges_activated(irg))
		edges_deactivate(irg);
	edges_assure(irg);
	remove_critical_cf_edges(irg);

	normalize_proj_nodes(irg);

	irg_block_walk_graph(irg, cond_eval, NULL, NULL);
#if 0
	irg_block_walk_graph(irg, NULL, cond_eval, NULL);
	irg_block_walk_graph(irg, NULL, cond_eval, NULL);
	irg_block_walk_graph(irg, NULL, cond_eval, NULL);
#endif
}
