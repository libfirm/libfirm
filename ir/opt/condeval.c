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
#include "iredges.h"
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

		if (is_Block(node)) pred = get_nodes_block(pred);
		exchange(node, pred);
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
static void construct_ssa(ir_node * const *vals, int n_vals)
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
		ir_node *value_block = get_nodes_block(value);

		assert(get_irn_mode(value) == mode);

		set_irn_link(value_block, value);
		mark_irn_visited(value_block);
	}

	for(i = 0; i < n_vals; ++i) {
		const ir_edge_t *edge;
		ir_node *value = vals[i];

		foreach_out_edge(value, edge) {
			ir_node *user = get_edge_src_irn(edge);
			ir_node *user_block = get_nodes_block(user);
			ir_node *newval;

			newval = search_def_and_create_phis(user_block, mode);
			set_irn_n(user, get_edge_src_pos(edge), newval);
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
static void handle_phi_const(ir_node* block, ir_node* cond_block, ir_node* phi, ir_node* cnst, pn_Cmp pnc)
{
	tarval* tv_cnst;
	int n_phi;
	int j;

	tv_cnst = get_Const_tarval(cnst);
	n_phi = get_Phi_n_preds(phi);
	for (j = 0; j < n_phi; j++) {
		ir_node* pred;
		tarval* tv_phi;
		pn_Cmp cmp_val;

		pred = get_Phi_pred(phi, j);
		// TODO handle Phi cascades
		if (!is_Const(pred)) continue;

		tv_phi  = get_Const_tarval(pred);

		cmp_val = tarval_cmp(tv_phi, tv_cnst);
		if (cmp_val == pn_Cmp_False) continue;
		if ((cmp_val & pnc) != cmp_val) continue;

		DB((
			dbg, LEVEL_1,
			"> Found condition evaluation candidate %+F->%+F predecessor %d\n",
			block, cond_block, j
		));

#if 0 // TODO repair data flow and dominance
		add_pred(block, get_Block_cfgpred(cond_block, j));

		remove_pred(phi, j);
		if (remove_pred(cond_block, j)) break;
#endif
	}
}


/**
 * Block-walker:
 */
static void cond_eval(ir_node* block, void* env)
{
	int n_block = get_Block_n_cfgpreds(block);
	int i;

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

	remove_critical_cf_edges(irg);

	irg_block_walk_graph(irg, NULL, cond_eval, NULL);
}
