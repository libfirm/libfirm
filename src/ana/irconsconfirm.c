/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Construction of Confirm nodes
 * @author   Michael Beck
 * @date     6.2005
 */
#include "irconsconfirm.h"

#include "array.h"
#include "debug.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "irflag.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "iropt_dbg.h"
#include "irtools.h"

/**
 * Walker environment.
 */
typedef struct env_t {
	bool optimize;          /**< Do other optimizations as well as inserting Confirms. */
	unsigned num_confirms;  /**< Number of inserted Confirm nodes. */
	unsigned num_consts;    /**< Number of constants placed. */
	unsigned num_eq;        /**< Number of equalities placed. */
	unsigned num_non_null;  /**< Number of non-null Confirms. */
} env_t;

/** The debug handle. */
DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/**
 * Return the effective use block of a node and its predecessor on
 * position pos.
 *
 * @param node  the node
 * @param pos   the position of the used input
 *
 * This handles correctly Phi nodes.
 */
static ir_node *get_effective_use_block(ir_node *node, int pos)
{
	if (is_Phi(node)) {
		/* the effective use of a Phi argument is in its predecessor block */
		node = get_nodes_block(node);
		return get_Block_cfgpred_block(node, pos);
	}
	return get_nodes_block(node);
}

static ir_node *get_case_value(ir_node *switchn, unsigned pn)
{
	ir_graph              *irg       = get_irn_irg(switchn);
	const ir_switch_table *table     = get_Switch_table(switchn);
	size_t                 n_entries = ir_switch_table_get_n_entries(table);
	ir_tarval             *val       = NULL;
	for (size_t e = 0; e < n_entries; ++e) {
		const ir_switch_table_entry *entry
			= ir_switch_table_get_entry_const(table, e);
		if (entry->pn != pn)
			continue;
		/* multiple matching entries gets too complicated for a single
		 * Confirm */
		if (val != NULL)
			return NULL;
		/* case ranges are too complicated too */
		if (entry->min != entry->max)
			return NULL;
		val = entry->min;
	}
	assert(val != NULL);
	return new_r_Const(irg, val);
}

/**
 * Handle a CASE-branch.
 *
 * Branch labels are a simple case. We can replace the value
 * by a Const with the branch label.
 */
static void handle_case(ir_node *block, ir_node *switchn, unsigned pn, env_t *env)
{
	if (!env->optimize) {
		return;
	}

	ir_node *c        = NULL;
	ir_node *selector = get_Switch_selector(switchn);

	/* we can't do useful things with the default label */
	if (pn == pn_Switch_default)
		return;

	foreach_out_edge_safe(selector, edge) {
		ir_node *succ = get_edge_src_irn(edge);
		int      pos  = get_edge_src_pos(edge);
		ir_node *blk  = get_effective_use_block(succ, pos);

		if (block_dominates(block, blk)) {
			/* Ok, we found a user of irn that is placed
			 * in a block dominated by the branch block.
			 * We can replace the input with the Constant
			 * branch label. */
			if (c == NULL) {
				c = get_case_value(switchn, pn);
				if (c == NULL)
					return;
			}

			set_irn_n(succ, pos, c);
			DBG_OPT_CONFIRM_C(selector, c);
			DB((dbg, LEVEL_2, "Replacing input %d of node %+F with %+F\n", pos, succ, c));

			env->num_consts += 1;
		}
	}
}

/**
 * Handle a mode_b input of Cond nodes.
 *
 * @param block     the block which is entered by the branch
 * @param selector  the mode_b node expressing the branch condition
 * @param pnc       the true/false condition branch
 * @param env       statistical environment
 */
static void handle_modeb(ir_node *block, ir_node *selector, pn_Cond pnc, env_t *env)
{
	if (!env->optimize) {
		return;
	}

	ir_node *other_blk = NULL;
	ir_node *con       = NULL;
	ir_node *c_b       = NULL;
	ir_node *c_o       = NULL;

	foreach_out_edge_safe(selector, edge) {
		ir_node *user     = get_edge_src_irn(edge);
		int      pos      = get_edge_src_pos(edge);
		ir_node *user_blk = get_effective_use_block(user, pos);

		if (block_dominates(block, user_blk)) {
			/*
			 * Ok, we found a usage of selector in a block
			 * dominated by the branch block.
			 * We can replace the input with true/false.
			 */
			if (con == NULL) {
				ir_graph *irg = get_irn_irg(block);
				con = new_r_Const(irg, pnc == pn_Cond_true ? tarval_b_true : tarval_b_false);
			}
			ir_node *old = get_irn_n(user, pos);
			set_irn_n(user, pos, con);
			DBG_OPT_CONFIRM_C(old, con);

			DB((dbg, LEVEL_2, "Replacing input %d of node %+F with %+F\n", pos, user, con));

			env->num_consts += 1;
		} else {
			/* get the other block */
			if (other_blk == NULL) {
				/* we have already tested, that block has only ONE Cond predecessor */
				ir_node *const cond = get_Proj_pred(get_Block_cfgpred(block, 0));
				ir_node *const proj = get_Proj_for_pn(cond, pn_Cond_false + pn_Cond_true - pnc);
				assert(proj);
				other_blk = get_edge_src_irn(get_irn_out_edge_first(proj));

				/*
				 * Note the special case here: if block is a then, there might be no else
				 * block. In that case the other_block is the user_blk itself and pred_block
				 * is the cond_block ...
				 *
				 * Best would be to introduce a block here, removing this critical edge.
				 * For some reasons I cannot repair dominance here, so I have to remove
				 * ALL critical edges...
				 * FIXME: This should not be needed if we could repair dominance ...
				 */
			}

			int n = get_Block_n_cfgpreds(user_blk);

			/*
			 * We have found a user in a non-dominated block:
			 * check, if all its block predecessors are dominated.
			 * If yes, place a Phi.
			 */
			int i;
			for (i = n; i-- > 0; ) {
				ir_node *pred_blk = get_Block_cfgpred_block(user_blk, i);

				if (!block_dominates(block, pred_blk) &&
				    !block_dominates(other_blk, pred_blk)) {
					/* can't do anything */
					break;
				}
			}
			if (i < 0 && n > 0) {
				ir_node **in = ALLOCAN(ir_node*, n);
				/* ok, ALL predecessors are either dominated by block OR other block */
				if (c_b == NULL) {
					ir_graph *irg     = get_irn_irg(block);
					ir_node  *c_true  = new_r_Const(irg, tarval_b_true);
					ir_node  *c_false = new_r_Const(irg, tarval_b_false);
					env->num_consts += 2;
					if (pnc == pn_Cond_true) {
						c_b = c_true;
						c_o = c_false;
					} else {
						c_b = c_false;
						c_o = c_true;
					}
				}
				for (i = n; i-- > 0; ) {
					ir_node *pred_blk = get_Block_cfgpred_block(user_blk, i);
					if (block_dominates(block, pred_blk))
						in[i] = c_b;
					else
						in[i] = c_o;
				}
				ir_node *phi = new_r_Phi(user_blk, n, in, mode_b);
				set_irn_n(user, pos, phi);
				env->num_eq += 1;
			}
		}
	}
}

/**
 * Handle an IF-branch.
 *
 * @param block   the block which is entered by the branch
 * @param cmp     the Cmp node expressing the branch condition
 * @param rel     the Compare relation for taking this branch
 * @param env     statistical environment
 */
static void handle_if(ir_node *block, ir_node *cmp, ir_relation rel, env_t *env)
{
	/* Beware of Bads */
	ir_node *left  = get_Cmp_left(cmp);
	ir_node *right = get_Cmp_right(cmp);
	if (is_Bad(left) || is_Bad(right))
		return;

	/* Do not create Confirm nodes for Cmp(Const, Const) constructs.
	   These are removed anyway */
	if (is_Const(left) && is_Const(right))
		return;

	/* try to place the constant on the right side for a Confirm */
	switch (get_irn_opcode(left)) {
	case iro_Address:
	case iro_Align:
	case iro_Const:
	case iro_Offset:
	case iro_Size: {
		ir_node *t = left;
		left  = right;
		right = t;

		rel = get_inversed_relation(rel);
		break;
	}

	default:
		break;
	}

	/*
	 * First case: both values are identical.
	 * replace the left one by the right (potentially const) one.
	 */
	if (env->optimize && rel == ir_relation_equal) {
		ir_node *cond_block = get_Block_cfgpred_block(block, 0);
		foreach_out_edge_safe(left, edge) {
			ir_node *user = get_edge_src_irn(edge);
			int     pos   = get_edge_src_pos(edge);
			ir_node *blk  = get_effective_use_block(user, pos);

			if (block_dominates(block, blk)) {
				/*
				 * Ok, we found a usage of left in a block
				 * dominated by the branch block.
				 * We can replace the input with right.
				 */
				set_irn_n(user, pos, right);
				DBG_OPT_CONFIRM(left, right);

				DB((dbg, LEVEL_2, "Replacing input %d of node %+F with %+F\n", pos, user, right));

				env->num_eq += 1;
			} else if (block_dominates(blk, cond_block)
			           && is_Const(right) && !get_irn_pinned(user)) {
				/*
				 * left == Const and we found a movable user of left in a
				 * dominator of the Cond block
				 */
				foreach_out_edge_safe(user, user_edge) {
					ir_node *usr_of_usr = get_edge_src_irn(user_edge);
					int      npos       = get_edge_src_pos(user_edge);
					ir_node *user_blk   = get_effective_use_block(usr_of_usr, npos);

					if (block_dominates(block, user_blk)) {
						/*
						 * The user of the user is dominated by our true/false
						 * block. So, create a copy of user WITH the constant
						 * replacing its pos'th input.
						 *
						 * This is always good for unop's and might be good
						 * for binops.
						 *
						 * If user has other user in the false/true block, code
						 * placement will move it down.
						 * If there are users in cond block or upper, we create
						 * "redundant ops", because one will have a const op,
						 * the other no const ...
						 */
						ir_node *new_op = exact_copy(user);
						set_nodes_block(new_op, block);
						set_irn_n(new_op, pos, right);
						set_irn_n(usr_of_usr, npos, new_op);
						env->num_eq += 1;
					}
				}
			}
		}
	} else { /* not ir_relation_equal cases */
		ir_node *c = NULL;

		foreach_out_edge_safe(left, edge) {
			ir_node *succ = get_edge_src_irn(edge);
			int      pos  = get_edge_src_pos(edge);
			ir_node *blk  = get_effective_use_block(succ, pos);

			if (block_dominates(block, blk)) {
				/*
				 * Ok, we found a usage of left in a block
				 * dominated by the branch block.
				 * We can replace the input with a Confirm(left, pnc, right).
				 */
				if (c == NULL)
					c = new_r_Confirm(block, left, right, rel);

				set_irn_n(succ, pos, c);
				DB((dbg, LEVEL_2, "Replacing input %d of node %+F with %+F\n", pos, succ, c));

				env->num_confirms += 1;
			}
		}

		if (!is_Const(right)) {
			/* also construct inverse Confirms */
			ir_node *rc = NULL;

			rel = get_inversed_relation(rel);
			foreach_out_edge_safe(right, edge) {
				ir_node *succ = get_edge_src_irn(edge);
				if (succ == c)
					continue;

				int      pos  = get_edge_src_pos(edge);
				ir_node *blk  = get_effective_use_block(succ, pos);

				if (block_dominates(block, blk)) {
					/*
					 * Ok, we found a usage of right in a block
					 * dominated by the branch block.
					 * We can replace the input with a Confirm(right, rel^-1, left).
					 */
					if (rc == NULL)
						rc = new_r_Confirm(block, right, left, rel);

					if (succ != rc) {
						set_irn_n(succ, pos, rc);
						DB((dbg, LEVEL_2, "Replacing input %d of node %+F with %+F\n", pos, succ, rc));
					}

					env->num_confirms += 1;
				}
			}
		}
	}
}

/**
 * Pre-block-walker: Called for every block to insert Confirm nodes
 */
static void insert_Confirm_in_block(ir_node *block, void *data)
{
	/*
	 * we can only handle blocks with only ONE control flow
	 * predecessor yet.
	 */
	if (get_Block_n_cfgpreds(block) != 1)
		return;

	ir_node *proj = get_Block_cfgpred(block, 0);
	if (!is_Proj(proj))
		return;

	env_t   *env = (env_t*)data;
	ir_node *cond = get_Proj_pred(proj);
	if (is_Switch(cond)) {
		unsigned proj_nr = get_Proj_num(proj);
		handle_case(block, cond, proj_nr, env);
	} else if (is_Cond(cond)) {
		ir_node *selector = get_Cond_selector(cond);
		handle_modeb(block, selector, (pn_Cond) get_Proj_num(proj), env);
		if (!is_Cmp(selector))
			return;

		ir_relation rel = get_Cmp_relation(selector);

		/* it's the false branch */
		if (get_Proj_num(proj) != pn_Cond_true)
			rel = get_negated_relation(rel);
		DB((dbg, LEVEL_2, "At %+F using %+F Confirm %=\n", block, selector, rel));

		handle_if(block, selector, rel, env);
	}
}

/**
 * Checks if a node is a non-null Confirm.
 */
static bool is_non_null_Confirm(const ir_node *ptr)
{
	for (;;) {
		if (!is_Confirm(ptr))
			break;
		if (get_Confirm_relation(ptr) == ir_relation_less_greater) {
			ir_node *bound = get_Confirm_bound(ptr);

			if (is_irn_null(bound))
				return true;
		}
		ptr = get_Confirm_value(ptr);
	}
	/*
	 * While an Address is not a Confirm, it is non-null
	 * anyway. This helps to reduce the number of
	 * constructed Confirms.
	 */
	if (is_Address(ptr))
		return true;
	return false;
}

/**
 * The given pointer will be dereferenced, add non-null Confirms.
 *
 * @param ptr    a node representing an address
 * @param block  the block of the dereferencing instruction
 * @param env    environment
 */
static void insert_non_null(ir_node *ptr, ir_node *block, env_t *env)
{
	ir_node *c = NULL;

	foreach_out_edge_safe(ptr, edge) {
		/* for now, we place a Confirm only in front of a Cmp */
		ir_node *succ = get_edge_src_irn(edge);
		if (!is_Cmp(succ))
			continue;

		int      pos = get_edge_src_pos(edge);
		ir_node *blk = get_effective_use_block(succ, pos);

		if (block_dominates(block, blk)) {
			/*
			 * Ok, we found a usage of ptr in a block
			 * dominated by the Load/Store block.
			 * We can replace the input with a Confirm(ptr, !=, NULL).
			 */
			if (c == NULL) {
				ir_mode  *mode = get_irn_mode(ptr);
				ir_graph *irg  = get_irn_irg(block);
				c = new_r_Const_null(irg, mode);
				c = new_r_Confirm(block, ptr, c, ir_relation_less_greater);
			}

			set_irn_n(succ, pos, c);
			DB((dbg, LEVEL_2, "Replacing input %d of node %+F with %+F\n", pos, succ, c));

			env->num_non_null += 1;
			env->num_confirms += 1;
		}
	}
}

/**
 * Pre-walker: Called for every node to insert Confirm nodes
 */
static void insert_Confirm(ir_node *node, void *data)
{
	env_t *env = (env_t*)data;
	switch (get_irn_opcode(node)) {
	case iro_Block:
		insert_Confirm_in_block(node, env);
		break;
	case iro_Load: {
		ir_node *ptr = get_Load_ptr(node);
		if (!is_non_null_Confirm(ptr))
			insert_non_null(ptr, get_nodes_block(node), env);
		break;
	}
	case iro_Store: {
		ir_node *ptr = get_Store_ptr(node);
		if (!is_non_null_Confirm(ptr))
			insert_non_null(ptr, get_nodes_block(node), env);
		break;
	}
	default:
		break;
	}
}

static void do_construct_confirms(ir_graph *irg, bool optimize)
{
	FIRM_DBG_REGISTER(dbg, "firm.ana.confirm");

	assure_irg_properties(irg,
		IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES
		| IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE
		| IR_GRAPH_PROPERTY_NO_BADS
		| IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES);

	assert(get_irg_pinned(irg) == op_pin_state_pinned);

	env_t env;
	env.optimize     = optimize;
	env.num_confirms = 0;
	env.num_consts   = 0;
	env.num_eq       = 0;
	env.num_non_null = 0;

	if (get_opt_global_null_ptr_elimination()) {
		/* do global NULL test elimination */
		irg_walk_graph(irg, insert_Confirm, NULL, &env);
	} else {
		/* now, visit all blocks and add Confirms where possible */
		irg_block_walk_graph(irg, insert_Confirm_in_block, NULL, &env);
	}

	DB((dbg, LEVEL_1, "# Confirms inserted : %u\n", env.num_confirms));
	DB((dbg, LEVEL_1, "# Const replacements: %u\n", env.num_consts));
	DB((dbg, LEVEL_1, "# node equalities   : %u\n", env.num_eq));
	DB((dbg, LEVEL_1, "# non-null Confirms : %u\n", env.num_non_null));

	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_CONTROL_FLOW);
}

void construct_confirms(ir_graph *irg)
{
	do_construct_confirms(irg, true);
}

void construct_confirms_only(ir_graph *irg)
{
	do_construct_confirms(irg, false);
}

static void remove_confirm(ir_node *n, void *env)
{
	(void)env;
	if (!is_Confirm(n))
		return;

	ir_node *value = get_Confirm_value(n);
	exchange(n, value);
}

void remove_confirms(ir_graph *irg)
{
	irg_walk_graph(irg, NULL, remove_confirm, NULL);
	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_CONTROL_FLOW);
}
