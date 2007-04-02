/*
 * Project:     libFIRM
 * File name:   ir/opt/ifconv.c
 * Purpose:     If conversion
 * Author:      Sebastian Hack.
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#if 1

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <assert.h>

#include "obst.h"
#include "irnode_t.h"
#include "cdep.h"
#include "ircons.h"
#include "ifconv.h"
#include "irdom.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irtools.h"
#include "return.h"
#include "array.h"
#include "xmalloc.h"

// debug
#include "irdump.h"
#include "debug.h"

DEBUG_ONLY(firm_dbg_module_t *dbg);

/** allow every Psi to be created. */
static int default_allow_ifconv(ir_node *sel, ir_node* phi_list, int i, int j)
{
	return 1;
}

/**
 * Default options.
 */
static const opt_if_conv_info_t default_info = {
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

#define get_block_blockinfo(block) ((block_info *)get_irn_link(block))

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
	 * therefore not moved
	 */
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
	for (k = 0; k < i; ++k) ins[l++] = get_irn_n(node, k);
	for (++k; k < j; ++k) ins[l++] = get_irn_n(node, k);
	for (++k; k < arity; ++k) ins[l++] = get_irn_n(node, k);
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
	int arity;
	int i;
	opt_if_conv_info_t *opt_info = env;

	/* Bail out, if there are no Phis at all */
	if (get_block_blockinfo(block)->phi == NULL) return;

restart:
	arity = get_irn_arity(block);
	for (i = 0; i < arity; ++i) {
		ir_node* pred;
		cdep* cdep;

		pred = get_nodes_block(get_irn_n(block, i));
		for (cdep = find_cdep(pred); cdep != NULL; cdep = cdep->next) {
			const ir_node* dependency = cdep->node;
			ir_node* projx0 = walk_to_projx(pred, dependency);
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
				ir_node* vals[2];
				ir_node* psi = NULL;
				ir_node* psi_block;
				ir_node* phi;

				pred = get_nodes_block(get_irn_n(block, j));

				if (!is_cdep_on(pred, dependency)) continue;

				projx1 = walk_to_projx(pred, dependency);

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
				do {
					ir_node* val_i = get_irn_n(phi, i);
					ir_node* val_j = get_irn_n(phi, j);

					if (val_i == val_j) {
						psi = val_i;
						DB((dbg, LEVEL_2,  "Generating no psi, because both values are equal\n"));
					} else {
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

						psi = new_r_Psi(
							current_ir_graph, psi_block, 1, conds, vals, get_irn_mode(phi)
						);
						DB((dbg, LEVEL_2, "Generating %+F for %+F\n", psi, phi));
					}

					/* only exchange if we have a Psi */
					if (arity == 2) {
						exchange(phi, psi);
					} else {
						rewire(phi, i, j, psi);
					}

					phi = get_irn_link(phi);
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
	if (is_Phi(node)) {
		ir_node *block = get_nodes_block(node);
		block_info *bi = get_block_blockinfo(block);

		set_irn_link(node, bi->phi);
		bi->phi = node;
	}
	else {
		if (is_no_Block(node) && get_irn_pinned(node) == op_pin_state_pinned) {
			/*
			 * Ignore control flow nodes, these will be removed.
			 * This ignores Raise. That is surely bad. FIXME.
			 */
			if (! is_cfop(node)) {
				ir_node *block = get_nodes_block(node);
				block_info *bi = get_block_blockinfo(block);

				DB((dbg, LEVEL_2, "Node %+F in block %+F is unmovable\n", node, block));
				bi->has_pinned = 1;
			}
		}
	}
}


/*
 * Transform multiple cascaded Psis into one Psi
 */
static ir_node* fold_psi(ir_node* psi)
{
	int arity = get_Psi_n_conds(psi);
	int new_arity = 0;
	int i;
	ir_node* n;
	ir_node** conds;
	ir_node** vals;
	int j;
	int k;
	int a;
	ir_node* new_psi;

	for (i = 0; i < arity; ++i) {
		n = get_Psi_val(psi, i);
		if (get_irn_op(n) == op_Psi) {
			new_arity += get_Psi_n_conds(n) + 1;
		} else {
			++new_arity;
		}
	}
	n = get_Psi_default(psi);
	if (get_irn_op(n) == op_Psi) {
		new_arity += get_Psi_n_conds(n);
	}

	if (arity == new_arity) return psi; // no attached Psis found
	DB((dbg, LEVEL_1, "Folding %+F from %d to %d conds\n", psi, arity, new_arity));

	NEW_ARR_A(ir_node *, conds, new_arity);
	NEW_ARR_A(ir_node *, vals, new_arity + 1);
	j = 0;
	for (i = 0; i < arity; ++i) {
		ir_node* c = get_Psi_cond(psi, i);

		n = get_Psi_val(psi, i);
		if (get_irn_op(n) == op_Psi) {
			a = get_Psi_n_conds(n);
			for (k = 0; k < a; ++k) {
				conds[j] = new_r_And(
					current_ir_graph, get_nodes_block(psi),
					c, get_Psi_cond(n, k), mode_b
				);
				vals[j] = get_Psi_val(n, k);
				++j;
			}
			conds[j] = c;
			vals[j] = get_Psi_default(n);
		} else {
			conds[j] = c;
			vals[j] = n;
		}
		++j;
	}
	n = get_Psi_default(psi);
	if (get_irn_op(n) == op_Psi) {
		a = get_Psi_n_conds(n);
		for (k = 0; k < a; ++k) {
			conds[j] = get_Psi_cond(n, k);
			vals[j] = get_Psi_val(n, k);
			++j;
		}
		vals[j] = get_Psi_default(n);
	} else {
		vals[j] = n;
	}
	assert(j == new_arity);
	new_psi = new_r_Psi(
		current_ir_graph, get_nodes_block(psi),
		new_arity, conds, vals, get_irn_mode(psi)
	);
	DB((dbg, LEVEL_1, "Folded %+F into new %+F\n", psi, new_psi));
	exchange(psi, new_psi);
	return new_psi;
}


/*
 * Merge consecutive psi inputs if the data inputs are the same
 */
static ir_node* meld_psi(ir_node* psi)
{
	int arity = get_Psi_n_conds(psi);
	int new_arity;
	ir_node** conds;
	ir_node** vals;
	ir_node* cond;
	ir_node* val;
	int i;
	int j;
	ir_node* new_psi;

	new_arity = 1;
	val = get_Psi_val(psi, 0);
	DB((dbg, LEVEL_1, "Pred  0 of %+F is %+F\n", psi, val));
	for (i = 1; i < arity; ++i) {
		ir_node* v = get_Psi_val(psi, i);
		DB((dbg, LEVEL_1, "Pred %2d of %+F is %+F\n", i, psi, v));
		if (val != v) {
			val = v;
			++new_arity;
		}
	}
	DB((dbg, LEVEL_1, "Default of %+F is %+F\n", psi, get_Psi_default(psi)));
	if (val == get_Psi_default(psi)) --new_arity;

	DB((dbg, LEVEL_1, "Melding Psi %+F from %d conds to %d\n", psi, arity, new_arity));

	if (new_arity == arity) return psi;

	/* If all data inputs of the Psi are equal, exchange the Psi with that value */
	if (new_arity == 0) {
		exchange(psi, val);
		return val;
	}

	NEW_ARR_A(ir_node *, conds, new_arity);
	NEW_ARR_A(ir_node *, vals, new_arity + 1);
	cond = get_Psi_cond(psi, 0);
	val = get_Psi_val(psi, 0);
	j = 0;
	for (i = 1; i < arity; ++i) {
		ir_node* v = get_Psi_val(psi, i);

		if (v == val) {
			cond = new_r_Or(
				current_ir_graph, get_nodes_block(psi),
				cond, get_Psi_cond(psi, i), mode_b
			);
		} else {
			conds[j] = cond;
			vals[j] = val;
			++j;
			val = v;
		}
	}
	if (val != get_Psi_default(psi)) {
		conds[j] = cond;
		vals[j] = val;
		++j;
	}
	vals[j] = get_Psi_default(psi);
	assert(j == new_arity);
	new_psi = new_r_Psi(
		current_ir_graph, get_nodes_block(psi),
		new_arity, conds, vals, get_irn_mode(psi)
	);
	DB((dbg, LEVEL_1, "Molded %+F into %+F\n", psi, new_psi));
	exchange(psi, new_psi);
	return new_psi;
}


/**
 * Split a Psi with multiple conditions into multiple Psis with one condtition
 * each
 */
static ir_node* split_psi(ir_node* psi)
{
	int arity = get_Psi_n_conds(psi);
	ir_mode* mode;
	ir_node* block;
	ir_node* rval;
	int i;

	if (arity == 1) return psi;

	mode = get_irn_mode(psi);
	block = get_nodes_block(psi);
	rval = get_Psi_default(psi);
	for (i = arity - 1; i >= 0; --i) {
		ir_node* conds[1];
		ir_node* vals[2];

		conds[0] = get_Psi_cond(psi, i);
		vals[0] = get_Psi_val(psi, i);
		vals[1] = rval;
		rval = new_r_Psi(
			current_ir_graph, block, 1, conds, vals, mode
		);
	}
	exchange(psi, rval);
	return rval;
}


static void optimise_psis(ir_node* node, void* env)
{
	if (get_irn_op(node) != op_Psi) return;
#if 1
	node = fold_psi(node);
#endif
#if 1
	node = meld_psi(node);
#endif
#if 1
	node = split_psi(node);
#endif
}


void opt_if_conv(ir_graph *irg, const opt_if_conv_info_t *params)
{
	struct obstack obst;
	opt_if_conv_info_t p;

	if (! get_opt_if_conversion())
		return;

	/* get the parameters */
	if (params)
		memcpy(&p, params, sizeof(p));
	else
		memcpy(&p, &default_info, sizeof(p));

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

	irg_walk_graph(irg, NULL, optimise_psis, NULL);

	obstack_free(&obst, NULL);

	free_dom(irg);
	free_cdep(irg);
}

#else

/**
 * @file ifconv.c
 * If conversion.
 * Make Mux nodes from Conds where it its possible.
 * @author Sebastian Hack
 * @date 4.2.2005
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#include "irgraph_t.h"
#include "irnode_t.h"
#include "irgwalk.h"
#include "iropt_t.h"
#include "irgmod.h"
#include "irmode_t.h"
#include "ircons_t.h"
#include "irdom_t.h"
#include "irgwalk.h"

#include "ifconv.h"
#include "irflag_t.h"

#include "irprintf.h"
#include "debug.h"
#include "obst.h"
#include "set.h"
#include "bitset.h"
#include "bitfiddle.h"
#include "irhooks.h"
#include "return.h"

#define MAX_DEPTH   4

/**
 * check, if a node is const and return its tarval or
 * return a default tarval.
 * @param cnst The node whose tarval to get.
 * @param or The alternative tarval, if the node is no Const.
 * @return The tarval of @p cnst, if the node is Const, @p otherwise.
 */
static tarval *get_value_or(ir_node *cnst, tarval *or)
{
  return get_irn_op(cnst) == op_Const ? get_Const_tarval(cnst) : or;
}


/**
 * Try to optimize nested muxes into a dis- or conjunction
 * of two muxes.
 * @param mux The parent mux, which has muxes as operands.
 * @return The replacement node for this mux. If the optimization is
 * successful, this might be an And or Or node, if not, its the mux
 * himself.
 */
static ir_node *optimize_mux_chain(ir_node *mux)
{
  int i;
  ir_node *res;
  ir_node *ops[2];
  ir_mode *mode = get_irn_mode(mux);
  tarval *null;
  tarval *minus_one;

  /*
   * If we have no mux, or its mode is not integer, we
   * can return.
   */
  if(get_irn_op(mux) != op_Mux || !mode_is_int(mode))
    return mux;

  res = mux;
  null = get_mode_null(mode);
  minus_one = tarval_sub(null, get_tarval_one(mode));

  ops[0] = get_Mux_false(mux);
  ops[1] = get_Mux_true(mux);

  for(i = 0; i < 2; ++i) {
    ir_node *a, *b, *d;
    tarval *tva, *tvb, *tvd;
    ir_node *child_mux;

    /*
     * A mux operand at the first position can be factored
     * out, if the operands fulfill several conditions:
     *
     * mux(c1, mux(c2, a, b), d)
     *
     * This can be made into:
     * 1) mux(c1, 0, d) | mux(c2, a, b)
     *    if a | d == d and b | d == d
     *
     * 2) mux(c1, -1, d) & mux(c2, a, b)
     *    if a & d == d and a & b == b
     */
    if(get_irn_op(ops[i]) == op_Mux) {

      child_mux = ops[i];
      a = get_Mux_false(child_mux);
      b = get_Mux_true(child_mux);
      d = ops[1 - i];

      /* Try the or stuff */
      tva = get_value_or(a, minus_one);
      tvb = get_value_or(b, minus_one);
      tvd = get_value_or(d, null);

      if(tarval_cmp(tarval_or(tva, tvd), tvd) == pn_Cmp_Eq
          && tarval_cmp(tarval_or(tvb, tvd), tvd) == pn_Cmp_Eq) {

        ops[i] = new_Const(mode, null);
        res = new_r_Or(current_ir_graph, get_nodes_block(mux),
            mux, child_mux, mode);
        break;
      }

      /* If the or didn't go, try the and stuff */
      tva = get_value_or(a, null);
      tvb = get_value_or(b, null);
      tvd = get_value_or(d, minus_one);

      if(tarval_cmp(tarval_and(tva, tvd), tvd) == pn_Cmp_Eq
          && tarval_cmp(tarval_and(tvb, tvd), tvd) == pn_Cmp_Eq) {

        ops[i] = new_Const(mode, minus_one);
        res = new_r_And(current_ir_graph, get_nodes_block(mux),
            mux, child_mux, mode);
        break;
      }
    }
  }

  /* recursively optimize nested muxes. */
  set_irn_n(mux, 1, optimize_mux_chain(ops[0]));
  set_irn_n(mux, 2, optimize_mux_chain(ops[1]));

  return res;
}


/***********************************************************
 * The If conversion itself.
 ***********************************************************/

/** allow every Mux to be created. */
static int default_allow_mux(ir_node *sel, ir_node *false_res, ir_node *true_res) {
  return 1;
}

/**
 * Default options.
 */
static const opt_if_conv_info_t default_info = {
  MAX_DEPTH,
  default_allow_mux
};

/** The debugging module. */
DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/**
 * A simple check for side effects upto an opcode of a ir node.
 * @param irn The ir node to check,
 * @return 1 if the opcode itself may produce side effects, 0 if not.
 */
static INLINE int has_side_effects(const ir_node *irn)
{
  ir_op *op = get_irn_op(irn);

  if (op == op_Cmp)
    return 0;

  return !mode_is_datab(get_irn_mode(irn));
}

/**
 * Possible failure reasons
 */
enum failure_reason_t {
  SUCCESS      = IF_RESULT_SUCCESS,
  TO_DEEP      = IF_RESULT_TOO_DEEP,
  SIDE_EFFECTS = IF_RESULT_SIDE_EFFECT,
  PHI_FOUND    = IF_RESULT_SIDE_EFFECT_PHI,
  DENIED       = IF_RESULT_DENIED
};

/**
 * Decides, if a given expression and its subexpressions
 * (to certain, also given extent) can be moved to a block.
 *
 * @param expr      The expression to examine.
 * @param block     The block where the expression should go.
 * @param depth     The current depth, passed recursively. Use 0 for
 *                  non-recursive calls.
 * @param info      The options for createing Mux nodes.
 * examined.
 *
 * @return a failure reason
 */
static int _can_move_to(ir_node *expr, ir_node *dest_block, int depth, const opt_if_conv_info_t *info)
{
  int i, n;
  int res = SUCCESS;
  ir_node *expr_block = get_nodes_block(expr);

  /*
   * If we are forced to look too deep into the expression,
   * treat it like it could not be moved.
   */
  if(depth >= info->max_depth) {
    res = TO_DEEP;
    goto end;
  }

  /*
   * If the block of the expression dominates the specified
   * destination block, it does not matter if the expression
   * has side effects or anything else. It is executed on each
   * path the destination block is reached.
   */
  if (block_dominates(expr_block, dest_block))
    goto end;

  /*
   * We cannot move phis!
   */
  if (is_Phi(expr)) {
    res = PHI_FOUND;
    goto end;
  }

  /*
   * This should be superfluous and could be converted into a assertion.
   * The destination block _must_ dominate the block of the expression,
   * else the expression could be used without its definition.
   */
  if (! block_dominates(dest_block, expr_block)) {
    res = IF_RESULT_SIDE_EFFECT;
    goto end;
  }

  /*
   * Surely, if the expression does not have a data mode, it is not
   * movable. Perhaps one should also test the floating property of
   * the opcode/node.
   */
  if (has_side_effects(expr)) {
    res = IF_RESULT_SIDE_EFFECT;
    goto end;
  }

  /*
   * If the node looks alright so far, look at its operands and
   * check them out. If one of them cannot be moved, this one
   * cannot be moved either.
   */
  for (i = 0, n = get_irn_arity(expr); i < n; ++i) {
    ir_node *op = get_irn_n(expr, i);
    int new_depth = is_Proj(op) ? depth : depth + 1;

    res = _can_move_to(op, dest_block, new_depth, info);

    if (res != SUCCESS)
      goto end;
  }

end:
  DBG((dbg, LEVEL_3, "\t\t\tcan move to %n: %d\n", expr, res));

  return res;
}

/**
 * Convenience function for _can_move_to.
 * Checks, if an expression can be moved to another block. The check can
 * be limited to a expression depth meaning if we need to crawl in
 * deeper into an expression than a given threshold to examine if
 * it can be moved, the expression is rejected and the test returns
 * false.
 *
 * @param expr       The expression to check for.
 * @param dest_block The destination block you want @p expr to be.
 * @param info       The options for createing Mux nodes.
 *
 * @return return a failure reason
 */
static INLINE int can_move_to(ir_node *expr, ir_node *dest_block, const opt_if_conv_info_t *info)
{
  return _can_move_to(expr, dest_block, 0, info);
}

/**
 * move a DAG given by a root node expr into a new block
 *
 * @param expr       the root of a dag
 * @param dest_block the destination block
 */
static void move_to(ir_node *expr, ir_node *dest_block)
{
  int i, n;
  ir_node *expr_block = get_nodes_block(expr);

  /*
   * If we reached the dominator, we are done.
   * We will never put code through the dominator
   */
  if (block_dominates(expr_block, dest_block))
    return;

  for (i = 0, n = get_irn_arity(expr); i < n; ++i)
    move_to(get_irn_n(expr, i), dest_block);

  set_nodes_block(expr, dest_block);
}

/**
 * return the common dominator of two blocks
 */
static INLINE ir_node *common_idom(ir_node *b1, ir_node *b2)
{
  if(block_dominates(b1, b2))
    return b1;
  else if(block_dominates(b2, b1))
    return b2;
  else {
    ir_node *p;

    for (p = get_Block_idom(b1); !block_dominates(p, b2); p = get_Block_idom(p));
    return p;
  }
}

/**
 * Information about a cond node.
 */
typedef struct _cond_t {
  ir_node *cond;          /**< The cond node. */
  struct list_head list;  /**< List head which is used for queuing this cond
                               into the cond bunch it belongs to. */
  unsigned is_new : 1;
  unsigned totally_covers : 1;
  struct _cond_t *link;
  long visited_nr;

  /**
   * Information about the both 'branches'
   * (true and false), the cond creates.
   */
  struct {
    int pos;         /**< Number of the predecessor of the
                          phi block by which this branch is
                          reached. It is -1, if this branch is
                          only reached through another cond. */

    struct _cond_t *masked_by; /**< If this cond's branch is only reached
                                    through another cond, we store this
                                    cond ir_node here. */
  } cases[2];
} cond_t;

/**
 * retrieve the conditional information from a Cond node
 */
static INLINE cond_t *get_cond(ir_node *irn, set *cond_set)
{
  cond_t templ;

  templ.cond = irn;
  return set_find(cond_set, &templ, sizeof(templ), HASH_PTR(templ.cond));
}


typedef void (cond_walker_t)(cond_t *cond, void *env);

static void _walk_conds(cond_t *cond, cond_walker_t *pre, cond_walker_t *post,
      long visited_nr, void *env)
{
  int i;

  if(cond->visited_nr >= visited_nr)
    return;

  cond->visited_nr = visited_nr;

  if(pre)
    pre(cond, env);

  for(i = 0; i < 2; ++i) {
    cond_t *c = cond->cases[i].masked_by;

    if(c)
      _walk_conds(c, pre, post, visited_nr, env);
  }

  if(post)
    post(cond, env);
}

static long cond_visited_nr = 0;

static void walk_conds(cond_t *cond, cond_walker_t *pre, cond_walker_t *post, void *env)
{
  _walk_conds(cond, pre, post, ++cond_visited_nr, env);
}

static void link_conds(cond_t *cond, void *env)
{
  cond_t **ptr = (cond_t **) env;

  cond->link = *ptr;
  *ptr = cond;
}

/**
 * Compare two conds for use in a firm set.
 * Two cond_t's are equal, if they designate the same cond node.
 * @param a A cond_t.
 * @param b Another one.
 * @param size Not used.
 * @return 0 (!) if they are equal, != 0 otherwise.
 */
static int cond_cmp(const void *a, const void *b, size_t size)
{
  const cond_t *x = a;
  const cond_t *y = b;
  return x->cond != y->cond;
}

/**
 * Information about conds which can be made to muxes.
 * Instances of this struct are attached to the link field of
 * blocks in which phis are located.
 */
typedef struct _cond_info_t {
  struct list_head list;      /**< Used to list all of these structs per class. */

  struct list_head roots;     /**< A list of non-depending Conds. Two Conds are
                                independent, if it's not possible not reach one from the
                                other (all Conds in this list have to dominate the
                                block this struct is attached to). */

  ir_node *first_phi;         /**< The first phi node this cond info was made for. */
  set *cond_set;              /**< A set of all dominating reachable Conds. */
} cond_info_t;

/**
 * @see find_conds.
 */
static void _find_conds(ir_node *irn, unsigned long visited_nr,
    ir_node *dominator, cond_t *masked_by, int pos, int depth, cond_info_t *ci)
{
  ir_node *block;
  int saw_select_cond = 0;

  block = get_nodes_block(irn);

  /*
   * Only check this block if it is dominated by the specified
   * dominator or it has not been visited yet.
   */
  if (block_dominates(dominator, block) && get_Block_block_visited(block) < visited_nr) {
    cond_t *res = masked_by;
    int i, n;

    /* check, if we're on a ProjX
     *
     * Further, the ProjX/Cond block must dominate the base block
     * (the block with the phi in it), otherwise, the Cond
     * is not affecting the phi so that a mux can be inserted.
     */
    if(is_Proj(irn) && get_irn_mode(irn) == mode_X) {

      int proj = get_Proj_proj(irn);
      ir_node *cond = get_Proj_pred(irn);

      /* true, if the mode is a mode_b cond _NO_ switch cond */
      int is_modeb_cond = get_irn_opcode(cond) == iro_Cond
        && get_irn_mode(get_Cond_selector(cond)) == mode_b;

      saw_select_cond = !is_modeb_cond;

      /* Check, if the pred of the proj is a Cond
       * with a Projb as selector.
       */
      if(is_modeb_cond) {
        cond_t c;

        memset(&c, 0, sizeof(c));
        c.cond = cond;
        c.is_new = 1;
        c.cases[0].pos = -1;
        c.cases[1].pos = -1;

        /* get or insert the cond info into the set. */
        res = set_insert(ci->cond_set, &c, sizeof(c), HASH_PTR(cond));

        /*
         * If this cond is already masked by the masked_by cond
         * return immediately, since we don't have anything to add.
         */
        if(masked_by && res->cases[proj].masked_by == masked_by)
          return;

        if(res->is_new) {
          res->is_new = 0;
          list_add(&res->list, &ci->roots);
        }

        /*
         * Set masked by (either NULL or another cond node.
         * If this cond is truly masked by another one, set
         * the position of the actually investigated branch
         * to -1. Since the cond is masked by another one,
         * there could be more ways from the start block
         * to this branch, so we choose -1.
         */
        res->cases[proj].masked_by = masked_by;

        if(!masked_by)
          res->cases[proj].pos = pos;

        /*
         * Since the masked_by nodes masks a cond, remove it from the
         * root list of the conf trees.
         */
        else {
          assert(res->cases[proj].pos < 0);
          list_del_init(&masked_by->list);
        }

        DBG((dbg, LEVEL_2, "%n (%s branch) "
              "for pos %d in block %n reached by %n\n",
              cond, proj ? "true" : "false", pos,
              block, masked_by ? masked_by->cond : NULL));
      }
    }

    if(get_Block_block_visited(block) < visited_nr && !saw_select_cond) {

      set_Block_block_visited(block, visited_nr);

      /* Search recursively from this cond. */
      for(i = 0, n = get_irn_arity(block); i < n; ++i) {
        ir_node *pred = get_irn_n(block, i);

        /*
         * If the depth is 0 (the first recursion), we set the pos to
         * the current viewed predecessor, else we adopt the position
         * as given by the caller. We also increase the depth for the
         * recursively called functions.
         */
        _find_conds(pred, visited_nr, dominator, res, pos, depth + (res != masked_by), ci);
      }
    }
  }
}


/**
 * A convenience function for _find_conds.
 * It sets some parameters needed for recursion to appropriate start
 * values. Always use this function.
 *
 * @param irn   The node to start looking for Conds from. This might
 *              be the phi node we are investigating.
 * @param conds The set to record the found Conds in.
 */
static INLINE void find_conds(ir_node *irn, cond_info_t *ci)
{
  int i, n;
  unsigned long visited_nr;
  ir_node *block = get_nodes_block(irn);
  ir_node *dom = get_Block_idom(block);

  for(i = 0, n = get_irn_arity(block); i < n; ++i) {
    ir_node *pred = get_irn_n(block, i);

    inc_irg_block_visited(current_ir_graph);
    visited_nr = get_irg_block_visited(current_ir_graph);
    set_Block_block_visited(block, visited_nr);

    DBG((dbg, LEVEL_2, "find conds at pred %d (%n) and idom %n\n", i, pred, dom));
    _find_conds(pred, visited_nr, dom, NULL, i, 0, ci);
  }
}

/**
 * Make the mux for a given cond.
 *
 * @param phi       The phi node which shall be replaced by a mux.
 * @param dom       The block where the muxes shall be placed.
 * @param cond      The cond information.
 * @param info      The options for createing Mux nodes.
 * @return The mux node made for this cond.
 */
static ir_node *make_mux_on_demand(ir_node *phi, ir_node *dom, cond_t *cond,
    const opt_if_conv_info_t *info, ir_node **mux, bitset_t *positions,
    int *muxes_made, long visited_nr)
{
  int i, can_move[2];
  ir_node *projb = get_Cond_selector(cond->cond);
  ir_node *bl = get_nodes_block(cond->cond);
  ir_node *operands[2];
  int set[2];

  cond->visited_nr = visited_nr;
  DBG((dbg, LEVEL_2, "%n\n", cond->cond));
  for(i = 0; i < 2; ++i) {
    cond_t *masked_by = cond->cases[i].masked_by;
    int pos = cond->cases[i].pos;

    operands[i] = NULL;
    set[i] = -1;

    /*
     * If this Cond branch is masked by another cond, make the mux
     * for that Cond first, since the Mux for this cond takes
     * it as an operand.
     */
    if(masked_by) {
      assert(pos < 0);
      DBG((dbg, LEVEL_2, "\tmasked by: %n\n", masked_by->cond));
      if(masked_by->visited_nr < visited_nr)
        operands[i] = make_mux_on_demand(phi, dom, masked_by, info, mux, positions, muxes_made, visited_nr);
    }

    /*
     * If this cond branch is not masked by another cond, take
     * the corresponding phi operand as an operand to the mux.
     */
    else if(pos >= 0) {
      operands[i] = get_irn_n(phi, pos);
      set[i] = pos;
    }
  }

  /*
   * Move the operands to the dominator block if the cond
   * made sense. Some Conds found are not suitable for making a mux
   * out of them, since one of their branches cannot be reached from
   * the phi block. In that case we do not make a mux and return NULL.
   */
  if(operands[0] && operands[1]) {
    if (operands[0] == operands[1]) {
      /* there is no gain in using mux in this case, as
         it will be optimized away. We will NOT move the
         content of the blocks either
        */
      for (i = 0; i < 2; ++i)
        if(set[i] >= 0)
          bitset_set(positions, set[i]);

      *mux = operands[0];
      return *mux;
    }

    can_move[0] = can_move_to(operands[0], bl, info);
    can_move[1] = can_move_to(operands[1], bl, info);

    if (can_move[0] == SUCCESS && can_move[1] == SUCCESS) {
      if (info->allow_mux(projb, operands[0], operands[1])) {
        move_to(operands[0], bl);
        move_to(operands[1], bl);

        /* Make the mux. */
        *mux = new_r_Mux(current_ir_graph, bl, projb,
            operands[0], operands[1], get_irn_mode(operands[0]));

        *muxes_made += 1;

        DBG((dbg, LEVEL_2, "\t%n(%n, %n, %n)[%d, %d]\n",
              *mux, projb, operands[0], operands[1], set[0], set[1]));

        for(i = 0; i < 2; ++i)
          if(set[i] >= 0) {
            bitset_set(positions, set[i]);

            /* we have done one */
            hook_if_conversion(current_ir_graph, phi, set[i], *mux, IF_RESULT_SUCCESS);
          }
      }
      else {
        hook_if_conversion(current_ir_graph, phi, set[i], *mux, IF_RESULT_DENIED);
      }
    }
    else {
      if(can_move[0] != SUCCESS)
        hook_if_conversion(current_ir_graph, phi, set[0], NULL, can_move[0]);
      if(can_move[1] != SUCCESS)
        hook_if_conversion(current_ir_graph, phi, set[1], NULL, can_move[1]);
    }
  }
  else {
    if(operands[0])
      hook_if_conversion(current_ir_graph, phi, set[0], NULL, IF_RESULT_BAD_CF);
    if(operands[1])
      hook_if_conversion(current_ir_graph, phi, set[1], NULL, IF_RESULT_BAD_CF);
  }

  return *mux;
}

typedef struct _phi_info_t {
  struct list_head list;
  cond_info_t *cond_info;
  ir_node *irn;
} phi_info_t;


/**
 * Examine a phi node if it can be replaced by some muxes.
 * @param irn A phi node.
 * @param info Parameters for the if conversion algorithm.
 */
static int check_out_phi(phi_info_t *phi_info, const opt_if_conv_info_t *info)
{
  ir_node *irn = phi_info->irn;
  ir_node *block, *nw;
  cond_info_t *cond_info = phi_info->cond_info;
  cond_t *cond;
  int i, arity;
  int muxes_made = 0;
  bitset_t *positions;

  block = get_nodes_block(irn);
  arity = get_irn_arity(irn);
  positions = bitset_alloca(arity);

  assert(is_Phi(irn));
  assert(get_irn_arity(irn) == get_irn_arity(block));
  assert(arity > 0);

  DBG((dbg, LEVEL_2, "phi candidate: %n\n", irn));

  list_for_each_entry(cond_t, cond, &cond_info->roots, list) {
    ir_node *cidom = block;
    ir_node *mux = NULL;
    cond_t *p, *head = NULL;
    long pos;

    bitset_clear_all(positions);

    DBG((dbg, LEVEL_2, "\tcond root: %n\n", cond->cond));
    /*
     * Link all conds which are in the subtree of
     * the current cond in the list together.
     */
    walk_conds(cond, link_conds, NULL, &head);

    cidom = block;
    for(p = head; p; p = p->link) {
      for(i = 0; i < 2; ++i) {
        int pos = p->cases[i].pos;
        if(pos != -1)
          cidom = common_idom(cidom, get_nodes_block(get_irn_n(block, pos)));
      }
    }

    DBG((dbg, LEVEL_2, "\tcommon idom: %n\n", cidom));
    make_mux_on_demand(irn, cidom, cond, info, &mux, positions, &muxes_made, ++cond_visited_nr);

    if(mux) {
      bitset_foreach(positions, pos)
        set_irn_n(irn, (int) pos, mux);
    }
  }

  /*
   * optimize the phi away. This can anable further runs of this
   * function. Look at _can_move. phis cannot be moved there.
   */
  nw = optimize_in_place_2(irn);
  if(nw != irn)
    exchange(irn, nw);

  return muxes_made;
}

typedef struct _cond_walk_info_t {
  struct obstack *obst;
  struct list_head cond_info_head;
  struct list_head phi_head;
} cond_walk_info_t;


static void annotate_cond_info_pre(ir_node *irn, void *data)
{
  set_irn_link(irn, NULL);
}

static void annotate_cond_info_post(ir_node *irn, void *data)
{
  cond_walk_info_t *cwi = data;

  /*
   * Check, if the node is a phi
   * we then compute a set of conds which are reachable from this
   * phi's block up to its dominator.
   * The set is attached to the blocks link field.
   */
  if(is_Phi(irn) && mode_is_datab(get_irn_mode(irn))) {
    ir_node *block = get_nodes_block(irn);

    cond_info_t *ci = get_irn_link(block);

    /* If the set is not yet computed, do it now. */
    if(!ci) {
      ci = obstack_alloc(cwi->obst, sizeof(*ci));
      ci->cond_set = new_set(cond_cmp, log2_ceil(get_irn_arity(block)));
      ci->first_phi = irn;

      INIT_LIST_HEAD(&ci->roots);
      INIT_LIST_HEAD(&ci->list);

      /*
       * Add this cond info to the list of all cond infos
       * in this graph. This is just done to xfree the
       * set easier afterwards (we save an irg_walk_graph).
       */
      list_add(&cwi->cond_info_head, &ci->list);

      DBG((dbg, LEVEL_2, "searching conds at %n\n", irn));

      /*
       * Fill the set with conds we find on the way from
       * the block to its dominator.
       */
      find_conds(irn, ci);

      /*
       * If there where no suitable conds, delete the set
       * immediately and reset the set pointer to NULL
       */
      if(set_count(ci->cond_set) == 0) {
        del_set(ci->cond_set);
        list_del(&ci->list);
        obstack_free(cwi->obst, ci);
        ci = NULL;
      }
    }

    else
      DBG((dbg, LEVEL_2, "conds already computed for %n (look at %n)\n", irn, ci->first_phi));

    set_irn_link(block, ci);

    if(ci) {
      phi_info_t *pi = obstack_alloc(cwi->obst, sizeof(*pi));
      pi->irn = irn;
      pi->cond_info = ci;
      INIT_LIST_HEAD(&pi->list);
      list_add(&pi->list, &cwi->phi_head);
    }

  }
}

static void dump_conds(cond_t *cond, void *env)
{
  int i;
  FILE *f = env;

  ir_fprintf(f, "node:{title:\"n%p\" label:\"%n(%d, %d)\n%n\"}\n",
      cond, cond->cond, cond->cases[0].pos, cond->cases[1].pos,
      get_nodes_block(cond->cond));

  for(i = 0; i < 2; ++i)
    if(cond->cases[i].masked_by)
      ir_fprintf(f, "edge:{sourcename:\"n%p\" targetname:\"n%p\" label:\"%d\"}\n",
          cond, cond->cases[i].masked_by, i);
}

static void vcg_dump_conds(ir_graph *irg, cond_walk_info_t *cwi)
{
  char buf[512];
  FILE *f;

  snprintf(buf, sizeof(buf), "%s-conds.vcg", get_entity_name(get_irg_entity(irg)));

  if((f = fopen(buf, "wt")) != NULL) {
    cond_info_t *ci;
    phi_info_t *phi;
    cond_t *cond;

    ir_fprintf(f, "graph:{\ndisplay_edge_labels:yes\n");
    list_for_each_entry(cond_info_t, ci, &cwi->cond_info_head, list) {
      ir_fprintf(f, "node:{title:\"n%p\" label:\"cond info\"}\n", ci);
      list_for_each_entry(cond_t, cond, &ci->roots, list) {
        walk_conds(cond, NULL, dump_conds, f);
        ir_fprintf(f, "edge:{sourcename:\"n%p\" targetname:\"n%p\"}\n", ci, cond);
      }
    }

    list_for_each_entry(phi_info_t, phi, &cwi->phi_head, list) {
      ir_fprintf(f, "node:{title:\"n%p\" label:\"%n\n%n\"}\n",
          phi->irn, phi->irn, get_nodes_block(phi->irn));
      ir_fprintf(f, "edge:{sourcename:\"n%p\" targetname:\"n%p\"}\n", phi->irn, phi->cond_info);
    }
    fprintf(f, "}\n");
  }
}

void opt_if_conv(ir_graph *irg, const opt_if_conv_info_t *params)
{
  int muxes_made = 0;
  struct obstack obst;
  phi_info_t *phi_info;
  cond_info_t *cond_info;
  cond_walk_info_t cwi;

  opt_if_conv_info_t p;

  if(!get_opt_if_conversion())
    return;

  /* get the parameters */
  if (params)
    memcpy(&p, params, sizeof(p));
  else
    memcpy(&p, &default_info, sizeof(p));

  if (! p.allow_mux)
    p.allow_mux = default_info.allow_mux;

  obstack_init(&obst);

  cwi.obst = &obst;
  INIT_LIST_HEAD(&cwi.cond_info_head);
  INIT_LIST_HEAD(&cwi.phi_head);

  /* Init the debug stuff. */
  FIRM_DBG_REGISTER(dbg, "firm.opt.ifconv");
#if 0
  firm_dbg_set_mask(dbg, LEVEL_1|LEVEL_2|LEVEL_3);
#endif

  /* if-conversion works better with normalized returns */
  normalize_one_return(irg);

  /* Ensure, that the dominators are computed. */
  assure_doms(irg);

  DBG((dbg, LEVEL_1, "if conversion for irg %s(%p)\n",
        get_entity_name(get_irg_entity(irg)), irg));

  /*
   * Collect information about the conds pu the phis on an obstack.
   * It is important that phi nodes which are 'higher' (with a
   * lower dfs pre order) are in front of the obstack. Since they are
   * possibly turned in to muxes this can enable the optimization
   * of 'lower' ones.
   */
  irg_walk_graph(irg, annotate_cond_info_pre, annotate_cond_info_post, &cwi);

#if 0
  vcg_dump_conds(irg, &cwi);
#endif

  /* Process each suitable phi found. */
  list_for_each_entry(phi_info_t, phi_info, &cwi.phi_head, list) {
    DBG((dbg, LEVEL_2, "phi node %n\n", phi_info->irn));
    muxes_made += check_out_phi(phi_info, &p);
  }

  list_for_each_entry(cond_info_t, cond_info, &cwi.cond_info_head, list) {
    del_set(cond_info->cond_set);
  }

  DBG((dbg, LEVEL_1, "muxes made: %d\n", muxes_made));

  obstack_free(&obst, NULL);
}

#endif
