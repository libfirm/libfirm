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

/**
 * @file
 * @brief   boolean condition/controlflow optimisations
 * @author  Matthias Braun, Christoph Mallon
 * @version $Id: cfopt.c 22579 2008-10-07 14:54:04Z beck $
 */
#include "config.h"

#include <assert.h>
#include <string.h>

#include "adt/obst.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "irnode_t.h"
#include "tv.h"

typedef struct cond_pair {
	ir_node *cmp_lo;
	ir_node *cmp_hi;
	pn_Cmp   pnc_lo;
	pn_Cmp   pnc_hi;
	ir_node *proj_lo;
	ir_node *proj_hi;
	tarval  *tv_lo;
	tarval  *tv_hi;
} cond_pair;

static int find_cond_pair(ir_node *const l, ir_node *const r, cond_pair *const res)
{
	if (is_Proj(l) && is_Proj(r)) {
		ir_node *const lo = get_Proj_pred(l);
		ir_node *const ro = get_Proj_pred(r);

		if (is_Cmp(lo) && is_Cmp(ro)) {
			ir_node *const lol = get_Cmp_left(lo);
			ir_node *const lor = get_Cmp_right(lo);
			ir_node *const rol = get_Cmp_left(ro);
			ir_node *const ror = get_Cmp_right(ro);

			if(is_Const(lor) && is_Const_null(lor) && is_Const(ror) && is_Const_null(ror) && get_Proj_proj(l) == pn_Cmp_Lg && get_Proj_proj(r) == pn_Cmp_Lg) {
				ir_fprintf(stderr, "found zero zero\n");
			}

			/* TODO float */
			/* The constants shall be unequal.  Local optimisations handle the
			 * equal case */
			if (lol == rol && mode_is_int(get_irn_mode(lol)) && lor != ror && is_Const(lor) && is_Const(ror)) {
				tarval *const tv_l  = get_Const_tarval(lor);
				tarval *const tv_r  = get_Const_tarval(ror);
				pn_Cmp  const pnc_l = get_Proj_proj(l);
				pn_Cmp  const pnc_r = get_Proj_proj(r);
				pn_Cmp  const rel   = tarval_cmp(tv_l, tv_r);

				assert(rel != pn_Cmp_Eq);

				if (rel == pn_Cmp_Lt) {
					res->cmp_lo  = lo;
					res->cmp_hi  = ro;
					res->pnc_lo  = pnc_l;
					res->pnc_hi  = pnc_r;
					res->proj_lo = l;
					res->proj_hi = r;
					res->tv_lo   = tv_l;
					res->tv_hi   = tv_r;
				} else {
					assert(rel == pn_Cmp_Gt);
					res->cmp_lo  = ro;
					res->cmp_hi  = lo;
					res->pnc_lo  = pnc_r;
					res->pnc_hi  = pnc_l;
					res->proj_lo = r;
					res->proj_hi = l;
					res->tv_lo   = tv_r;
					res->tv_hi   = tv_l;
				}
				return 1;
			}
		}
	}
	return 0;
}

static ir_node *bool_and(cond_pair* const cpair)
{
	ir_node *const cmp_lo  = cpair->cmp_lo;
	ir_node *const cmp_hi  = cpair->cmp_hi;
	pn_Cmp   const pnc_lo  = cpair->pnc_lo;
	pn_Cmp   const pnc_hi  = cpair->pnc_hi;
	ir_node *const proj_lo = cpair->proj_lo;
	ir_node *const proj_hi = cpair->proj_hi;
	tarval  *const tv_lo   = cpair->tv_lo;
	tarval  *const tv_hi   = cpair->tv_hi;

	/* Beware of NaN's, we can only check for (ordered) != here (which is Lg, not Ne) */
	if ((pnc_lo == pn_Cmp_Lt || pnc_lo == pn_Cmp_Le || pnc_lo == pn_Cmp_Eq) &&
			(pnc_hi == pn_Cmp_Eq || pnc_hi == pn_Cmp_Ge || pnc_hi == pn_Cmp_Gt)) {
		/* x <|<=|== lo | x ==|>=|> hi -> false */
		ir_node *const t = new_Const(mode_b, tarval_b_false);
		return t;
	} else if ((pnc_lo == pn_Cmp_Lt || pnc_lo == pn_Cmp_Le || pnc_lo == pn_Cmp_Eq) &&
						 (pnc_hi == pn_Cmp_Lt || pnc_hi == pn_Cmp_Le || pnc_hi == pn_Cmp_Lg)) {
		/* x <|<=|== lo && x <|<=|!= hi -> x <|<=|== lo */
		return proj_lo;
	} else if ((pnc_lo == pn_Cmp_Ge || pnc_lo == pn_Cmp_Gt || pnc_lo == pn_Cmp_Lg) &&
						 (pnc_hi == pn_Cmp_Eq || pnc_hi == pn_Cmp_Ge || pnc_hi == pn_Cmp_Gt)) {
		/* x >=|>|!= lo || x ==|>=|> hi -> x ==|>=|> hi */
		return proj_hi;
	} else if (tarval_is_one(tarval_sub(tv_hi, tv_lo, NULL))) { /* lo + 1 == hi */
		if (pnc_lo == pn_Cmp_Ge && pnc_hi == pn_Cmp_Lt) {
			/* x >= c || x < c + 1 -> x == c */
			ir_graph *const irg   = current_ir_graph;
			ir_node  *const block = get_nodes_block(cmp_lo);
			ir_node  *const p = new_r_Proj(irg, block, cmp_lo, mode_b, pn_Cmp_Eq);
			return p;
		} else if (pnc_lo == pn_Cmp_Gt) {
			if (pnc_hi == pn_Cmp_Lg) {
				/* x > c || x != c + 1 -> x > c + 1 */
				ir_graph *const irg   = current_ir_graph;
				ir_node  *const block = get_nodes_block(cmp_hi);
				ir_node  *const p = new_r_Proj(irg, block, cmp_hi, mode_b, pn_Cmp_Gt);
				return p;
			} else if (pnc_hi == pn_Cmp_Lt) {
				/* x > c || x < c + 1 -> false */
				ir_node *const t = new_Const(mode_b, tarval_b_false);
				return t;
			} else if (pnc_hi == pn_Cmp_Le) {
				/* x > c || x <= c + 1 -> x != c + 1 */
				ir_graph *const irg   = current_ir_graph;
				ir_node  *const block = get_nodes_block(cmp_hi);
				ir_node  *const p = new_r_Proj(irg, block, cmp_hi, mode_b, pn_Cmp_Eq);
				return p;
			}
		} else if (pnc_lo == pn_Cmp_Lg && pnc_hi == pn_Cmp_Lt) {
			/* x != c || c < c + 1 -> x < c */
			ir_graph *const irg   = current_ir_graph;
			ir_node  *const block = get_nodes_block(cmp_lo);
			ir_node  *const p     = new_r_Proj(irg, block, cmp_lo, mode_b, pn_Cmp_Lt);
			return p;
		}
	}
	return NULL;
}

static ir_node *bool_or(cond_pair *const cpair)
{
	ir_node *const cmp_lo  = cpair->cmp_lo;
	ir_node *const cmp_hi  = cpair->cmp_hi;
	pn_Cmp   const pnc_lo  = cpair->pnc_lo;
	pn_Cmp   const pnc_hi  = cpair->pnc_hi;
	ir_node *const proj_lo = cpair->proj_lo;
	ir_node *const proj_hi = cpair->proj_hi;
	tarval  *const tv_lo   = cpair->tv_lo;
	tarval  *const tv_hi   = cpair->tv_hi;

	/* Beware of NaN's, we can only check for (ordered) != here (which is Lg, not Ne) */
	if ((pnc_lo == pn_Cmp_Ge || pnc_lo == pn_Cmp_Gt || pnc_lo == pn_Cmp_Lg) &&
			(pnc_hi == pn_Cmp_Lt || pnc_hi == pn_Cmp_Le || pnc_hi == pn_Cmp_Lg)) {
		/* x >=|>|!= lo | x <|<=|!= hi -> true */
		ir_node *const t = new_Const(mode_b, tarval_b_true);
		return t;
	} else if ((pnc_lo == pn_Cmp_Lt || pnc_lo == pn_Cmp_Le || pnc_lo == pn_Cmp_Eq) &&
						 (pnc_hi == pn_Cmp_Lt || pnc_hi == pn_Cmp_Le || pnc_hi == pn_Cmp_Lg)) {
		/* x <|<=|== lo || x <|<=|!= hi -> x <|<=|!= hi */
		return proj_hi;
	} else if ((pnc_lo == pn_Cmp_Ge || pnc_lo == pn_Cmp_Gt || pnc_lo == pn_Cmp_Lg) &&
						 (pnc_hi == pn_Cmp_Eq || pnc_hi == pn_Cmp_Ge || pnc_hi == pn_Cmp_Gt)) {
		/* x >=|>|!= lo || x ==|>=|> hi -> x >=|>|!= lo */
		return proj_lo;
	} else if (tarval_is_one(tarval_sub(tv_hi, tv_lo, NULL))) { /* lo + 1 == hi */
		if (pnc_lo == pn_Cmp_Lt && pnc_hi == pn_Cmp_Ge) {
			/* x < c || x >= c + 1 -> x != c */
			ir_graph *const irg   = current_ir_graph;
			ir_node  *const block = get_nodes_block(cmp_lo);
			ir_node  *const p = new_r_Proj(irg, block, cmp_lo, mode_b, pn_Cmp_Lg);
			return p;
		} else if (pnc_lo == pn_Cmp_Le) {
			if (pnc_hi == pn_Cmp_Eq) {
				/* x <= c || x == c + 1 -> x <= c + 1 */
				ir_graph *const irg   = current_ir_graph;
				ir_node  *const block = get_nodes_block(cmp_hi);
				ir_node  *const p = new_r_Proj(irg, block, cmp_hi, mode_b, pn_Cmp_Le);
				return p;
			} else if (pnc_hi == pn_Cmp_Ge) {
				/* x <= c || x >= c + 1 -> true */
				ir_node *const t = new_Const(mode_b, tarval_b_true);
				return t;
			} else if (pnc_hi == pn_Cmp_Gt) {
				/* x <= c || x > c + 1 -> x != c + 1 */
				ir_graph *const irg   = current_ir_graph;
				ir_node  *const block = get_nodes_block(cmp_hi);
				ir_node  *const p = new_r_Proj(irg, block, cmp_hi, mode_b, pn_Cmp_Lg);
				return p;
			}
		} else if (pnc_lo == pn_Cmp_Eq && pnc_hi == pn_Cmp_Ge) {
			/* x == c || x >= c + 1 -> x >= c */
			ir_graph *const irg   = current_ir_graph;
			ir_node  *const block = get_nodes_block(cmp_lo);
			ir_node  *const p     = new_r_Proj(irg, block, cmp_lo, mode_b, pn_Cmp_Ge);
			return p;
		}
	}
	return NULL;
}

static void bool_walk(ir_node *n, void *env)
{
	(void)env;

	if (get_irn_mode(n) != mode_b)
		return;

	if (is_And(n)) {
		ir_node *const l = get_And_left(n);
		ir_node *const r = get_And_right(n);
		ir_node *      replacement;
		cond_pair      cpair;
		if (!find_cond_pair(l, r, &cpair))
			return;
		replacement = bool_and(&cpair);
		if (replacement)
			exchange(n, replacement);
	} else if (is_Or(n)) {
		ir_node *const l = get_Or_left(n);
		ir_node *const r = get_Or_right(n);
		ir_node *      replacement;
		cond_pair      cpair;
		if (!find_cond_pair(l, r, &cpair))
			return;
		replacement = bool_or(&cpair);
		if (replacement)
			exchange(n, replacement);
	}
}

/**
 * Walker, clear Block mark and Phi list
 */
static void clear_block_infos(ir_node *node, void *env)
{
	(void) env;

	/* we visit blocks before any other nodes (from the block) */
	if (!is_Block(node))
		return;

	/* clear the PHI list */
	set_Block_phis(node, NULL);
	set_Block_mark(node, 0);
}

/**
 * Walker: collect Phi nodes and update the
 */
static void collect_phis(ir_node *node, void *env)
{
	(void) env;

	if (is_Phi(node)) {
		ir_node *block = get_nodes_block(node);
		add_Block_phi(block, node);
		return;
	}

	/* Ignore control flow nodes, these will be removed. */
	if (get_irn_pinned(node) == op_pin_state_pinned &&
			!is_Block(node) && !is_cfop(node)) {
		ir_node *block = get_nodes_block(node);
		set_Block_mark(block, 1);
	}
}

/**
 * If node is a Jmp in a block containing no pinned instruction
 * and having only one predecessor, skip the block and return its
 * cf predecessor, else the node itself.
 */
static ir_node *skip_empty_block(ir_node *node)
{
	ir_node      *block;

	if(!is_Jmp(node))
		return node;

	block = get_nodes_block(node);
	if(get_Block_n_cfgpreds(block) != 1)
		return node;

	if(get_Block_mark(block))
		return node;

	return get_Block_cfgpred(block, 0);
}

static void find_cf_and_or_walker(ir_node *block, void *env)
{
	int i, i2;
	int n_cfgpreds = get_Block_n_cfgpreds(block);
	(void) env;

	if(n_cfgpreds < 2)
		return;

	/* Find the following structure:
	 *
	 *        upper_block
	 *         /       |
	 *        /        |
	 *   lower_block   |
	 *     /  \        |
	 *   ...   \       |
	 *           block
	 */

restart:
	for(i = 0; i < n_cfgpreds; ++i) {
		ir_node      *lower_block;
		ir_node      *lower_cf;
		ir_node      *cond;
		ir_node      *cond_selector;
		ir_node      *lower_pred;

		lower_cf = get_Block_cfgpred(block, i);
		lower_cf = skip_empty_block(lower_cf);
		if(!is_Proj(lower_cf))
			continue;

		cond = get_Proj_pred(lower_cf);
		if(!is_Cond(cond))
			continue;

		lower_block = get_nodes_block(cond);
		if(get_Block_n_cfgpreds(lower_block) != 1)
			continue;

		/* the block must not produce any side-effects */
		if(get_Block_mark(lower_block))
			continue;

		cond_selector = get_Cond_selector(cond);
		if(get_irn_mode(cond_selector) != mode_b)
			continue;

		lower_pred = get_Block_cfgpred_block(lower_block, 0);

		for(i2 = 0; i2 < n_cfgpreds; ++i2) {
			ir_node   *upper_block;
			ir_node   *upper_cf;
			ir_node   *upper_cond;
			ir_node   *upper_cond_selector;
			ir_node   *replacement;
			ir_graph  *irg;
			cond_pair  cpair;

			upper_cf    = get_Block_cfgpred(block, i2);
			upper_cf    = skip_empty_block(upper_cf);
			if(is_Bad(upper_cf))
				continue;
			upper_block = get_nodes_block(upper_cf);
			if(upper_block != lower_pred)
				continue;

			assert(is_Proj(upper_cf));
			upper_cond = get_Proj_pred(upper_cf);
			assert(is_Cond(upper_cond));
			upper_cond_selector = get_Cond_selector(upper_cond);
			if(get_irn_mode(upper_cond_selector) != mode_b)
				continue;

			/* we have found the structure */
			/* TODO: check phis */
			if(!find_cond_pair(cond_selector, upper_cond_selector, &cpair))
				continue;

			/* normalize pncs: we need the true case to jump into the
			 * common block (ie. conjunctive normal form) */
			irg = current_ir_graph;
			if(get_Proj_proj(lower_cf) == pn_Cond_false) {
				if(cpair.proj_lo == cond_selector) {
					ir_mode *mode = get_tarval_mode(cpair.tv_lo);
					cpair.pnc_lo  = get_negated_pnc(cpair.pnc_lo, mode);
					cpair.proj_lo = new_r_Proj(irg, lower_block,
							get_Proj_pred(cpair.proj_lo), mode_b, cpair.pnc_lo);
				} else {
					ir_mode *mode = get_tarval_mode(cpair.tv_hi);
					assert(cpair.proj_hi == cond_selector);
					cpair.pnc_hi  = get_negated_pnc(cpair.pnc_hi, mode);
					cpair.proj_hi = new_r_Proj(irg, lower_block,
							get_Proj_pred(cpair.proj_hi), mode_b, cpair.pnc_hi);
				}
			}
			if(get_Proj_proj(upper_cf) == pn_Cond_false) {
				if(cpair.proj_lo == upper_cond_selector) {
					ir_mode *mode = get_tarval_mode(cpair.tv_lo);
					cpair.pnc_lo  = get_negated_pnc(cpair.pnc_lo, mode);
					cpair.proj_lo = new_r_Proj(irg, upper_block,
							get_Proj_pred(cpair.proj_lo), mode_b, cpair.pnc_lo);
				} else {
					ir_mode *mode = get_tarval_mode(cpair.tv_hi);
					assert(cpair.proj_hi == upper_cond_selector);
					cpair.pnc_hi  = get_negated_pnc(cpair.pnc_hi, mode);
					cpair.proj_hi = new_r_Proj(irg, upper_block,
							get_Proj_pred(cpair.proj_hi), mode_b, cpair.pnc_hi);
				}
			}

			/* can we optimize the case? */
			replacement = bool_or(&cpair);
			if(replacement == NULL)
				continue;

			/* move all nodes from lower block to upper block */
			exchange(lower_block, upper_block);

			set_Block_cfgpred(block, i2, new_Bad());

			/* the optimisations expected the true case to jump */
			if(get_Proj_proj(lower_cf) == pn_Cond_false) {
				ir_node *block = get_nodes_block(replacement);
				replacement    = new_rd_Not(NULL, current_ir_graph, block,
				                            replacement, mode_b);
			}
			set_Cond_selector(cond, replacement);

			ir_fprintf(stderr, "replaced (ub %+F)\n", upper_block);
			goto restart;
		}
	}
}

void opt_bool(ir_graph *const irg)
{
	irg_walk_graph(irg, NULL, bool_walk, NULL);

	ir_reserve_resources(irg, IR_RESOURCE_BLOCK_MARK);

	irg_walk_graph(irg, clear_block_infos, collect_phis, NULL);

	irg_block_walk_graph(irg, NULL, find_cf_and_or_walker, NULL);

	set_irg_outs_inconsistent(irg);
	set_irg_doms_inconsistent(irg);
	set_irg_extblk_inconsistent(irg);
	set_irg_loopinfo_inconsistent(irg);

	ir_free_resources(irg, IR_RESOURCE_BLOCK_MARK);
}
