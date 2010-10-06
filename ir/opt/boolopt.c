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
 * @brief   boolean condition/control flow optimizations
 * @author  Matthias Braun, Christoph Mallon, Michael Beck
 * @version $Id: cfopt.c 22579 2008-10-07 14:54:04Z beck $
 */
#include "config.h"

#include <assert.h>
#include <string.h>

#include "adt/obst.h"
#include "../adt/array_t.h"
#include "iroptimize.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "irnode_t.h"
#include "tv.h"
#include "irpass.h"
#include "debug.h"

/** Describes a pair of relative conditions lo < hi, lo pnc_lo x, hi pnc_hi x */
typedef struct cond_pair {
	ir_node *cmp_lo;  /**< The lo compare node. */
	ir_node *cmp_hi;  /**< The hi compare node. */
	pn_Cmp   pnc_lo;  /**< The lo relation node. */
	pn_Cmp   pnc_hi;  /**< The hi relation node. */
	ir_node *proj_lo; /**< The mode_b result proj of cmp_lo. */
	ir_node *proj_hi; /**< The mode_b result proj of cmp_hi. */
	tarval  *tv_lo;   /**< The tarval of cmp_lo node. */
	tarval  *tv_hi;   /**< The tarval of cmp_hi node. */
	ir_mode *lo_mode; /**< The mode of the cmp_lo operands. */
} cond_pair;

/** Environment for all walker in boolopt. */
typedef struct {
	int changed;  /**< Set if the graph was changed. */
} bool_opt_env_t;

DEBUG_ONLY(static firm_dbg_module_t *dbg);

/**
 * Check if tho given nodes, l and r, represent two compares with
 * ... . If yes, return non-zero and fill the res struct.
 */
static int find_cond_pair(ir_node *const l, ir_node *const r, cond_pair *const res)
{
	if (is_Proj(l) && is_Proj(r)) {
		ir_node *const lo = get_Proj_pred(l);
		ir_node *const ro = get_Proj_pred(r);

		if (is_Cmp(lo) && is_Cmp(ro)) {
			ir_node *const lol   = get_Cmp_left(lo);
			ir_node *const lor   = get_Cmp_right(lo);
			ir_node *const rol   = get_Cmp_left(ro);
			ir_node *const ror   = get_Cmp_right(ro);
			pn_Cmp   const pnc_l = get_Proj_proj(l);
			pn_Cmp   const pnc_r = get_Proj_proj(r);

			if (is_Const(lor) && is_Const_null(lor) &&
			    is_Const(ror) && is_Const_null(ror) &&
			    pnc_l == pnc_r &&
			    (pnc_l == pn_Cmp_Lg || pnc_l == pn_Cmp_Eq)) {
				/* lo == (lol !=|== NULL) && ro == (rol !=|== NULL) */
				res->cmp_lo  = lo;
				res->cmp_hi  = ro;
				res->pnc_lo  = pnc_l;
				res->pnc_hi  = pnc_l;
				res->proj_lo = l;
				res->proj_hi = r;
				res->tv_lo   = get_Const_tarval(lor);
				res->tv_hi   = get_Const_tarval(ror);
				res->lo_mode = get_irn_mode(lor);

				return 1;
			}

			if (lol == rol && lor != ror && is_Const(lor) && is_Const(ror)) {
				/* lo == (x CMP c_l), ro == (x cmp c_r) */
				tarval *const tv_l  = get_Const_tarval(lor);
				tarval *const tv_r  = get_Const_tarval(ror);
				pn_Cmp  const rel   = tarval_cmp(tv_l, tv_r);

				res->lo_mode = get_irn_mode(lol);

				if (rel == pn_Cmp_Lt) {
					/* c_l < c_r */
					res->cmp_lo  = lo;
					res->cmp_hi  = ro;
					res->pnc_lo  = pnc_l;
					res->pnc_hi  = pnc_r;
					res->proj_lo = l;
					res->proj_hi = r;
					res->tv_lo   = tv_l;
					res->tv_hi   = tv_r;
				} else if (rel == pn_Cmp_Gt) {
					/* c_l > c_r */
					res->cmp_lo  = ro;
					res->cmp_hi  = lo;
					res->pnc_lo  = pnc_r;
					res->pnc_hi  = pnc_l;
					res->proj_lo = r;
					res->proj_hi = l;
					res->tv_lo   = tv_r;
					res->tv_hi   = tv_l;
				} else {
					/* The constants shall be unequal but comparable.
					 * Local optimizations handle the equal case. */
					return 0;
				}
				return 1;
			}
		}
	}
	return 0;
}

/**
 * Handle (lo pnc_lo x) AND (hi pnc_hi x)
 */
static ir_node *bool_and(cond_pair* const cpair, ir_node *dst_block)
{
	ir_node *const cmp_lo  = cpair->cmp_lo;
	ir_node *const cmp_hi  = cpair->cmp_hi;
	pn_Cmp         pnc_lo  = cpair->pnc_lo;
	pn_Cmp   const pnc_hi  = cpair->pnc_hi;
	ir_node *const proj_lo = cpair->proj_lo;
	ir_node *const proj_hi = cpair->proj_hi;
	tarval  *      tv_lo   = cpair->tv_lo;
	tarval  *      tv_hi   = cpair->tv_hi;
	ir_mode *      mode    = cpair->lo_mode;
	ir_graph *     irg     = get_irn_irg(cmp_lo);

	if (pnc_lo == pn_Cmp_Eq && pnc_hi == pn_Cmp_Eq &&
	    tarval_is_null(tv_lo) && tarval_is_null(tv_hi) &&
	    mode == get_tarval_mode(tv_hi)) {
		/* p == NULL && q == NULL ==> (p&q) == NULL) */
		ir_node *lol, *hil, *cmp, *c, *p;

		if (mode_is_reference(mode)) {
			mode = find_unsigned_mode(mode);
			if (! mode)
				return NULL;
			tv_lo = tarval_convert_to(tv_lo, mode);
			if (tv_lo == tarval_bad)
				return NULL;
		}
		if (mode_is_int(mode)) {
			lol   = get_Cmp_left(cmp_lo);
			lol   = new_r_Conv(dst_block, lol, mode);
			hil   = get_Cmp_left(cmp_hi);
			hil   = new_r_Conv(dst_block, hil, mode);
			p     = new_r_And(dst_block, lol, hil, mode);
			c     = new_r_Const(irg, tv_lo);
			cmp   = new_r_Cmp(dst_block, p, c);
			p     = new_r_Proj(cmp, mode_b, pn_Cmp_Eq);
			return p;
		}
	}

	/* the following tests expect one common operand */
	if (get_Cmp_left(cmp_lo) !=  get_Cmp_left(cmp_hi))
		return 0;

	/* TODO: for now reject float modes */
	if (! mode_is_int(mode))
		return 0;

	/* Beware of NaN's, we can only check for (ordered) != here (which is Lg, not Ne) */
	if ((pnc_lo == pn_Cmp_Lt || pnc_lo == pn_Cmp_Le || pnc_lo == pn_Cmp_Eq) &&
	    (pnc_hi == pn_Cmp_Eq || pnc_hi == pn_Cmp_Ge || pnc_hi == pn_Cmp_Gt)) {
		/* x <|<=|== lo && x ==|>=|> hi ==> false */
		ir_node *const t = new_r_Const(irg, tarval_b_false);
		return t;
	} else if ((pnc_lo == pn_Cmp_Lt || pnc_lo == pn_Cmp_Le || pnc_lo == pn_Cmp_Eq) &&
	           (pnc_hi == pn_Cmp_Lt || pnc_hi == pn_Cmp_Le || pnc_hi == pn_Cmp_Lg)) {
		/* x <|<=|== lo && x <|<=|!= hi ==> x <|<=|== lo */
		return proj_lo;
	} else if ((pnc_lo == pn_Cmp_Ge || pnc_lo == pn_Cmp_Gt || pnc_lo == pn_Cmp_Lg) &&
	           (pnc_hi == pn_Cmp_Eq || pnc_hi == pn_Cmp_Ge || pnc_hi == pn_Cmp_Gt)) {
		/* x >=|>|!= lo && x ==|>=|> hi ==> x ==|>=|> hi */
		return proj_hi;
	} else if (tarval_is_one(tarval_sub(tv_hi, tv_lo, NULL))) { /* lo + 1 == hi */
		if (pnc_lo == pn_Cmp_Ge && pnc_hi == pn_Cmp_Lt) {
			/* x >= c && x < c + 1 ==> x == c */
			ir_node  *const p = new_r_Proj(cmp_lo, mode_b, pn_Cmp_Eq);
			return p;
		} else if (pnc_lo == pn_Cmp_Gt) {
			if (pnc_hi == pn_Cmp_Lg) {
				/* x > c && x != c + 1 ==> x > c + 1 */
				ir_node  *const p = new_r_Proj(cmp_hi, mode_b, pn_Cmp_Gt);
				return p;
			} else if (pnc_hi == pn_Cmp_Lt) {
				/* x > c && x < c + 1 ==> false */
				ir_node *const t = new_r_Const(irg, tarval_b_false);
				return t;
			} else if (pnc_hi == pn_Cmp_Le) {
				/* x > c && x <= c + 1 ==> x != c + 1 */
				ir_node  *const p = new_r_Proj(cmp_hi, mode_b, pn_Cmp_Eq);
				return p;
			}
		} else if (pnc_lo == pn_Cmp_Lg && pnc_hi == pn_Cmp_Lt) {
			/* x != c && c < c + 1 ==> x < c */
			ir_node  *const p     = new_r_Proj(cmp_lo, mode_b, pn_Cmp_Lt);
			return p;
		}
	} else if ((pnc_lo == pn_Cmp_Gt || pnc_lo == pn_Cmp_Ge) &&
	           (pnc_hi == pn_Cmp_Lt || pnc_lo == pn_Cmp_Le) &&
	           get_mode_arithmetic(mode) == irma_twos_complement) {
		/* works for two-complements only */
		/* x >|\= lo && x <|<= hi ==> (x - lo) <u|<=u (hi-lo) */
		if (pnc_lo == pn_Cmp_Gt) {
			/* must convert to >= */
			ir_mode *mode = get_tarval_mode(tv_lo);
			tarval *n = tarval_add(tv_lo, get_mode_one(mode));
			if (n != tarval_bad && tarval_cmp(n, tv_lo) == pn_Cmp_Gt) {
				/* no overflow */
				tv_lo = n;
				pnc_lo = pn_Cmp_Ge;
			}
		}
		if (pnc_lo == pn_Cmp_Ge) {
			/* all fine */
			ir_node *const block = get_nodes_block(cmp_hi);
			ir_node *      x     = get_Cmp_left(cmp_hi);
			ir_mode *      mode  = get_irn_mode(x);
			ir_node *sub, *cmp, *c, *subc, *p;

			if (mode_is_signed(mode)) {
				/* convert to unsigned */
				mode = find_unsigned_mode(mode);
				if (mode == NULL)
					return NULL;
				x     = new_r_Conv(block, x, mode);
				tv_lo = tarval_convert_to(tv_lo, mode);
				tv_hi = tarval_convert_to(tv_hi, mode);
				if (tv_lo == tarval_bad || tv_hi == tarval_bad)
					return NULL;
			}
			c    = new_r_Const(irg, tv_lo);
			sub  = new_r_Sub(block, x, c, mode);
			subc = new_r_Sub(block, new_r_Const(irg, tv_hi), c, mode);
			cmp  = new_r_Cmp(block, sub, subc);
			p    = new_r_Proj(cmp, mode_b, pnc_hi);
			return p;
		}
	}
	return NULL;
}

/**
 * Handle (lo pnc_lo x) OR (hi pnc_hi x)
 */
static ir_node *bool_or(cond_pair *const cpair, ir_node *dst_block)
{
	ir_node *const cmp_lo  = cpair->cmp_lo;
	ir_node *const cmp_hi  = cpair->cmp_hi;
	pn_Cmp         pnc_lo  = cpair->pnc_lo;
	pn_Cmp   const pnc_hi  = cpair->pnc_hi;
	ir_node *const proj_lo = cpair->proj_lo;
	ir_node *const proj_hi = cpair->proj_hi;
	tarval  *      tv_lo   = cpair->tv_lo;
	tarval  *      tv_hi   = cpair->tv_hi;
	ir_mode *      mode    = cpair->lo_mode;
	ir_graph *     irg     = get_irn_irg(cmp_lo);

	if (pnc_lo == pn_Cmp_Lg && pnc_hi == pn_Cmp_Lg &&
		tarval_is_null(tv_lo) && tarval_is_null(tv_hi) &&
		mode == get_tarval_mode(tv_hi)) {
		/* p != NULL || q != NULL ==> (p|q) != NULL) */
		ir_node *lol, *hil, *cmp, *c, *p;

		if (mode_is_reference(mode)) {
			mode = find_unsigned_mode(mode);
			if (! mode)
				return NULL;
			tv_lo = tarval_convert_to(tv_lo, mode);
			if (tv_lo == tarval_bad)
				return NULL;
		}
		if (mode_is_int(mode)) {
			lol   = get_Cmp_left(cmp_lo);
			lol   = new_r_Conv(dst_block, lol, mode);
			hil   = get_Cmp_left(cmp_hi);
			hil   = new_r_Conv(dst_block, hil, mode);
			p     = new_r_Or(dst_block, lol, hil, mode);
			c     = new_r_Const(irg, tv_lo);
			cmp   = new_r_Cmp(dst_block, p, c);
			p     = new_r_Proj(cmp, mode_b, pn_Cmp_Lg);
			return p;
		}
	}

	/* the following tests expect one common operand */
	if (get_Cmp_left(cmp_lo) !=  get_Cmp_left(cmp_hi))
		return 0;

	/* TODO: for now reject float modes */
	if (! mode_is_int(mode))
		return 0;

	/* Beware of NaN's, we can only check for (ordered) != here (which is Lg, not Ne) */
	if ((pnc_lo == pn_Cmp_Ge || pnc_lo == pn_Cmp_Gt || pnc_lo == pn_Cmp_Lg) &&
	    (pnc_hi == pn_Cmp_Lt || pnc_hi == pn_Cmp_Le || pnc_hi == pn_Cmp_Lg)) {
		/* x >=|>|!= lo | x <|<=|!= hi ==> true */
		ir_node *const t = new_r_Const(irg, tarval_b_true);
		return t;
	} else if ((pnc_lo == pn_Cmp_Lt || pnc_lo == pn_Cmp_Le || pnc_lo == pn_Cmp_Eq) &&
	           (pnc_hi == pn_Cmp_Lt || pnc_hi == pn_Cmp_Le || pnc_hi == pn_Cmp_Lg)) {
		/* x <|<=|== lo || x <|<=|!= hi ==> x <|<=|!= hi */
		return proj_hi;
	} else if ((pnc_lo == pn_Cmp_Ge || pnc_lo == pn_Cmp_Gt || pnc_lo == pn_Cmp_Lg) &&
	           (pnc_hi == pn_Cmp_Eq || pnc_hi == pn_Cmp_Ge || pnc_hi == pn_Cmp_Gt)) {
		/* x >=|>|!= lo || x ==|>=|> hi ==> x >=|>|!= lo */
		return proj_lo;
	} else if (tarval_is_one(tarval_sub(tv_hi, tv_lo, NULL))) { /* lo + 1 == hi */
		if (pnc_lo == pn_Cmp_Lt && pnc_hi == pn_Cmp_Ge) {
			/* x < c || x >= c + 1 ==> x != c */
			ir_node  *const p = new_r_Proj(cmp_lo, mode_b, pn_Cmp_Lg);
			return p;
		} else if (pnc_lo == pn_Cmp_Le) {
			if (pnc_hi == pn_Cmp_Eq) {
				/* x <= c || x == c + 1 ==> x <= c + 1 */
				ir_node  *const p = new_r_Proj(cmp_hi, mode_b, pn_Cmp_Le);
				return p;
			} else if (pnc_hi == pn_Cmp_Ge) {
				/* x <= c || x >= c + 1 ==> true */
				ir_node *const t = new_r_Const(irg, tarval_b_true);
				return t;
			} else if (pnc_hi == pn_Cmp_Gt) {
				/* x <= c || x > c + 1 ==> x != c + 1 */
				ir_node  *const p = new_r_Proj(cmp_hi, mode_b, pn_Cmp_Lg);
				return p;
			}
		} else if (pnc_lo == pn_Cmp_Eq && pnc_hi == pn_Cmp_Ge) {
			/* x == c || x >= c + 1 ==> x >= c */
			ir_node  *const p     = new_r_Proj(cmp_lo, mode_b, pn_Cmp_Ge);
			return p;
		}
	} else if ((pnc_lo == pn_Cmp_Lt || pnc_lo == pn_Cmp_Le) &&
	           (pnc_hi == pn_Cmp_Gt || pnc_lo == pn_Cmp_Ge) &&
	           get_mode_arithmetic(mode) == irma_twos_complement) {
		/* works for two-complements only */
		/* x <|<= lo  || x >|>= hi ==> (x - lo) >u|>=u (hi-lo) */
		if (pnc_lo == pn_Cmp_Le) {
			/* must convert to < */
			ir_mode *mode = get_tarval_mode(tv_lo);
			tarval *n = tarval_add(tv_lo, get_mode_one(mode));
			if (n != tarval_bad && tarval_cmp(n, tv_lo) == pn_Cmp_Gt) {
				/* no overflow */
				tv_lo = n;
				pnc_lo = pn_Cmp_Lt;
			}
		}
		if (pnc_lo == pn_Cmp_Lt) {
			/* all fine */
			ir_node *const block = get_nodes_block(cmp_hi);
			ir_node *      x     = get_Cmp_left(cmp_hi);
			ir_mode *      mode  = get_irn_mode(x);
			ir_node *sub, *cmp, *c, *subc, *p;

			if (mode_is_signed(mode)) {
				/* convert to unsigned */
				mode = find_unsigned_mode(mode);
				if (mode == NULL)
					return NULL;
				x     = new_r_Conv(block, x, mode);
				tv_lo = tarval_convert_to(tv_lo, mode);
				tv_hi = tarval_convert_to(tv_hi, mode);
				if (tv_lo == tarval_bad || tv_hi == tarval_bad)
					return NULL;
			}
			c    = new_r_Const(irg, tv_lo);
			sub  = new_r_Sub(block, x, c, mode);
			subc = new_r_Sub(block, new_r_Const(irg, tv_hi), c, mode);
			cmp  = new_r_Cmp(block, sub, subc);
			p    = new_r_Proj(cmp, mode_b, pnc_hi);
			return p;
		}
	}
	return NULL;
}

/**
 * Walker, tries to optimize Andb and Orb nodes.
 */
static void bool_walk(ir_node *n, void *ctx)
{
	bool_opt_env_t *env = ctx;

	if (get_irn_mode(n) != mode_b)
		return;

	if (is_And(n)) {
		ir_node *const l = get_And_left(n);
		ir_node *const r = get_And_right(n);
		ir_node *      replacement;
		cond_pair      cpair;
		if (!find_cond_pair(l, r, &cpair))
			return;
		replacement = bool_and(&cpair, get_nodes_block(n));
		if (replacement) {
			exchange(n, replacement);
			env->changed = 1;
		}
	} else if (is_Or(n)) {
		ir_node *const l = get_Or_left(n);
		ir_node *const r = get_Or_right(n);
		ir_node *      replacement;
		cond_pair      cpair;
		if (!find_cond_pair(l, r, &cpair))
			return;
		replacement = bool_or(&cpair, get_nodes_block(n));
		if (replacement) {
			exchange(n, replacement);
			env->changed = 1;
		}
	}
}

/**
 * Walker, clear Block marker and Phi lists.
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
 * Walker: collect Phi nodes and mark
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
				/* found a pinned non-cf node, mark its block */
		ir_node *block = get_nodes_block(node);
		set_Block_mark(block, 1);
	}
}

/**
 * If node is a Jmp in a block containing no pinned instruction
 * and having only one predecessor, skip the block and return its
 * cf predecessor, else the node itself.
 */
static ir_node *skip_empty_blocks(ir_node *node)
{
	while (is_Jmp(node)) {
		ir_node *block = get_nodes_block(node);

		if (get_Block_n_cfgpreds(block) != 1)
			break;

		if (get_Block_mark(block))
			break;

		node = get_Block_cfgpred(block, 0);
	}
	return node;
}

/**
 * Check if two block inputs can be fused.
 * This can be done, if block contains no Phi node that depends on
 * different inputs idx_i and idx_j.
 */
static int can_fuse_block_inputs(const ir_node *block, int idx_i, int idx_j)
{
	const ir_node *phi;

	for (phi = get_Block_phis(block); phi != NULL; phi = get_Phi_next(phi)) {
		if (get_Phi_pred(phi, idx_i) != get_Phi_pred(phi, idx_j))
			return 0;
	}
	return 1;
}

/**
 * Remove block input with given index.
 */
static void remove_block_input(ir_node *block, int idx)
{
	int i, j, n = get_Block_n_cfgpreds(block) - 1;
	ir_node *phi, **ins;

	NEW_ARR_A(ir_node *, ins, n);

	if (n == 1) {
		/* all Phis will be deleted */
		ir_node *next_phi;

		for (phi = get_Block_phis(block); phi != NULL; phi = next_phi) {
			next_phi = get_Phi_next(phi);
			exchange(phi, get_Phi_pred(phi, idx ^ 1));
		}
		set_Block_phis(block, NULL);
	} else {
		for (phi = get_Block_phis(block); phi != NULL; phi = get_Phi_next(phi)) {
			for (i = j = 0; i <= n; ++i) {
				if (i != idx)
					ins[j++] = get_Phi_pred(phi, i);
			}
			set_irn_in(phi, n, ins);
		}
	}
	for (i = j = 0; i <= n; ++i) {
		if (i != idx)
			ins[j++] = get_Block_cfgpred(block, i);
	}
	set_irn_in(block, n, ins);
}

/**
 * Under the preposition that we have a chain of blocks from
 * from_block to to_block, collapse them all into to_block.
 */
static void move_nodes_to_block(ir_node *jmp, ir_node *to_block)
{
	ir_node *new_jmp = NULL;
	ir_node *block, *next_block;

	for (block = get_nodes_block(jmp); block != to_block; block = next_block) {
		new_jmp = get_Block_cfgpred(block, 0);
		next_block = get_nodes_block(new_jmp);
		exchange(block, to_block);
	}
	if (new_jmp)
		exchange(jmp, new_jmp);
}

/**
 * Block walker:
 *
 * if we can find the following structure,
 *
 *        upper_block
 *         /       |
 *        /        |
 *   lower_block   |
 *     /  \        |
 *   ... low_idx up_idx
 *          \      |
 *            block
 *
 * try to convert it into a (x pnc_lo c_lo) || (x pnc_hi c_hi)
 * and optimize.
 */
static void find_cf_and_or_walker(ir_node *block, void *ctx)
{
	int low_idx, up_idx;
	int n_cfgpreds;
	bool_opt_env_t *env = ctx;

	/* because we modify the graph in regions we might not visited yet,
	 * Id nodes might arise here. Ignore them.
	 */
	if (is_Id(block))
		return;

	n_cfgpreds = get_Block_n_cfgpreds(block);
restart:
	if (n_cfgpreds < 2)
		return;

	for (low_idx = 0; low_idx < n_cfgpreds; ++low_idx) {
		ir_node      *lower_block;
		ir_node      *lower_cf;
		ir_node      *cond;
		ir_node      *cond_selector;
		ir_node      *lower_pred;

		lower_cf = get_Block_cfgpred(block, low_idx);
		lower_cf = skip_empty_blocks(lower_cf);
		if (!is_Proj(lower_cf))
			continue;

		cond = get_Proj_pred(lower_cf);
		if (!is_Cond(cond))
			continue;

		lower_block = get_nodes_block(cond);
		if (get_Block_n_cfgpreds(lower_block) != 1)
			continue;

		/* the block must not produce any side-effects */
		if (get_Block_mark(lower_block))
			continue;

		cond_selector = get_Cond_selector(cond);
		if (get_irn_mode(cond_selector) != mode_b)
			continue;

		lower_pred = get_Block_cfgpred_block(lower_block, 0);

		for (up_idx = 0; up_idx < n_cfgpreds; ++up_idx) {
			ir_node   *upper_block;
			ir_node   *upper_cf;
			ir_node   *upper_cond;
			ir_node   *upper_cond_selector;
			ir_node   *replacement;
			cond_pair  cpair;

			upper_cf    = get_Block_cfgpred(block, up_idx);
			upper_cf    = skip_empty_blocks(upper_cf);
			if (is_Bad(upper_cf))
				continue;
			upper_block = get_nodes_block(upper_cf);
			if (upper_block != lower_pred)
				continue;
			if (!block_dominates(upper_block, block))
				continue;

			assert(is_Proj(upper_cf));
			upper_cond = get_Proj_pred(upper_cf);
			assert(is_Cond(upper_cond));
			upper_cond_selector = get_Cond_selector(upper_cond);
			if (get_irn_mode(upper_cond_selector) != mode_b)
				continue;

			/* we have found the structure */
			/* check Phis: There must be NO Phi in block that
			   depends on the existence of low block */
			if (!can_fuse_block_inputs(block, low_idx, up_idx))
				continue;

			/* all fine, try it */
			if (!find_cond_pair(cond_selector, upper_cond_selector, &cpair))
				continue;

			/* normalize pncs: we need the true case to jump into the
			 * common block (ie. conjunctive normal form) */
			if (get_Proj_proj(lower_cf) == pn_Cond_false) {
				if (cpair.proj_lo == cond_selector) {
					ir_mode *mode  = get_tarval_mode(cpair.tv_lo);
					ir_node *cmp   = get_Proj_pred(cpair.proj_lo);
					cpair.pnc_lo   = get_negated_pnc(cpair.pnc_lo, mode);
					cpair.proj_lo  = new_r_Proj(cmp, mode_b, cpair.pnc_lo);
				} else {
					ir_mode *mode  = get_tarval_mode(cpair.tv_hi);
					ir_node *cmp   = get_Proj_pred(cpair.proj_hi);
					assert(cpair.proj_hi == cond_selector);
					cpair.pnc_hi   = get_negated_pnc(cpair.pnc_hi, mode);
					cpair.proj_hi  = new_r_Proj(cmp, mode_b, cpair.pnc_hi);
				}
			}
			if (get_Proj_proj(upper_cf) == pn_Cond_false) {
				if (cpair.proj_lo == upper_cond_selector) {
					ir_mode *mode  = get_tarval_mode(cpair.tv_lo);
					ir_node *cmp   = get_Proj_pred(cpair.proj_lo);
					cpair.pnc_lo   = get_negated_pnc(cpair.pnc_lo, mode);
					cpair.proj_lo  = new_r_Proj(cmp, mode_b, cpair.pnc_lo);
				} else {
					ir_mode *mode  = get_tarval_mode(cpair.tv_hi);
					ir_node *cmp   = get_Proj_pred(cpair.proj_hi);
					assert(cpair.proj_hi == upper_cond_selector);
					cpair.pnc_hi   = get_negated_pnc(cpair.pnc_hi, mode);
					cpair.proj_hi  = new_r_Proj(cmp, mode_b, cpair.pnc_hi);
				}
			}

			/* can we optimize the case? */
			replacement = bool_or(&cpair, upper_block);
			if (replacement == NULL)
				continue;

			env->changed = 1;

			DB((dbg, LEVEL_1, "boolopt: %+F: fusing (ub %+F lb %+F)\n",
				current_ir_graph, upper_block, lower_block));

			/* move all expressions on the path to lower/upper block */
			move_nodes_to_block(get_Block_cfgpred(block, up_idx), upper_block);
			move_nodes_to_block(get_Block_cfgpred(block, low_idx), lower_block);

			/* move all nodes from lower block to upper block */
			exchange(lower_block, upper_block);

			remove_block_input(block, up_idx);
			--n_cfgpreds;

			/* the optimizations expected the true case to jump */
			if (get_Proj_proj(lower_cf) == pn_Cond_false) {
				ir_node *block = get_nodes_block(replacement);
				replacement    = new_rd_Not(NULL, block, replacement, mode_b);
			}
			set_Cond_selector(cond, replacement);

			goto restart;
		}
	}
}

void opt_bool(ir_graph *const irg)
{
	bool_opt_env_t env;

	/* register a debug mask */
	FIRM_DBG_REGISTER(dbg, "firm.opt.bool");

	/* works better with one return block only */
	normalize_one_return(irg);

	env.changed = 0;

	/* optimize simple Andb and Orb cases */
	irg_walk_graph(irg, NULL, bool_walk, &env);

	ir_reserve_resources(irg, IR_RESOURCE_BLOCK_MARK | IR_RESOURCE_PHI_LIST);

	/* now more complicated cases: find control flow And/Or and optimize. */
	irg_walk_graph(irg, clear_block_infos, collect_phis, NULL);
	irg_block_walk_graph(irg, NULL, find_cf_and_or_walker, &env);

	if (env.changed) {
		set_irg_outs_inconsistent(irg);
		set_irg_doms_inconsistent(irg);
		set_irg_extblk_inconsistent(irg);
		set_irg_loopinfo_inconsistent(irg);
	}

	ir_free_resources(irg, IR_RESOURCE_BLOCK_MARK | IR_RESOURCE_PHI_LIST);
}

/* Creates an ir_graph pass for opt_bool. */
ir_graph_pass_t *opt_bool_pass(const char *name)
{
	return def_graph_pass(name ? name : "opt_bool", opt_bool);
}  /* opt_bool_pass */
