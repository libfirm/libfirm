/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   boolean condition/control flow optimizations
 * @author  Matthias Braun, Christoph Mallon, Michael Beck
 */
#include "../adt/array.h"
#include "adt/obst.h"
#include "debug.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "iroptimize.h"
#include "tv.h"
#include <assert.h>

/** Describes a pair of relative conditions lo < hi, lo rel_lo x, hi rel_hi x */
typedef struct cond_pair {
	ir_node    *cmp_lo;  /**< The lo compare node. */
	ir_node    *cmp_hi;  /**< The hi compare node. */
	ir_relation rel_lo;  /**< The lo relation node. */
	ir_relation rel_hi;  /**< The hi relation node. */
	ir_tarval  *tv_lo;   /**< The tarval of cmp_lo node. */
	ir_tarval  *tv_hi;   /**< The tarval of cmp_hi node. */
	ir_mode    *lo_mode; /**< The mode of the cmp_lo operands. */
} cond_pair;

/** Environment for all walker in boolopt. */
typedef struct {
	int changed;  /**< Set if the graph was changed. */
} bool_opt_env_t;

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/**
 * Check if the given nodes, l and r, represent two compares with
 * ... . If yes, return non-zero and fill the res struct.
 */
static bool find_cond_pair(ir_node *const l, ir_node *const r, cond_pair *const res)
{
	if (is_Cmp(l) && is_Cmp(r)) {
		ir_node    *const lol   = get_Cmp_left(l);
		ir_node    *const lor   = get_Cmp_right(l);
		ir_node    *const rol   = get_Cmp_left(r);
		ir_node    *const ror   = get_Cmp_right(r);
		ir_relation const pnc_l = get_Cmp_relation(l);
		ir_relation const pnc_r = get_Cmp_relation(r);

		if (is_irn_null(lor) && is_irn_null(ror) &&
			pnc_l == pnc_r &&
			(pnc_l == ir_relation_less_greater || pnc_l == ir_relation_equal)) {
			/* l == (lol !=|== NULL) && r == (rol !=|== NULL) */
			res->cmp_lo  = l;
			res->cmp_hi  = r;
			res->rel_lo  = pnc_l;
			res->rel_hi  = pnc_l;
			res->tv_lo   = get_Const_tarval(lor);
			res->tv_hi   = get_Const_tarval(ror);
			res->lo_mode = get_irn_mode(lor);

			return true;
		}

		if (lol == rol && lor != ror && is_Const(lor) && is_Const(ror)) {
			/* l == (x CMP c_l), r == (x cmp c_r) */
			ir_tarval  *const tv_l  = get_Const_tarval(lor);
			ir_tarval  *const tv_r  = get_Const_tarval(ror);
			ir_relation const rel   = tarval_cmp(tv_l, tv_r);

			res->lo_mode = get_irn_mode(lol);

			if (rel == ir_relation_less) {
				/* c_l < c_r */
				res->cmp_lo  = l;
				res->cmp_hi  = r;
				res->rel_lo  = pnc_l;
				res->rel_hi  = pnc_r;
				res->tv_lo   = tv_l;
				res->tv_hi   = tv_r;
			} else if (rel == ir_relation_greater) {
				/* c_l > c_r */
				res->cmp_lo  = r;
				res->cmp_hi  = l;
				res->rel_lo  = pnc_r;
				res->rel_hi  = pnc_l;
				res->tv_lo   = tv_r;
				res->tv_hi   = tv_l;
			} else {
				/* The constants shall be unequal but comparable.
				 * Local optimizations handle the equal case. */
				return false;
			}
			return true;
		}
	}
	return false;
}

static ir_node *make_Cmp(ir_node *const block, ir_node *const cmp, ir_relation const rel)
{
	ir_node *const l = get_Cmp_left(cmp);
	ir_node *const r = get_Cmp_right(cmp);
	return new_r_Cmp(block, l, r, rel);
}

/**
 * Handle (lo rel_lo x) AND (hi rel_hi x)
 */
static ir_node *bool_and(cond_pair* const cpair, ir_node *dst_block)
{
	ir_node *const cmp_lo = cpair->cmp_lo;
	ir_node *const cmp_hi = cpair->cmp_hi;
	if (is_Const(cmp_lo))
		return is_Const_null(cmp_lo) ? cmp_hi : cmp_lo;

	if (is_Const(cmp_hi))
		return is_Const_null(cmp_hi) ? cmp_lo : cmp_hi;

	ir_relation        rel_lo = cpair->rel_lo;
	ir_relation  const rel_hi = cpair->rel_hi;
	ir_tarval   *      tv_lo  = cpair->tv_lo;
	ir_tarval   *      tv_hi  = cpair->tv_hi;
	ir_mode     *      mode   = cpair->lo_mode;
	ir_graph    *      irg    = get_irn_irg(cmp_lo);
	if ((rel_lo & ~ir_relation_unordered) == ir_relation_equal
	    && (rel_hi & ~ir_relation_unordered) == rel_lo
	    && tarval_is_null(tv_lo) && tarval_is_null(tv_hi)
	    && mode == get_tarval_mode(tv_hi)) {
		/* p == NULL && q == NULL ==> (p&q) == NULL) */
		ir_node *lol, *hil, *cmp, *c, *p;

		if (mode_is_reference(mode)) {
			mode = find_unsigned_mode(mode);
			if (!mode)
				return NULL;
			tv_lo = tarval_convert_to(tv_lo, mode);
			if (tv_lo == tarval_bad)
				return NULL;
		}
		if (mode_is_int(mode)) {
			lol = get_Cmp_left(cmp_lo);
			lol = new_r_Conv(dst_block, lol, mode);
			hil = get_Cmp_left(cmp_hi);
			hil = new_r_Conv(dst_block, hil, mode);
			p   = new_r_And(dst_block, lol, hil);
			c   = new_r_Const(irg, tv_lo);
			cmp = new_r_Cmp(dst_block, p, c, ir_relation_equal);
			return cmp;
		}
	}

	/* the following tests expect one common operand */
	if (get_Cmp_left(cmp_lo) != get_Cmp_left(cmp_hi))
		return NULL;

	/* TODO: for now reject float modes */
	if (!mode_is_int(mode))
		return NULL;

	/* special case: both Cmp compare with the same constant */
	if (tarval_cmp(tv_lo, tv_hi) == ir_relation_equal) {
		ir_relation rel = rel_lo & rel_hi;
		if (rel == ir_relation_false) {
			ir_node *const t = new_r_Const(irg, tarval_b_false);
			return t;
		}
		if (rel == rel_lo) {
			return cmp_lo;
		} else if (rel == rel_hi) {
			return cmp_hi;
		} else {
			return make_Cmp(dst_block, cmp_hi, rel);
		}
	}
	/* Beware of NaN's, we can only check for (ordered) != here (which is Lg, not Ne) */
	if ((rel_lo == ir_relation_less || rel_lo == ir_relation_less_equal || rel_lo == ir_relation_equal) &&
	    (rel_hi == ir_relation_equal || rel_hi == ir_relation_greater_equal || rel_hi == ir_relation_greater)) {
		/* x <|<=|== lo && x ==|>=|> hi ==> false */
		ir_node *const t = new_r_Const(irg, tarval_b_false);
		return t;
	} else if ((rel_lo == ir_relation_less || rel_lo == ir_relation_less_equal || rel_lo == ir_relation_equal) &&
	           (rel_hi == ir_relation_less || rel_hi == ir_relation_less_equal || rel_hi == ir_relation_less_greater)) {
		/* x <|<=|== lo && x <|<=|!= hi ==> x <|<=|== lo */
		return cmp_lo;
	} else if ((rel_lo == ir_relation_greater_equal || rel_lo == ir_relation_greater || rel_lo == ir_relation_less_greater) &&
	           (rel_hi == ir_relation_equal || rel_hi == ir_relation_greater_equal || rel_hi == ir_relation_greater)) {
		/* x >=|>|!= lo && x ==|>=|> hi ==> x ==|>=|> hi */
		return cmp_hi;
	} else if (tarval_is_one(tarval_sub(tv_hi, tv_lo))) { /* lo + 1 == hi */
		if (rel_lo == ir_relation_greater_equal && rel_hi == ir_relation_less) {
			/* x >= c && x < c + 1 ==> x == c */
			return make_Cmp(dst_block, cmp_lo, ir_relation_equal);
		} else if (rel_lo == ir_relation_greater) {
			if (rel_hi == ir_relation_less_greater) {
				/* x > c && x != c + 1 ==> x > c + 1 */
				return make_Cmp(dst_block, cmp_hi, ir_relation_greater);
			} else if (rel_hi == ir_relation_less) {
				/* x > c && x < c + 1 ==> false */
				ir_node *const t = new_r_Const(irg, tarval_b_false);
				return t;
			} else if (rel_hi == ir_relation_less_equal) {
				/* x > c && x <= c + 1 ==> x == c + 1 */
				return make_Cmp(dst_block, cmp_hi, ir_relation_equal);
			}
		} else if (rel_lo == ir_relation_less_greater && rel_hi == ir_relation_less) {
			/* x != c && c < c + 1 ==> x < c */
			return make_Cmp(dst_block, cmp_lo, ir_relation_less);
		}
	} else if ((rel_lo == ir_relation_greater || rel_lo == ir_relation_greater_equal) &&
	           (rel_hi == ir_relation_less || rel_hi == ir_relation_less_equal) &&
	           get_mode_arithmetic(mode) == irma_twos_complement) {
		/* works for two-complements only */
		/* x >|\= lo && x <|<= hi ==> (x - lo) <u|<=u (hi-lo) */
		if (rel_lo == ir_relation_greater) {
			/* must convert to >= */
			ir_mode   *mode = get_tarval_mode(tv_lo);
			ir_tarval *n    = tarval_add(tv_lo, get_mode_one(mode));
			if (n != tarval_bad && tarval_cmp(n, tv_lo) == ir_relation_greater) {
				/* no overflow */
				tv_lo = n;
				rel_lo = ir_relation_greater_equal;
			}
		}
		if (rel_lo == ir_relation_greater_equal) {
			/* all fine */
			ir_node *const block = get_nodes_block(cmp_hi);
			ir_node *      x     = get_Cmp_left(cmp_hi);
			ir_mode *      mode  = get_irn_mode(x);
			ir_node *sub, *cmp, *c, *subc;

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
			sub  = new_r_Sub(block, x, c);
			subc = new_r_Const(irg, tarval_sub(tv_hi, tv_lo));
			cmp  = new_r_Cmp(block, sub, subc, rel_hi);
			return cmp;
		}
	}
	return NULL;
}

/**
 * Handle (lo rel_lo x) OR (hi rel_hi x)
 */
static ir_node *bool_or(cond_pair *const cpair, ir_node *dst_block)
{
	ir_node *const cmp_lo = cpair->cmp_lo;
	ir_node *const cmp_hi = cpair->cmp_hi;
	if (is_Const(cmp_lo))
		return is_Const_null(cmp_lo) ? cmp_hi : cmp_lo;

	if (is_Const(cmp_hi))
		return is_Const_null(cmp_hi) ? cmp_lo : cmp_hi;

	ir_relation        rel_lo = cpair->rel_lo;
	ir_relation  const rel_hi = cpair->rel_hi;
	ir_tarval   *      tv_lo  = cpair->tv_lo;
	ir_tarval   *      tv_hi  = cpair->tv_hi;
	ir_mode     *      mode   = cpair->lo_mode;
	ir_graph    *      irg    = get_irn_irg(cmp_lo);
	if ((rel_lo & ~ir_relation_unordered) == ir_relation_less_greater
	    && (rel_hi & ~ir_relation_unordered) == ir_relation_less_greater
	    && tarval_is_null(tv_lo) && tarval_is_null(tv_hi)
	    && mode == get_tarval_mode(tv_hi)) {
		/* p != NULL || q != NULL ==> (p|q) != NULL) */
		ir_node *lol, *hil, *cmp, *c, *p;

		if (mode_is_reference(mode)) {
			mode = find_unsigned_mode(mode);
			if (!mode)
				return NULL;
			tv_lo = tarval_convert_to(tv_lo, mode);
			if (tv_lo == tarval_bad)
				return NULL;
		}
		if (mode_is_int(mode)) {
			lol = get_Cmp_left(cmp_lo);
			lol = new_r_Conv(dst_block, lol, mode);
			hil = get_Cmp_left(cmp_hi);
			hil = new_r_Conv(dst_block, hil, mode);
			p   = new_r_Or(dst_block, lol, hil);
			c   = new_r_Const(irg, tv_lo);
			cmp = new_r_Cmp(dst_block, p, c, ir_relation_less_greater);
			return cmp;
		}
	}

	/* the following tests expect one common operand */
	if (get_Cmp_left(cmp_lo) != get_Cmp_left(cmp_hi))
		return NULL;

	/* TODO: for now reject float modes */
	if (!mode_is_int(mode))
		return NULL;

	/* special case: both Cmp compare with the same constant */
	if (tarval_cmp(tv_lo, tv_hi) == ir_relation_equal) {
		ir_relation rel = rel_lo | rel_hi;
		if (rel == ir_relation_true) {
			ir_node *const t = new_r_Const(irg, tarval_b_true);
			return t;
		}
		if (rel == rel_lo) {
			return cmp_lo;
		} else if (rel == rel_hi) {
			return cmp_hi;
		} else {
			return make_Cmp(dst_block, cmp_hi, rel);
		}
	}
	/* Beware of NaN's, we can only check for (ordered) != here (which is Lg, not Ne) */
	if ((rel_lo == ir_relation_greater_equal || rel_lo == ir_relation_greater || rel_lo == ir_relation_less_greater) &&
	    (rel_hi == ir_relation_less || rel_hi == ir_relation_less_equal || rel_hi == ir_relation_less_greater)) {
		/* x >=|>|!= lo | x <|<=|!= hi ==> true */
		ir_node *const t = new_r_Const(irg, tarval_b_true);
		return t;
	} else if ((rel_lo == ir_relation_less || rel_lo == ir_relation_less_equal || rel_lo == ir_relation_equal) &&
	           (rel_hi == ir_relation_less || rel_hi == ir_relation_less_equal || rel_hi == ir_relation_less_greater)) {
		/* x <|<=|== lo || x <|<=|!= hi ==> x <|<=|!= hi */
		return cmp_hi;
	} else if ((rel_lo == ir_relation_greater_equal || rel_lo == ir_relation_greater || rel_lo == ir_relation_less_greater) &&
	           (rel_hi == ir_relation_equal || rel_hi == ir_relation_greater_equal || rel_hi == ir_relation_greater)) {
		/* x >=|>|!= lo || x ==|>=|> hi ==> x >=|>|!= lo */
		return cmp_lo;
	} else if (tarval_is_one(tarval_sub(tv_hi, tv_lo))) { /* lo + 1 == hi */
		if (rel_lo == ir_relation_less && rel_hi == ir_relation_greater_equal) {
			/* x < c || x >= c + 1 ==> x != c */
			return make_Cmp(dst_block, cmp_lo, ir_relation_less_greater);
		} else if (rel_lo == ir_relation_less_equal) {
			if (rel_hi == ir_relation_equal) {
				/* x <= c || x == c + 1 ==> x <= c + 1 */
				return make_Cmp(dst_block, cmp_hi, ir_relation_less_equal);
			} else if (rel_hi == ir_relation_greater_equal) {
				/* x <= c || x >= c + 1 ==> true */
				ir_node *const t = new_r_Const(irg, tarval_b_true);
				return t;
			} else if (rel_hi == ir_relation_greater) {
				/* x <= c || x > c + 1 ==> x != c + 1 */
				return make_Cmp(dst_block, cmp_hi, ir_relation_less_greater);
			}
		} else if (rel_lo == ir_relation_equal && rel_hi == ir_relation_greater_equal) {
			/* x == c || x >= c + 1 ==> x >= c */
			return make_Cmp(dst_block, cmp_lo, ir_relation_greater_equal);
		}
	} else if ((rel_lo == ir_relation_less || rel_lo == ir_relation_less_equal) &&
	           (rel_hi == ir_relation_greater || rel_hi == ir_relation_greater_equal) &&
	           get_mode_arithmetic(mode) == irma_twos_complement) {
		/* works for two-complements only */
		/* x <|<= lo  || x >|>= hi ==> (x - lo) >u|>=u (hi-lo) */
		if (rel_lo == ir_relation_less_equal) {
			/* must convert to < */
			ir_mode   *mode = get_tarval_mode(tv_lo);
			ir_tarval *n    = tarval_add(tv_lo, get_mode_one(mode));
			if (n != tarval_bad && tarval_cmp(n, tv_lo) == ir_relation_greater) {
				/* no overflow */
				tv_lo = n;
				rel_lo = ir_relation_less;
			}
		}
		if (rel_lo == ir_relation_less) {
			/* all fine */
			ir_node *const block = get_nodes_block(cmp_hi);
			ir_node *      x     = get_Cmp_left(cmp_hi);
			ir_mode *      mode  = get_irn_mode(x);
			ir_node *sub, *cmp, *c, *subc;

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
			sub  = new_r_Sub(block, x, c);
			subc = new_r_Const(irg, tarval_sub(tv_hi, tv_lo));
			cmp  = new_r_Cmp(block, sub, subc, rel_hi);
			return cmp;
		}
	}
	return NULL;
}

/**
 * Walker, tries to optimize Andb and Orb nodes.
 */
static void bool_walk(ir_node *n, void *ctx)
{
	bool_opt_env_t *env = (bool_opt_env_t*)ctx;

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
	(void)env;

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
	(void)env;

	if (is_Phi(node)) {
		ir_node *block = get_nodes_block(node);
		add_Block_phi(block, node);
		return;
	}

	/* Ignore control flow nodes, these will be removed. */
	if (get_irn_pinned(node) && !is_Block(node) && !is_cfop(node)) {
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
	ir_node *phi;

	ir_node **ins = ALLOCAN(ir_node*, n);

	if (n == 1) {
		/* all Phis will be deleted */
		ir_node *next_phi;

		for (phi = get_Block_phis(block); phi != NULL; phi = next_phi) {
			next_phi = get_Phi_next(phi);
			if (get_Phi_loop(phi)) {
				remove_keep_alive(phi);
				set_Phi_loop(phi, false);
			}
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

static void normalize_cmp(ir_node **io_cmp, ir_relation *io_rel, ir_tarval **io_tv)
{
	ir_node     *cmp   = *io_cmp;
	ir_relation  rel   = *io_rel;
	ir_node     *block = get_nodes_block(cmp);
	dbg_info    *dbgi  = get_irn_dbg_info(cmp);
	ir_node     *left  = get_Cmp_left(cmp);
	ir_node     *right = get_Cmp_right(cmp);
	*io_rel = get_negated_relation(rel);
	ir_mode     *mode  = get_irn_mode(left);
	if (mode_is_int(mode)) {
		*io_rel &= ~ir_relation_unordered;
	}
	*io_cmp = new_rd_Cmp(dbgi, block, left, right, *io_rel);
	if (is_Cmp(*io_cmp)) {
		*io_tv  = get_Const_tarval(get_Cmp_right(*io_cmp));
	}
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
 * try to convert it into a (x rel_lo c_lo) || (x rel_hi c_hi)
 * and optimize.
 */
static void find_cf_and_or_walker(ir_node *block, void *ctx)
{
	bool_opt_env_t *env = (bool_opt_env_t*)ctx;
	int low_idx, up_idx;
	int n_cfgpreds;

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

		ir_node *const cond_selector = get_Cond_selector(cond);
		if (!is_Cond(cond))
			continue;

		ir_node *const lower_pred = get_Block_cfgpred_block(lower_block, 0);
		if (lower_pred == NULL)
			continue;
		for (up_idx = 0; up_idx < n_cfgpreds; ++up_idx) {
			ir_node   *upper_block;
			ir_node   *upper_cf;
			ir_node   *replacement;
			cond_pair  cpair;

			upper_cf = get_Block_cfgpred(block, up_idx);
			upper_cf = skip_empty_blocks(upper_cf);
			if (is_Bad(upper_cf))
				continue;
			upper_block = get_nodes_block(upper_cf);
			if (upper_block != lower_pred)
				continue;
			if (!block_dominates(upper_block, block))
				continue;

			/* we have found the structure */
			/* check Phis: There must be NO Phi in block that
			   depends on the existence of low block */
			if (!can_fuse_block_inputs(block, low_idx, up_idx))
				continue;

			/* all fine, try it */
			ir_node *const upper_cond = get_Proj_pred(upper_cf);
			if (!is_Cond(upper_cond))
				continue;

			ir_node *const upper_cond_selector = get_Cond_selector(upper_cond);
			if (!find_cond_pair(cond_selector, upper_cond_selector, &cpair))
				continue;

			/* normalize pncs: we need the true case to jump into the
			 * common block (i.e. conjunctive normal form) */
			if (get_Proj_num(lower_cf) == pn_Cond_false) {
				if (cpair.cmp_lo == cond_selector) {
					normalize_cmp(&cpair.cmp_lo, &cpair.rel_lo, &cpair.tv_lo);
				} else {
					normalize_cmp(&cpair.cmp_hi, &cpair.rel_hi, &cpair.tv_hi);
				}
			}
			if (get_Proj_num(upper_cf) == pn_Cond_false) {
				if (cpair.cmp_lo == upper_cond_selector) {
					normalize_cmp(&cpair.cmp_lo, &cpair.rel_lo, &cpair.tv_lo);
				} else {
					normalize_cmp(&cpair.cmp_hi, &cpair.rel_hi, &cpair.tv_hi);
				}
			}

			/* can we optimize the case? */
			replacement = bool_or(&cpair, upper_block);
			if (replacement == NULL)
				continue;

			env->changed = 1;

			DB((dbg, LEVEL_1, "boolopt: %+F: fusing (ub %+F lb %+F)\n",
				get_irn_irg(upper_block), upper_block, lower_block));

			/* move all expressions on the path to lower/upper block */
			move_nodes_to_block(get_Block_cfgpred(block, up_idx), upper_block);
			move_nodes_to_block(get_Block_cfgpred(block, low_idx), lower_block);

			/* move all nodes from lower block to upper block */
			exchange(lower_block, upper_block);

			remove_block_input(block, up_idx);
			--n_cfgpreds;

			/* the optimizations expected the true case to jump */
			if (get_Proj_num(lower_cf) == pn_Cond_false) {
				ir_node *block = get_nodes_block(replacement);
				replacement    = new_r_Not(block, replacement);
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

	env.changed = 0;

	/* optimize simple Andb and Orb cases */
	irg_walk_graph(irg, NULL, bool_walk, &env);

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);

	/* now more complicated cases: find control flow And/Or and optimize. */
	ir_reserve_resources(irg, IR_RESOURCE_BLOCK_MARK | IR_RESOURCE_PHI_LIST);
	irg_walk_graph(irg, clear_block_infos, collect_phis, NULL);
	irg_block_walk_graph(irg, NULL, find_cf_and_or_walker, &env);
	ir_free_resources(irg, IR_RESOURCE_BLOCK_MARK | IR_RESOURCE_PHI_LIST);

	confirm_irg_properties(irg,
		env.changed ? IR_GRAPH_PROPERTIES_NONE : IR_GRAPH_PROPERTIES_ALL);
}
