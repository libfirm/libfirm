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
 * @brief   Optimizations regarding Confirm nodes.
 * @author  Michael Beck
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#undef DEBUG_CONFIRM

#include "tv_t.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "iropt_dbg.h"
#include "opt_confirms.h"
#include "irflag_t.h"
#include "irprintf.h"

enum range_tags {
	MIN_INCLUDED = 0x00,  /**< [min, ... */
	MAX_INCLUDED = 0x00,  /**< ..., max] */
	MIN_EXCLUDED = 0x01,  /**< (min, ... */
	MAX_EXCLUDED = 0x02   /**< ..., max) */
};

/**
 * An interval. We could use
 * intervals that ALWAYS include its borders, even for
 * floating point, as the precision is limited.
 * However, as our tarval module did not support
 * such kind of operation, we use border flags allowing
 * all intervals.
 */
typedef struct _interval_t {
	tarval        *min;   /**< lowest border */
	tarval        *max;   /**< highest border */
	unsigned char flags;  /**< border flags */
} interval_t;

#ifdef DEBUG_CONFIRM

#define compare_iv(l_iv, r_iv, pnc)		compare_iv_dbg(l_iv, r_iv, pnc)

/* forward */
static tarval *compare_iv_dbg(const interval_t *l_iv, const interval_t *r_iv, pn_Cmp pnc);

/* triangle */
#define DBG_OUT_TR(l_pnc, l_bound, r_pnc, r_bound, pnc, v) \
  ir_printf("In %e:\na %= %n && b %= %n  ==>  a %= b == %s\n", \
    get_irg_entity(current_ir_graph), \
    l_pnc, l_bound, r_pnc, r_bound, pnc, v);

/* right side */
#define DBG_OUT_R(r_pnc, r_bound, left, pnc, right, v) \
  ir_printf("In %e:\na %= %n ==>  %n %= %n == %s\n", \
    get_irg_entity(current_ir_graph), \
    r_pnc, r_bound, left, pnc, right, v);

/* left side */
#define DBG_OUT_L(l_pnc, l_bound, left, pnc, right, v) \
  ir_printf("In %e:\na %= %n ==>  %n %= %n == %s\n", \
    get_irg_entity(current_ir_graph), \
    l_pnc, l_bound, left, pnc, right, v);

#else

#define DBG_OUT_TR(l_pnc, l_bound, r_pnc, r_bound, pnc, v)
#define DBG_OUT_R(r_pnc, r_bound, left, pnc, right, v)
#define DBG_OUT_L(l_pnc, l_bound, left, pnc, right, v)

#endif /* DEBUG_CONFIRM */

/*
 * Check, if the value of a node is != 0.
 *
 * This is a often needed case, so we handle here Confirm
 * nodes too.
 */
int value_not_zero(ir_node *blk, ir_node *n, ir_node **confirm) {
#define RET_ON(x)  if (x) { *confirm = n; return 1; }; break

	tarval *tv;
	ir_mode *mode = get_irn_mode(n);
	pn_Cmp pnc;

	if (get_irg_pinned(get_irn_irg(blk)) != op_pin_state_pinned)
		blk = NULL;

	*confirm = NULL;

	/* there might be several Confirms one after other that form an interval */
	for (; is_Confirm(n); n = get_Confirm_value(n)) {
		unsigned long region = get_Confirm_region(n);

		/* check if it's legal to use this confirm. */
		if (region != 0) {
			ir_node *curr_blk = get_irn_n(n, -1);

			/* this confirm is bound to a region. */
			if (! blk)
				continue;

			if (get_Block_exc_region(curr_blk) != region)
				continue;
			/* all went fine, the current Confirm belongs to the same region */
		}

		/*
		 * Note: A Confirm is never after a Const. So,
		 * we simply can check the bound for being a Const
		 * without the fear that is might be hidden by a further Confirm.
		 */
		tv = value_of(get_Confirm_bound(n));
		if (tv == tarval_bad)
			return 0;

		pnc = tarval_cmp(tv, get_mode_null(mode));

		/*
		 * Beware: C might by a NaN. It is not clear, what we should do
		 * than. Of course a NaN is != 0, but we might use this function
		 * to remove up Exceptions, and NaN's might generate Exception.
		 * So, we do NOT handle NaNs here for safety.
		 *
		 * Note that only the C != 0 case need additional checking.
		 */
		switch (get_Confirm_cmp(n)) {
		case pn_Cmp_Eq: /* n == C /\ C != 0 ==> n != 0 */
			RET_ON(pnc != pn_Cmp_Eq && pnc != pn_Cmp_Uo);
		case pn_Cmp_Lg: /* n != C /\ C == 0 ==> n != 0 */
			RET_ON(pnc == pn_Cmp_Eq);
		case pn_Cmp_Lt: /* n <  C /\ C <= 0 ==> n != 0 */
			RET_ON(pnc == pn_Cmp_Lt || pnc == pn_Cmp_Eq);
		case pn_Cmp_Le: /* n <= C /\ C <  0 ==> n != 0 */
			RET_ON(pnc == pn_Cmp_Lt);
		case pn_Cmp_Ge: /* n >= C /\ C >  0 ==> n != 0 */
			RET_ON(pnc == pn_Cmp_Gt);
		case pn_Cmp_Gt: /* n >  C /\ C >= 0 ==> n != 0 */
			RET_ON(pnc == pn_Cmp_Gt || pnc == pn_Cmp_Eq);
		default:
			break;
		}
	}
	tv = value_of(n);

	if (tv == tarval_bad)
		return 0;

	pnc = tarval_cmp(tv, get_mode_null(mode));

	/* again, need check for NaN */
	return (pnc != pn_Cmp_Eq) && (pnc != pn_Cmp_Uo);

#undef RET_ON
}  /* value_not_zero */

/*
 * Check, if the value of a node cannot represent a NULL pointer.
 *
 * - Casts are skipped
 * - If sel_based_null_check_elim is enabled, all
 *   Sel nodes can be skipped.
 * - A SymConst(entity) is NEVER a NULL pointer
 * - Confirms are evaluated
 */
int value_not_null(ir_node *blk, ir_node *n, ir_node **confirm) {
	ir_op *op;

	if (get_irg_pinned(get_irn_irg(blk)) != op_pin_state_pinned)
		blk = NULL;
	*confirm = NULL;
	n  = skip_Cast(n);

	op = get_irn_op(n);
	assert(mode_is_reference(get_irn_mode(n)));
	if (get_opt_sel_based_null_check_elim()) {
		/* skip all Sel nodes and Cast's */
		while (op == op_Sel) {
			n = skip_Cast(get_Sel_ptr(n));
			op = get_irn_op(n);
		}
	}
	if (op == op_SymConst && get_SymConst_kind(n) == symconst_addr_ent)
		return 1;
	if (op == op_Const) {
		tarval *tv = get_Const_tarval(n);

		if (tv != tarval_bad && classify_tarval(tv) != TV_CLASSIFY_NULL)
			return 1;
	} else {
		for (; is_Confirm(n); n = skip_Cast(get_Confirm_value(n))) {
			unsigned long region = get_Confirm_region(n);

			/* check if it's legal to use this confirm. */
			if (region != 0) {
				ir_node *curr_blk = get_irn_n(n, -1);

				/* this confirm is bound to a region. */
				if (! blk)
					continue;

				if (get_Block_exc_region(curr_blk) != region)
					continue;
				/* all went fine, the current Confirm belongs to the same region */
			}

			if (get_Confirm_cmp(n) == pn_Cmp_Lg &&
				classify_Const(get_Confirm_bound(n)) == CNST_NULL) {
					*confirm = n;
					return 1;
			}
		}
	}
	return 0;
}  /* value_not_null */

/*
 * Check, if the value of a node can be confirmed >= 0 or <= 0,
 * If the mode of the value did not honor signed zeros, else
 * check for >= 0 or < 0.
 */
value_classify_sign classify_value_sign(ir_node *n) {
	tarval *tv, *c;
	ir_mode *mode;
	pn_Cmp cmp, ncmp;

	if (get_irn_op(n) != op_Confirm)
		return value_classified_unknown;

	tv  = value_of(get_Confirm_bound(n));
	if (tv == tarval_bad)
		return value_classified_unknown;

	mode = get_irn_mode(n);

	/*
	 * We can handle only >=, >, <, <= cases.
	 * We could handle == too, but this will be optimized into
	 * a constant either.
	 *
	 * Note that for integer modes we have a slightly better
	 * optimization possibilities, so we handle this
	 * different.
	 */
	cmp = get_Confirm_cmp(n);

	switch (cmp) {
	case pn_Cmp_Lt:
		/*
		 * must be x < c <= 1 to be useful if integer mode and -0 = 0
		 *         x < c <= 0 to be useful else
		 */
	case pn_Cmp_Le:
		/*
		 * must be x <= c < 1 to be useful if integer mode and -0 = 0
		 *         x <= c < 0 to be useful else
		 */
		c = mode_is_int(mode) && mode_honor_signed_zeros(mode) ?
			get_mode_one(mode) : get_mode_null(mode);

		ncmp = tarval_cmp(tv, c);
		if (ncmp == pn_Cmp_Eq)
			ncmp = pn_Cmp_Le;

		if (cmp != (ncmp ^ pn_Cmp_Eq))
			return value_classified_unknown;

		/* yep, negative */
		return value_classified_negative;

	case pn_Cmp_Ge:
		/*
		 * must be x >= c > -1 to be useful if integer mode
		 *         x >= c >= 0 to be useful else
		 */
	case pn_Cmp_Gt:
		/*
		 * must be x > c >= -1 to be useful if integer mode
		 *         x > c >= 0 to be useful else
		 */
		if (mode_is_int(mode)) {
			c = get_mode_minus_one(mode);

			ncmp = tarval_cmp(tv, c);
			if (ncmp == pn_Cmp_Eq)
				ncmp = pn_Cmp_Ge;

			if (cmp != (ncmp ^ pn_Cmp_Eq))
				return value_classified_unknown;
		} else {
			c = get_mode_minus_one(mode);

			ncmp = tarval_cmp(tv, c);

			if (ncmp != pn_Cmp_Eq && ncmp != pn_Cmp_Gt)
				return value_classified_unknown;
		}

		/* yep, positive */
		return value_classified_positive;

	default:
		return value_classified_unknown;
	}
}  /* classify_value_sign */

/**
 * construct an interval from a value
 *
 * @return the filled interval or NULL if no interval
 *         can be created (happens only on floating point
 */
static interval_t *get_interval_from_tv(interval_t *iv, tarval *tv) {
	ir_mode *mode = get_tarval_mode(tv);

	if (tv == tarval_bad) {
		if (mode_is_float(mode)) {
			/* NaN could be included which we cannot handle */
			iv->min   = tarval_bad;
			iv->max   = tarval_bad;
			iv->flags = MIN_EXCLUDED | MAX_EXCLUDED;
			return NULL;
		} else {
			/* [-oo, +oo] */
			iv->min   = get_mode_min(mode);
			iv->max   = get_mode_max(mode);
			iv->flags = MIN_INCLUDED | MAX_INCLUDED;
			return iv;
		}
	}

	if (mode_is_float(mode)) {
		if (tv == get_mode_NAN(mode)) {
			/* arg, we cannot handle NaN's. */
			iv->min   = tarval_bad;
			iv->max   = tarval_bad;
			iv->flags = MIN_EXCLUDED | MAX_EXCLUDED;
			return NULL;
		}
	}

	/* [tv, tv] */
	iv->min   = tv;
	iv->max   = tv;
	iv->flags = MIN_INCLUDED | MAX_INCLUDED;

	return iv;
}  /* get_interval_from_tv */

/**
 * construct an interval from a Confirm
 *
 * @param iv     an empty interval, will be filled
 * @param bound  the bound value
 * @param pnc    the Confirm compare relation
 *
 * @return the filled interval or NULL if no interval
 *         can be created (happens only on floating point
 */
static interval_t *get_interval(interval_t *iv, ir_node *bound, pn_Cmp pnc) {
	ir_mode *mode = get_irn_mode(bound);
	tarval  *tv   = value_of(bound);

	if (tv == tarval_bad) {
		/* There is nothing we could do here. For integer
		 * modes we could return [-oo, +oo], but there is
		 * nothing we could deduct from such an interval.
		 * So, speed things up and return unknown.
		 */
		iv->min   = tarval_bad;
		iv->max   = tarval_bad;
		iv->flags = MIN_EXCLUDED | MAX_EXCLUDED;
		return NULL;
	}

	if (mode_is_float(mode)) {
		if (tv == get_mode_NAN(mode)) {
			/* arg, we cannot handle NaN's. */
			iv->min   = tarval_bad;
			iv->max   = tarval_bad;
			iv->flags = MIN_EXCLUDED | MAX_EXCLUDED;

			return NULL;
		}
	}

	/* check which side is known */
	switch (pnc) {
	case pn_Cmp_Eq:
		/* [tv, tv] */
		iv->min   =
			iv->max   = tv;
		iv->flags = MIN_INCLUDED | MAX_INCLUDED;
		break;

	case pn_Cmp_Le:
		/* [-oo, tv] */
		iv->min   = get_mode_min(mode);
		iv->max   = tv;
		iv->flags = MIN_INCLUDED | MAX_INCLUDED;
		break;

	case pn_Cmp_Lt:
		/* [-oo, tv) */
		iv->min   = get_mode_min(mode);
		iv->max   = tv;
		iv->flags = MIN_INCLUDED | MAX_EXCLUDED;
		break;

	case pn_Cmp_Gt:
		/* (tv, +oo] */
		iv->min   = tv;
		iv->max   = get_mode_max(mode);
		iv->flags = MIN_EXCLUDED | MAX_INCLUDED;
		break;

	case pn_Cmp_Ge:
		/* [tv, +oo] */
		iv->min   = tv;
		iv->max   = get_mode_max(mode);
		iv->flags = MIN_INCLUDED | MAX_INCLUDED;
		break;

	case pn_Cmp_Leg:
		/*
		 * Ordered means, that at least neither
		 * our bound nor our value ara NaN's
		 */
		/* [-oo, +oo] */
		iv->min   = get_mode_min(mode);
		iv->max   = get_mode_max(mode);
		iv->flags = MIN_INCLUDED | MAX_INCLUDED;
		break;

	default:
		/*
		 * We do not handle UNORDERED, as a NaN
		 * could be included in the interval.
		 */
		iv->min   = tarval_bad;
		iv->max   = tarval_bad;
		iv->flags = MIN_EXCLUDED | MAX_EXCLUDED;
		return NULL;
	}

	if (iv->min != tarval_bad && iv->max != tarval_bad)
		return iv;
	return NULL;
}  /* get_interval */

/**
 * Try to evaluate l_iv pnc r_iv.
 *
 * @param l_iv   the left interval
 * @param r_iv   the right interval
 * @param pnc    the compare relation
 *
 * @return
 *   tarval_b_true or tarval_b_false it it can be evaluated,
 *   tarval_bad else
 */
static tarval *(compare_iv)(const interval_t *l_iv, const interval_t *r_iv, pn_Cmp pnc) {
	pn_Cmp res;
	unsigned flags;
	tarval *tv_true = tarval_b_true, *tv_false = tarval_b_false;

	/* if one interval contains NaNs, we cannot evaluate anything */
	if (! l_iv || ! r_iv)
		return tarval_bad;

	/* we can only check ordered relations */
	if (pnc & pn_Cmp_Uo) {
		tarval *t;

		pnc      = get_negated_pnc(pnc, get_tarval_mode(l_iv->min));
		t        = tv_true;
		tv_true  = tv_false;
		tv_false = t;
	}

	/* if we have > or >=, we do the inverse to save some cases */
	if (pnc == pn_Cmp_Ge || pnc == pn_Cmp_Gt) {
		const interval_t *t;

		pnc  = get_inversed_pnc(pnc);
		t    = l_iv;
		l_iv = r_iv;
		r_iv = t;
	}

	/* now, only the following cases remains */
	switch (pnc) {
	case pn_Cmp_Eq:
		/* two intervals can be compared for equality only if they are a single value */
		if (l_iv->min == l_iv->max && r_iv->min == r_iv->max)
			return tarval_cmp(l_iv->min, r_iv->min) == pn_Cmp_Eq ? tv_true : tv_false;

		/* if both intervals do not intersect, it is never equal */
		res = tarval_cmp(l_iv->max, r_iv->min);

		/* b < c ==> [a,b] != [c,d] */
		if (res == pn_Cmp_Lt)
			return tv_false;

		/* b <= c ==> [a,b) != [c,d]  AND [a,b] != (c,d] */
		if ((l_iv->flags & MAX_EXCLUDED || r_iv->flags & MIN_EXCLUDED)
			&& (res == pn_Cmp_Eq))
			return tv_false;

		res = tarval_cmp(r_iv->max, l_iv->min);

		/* d < a ==> [c,d] != [a,b] */
		if (res == pn_Cmp_Lt)
			return tv_false;

		/* d <= a ==> [c,d) != [a,b]  AND [c,d] != (a,b] */
		if ((r_iv->flags & MAX_EXCLUDED || l_iv->flags & MIN_EXCLUDED)
			&& (res == pn_Cmp_Eq))
			return tv_false;
		break;

	case pn_Cmp_Lg:
		/* two intervals can be compared for not equality only if they are a single value */
		if (l_iv->min == l_iv->max && r_iv->min == r_iv->max)
			return tarval_cmp(l_iv->min, r_iv->min) != pn_Cmp_Eq ? tv_true : tv_false;
		break;

	case pn_Cmp_Lt:
		res = tarval_cmp(l_iv->max, r_iv->min);

		/* [a, b] < [c, d]  <==> b < c */
		if (res == pn_Cmp_Lt)
			return tv_true;

		/* if one border is excluded, b <= c is enough */
		if ((l_iv->flags & MAX_EXCLUDED || r_iv->flags & MIN_EXCLUDED) &&
			res == pn_Cmp_Eq)
			return tv_true;

		/* [a, b] >= [c, d] <==> a > d */
		res = tarval_cmp(l_iv->min, r_iv->max);
		if (res == pn_Cmp_Gt)
			return tv_false;

		/* if one border is excluded, a >= d is enough */
		if ((l_iv->flags & MIN_EXCLUDED || r_iv->flags & MAX_EXCLUDED) &&
			res == pn_Cmp_Eq)
			return tv_false;
		break;

	case pn_Cmp_Le:
		/* [a, b) <= [c, d] or [a, b] <= (c, d]  <==> b <= c */
		flags = (l_iv->flags & MAX_EXCLUDED) | (r_iv->flags & MIN_EXCLUDED);
		if (flags) {
			res = tarval_cmp(l_iv->max, r_iv->min);

			if (res == pn_Cmp_Lt || res == pn_Cmp_Eq)
				return tv_true;
		}

		res = tarval_cmp(l_iv->min, r_iv->max);

		/* [a, b] > [c, d] <==> a > d */
		if (res == pn_Cmp_Gt)
			return tv_false;

		/* if one border is excluded, a >= d is enough */
		if ((l_iv->flags & MIN_EXCLUDED || r_iv->flags & MAX_EXCLUDED) &&
			res == pn_Cmp_Eq)
			return tv_false;
		break;

	case pn_Cmp_Leg:
		/* Hmm. if both are intervals, we can find an order */
		return tv_true;

	default:
		return tarval_bad;
	}
	return tarval_bad;
}  /* compare_iv */

/**
 * Returns non-zero, if a given relation is transitive.
 */
static int is_transitive(pn_Cmp pnc) {
	return (pn_Cmp_False < pnc && pnc < pn_Cmp_Lg);
}  /* is_transitive */

/**
 * returns a confirm node starting from n that is allowed to be used in the exception
 * region.
 *
 * @param blk  the block of the region
 * @param n    the node
 */
static ir_node *get_allowed_confirm(ir_node *blk, ir_node *n) {
	for (; is_Confirm(n); n = get_Confirm_value(n)) {
		ir_exc_region_t reg = get_Confirm_region(n);

		if (reg == 0 || (blk != NULL && reg == get_Block_exc_region(blk))) {
			/* found an allowed Confirm */
			return n;
		}

	}
	return NULL;
}  /* get_allowed_confirm */

/**
 * Return the value of a Cmp if one or both predecessors
 * are Confirm nodes.
 *
 * @param cmp    the Cmp node
 * @param left   the left operand of the Cmp
 * @param right  the right operand of the Cmp
 * @param pnc    the compare relation
 */
tarval *computed_value_Cmp_Confirm(ir_node *cmp, ir_node *left, ir_node *right, pn_Cmp pnc) {
	ir_node         *l_bound, *confirm;
	pn_Cmp          l_pnc, res_pnc, neg_pnc;
	interval_t      l_iv, r_iv;
	tarval          *tv;
	ir_mode         *mode;
	ir_node         *blk;

	/* beware, Cmp is not pinned. */
	blk = get_irn_n(cmp, -1);
	if (get_irg_pinned(get_Block_irg(blk)) != op_pin_state_pinned)
		blk = NULL;

	if ((confirm = get_allowed_confirm(blk, right)) != NULL) {
		/* we want the Confirm on the left side */
		right = left;
		left  = confirm;

		pnc = get_inversed_pnc(pnc);
	} else if ((confirm = get_allowed_confirm(blk, left)) != NULL) {
		left = confirm;
	} else {
		/* no Confirm on either one side, finish */
		return tarval_bad;
	}

	/* ok, here at least left is a Confirm, right might be */
	l_bound = get_Confirm_bound(left);
	l_pnc   = get_Confirm_cmp(left);

	if (is_Confirm(right)) {
		/*
		 * both sides are Confirm's. Check some rare cases first.
		 */
		ir_node *r_bound = get_Confirm_bound(right);
		pn_Cmp  r_pnc    = get_Confirm_cmp(right);

		/*
		 * some check can be made WITHOUT constant bounds
		 */
		if (r_bound == l_bound) {
			if (is_transitive(l_pnc)) {
				pn_Cmp r_inc_pnc = get_inversed_pnc(r_pnc);

				/*
				 * triangle inequality:
				 *
				 * a CMP B && B CMP b => a CMP b, !(a ~CMP b)
				 *
				 * We handle correctly cases with some <=/>= here
				 */
				if ((l_pnc & ~pn_Cmp_Eq) == (r_inc_pnc & ~pn_Cmp_Eq)) {
					res_pnc = (l_pnc & ~pn_Cmp_Eq) | (l_pnc & r_inc_pnc & pn_Cmp_Eq);

					if ((pnc == res_pnc) || ((pnc & ~pn_Cmp_Eq) == res_pnc)) {
						DBG_OUT_TR(l_pnc, l_bound, r_pnc, r_bound, pnc, "true");
						DBG_EVAL_CONFIRM(cmp);
						return tarval_b_true;
					} else {
						pn_Cmp neg_pnc = get_negated_pnc(pnc, get_irn_mode(left));

						if ((neg_pnc == res_pnc) || ((neg_pnc & ~pn_Cmp_Eq) == res_pnc)) {
							DBG_OUT_TR(l_pnc, l_bound, r_pnc, r_bound, pnc, "false");
							DBG_EVAL_CONFIRM(cmp);
							return tarval_b_false;
						}
					}
				}
			}
		}

		/*
		 * Here, we check only the right Confirm, as the left Confirms are
		 * checked later anyway.
		 */
		if (left == r_bound) {
			/*
			 * l == bound(r) AND pnc(r) == pnc:
			 *
			 * We know that a CMP b and check for that
			 */
			if ((r_pnc == pnc) || (r_pnc == (pnc & ~pn_Cmp_Eq))) {
				DBG_OUT_R(r_pnc, r_bound, left, pnc, right, "true");
				DBG_EVAL_CONFIRM(cmp);
				return tarval_b_true;
			}
			/*
			 * l == bound(r) AND pnc(r) != pnc:
			 *
			 * We know that a CMP b and check for a ~CMP b
			 */
			else {
				mode    = get_irn_mode(left);
				neg_pnc = get_negated_pnc(pnc, mode);

				if ((r_pnc == neg_pnc) || (r_pnc == (neg_pnc & ~pn_Cmp_Eq))) {
					DBG_OUT_R(r_pnc, r_bound, left, pnc, right, "false");
					DBG_EVAL_CONFIRM(cmp);
					return tarval_b_false;
				}
			}
		}

		/* now, try interval magic */
		tv = compare_iv(
			get_interval(&l_iv, l_bound, l_pnc),
			get_interval(&r_iv, r_bound, r_pnc),
			pnc);

		if (tv != tarval_bad) {
			DBG_EVAL_CONFIRM(cmp);
			return tv;
		}
	}

	/* from Here, check only left Confirm */

	/*
	 * some checks can be made WITHOUT constant bounds
	 */
	if (right == l_bound) {
		/*
		 * r == bound(l) AND pnc(l) == pnc:
		 *
		 * We know that a CMP b and check for that
		 */
		if ((l_pnc == pnc) || (l_pnc == (pnc & ~pn_Cmp_Eq))) {
			DBG_OUT_L(l_pnc, l_bound, left, pnc, right, "true");
			DBG_EVAL_CONFIRM(cmp);
			return tarval_b_true;
		}
		/*
		 * r == bound(l) AND pnc(l) is Not(pnc):
		 *
		 * We know that a CMP b and check for a ~CMP b
		 */
		else {
			mode = get_irn_mode(left);
			neg_pnc = get_negated_pnc(pnc, mode);

			if ((l_pnc == neg_pnc) || (l_pnc == (neg_pnc & ~pn_Cmp_Eq))) {
				DBG_OUT_L(l_pnc, l_bound, left, pnc, right, "false");
				DBG_EVAL_CONFIRM(cmp);
				return tarval_b_false;
			}
		}
	}

	/* now, only right == Const can help */
	tv = value_of(right);

	if (tv != tarval_bad) {
		tv = compare_iv(
			get_interval(&l_iv, l_bound, l_pnc),
			get_interval_from_tv(&r_iv, tv),
			pnc);
	}

	if (tv != tarval_bad)
		DBG_EVAL_CONFIRM(cmp);

	return tv;
}  /* computed_value_Cmp_Confirm */

#ifdef DEBUG_CONFIRM
/**
 * For debugging. Prints an interval into a string.
 *
 * @param buf   address of a string buffer
 * @param len   length of the string buffer
 * @param iv    the interval
 */
static int iv_snprintf(char *buf, size_t len, const interval_t *iv) {
	char smin[64], smax[64];

	if (iv) {
		tarval_snprintf(smin, sizeof(smin), iv->min);

		if (iv->min != iv->max || (iv->flags & (MIN_EXCLUDED|MAX_EXCLUDED))) {
			tarval_snprintf(smax, sizeof(smax), iv->max);

			return snprintf(buf, len, "%c%s, %s%c",
				iv->flags & MIN_EXCLUDED ? '(' : '[',
				smin, smax,
				iv->flags & MAX_EXCLUDED ? ')' : ']'
				);
		} else
			return snprintf(buf, len, "%s", smin);
	}
	return snprintf(buf, len, "<UNKNOWN>");
}  /* iv_snprintf */

/**
 * For debugging. Prints an interval compare.
 *
 * @param l_iv  the left interval
 * @param r_iv  the right interval
 * @param pnc   the compare relation
 */
static void print_iv_cmp(const interval_t *l_iv, const interval_t *r_iv, pn_Cmp pnc) {
	char sl[128], sr[128];

	iv_snprintf(sl, sizeof(sl), l_iv);
	iv_snprintf(sr, sizeof(sr), r_iv);

	ir_printf("%s %= %s", sl, pnc, sr);
}  /* print_iv_cmp */

/**
 * For debugging. call *compare_iv() and prints inputs and result.
 *
 * @param l_iv  the left interval
 * @param r_iv  the right interval
 * @param pnc   the compare relation
 */
static tarval *compare_iv_dbg(const interval_t *l_iv, const interval_t *r_iv, pn_Cmp pnc) {
	tarval *tv = (compare_iv)(l_iv, r_iv, pnc);

	if (tv == tarval_bad)
	return tv;

	ir_printf("In %e:\n", get_irg_entity(current_ir_graph));
	print_iv_cmp(l_iv, r_iv, pnc);
	ir_printf(" = %T\n", tv);
	return tv;
}  /* compare_iv_dbg */

#endif /* DEBUG_CONFIRM */
