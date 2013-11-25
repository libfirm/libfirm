/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Optimizations regarding Confirm nodes.
 * @author  Michael Beck
 */
#undef DEBUG_CONFIRM

#include "tv_t.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "iropt_dbg.h"
#include "iroptimize.h"
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
typedef struct interval_t {
	ir_tarval     *min;   /**< lowest border */
	ir_tarval     *max;   /**< highest border */
	unsigned char flags;  /**< border flags */
} interval_t;

#ifdef DEBUG_CONFIRM

#define compare_iv(l_iv, r_iv, relation)    compare_iv_dbg(l_iv, r_iv, relation)

/* forward */
static tarval *compare_iv_dbg(const interval_t *l_iv, const interval_t *r_iv, ir_relation relation);

/* triangle */
#define DBG_OUT_TR(l_relation, l_bound, r_relation, r_bound, relation, v) \
  ir_printf("In %e:\na %= %n && b %= %n  ==>  a %= b == %s\n", \
    get_irg_entity(current_ir_graph), \
    l_relation, l_bound, r_relation, r_bound, relation, v)

/* right side */
#define DBG_OUT_R(r_relation, r_bound, left, relation, right, v) \
  ir_printf("In %e:\na %= %n ==>  %n %= %n == %s\n", \
    get_irg_entity(current_ir_graph), \
    r_relation, r_bound, left, relation, right, v)

/* left side */
#define DBG_OUT_L(l_relation, l_bound, left, relation, right, v) \
  ir_printf("In %e:\na %= %n ==>  %n %= %n == %s\n", \
    get_irg_entity(current_ir_graph), \
    l_relation, l_bound, left, relation, right, v)

#else

#define DBG_OUT_TR(l_relation, l_bound, r_relation, r_bound, relation, v)  (void)0
#define DBG_OUT_R(r_relation, r_bound, left, relation, right, v)  (void)0
#define DBG_OUT_L(l_relation, l_bound, left, relation, right, v)  (void)0

#endif /* DEBUG_CONFIRM */

/*
 * Check, if the value of a node is != 0.
 *
 * This is a often needed case, so we handle here Confirm
 * nodes too.
 */
int value_not_zero(const ir_node *n, const ir_node **confirm)
{
#define RET_ON(x)  if (x) { *confirm = n; return 1; } break

	ir_tarval *tv;
	ir_mode *mode = get_irn_mode(n);
	ir_relation relation;

	*confirm = NULL;

	/* there might be several Confirms one after other that form an interval */
	for (;;) {
		if (is_Minus(n)) {
			/* we can safely skip Minus when checking for != 0 */
			n = get_Minus_op(n);
			continue;
		}
		if (! is_Confirm(n))
			break;

		/*
		 * Note: A Confirm is never after a Const. So,
		 * we simply can check the bound for being a Const
		 * without the fear that is might be hidden by a further Confirm.
		 */
		tv = value_of(get_Confirm_bound(n));
		if (tv == tarval_bad) {
			n = get_Confirm_value(n);
			continue;
		}

		relation = tarval_cmp(tv, get_mode_null(mode));

		/*
		 * Beware: C might by a NaN. It is not clear, what we should do
		 * than. Of course a NaN is != 0, but we might use this function
		 * to remove up Exceptions, and NaN's might generate Exception.
		 * So, we do NOT handle NaNs here for safety.
		 *
		 * Note that only the C != 0 case need additional checking.
		 */
		switch (get_Confirm_relation(n)) {
		case ir_relation_equal: /* n == C /\ C != 0 ==> n != 0 */
			RET_ON(relation != ir_relation_equal && relation != ir_relation_unordered);
		case ir_relation_less_greater: /* n != C /\ C == 0 ==> n != 0 */
			RET_ON(relation == ir_relation_equal);
		case ir_relation_less: /* n <  C /\ C <= 0 ==> n != 0 */
			RET_ON(relation == ir_relation_less || relation == ir_relation_equal);
		case ir_relation_less_equal: /* n <= C /\ C <  0 ==> n != 0 */
			RET_ON(relation == ir_relation_less);
		case ir_relation_greater_equal: /* n >= C /\ C >  0 ==> n != 0 */
			RET_ON(relation == ir_relation_greater);
		case ir_relation_greater: /* n >  C /\ C >= 0 ==> n != 0 */
			RET_ON(relation == ir_relation_greater || relation == ir_relation_equal);
		default:
			break;
		}
		n = get_Confirm_value(n);
	}
	/* global entities are never NULL */
	if (is_SymConst_addr_ent(n))
		return true;

	tv = value_of(n);
	if (tv == tarval_bad)
		return false;

	relation = tarval_cmp(tv, get_mode_null(mode));

	/* again, need check for NaN */
	return (relation != ir_relation_equal) && (relation != ir_relation_unordered);

#undef RET_ON
}

/*
 * Check, if the value of a node cannot represent a NULL pointer.
 *
 * - Sels are skipped
 * - A SymConst(entity) is NEVER a NULL pointer
 * - Confirms are evaluated
 */
int value_not_null(const ir_node *n, const ir_node **confirm)
{
	ir_tarval *tv;

	*confirm = NULL;

	tv = value_of(n);
	if (tarval_is_constant(tv) && ! tarval_is_null(tv))
		return 1;

	assert(mode_is_reference(get_irn_mode(n)));
	/* skip all Sel nodes */
	while (is_Sel(n)) {
		n = get_Sel_ptr(n);
	}
	while (1) {
		if (is_Proj(n)) { n = get_Proj_pred(n); continue; }
		break;
	}

	if (is_SymConst_addr_ent(n)) {
		/* global references are never NULL */
		return 1;
	} else if (n == get_irg_frame(get_irn_irg(n))) {
		/* local references are never NULL */
		return 1;
	} else if (is_Alloc(n)) {
		/* alloc never returns NULL (it throws an exception instead) */
		return 1;
	} else {
		/* check for more Confirms */
		for (; is_Confirm(n); n = get_Confirm_value(n)) {
			if (get_Confirm_relation(n) == ir_relation_less_greater) {
				ir_node   *bound = get_Confirm_bound(n);
				ir_tarval *tv    = value_of(bound);

				if (tarval_is_null(tv)) {
					*confirm = n;
					return 1;
				}
			}
		}
	}
	return 0;
}

/**
 * construct an interval from a value
 *
 * @return the filled interval or NULL if no interval
 *         can be created (happens only on floating point
 */
static interval_t *get_interval_from_tv(interval_t *iv, ir_tarval *tv)
{
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

	if (mode_is_float(mode) && tarval_is_nan(tv)) {
		/* arg, we cannot handle NaN's. */
		iv->min   = tarval_bad;
		iv->max   = tarval_bad;
		iv->flags = MIN_EXCLUDED | MAX_EXCLUDED;
		return NULL;
	}

	/* [tv, tv] */
	iv->min   = tv;
	iv->max   = tv;
	iv->flags = MIN_INCLUDED | MAX_INCLUDED;

	return iv;
}

/**
 * construct an interval from a Confirm
 *
 * @param iv       an empty interval, will be filled
 * @param bound    the bound value
 * @param relation the Confirm compare relation
 *
 * @return the filled interval or NULL if no interval
 *         can be created (happens only on floating point
 */
static interval_t *get_interval(interval_t *iv, ir_node *bound, ir_relation relation)
{
	ir_mode   *mode = get_irn_mode(bound);
	ir_tarval *tv   = value_of(bound);

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

	if (mode_is_float(mode) && tarval_is_nan(tv)) {
		/* arg, we cannot handle NaN's. */
		iv->min   = tarval_bad;
		iv->max   = tarval_bad;
		iv->flags = MIN_EXCLUDED | MAX_EXCLUDED;

		return NULL;
	}

	/* check which side is known */
	switch (relation) {
	case ir_relation_equal:
		/* [tv, tv] */
		iv->min   =
			iv->max   = tv;
		iv->flags = MIN_INCLUDED | MAX_INCLUDED;
		break;

	case ir_relation_less_equal:
		/* [-oo, tv] */
		iv->min   = get_mode_min(mode);
		iv->max   = tv;
		iv->flags = MIN_INCLUDED | MAX_INCLUDED;
		break;

	case ir_relation_less:
		/* [-oo, tv) */
		iv->min   = get_mode_min(mode);
		iv->max   = tv;
		iv->flags = MIN_INCLUDED | MAX_EXCLUDED;
		break;

	case ir_relation_greater:
		/* (tv, +oo] */
		iv->min   = tv;
		iv->max   = get_mode_max(mode);
		iv->flags = MIN_EXCLUDED | MAX_INCLUDED;
		break;

	case ir_relation_greater_equal:
		/* [tv, +oo] */
		iv->min   = tv;
		iv->max   = get_mode_max(mode);
		iv->flags = MIN_INCLUDED | MAX_INCLUDED;
		break;

	case ir_relation_less_equal_greater:
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
}

/**
 * Try to evaluate l_iv relation r_iv.
 *
 * @param l_iv      the left interval
 * @param r_iv      the right interval
 * @param relation  the compare relation
 *
 * @return
 *   tarval_b_true or tarval_b_false it it can be evaluated,
 *   tarval_bad else
 */
static ir_tarval *(compare_iv)(const interval_t *l_iv, const interval_t *r_iv, ir_relation relation)
{
	ir_relation res;
	unsigned    flags;
	ir_tarval  *tv_true = tarval_b_true, *tv_false = tarval_b_false;

	/* if one interval contains NaNs, we cannot evaluate anything */
	if (! l_iv || ! r_iv)
		return tarval_bad;

	/* we can only check ordered relations */
	if (relation & ir_relation_unordered) {
		ir_tarval *t;

		relation = get_negated_relation(relation);
		t        = tv_true;
		tv_true  = tv_false;
		tv_false = t;
	}

	/* if we have > or >=, we do the inverse to save some cases */
	if (relation == ir_relation_greater_equal || relation == ir_relation_greater) {
		const interval_t *t;

		relation = get_inversed_relation(relation);
		t        = l_iv;
		l_iv     = r_iv;
		r_iv     = t;
	}

	/* now, only the following cases remains */
	switch (relation) {
	case ir_relation_equal:
		/* two intervals can be compared for equality only if they are a single value */
		if (l_iv->min == l_iv->max && r_iv->min == r_iv->max)
			return tarval_cmp(l_iv->min, r_iv->min) == ir_relation_equal ? tv_true : tv_false;

		/* if both intervals do not intersect, it is never equal */
		res = tarval_cmp(l_iv->max, r_iv->min);

		/* b < c ==> [a,b] != [c,d] */
		if (res == ir_relation_less)
			return tv_false;

		/* b <= c ==> [a,b) != [c,d]  AND [a,b] != (c,d] */
		if ((l_iv->flags & MAX_EXCLUDED || r_iv->flags & MIN_EXCLUDED)
			&& (res == ir_relation_equal))
			return tv_false;

		res = tarval_cmp(r_iv->max, l_iv->min);

		/* d < a ==> [c,d] != [a,b] */
		if (res == ir_relation_less)
			return tv_false;

		/* d <= a ==> [c,d) != [a,b]  AND [c,d] != (a,b] */
		if ((r_iv->flags & MAX_EXCLUDED || l_iv->flags & MIN_EXCLUDED)
			&& (res == ir_relation_equal))
			return tv_false;
		break;

	case ir_relation_less_greater:
		/* two intervals can be compared for not equality only if they are a single value */
		if (l_iv->min == l_iv->max && r_iv->min == r_iv->max)
			return tarval_cmp(l_iv->min, r_iv->min) != ir_relation_equal ? tv_true : tv_false;
		break;

	case ir_relation_less:
		res = tarval_cmp(l_iv->max, r_iv->min);

		/* [a, b] < [c, d]  <==> b < c */
		if (res == ir_relation_less)
			return tv_true;

		/* if one border is excluded, b <= c is enough */
		if ((l_iv->flags & MAX_EXCLUDED || r_iv->flags & MIN_EXCLUDED) &&
			res == ir_relation_equal)
			return tv_true;

		/* [a, b] >= [c, d] <==> a > d */
		res = tarval_cmp(l_iv->min, r_iv->max);
		if (res == ir_relation_greater)
			return tv_false;

		/* if one border is excluded, a >= d is enough */
		if ((l_iv->flags & MIN_EXCLUDED || r_iv->flags & MAX_EXCLUDED) &&
			res == ir_relation_equal)
			return tv_false;
		break;

	case ir_relation_less_equal:
		/* [a, b) <= [c, d] or [a, b] <= (c, d]  <==> b <= c */
		flags = (l_iv->flags & MAX_EXCLUDED) | (r_iv->flags & MIN_EXCLUDED);
		if (flags) {
			res = tarval_cmp(l_iv->max, r_iv->min);

			if (res == ir_relation_less || res == ir_relation_equal)
				return tv_true;
		}

		res = tarval_cmp(l_iv->min, r_iv->max);

		/* [a, b] > [c, d] <==> a > d */
		if (res == ir_relation_greater)
			return tv_false;

		/* if one border is excluded, a >= d is enough */
		if ((l_iv->flags & MIN_EXCLUDED || r_iv->flags & MAX_EXCLUDED) &&
			res == ir_relation_equal)
			return tv_false;
		break;

	case ir_relation_less_equal_greater:
		/* Hmm. if both are intervals, we can find an order */
		return tv_true;

	default:
		return tarval_bad;
	}
	return tarval_bad;
}

/**
 * Returns non-zero, if a given relation is transitive.
 */
static int is_transitive(ir_relation relation)
{
	return (ir_relation_false < relation && relation < ir_relation_less_greater);
}

/**
 * Return the value of a Cmp if one or both predecessors
 * are Confirm nodes.
 *
 * @param cmp      the Cmp node
 * @param left     the left operand of the Cmp
 * @param right    the right operand of the Cmp
 * @param relation the compare relation
 */
ir_tarval *computed_value_Cmp_Confirm(const ir_node *cmp, ir_node *left, ir_node *right, ir_relation relation)
{
	ir_node    *l_bound;
	ir_relation l_relation, res_relation, neg_relation;
	interval_t  l_iv, r_iv;
	ir_tarval  *tv;

	if (is_Confirm(right)) {
		/* we want the Confirm on the left side */
		ir_node *t = right;
		right = left;
		left  = t;

		relation = get_inversed_relation(relation);
	} else if (! is_Confirm(left)) {
		/* nothing more found */
		tv = tarval_bad;
		goto check_null_case;
	}

	/* ok, here at least left is a Confirm, right might be */
	l_bound    = get_Confirm_bound(left);
	l_relation = get_Confirm_relation(left);

	if (is_Confirm(right)) {
		/*
		 * both sides are Confirm's. Check some rare cases first.
		 */
		ir_node    *r_bound    = get_Confirm_bound(right);
		ir_relation r_relation = get_Confirm_relation(right);

		/*
		 * some check can be made WITHOUT constant bounds
		 */
		if (r_bound == l_bound) {
			if (is_transitive(l_relation)) {
				ir_relation r_inc_relation = get_inversed_relation(r_relation);

				/*
				 * triangle inequality:
				 *
				 * a CMP B && B CMP b => a CMP b, !(a ~CMP b)
				 *
				 * We handle correctly cases with some <=/>= here
				 */
				if ((l_relation & ~ir_relation_equal) == (r_inc_relation & ~ir_relation_equal)) {
					res_relation = (l_relation & ~ir_relation_equal) | (l_relation & r_inc_relation & ir_relation_equal);

					if ((relation == res_relation) || ((relation & ~ir_relation_equal) == res_relation)) {
						DBG_OUT_TR(l_relation, l_bound, r_relation, r_bound, relation, "true");
						DBG_EVAL_CONFIRM(cmp);
						return tarval_b_true;
					} else {
						ir_relation neg_relation = get_negated_relation(relation);

						if ((neg_relation == res_relation) || ((neg_relation & ~ir_relation_equal) == res_relation)) {
							DBG_OUT_TR(l_relation, l_bound, r_relation, r_bound, relation, "false");
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
			 * l == bound(r) AND relation(r) == relation:
			 *
			 * We know that a CMP b and check for that
			 */
			if ((r_relation == relation) || (r_relation == (relation & ~ir_relation_equal))) {
				DBG_OUT_R(r_relation, r_bound, left, relation, right, "true");
				DBG_EVAL_CONFIRM(cmp);
				return tarval_b_true;
			}
			/*
			 * l == bound(r) AND relation(r) != relation:
			 *
			 * We know that a CMP b and check for a ~CMP b
			 */
			else {
				neg_relation = get_negated_relation(relation);

				if ((r_relation == neg_relation) || (r_relation == (neg_relation & ~ir_relation_equal))) {
					DBG_OUT_R(r_relation, r_bound, left, relation, right, "false");
					DBG_EVAL_CONFIRM(cmp);
					return tarval_b_false;
				}
			}
		}

		/* now, try interval magic */
		tv = compare_iv(
			get_interval(&l_iv, l_bound, l_relation),
			get_interval(&r_iv, r_bound, r_relation),
			relation);

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
		 * r == bound(l) AND relation(l) == relation:
		 *
		 * We know that a CMP b and check for that
		 */
		if ((l_relation == relation) || (l_relation == (relation & ~ir_relation_equal))) {
			DBG_OUT_L(l_relation, l_bound, left, relation, right, "true");
			DBG_EVAL_CONFIRM(cmp);
			return tarval_b_true;
		}
		/*
		 * r == bound(l) AND relation(l) is Not(relation):
		 *
		 * We know that a CMP b and check for a ~CMP b
		 */
		else {
			neg_relation = get_negated_relation(relation);

			if ((l_relation == neg_relation) || (l_relation == (neg_relation & ~ir_relation_equal))) {
				DBG_OUT_L(l_relation, l_bound, left, relation, right, "false");
				DBG_EVAL_CONFIRM(cmp);
				return tarval_b_false;
			}
		}
	}

	/* now, only right == Const can help */
	tv = value_of(right);

	if (tv != tarval_bad) {
		tv = compare_iv(
			get_interval(&l_iv, l_bound, l_relation),
			get_interval_from_tv(&r_iv, tv),
			relation);
	} else {
check_null_case:
		/* check some other cases */
		if ((relation == ir_relation_equal || relation == ir_relation_less_greater) &&
			is_Const(right) && is_Const_null(right)) {
			/* for == 0 or != 0 we have some special tools */
			ir_mode       *mode = get_irn_mode(left);
			const ir_node *dummy;
			if (mode_is_reference(mode)) {
				if (value_not_null(left, &dummy)) {
					tv = relation == ir_relation_equal ? tarval_b_false : tarval_b_true;
				}
			} else {
				if (value_not_zero(left, &dummy)) {
					tv = relation == ir_relation_equal ? tarval_b_false : tarval_b_true;
				}
			}
		}
	}

	if (tv != tarval_bad)
		DBG_EVAL_CONFIRM(cmp);

	return tv;
}

#ifdef DEBUG_CONFIRM
/**
 * For debugging. Prints an interval into a string.
 *
 * @param buf   address of a string buffer
 * @param len   length of the string buffer
 * @param iv    the interval
 */
static int iv_snprintf(char *buf, size_t len, const interval_t *iv)
{
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
}

/**
 * For debugging. Prints an interval compare.
 *
 * @param l_iv      the left interval
 * @param r_iv      the right interval
 * @param relation  the compare relation
 */
static void print_iv_cmp(const interval_t *l_iv, const interval_t *r_iv, ir_relation relation)
{
	char sl[128], sr[128];

	iv_snprintf(sl, sizeof(sl), l_iv);
	iv_snprintf(sr, sizeof(sr), r_iv);

	ir_printf("%s %= %s", sl, relation, sr);
}

/**
 * For debugging. call *compare_iv() and prints inputs and result.
 *
 * @param l_iv     the left interval
 * @param r_iv     the right interval
 * @param relation the compare relation
 */
static tarval *compare_iv_dbg(const interval_t *l_iv, const interval_t *r_iv, ir_relation relation)
{
	tarval *tv = (compare_iv)(l_iv, r_iv, relation);

	if (tv == tarval_bad)
	return tv;

	ir_printf("In %e:\n", get_irg_entity(current_ir_graph));
	print_iv_cmp(l_iv, r_iv, relation);
	ir_printf(" = %T\n", tv);
	return tv;
}

#endif /* DEBUG_CONFIRM */
