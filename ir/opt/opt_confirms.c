/*
 * Project:     libFIRM
 * File name:   ir/opt/opt_confirms.c
 * Purpose:     Optimizations regarding Confirm nodes
 * Author:      Michael Beck
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "tv_t.h"
#include "iropt_t.h"
#include "opt_confirms.h"

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

/**
 * Check, if the value of a node is != 0.
 *
 * This is a often needed case, so we handle here Confirm
 * nodes too.
 */
int value_not_zero(ir_node *n)
{
#define RET_ON(x)  if (x) return 1; break

  tarval *tv;
  ir_mode *mode;
  pn_Cmp pnc;

  while (get_irn_op(n) == op_Confirm) {
    /*
     * Note: A Confirm is never after a Const. So,
     * we simply can check the bound for beeing a Const
     * without the fear that is might be hidden by a further Confirm.
     */
    tv = value_of(get_Confirm_bound(n));
    if (tv == tarval_bad)
      return 0;

    mode = get_tarval_mode(tv);
    pnc  = tarval_cmp(tv, get_mode_null(mode));

    switch (get_Confirm_cmp(n)) {
    case pn_Cmp_Eq: /* n == C /\ C != 0 ==> n != 0, should never happen, but simple */
      RET_ON(pnc != pn_Cmp_Eq);
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

    /* there might be several Confirms one after other that form an interval */
    n = get_Confirm_value(n);
  }
  tv = value_of(n);

  return (tv != tarval_bad) && (classify_tarval(tv) != TV_CLASSIFY_NULL);

#undef RET_ON
}

/*
 * Check, if the value of a node can be confirmed >= 0 or <= 0,
 * If the mode of the value did not honor signed zeros, else
 * check for >= 0 or < 0.
 */
value_classify classify_value_sign(ir_node *n)
{
  tarval *tv, *c;
  ir_mode *mode;
  pn_Cmp cmp, ncmp;

  if (get_irn_op(n) != op_Confirm)
    return VALUE_UNKNOWN;

  tv  = value_of(get_Confirm_bound(n));
  if (tv == tarval_bad)
    return VALUE_UNKNOWN;

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

    ncmp = tarval_cmp(tv, c) ^ pn_Cmp_Eq;
    if (cmp != ncmp)
      return VALUE_UNKNOWN;

    /* yep, negative */
    return VALUE_NEGATIVE;

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

      ncmp = tarval_cmp(tv, c) ^ pn_Cmp_Eq;
      if (cmp != ncmp)
        return VALUE_UNKNOWN;
    }
    else {
      c = get_mode_minus_one(mode);

      ncmp = tarval_cmp(tv, c);

      if (ncmp != pn_Cmp_Eq && ncmp != pn_Cmp_Gt)
        return VALUE_UNKNOWN;
    }

    /* yep, positive */
    return VALUE_POSITIVE;

  default:
    return VALUE_UNKNOWN;
  }
}

/**
 * construct an interval from a value
 */
static interval_t *get_interval_from_tv(interval_t *iv, tarval *tv)
{
  ir_mode *mode = get_tarval_mode(tv);

  if (tv == tarval_bad) {
    /* [-oo, +oo] */
    iv->min   = get_mode_min(mode);
    iv->max   = get_mode_max(mode);
    iv->flags = MIN_INCLUDED | MAX_INCLUDED;

    return iv;
  }

  /* [tv, tv] */
  iv->min   = tv;
  iv->max   = tv;
  iv->flags = MIN_INCLUDED | MAX_INCLUDED;

  return iv;
}

/**
 * construct an interval from a Confirm
 */
static interval_t *get_interval(interval_t *iv, ir_node *bound, pn_Cmp pnc)
{
  ir_mode *mode = get_irn_mode(bound);
  tarval  *tv   = value_of(bound);

  if (tv == tarval_bad) {
    /* [-oo, +oo] */
    iv->min   = get_mode_min(mode);
    iv->max   = get_mode_max(mode);
    iv->flags = MIN_INCLUDED | MAX_INCLUDED;

    return iv;
  }

  /* check which side is known */
  switch (pnc) {
  case pn_Cmp_Eq:
    /* [tv, tv] */
    iv->min   =
    iv->max   = tv;
    iv->flags = MIN_INCLUDED | MAX_INCLUDED;
    return iv;

  case pn_Cmp_Le:
    /* [-oo, tv] */
    iv->min   = get_mode_min(mode);
    iv->max   = tv;
    iv->flags = MIN_INCLUDED | MAX_INCLUDED;
    return iv;

  case pn_Cmp_Lt:
    /* [-oo, tv) */
    iv->min   = get_mode_min(mode);
    iv->max   = tv;
    iv->flags = MIN_INCLUDED | MAX_EXCLUDED;
    return iv;

  case pn_Cmp_Gt:
    /* (tv, +oo] */
    iv->min   = tv;
    iv->max   = get_mode_max(mode);
    iv->flags = MIN_EXCLUDED | MAX_INCLUDED;
    return iv;

  case pn_Cmp_Ge:
    /* [tv, +oo] */
    iv->min   = tv;
    iv->max   = get_mode_max(mode);
    iv->flags = MIN_INCLUDED | MAX_INCLUDED;
    return iv;

  default:
    /*
     * We did not handle UNORDERED yet. It is not
     * clear, if this could give any gain.
     */

    /* [-oo, +oo] */
    iv->min   = get_mode_min(mode);
    iv->max   = get_mode_max(mode);
    iv->flags = MIN_INCLUDED | MAX_INCLUDED;
    return iv;
  }
}

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
static tarval *compare_iv(const interval_t *l_iv, const interval_t *r_iv, pn_Cmp pnc)
{
  pn_Cmp res;
  unsigned flags;
  tarval *tv_true = tarval_b_true, *tv_false = tarval_b_false;

  /*
   * save some cases: we invert all cases with a >
   * Note that this turn Lg into Eq also (which is desired).
   */
  if (pnc & pn_Cmp_Gt) {
    pnc = get_inversed_pnc(pnc);

    tv_true  = tarval_b_false;
    tv_false = tarval_b_true;
  }

  switch (pnc) {
  case pn_Cmp_Eq:
    /* two intervals can be compared for equality only if they are a single value */
    if (l_iv->min == l_iv->max && r_iv->min == r_iv->max)
      return l_iv->min == r_iv->min ? tv_true : tv_false;
    break;

  case pn_Cmp_Lt:
    res = tarval_cmp(l_iv->max, r_iv->min);

    /* [a, b] < [c, d] */
    if (res == pn_Cmp_Lt)
      return tv_true;
    /* if one border is excluded, <= is ok */
    if ((l_iv->flags & MAX_EXCLUDED || r_iv->flags & MIN_EXCLUDED) &&
        res == pn_Cmp_Eq)
      return tv_true;

    /* [a, b) >= [c, d] or [a, b] >= (c, d] */
    flags = (l_iv->flags & MAX_EXCLUDED) | (r_iv->flags & MIN_EXCLUDED);

    if (flags) {
      res = tarval_cmp(l_iv->max, r_iv->min);

      if (res == pn_Cmp_Gt || res == pn_Cmp_Eq)
        return tv_false;
    }
    break;

  case pn_Cmp_Le:
    /* [a, b) <= [c, d] or [a, b] <= (c, d] */
    flags = (l_iv->flags & MAX_EXCLUDED) | (r_iv->flags & MIN_EXCLUDED);

    if (flags) {
      res = tarval_cmp(l_iv->max, r_iv->min);

      if (res == pn_Cmp_Lt || res == pn_Cmp_Eq)
        return tv_true;
    }

    res = tarval_cmp(l_iv->max, r_iv->min);

    /* [a, b] > [c, d] */
    if (res == pn_Cmp_Gt)
      return tv_false;
    /* if one border is excluded, >= is ok */
    if ((l_iv->flags & MAX_EXCLUDED || r_iv->flags & MIN_EXCLUDED) &&
        res == pn_Cmp_Eq)
      return tv_false;
    break;

  default:
    return tarval_bad;
  }
  return tarval_bad;
}

/**
 * Return the value of a Cmp if one or both predecessors
 * are Confirm nodes.
 *
 * @param left   the left operand of the Cmp
 * @param right  the right operand of the Cmp
 */
tarval *computed_value_Cmp_Confirm(ir_node *left, ir_node *right, pn_Cmp pnc)
{
  ir_node    *l_bound;
  pn_Cmp     l_pnc;
  interval_t l_iv, r_iv;
  tarval     *tv;

  if (get_irn_op(right) == op_Confirm) {
    ir_node *t;

    /* we want the Confirm on the left side */
    t     = left;
    left  = right;
    right = t;

    pnc = get_inversed_pnc(pnc);
  }
  else if (get_irn_op(left) != op_Confirm) {
    /* no Confirm on either one side, finish */
    return tarval_bad;
  }

  /* ok, here at least left is a Confirm, right might be */
  l_bound = get_Confirm_bound(left);
  l_pnc   = get_Confirm_cmp(left);

  if (get_irn_op(right) == op_Confirm) {
    /*
     * both sides are Confirm's. Check some rare cases first.
     */
    ir_node *r_bound = get_Confirm_bound(right);
    pn_Cmp  r_pnc = get_Confirm_cmp(right);

    /* check for == or != can sometime be made WITHOUT constant bounds */
    if (pnc == pn_Cmp_Eq || pnc == pn_Cmp_Lg) {
      /* l == r if bound(l) == bound(r) AND pnc(l) == pnc(r) == '=' */
      if (r_bound == l_bound && r_pnc == l_pnc && r_pnc == pn_Cmp_Eq) {
        return pnc == pn_Cmp_Eq ? tarval_b_true : tarval_b_false;
      }

      /*
       * Here, we check only the right Confirm, as the left Confirms are
       * checked later anyway.
       */

      if (left == r_bound && (r_pnc == pn_Cmp_Eq || r_pnc == pn_Cmp_Lg)) {
        /* l == bound(r) AND pnc(r) == pnc */
        if (r_pnc == pnc)
          return tarval_b_true;

        /* l == bound(r) AND pnc(r) != pnc */
        else
          return tarval_b_false;
      }
    }

    /* now, try interval magic */
    get_interval(&r_iv, r_bound, r_pnc);
    get_interval(&l_iv, l_bound, l_pnc);

    tv = compare_iv(&l_iv, &r_iv, pnc);
    if (tv != tarval_bad)
      return tv;
  }

  /* from Here, check only left Confirm */

  /* check for == or != can sometime be made WITHOUT constant bounds */
  if (pnc == pn_Cmp_Eq || pnc == pn_Cmp_Lg) {
    if (right == l_bound && (l_pnc == pn_Cmp_Eq || l_pnc == pn_Cmp_Lg)) {
      /* r == bound(l) AND pnc(l) == pnc */
      if (l_pnc == pnc)
        return tarval_b_true;

      /* r == bound(l) AND pnc(l) != pnc */
      else
        return tarval_b_false;
    }
  }

  /* now, only right == Const can help */
  tv = value_of(right);

  if (tv != tarval_bad) {
    get_interval_from_tv(&r_iv, tv);
    get_interval(&l_iv, l_bound, l_pnc);

    return compare_iv(&l_iv, &r_iv, pnc);
  }

  return tarval_bad;
}
