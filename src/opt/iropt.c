/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   iropt --- optimizations intertwined with IR construction.
 * @author  Christian Schaefer, Goetz Lindenmaier, Michael Beck
 */
#include "iropt_t.h"

#include "array.h"
#include "bitfiddle.h"
#include "constbits.h"
#include "dbginfo_t.h"
#include "entity_t.h"
#include "firm_types.h"
#include "hashptr.h"
#include "irarch.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "irflag_t.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgraph_t.h"
#include "irhooks.h"
#include "irhooks.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "iropt_dbg.h"
#include "iroptimize.h"
#include "irtools.h"
#include "irverify.h"
#include "panic.h"
#include "target_t.h"
#include "tv_t.h"
#include "vrp.h"
#include <stdbool.h>
#include <string.h>

static bool imprecise_float_transforms_allowed;

void ir_allow_imprecise_float_transforms(int enable)
{
	imprecise_float_transforms_allowed = enable;
}

int ir_imprecise_float_transforms_allowed(void)
{
	return imprecise_float_transforms_allowed;
}

/** Returns true if using an Add, Eor or Or instead of @p node would produce
 * the same result. */
static bool is_Or_Eor_Add(const ir_node *node)
{
	if (is_Or(node) || is_Eor(node) || is_Add(node)) {
		const ir_node *const left  = get_binop_left(node);
		const ir_node *const right = get_binop_right(node);
		const bitinfo *const bl    = get_bitinfo(left);
		const bitinfo *const br    = get_bitinfo(right);
		/* if each bit is guaranteed to be zero on either the left or right
		 * then an Add will have the same effect as the Eor/Or.
		 */
		if (bl && br && tarval_is_null(tarval_and(bl->z, br->z)))
			return true;
	}
	return false;
}

/** Returns true if using an Add or Eor instead of @p node would produce the
 * same result. */
static bool is_Eor_Add(const ir_node *node)
{
	if (is_Or_Eor_Add(node))
		return true;
	if (!is_Eor(node) && !is_Add(node))
		return false;

	const ir_mode *mode = get_irn_mode(node);
	if (get_mode_arithmetic(mode) != irma_twos_complement)
		return false;

	const ir_node *const left        = get_binop_left(node);
	const ir_node *const right       = get_binop_right(node);
	const bitinfo *const bl          = get_bitinfo(left);
	const bitinfo *const br          = get_bitinfo(right);
	const int            highest_bit = (int)get_mode_size_bits(mode) - 1;

	return (bl && get_tarval_lowest_bit(bl->z) == highest_bit) ||
	       (br && get_tarval_lowest_bit(br->z) == highest_bit);
}

/**
 * If node produces a left shift with a constant value return that value.
 * This matches for x << c and x * C2 with C2 being a power of 2.
 */
static ir_tarval *is_shl_const_like(const ir_node *node)
{
	if (is_Shl(node)) {
		ir_node *right = get_Shl_right(node);
		if (is_Const(right))
			return get_Const_tarval(right);
	} else if (is_Mul(node)) {
		ir_node *right = get_Mul_right(node);
		if (is_Const(right)) {
			const ir_tarval *tv = get_Const_tarval(right);
			/* is value a power of 2? */
			if (get_tarval_popcount(tv) == 1) {
				int low_bit = get_tarval_lowest_bit(tv);
				return new_tarval_from_long(low_bit, get_tarval_mode(tv));
			}
		}
	}
	return NULL;
}

static bool is_size_minus_1(ir_node const *const irn, ir_mode *const mode)
{
	if (is_Const(irn)) {
		ir_tarval *const tv = get_Const_tarval(irn);
		if (tarval_is_long(tv) && get_tarval_long(tv) == (long)get_mode_size_bits(mode) - 1)
			return true;
	}
	return false;
}

/**
 * Returns the tarval of a Const node or tarval_unknown for all other nodes.
 */
static ir_tarval *default_value_of(const ir_node *n)
{
	if (is_Const(n))
		return get_Const_tarval(n);
	else
		return tarval_unknown;
}

value_of_func value_of_ptr = default_value_of;

void set_value_of_func(value_of_func func)
{
	if (func != NULL)
		value_of_ptr = func;
	else
		value_of_ptr = default_value_of;
}

int value_not_null(const ir_node *n, const ir_node **confirm)
{
	if (confirm)
		*confirm = NULL;

	/* walk confirm sequence and look for matching confirms */
	for (;;) {
		/* -x != 0  =>  x != 0 */
		if (is_Minus(n)) {
			n = get_Minus_op(n);
			continue;
		}
		/* we can ignore Sels/Members: either the base pointer points to null or
		 * if it does not then members addresses cannot be at NULL or we have
		 * undefined behavior because we are obviously not pointing to an
		 * object. */
		if (is_Sel(n)) {
			n = get_Sel_ptr(n);
			continue;
		} else if (is_Member(n)) {
			n = get_Member_ptr(n);
			continue;
		}

		if (!is_Confirm(n))
			break;

		ir_node *bound = get_Confirm_bound(n);
		if (!is_Const(bound)) {
			n = get_Confirm_value(n);
			continue;
		}
		ir_tarval  *tv       = get_Const_tarval(bound);
		ir_mode    *mode     = get_irn_mode(n);
		ir_tarval  *null     = get_mode_null(mode);
		ir_relation relation = tarval_cmp(tv, null);

		switch (get_Confirm_relation(n)) {
		case ir_relation_equal: /* n == C && C != 0 ==> n != 0 */
			if (relation != ir_relation_equal)
				goto confirmed;
			break;
		case ir_relation_less_greater: /* n != C /\ C == 0 ==> n != 0 */
			if (relation == ir_relation_equal)
				goto confirmed;
			break;
		case ir_relation_less: /* n <  C /\ C <= 0 ==> n != 0 */
			if (relation == ir_relation_less || relation == ir_relation_equal)
				goto confirmed;
			break;
		case ir_relation_less_equal: /* n <= C /\ C <  0 ==> n != 0 */
			if (relation == ir_relation_less)
				goto confirmed;
			break;
		case ir_relation_greater_equal: /* n >= C /\ C >  0 ==> n != 0 */
			if (relation == ir_relation_greater)
				goto confirmed;
			break;
		case ir_relation_greater: /* n >  C /\ C >= 0 ==> n != 0 */
			if (relation == ir_relation_greater
			    || relation == ir_relation_equal) {
confirmed:
				if (confirm)
					*confirm = n;
				return true;
			}
			break;
		default:
			break;
		}
		n = get_Confirm_value(n);
	}

	if (is_Const(n)) {
		ir_tarval *tv = get_Const_tarval(n);
		return !tarval_is_null(tv);
	}

	/* global entities are never NULL */
	if (is_Address(n))
		return true;

	/* the frame pointer is never NULL */
	if (is_Proj(n) && is_Start(get_Proj_pred(n))
	    && get_Proj_num(n) == pn_Start_P_frame_base)
	    return true;

	/* alloc never returns NULL (but throws an exception in the error case) */
	if (is_Alloc(n))
		return true;

	const bitinfo *bi = get_bitinfo(n);
	if (bi != NULL && !tarval_is_null(bi->o))
		return true;

	/* for all we know the value may be null */
	return false;
}

/**
 * Return the value of a Constant.
 */
static ir_tarval *computed_value_Const(const ir_node *n)
{
	return get_Const_tarval(n);
}

/**
 * Return the value of an Offset.
 */
static ir_tarval *computed_value_Offset(const ir_node *n)
{
	const ir_entity *ent  = get_Offset_entity(n);
	const ir_type   *type = get_entity_owner(ent);
	if (get_type_state(type) == layout_fixed)
		return new_tarval_from_long(get_entity_offset(ent), get_irn_mode(n));
	return tarval_unknown;
}

/**
 * Return the value of an Align.
 */
static ir_tarval *computed_value_Align(const ir_node *n)
{
	ir_type const *const type = get_Align_type(n);
	if (get_type_state(type) == layout_fixed)
		return new_tarval_from_long(get_type_alignment(type), get_irn_mode(n));
	return tarval_unknown;
}

/**
 * Return the value of a Size.
 */
static ir_tarval *computed_value_Size(const ir_node *n)
{
	ir_type const *const type = get_Size_type(n);
	if (get_type_state(type) == layout_fixed)
		return new_tarval_from_long(get_type_size(type), get_irn_mode(n));
	return tarval_unknown;
}

static bool complement_values(const ir_node *a, const ir_node *b)
{
	if (is_Eor(a) && is_Eor(b) && get_Eor_left(a) == get_Eor_left(b)) {
		a = get_Eor_right(a);
		b = get_Eor_right(b);
	}
	if ((is_Not(a) && get_Not_op(a) == b) ||
	    (is_Not(b) && get_Not_op(b) == a))
		return true;
	if (is_Const(a) && is_Const(b)) {
		ir_tarval *const tv_a = get_Const_tarval(a);
		ir_tarval *const tv_b = get_Const_tarval(b);
		return tarval_is_all_one(tarval_eor(tv_a, tv_b));
	}
	return false;
}

/**
 * Return the value of an Add.
 */
static ir_tarval *computed_value_Add(const ir_node *n)
{
	const ir_node *a  = get_Add_left(n);
	const ir_node *b  = get_Add_right(n);
	ir_tarval     *ta = value_of(a);
	ir_tarval     *tb = value_of(b);

	if (ta != tarval_unknown && tb != tarval_unknown)
		return tarval_add(ta, tb);

	/* x+~x => -1 */
	if (complement_values(a, b))
		return get_mode_all_one(get_irn_mode(n));
	/* x + -x => 0 */
	if (ir_is_negated_value(a, b)) {
		ir_mode *mode = get_irn_mode(n);
		if (get_mode_arithmetic(mode) == irma_twos_complement)
			return get_mode_null(mode);
	}

	return tarval_unknown;
}

/**
 * Return the value of a Sub.
 * Special case: a - a
 */
static ir_tarval *computed_value_Sub(const ir_node *n)
{
	ir_mode       *mode = get_irn_mode(n);
	const ir_node *a    = get_Sub_left(n);
	const ir_node *b    = get_Sub_right(n);

	/* a - a == 0 (not possible for float because NaN - NaN == NaN */
	if (a == b && get_mode_arithmetic(mode) == irma_twos_complement)
		return get_mode_null(mode);

	ir_tarval *ta = value_of(a);
	ir_tarval *tb = value_of(b);
	if (ta != tarval_unknown && tb != tarval_unknown)
		return tarval_sub(ta, tb);

	return tarval_unknown;
}

/**
 * Return the value of a unary Minus.
 */
static ir_tarval *computed_value_Minus(const ir_node *n)
{
	const ir_node *a  = get_Minus_op(n);
	ir_tarval     *ta = value_of(a);

	if (ta != tarval_unknown)
		return tarval_neg(ta);

	return tarval_unknown;
}

/**
 * Return the value of a Mul.
 */
static ir_tarval *computed_value_Mul(const ir_node *n)
{
	const ir_node *a    = get_Mul_left(n);
	const ir_node *b    = get_Mul_right(n);
	ir_tarval     *ta   = value_of(a);
	ir_tarval     *tb   = value_of(b);
	ir_mode       *mode = get_irn_mode(n);

	if (ta != tarval_unknown && tb != tarval_unknown)
		return tarval_mul(ta, tb);

	/* a * 0 != 0 if a == NaN or a == Inf */
	if (!mode_is_float(mode)) {
		/* a*0 = 0 or 0*b = 0 */
		if (tarval_is_null(ta))
			return ta;
		if (tarval_is_null(tb))
			return tb;
	}
	return tarval_unknown;
}

/**
 * Return the value of an And.
 * Special case: a & 0, 0 & b
 */
static ir_tarval *computed_value_And(const ir_node *n)
{
	const ir_node *a  = get_And_left(n);
	const ir_node *b  = get_And_right(n);
	ir_tarval     *ta = value_of(a);
	ir_tarval     *tb = value_of(b);

	if (ta != tarval_unknown && tb != tarval_unknown)
		return tarval_and(ta, tb);

	if (tarval_is_null(ta)) return ta;
	if (tarval_is_null(tb)) return tb;

	/* x&~x => 0 */
	if (complement_values(a, b))
		return get_mode_null(get_irn_mode(n));

	return tarval_unknown;
}

/**
 * Return the value of an Or.
 * Special case: a | 1...1, 1...1 | b
 */
static ir_tarval *computed_value_Or(const ir_node *n)
{
	const ir_node *a  = get_Or_left(n);
	const ir_node *b  = get_Or_right(n);
	ir_tarval     *ta = value_of(a);
	ir_tarval     *tb = value_of(b);

	if (ta != tarval_unknown && tb != tarval_unknown)
		return tarval_or(ta, tb);

	if (tarval_is_all_one(ta)) return ta;
	if (tarval_is_all_one(tb)) return tb;

	/* x|~x => ~0 */
	if (complement_values(a, b))
		return get_mode_all_one(get_irn_mode(n));

	return tarval_unknown;
}

/**
 * Return the value of an Eor.
 */
static ir_tarval *computed_value_Eor(const ir_node *n)
{
	const ir_node *a = get_Eor_left(n);
	const ir_node *b = get_Eor_right(n);

	/* a ^ a == 0 */
	if (a == b)
		return get_mode_null(get_irn_mode(n));

	/* x^~x => ~0 */
	if (complement_values(a, b))
		return get_mode_all_one(get_irn_mode(n));

	ir_tarval *ta = value_of(a);
	ir_tarval *tb = value_of(b);
	if (ta != tarval_unknown && tb != tarval_unknown)
		return tarval_eor(ta, tb);

	return tarval_unknown;
}

/**
 * Return the value of a Not.
 */
static ir_tarval *computed_value_Not(const ir_node *n)
{
	const ir_node *a  = get_Not_op(n);
	ir_tarval     *ta = value_of(a);

	if (ta != tarval_unknown)
		return tarval_not(ta);

	return tarval_unknown;
}

/**
 * Tests whether a shift shifts more bits than available in the mode
 */
static bool is_oversize_shift(const ir_node *n)
{
	const ir_node *count = get_binop_right(n);
	ir_tarval     *tv    = value_of(count);
	if (!tarval_is_constant(tv))
		return false;
	/* adjust for modulo shift */
	ir_mode *mode         = get_irn_mode(n);
	unsigned modulo_shift = get_mode_modulo_shift(mode);
	if (modulo_shift > 0) {
		if (modulo_shift <= get_mode_size_bits(mode))
			return false;
		ir_tarval *modulo_shift_val
			= new_tarval_from_long(modulo_shift, get_tarval_mode(tv));
		tv = tarval_mod(tv, modulo_shift_val);
	}
	if (!tarval_is_long(tv))
		return false;

	long shiftval = get_tarval_long(tv);
	return shiftval < 0 || shiftval >= (long)get_mode_size_bits(mode);
}

/**
 * Return the value of a Shl.
 */
static ir_tarval *computed_value_Shl(const ir_node *n)
{
	const ir_node *a  = get_Shl_left(n);
	const ir_node *b  = get_Shl_right(n);
	ir_tarval     *ta = value_of(a);
	ir_tarval     *tb = value_of(b);

	if (ta != tarval_unknown && tb != tarval_unknown)
		return tarval_shl(ta, tb);
	if (is_oversize_shift(n))
		return get_mode_null(get_irn_mode(n));

	return tarval_unknown;
}

/**
 * Return the value of a Shr.
 */
static ir_tarval *computed_value_Shr(const ir_node *n)
{
	const ir_node *a  = get_Shr_left(n);
	const ir_node *b  = get_Shr_right(n);
	ir_tarval     *ta = value_of(a);
	ir_tarval     *tb = value_of(b);

	if (ta != tarval_unknown && tb != tarval_unknown)
		return tarval_shr(ta, tb);
	if (a == b || is_oversize_shift(n))
		return get_mode_null(get_irn_mode(n));
	return tarval_unknown;
}

/**
 * Return the value of a Shrs.
 */
static ir_tarval *computed_value_Shrs(const ir_node *n)
{
	const ir_node *a  = get_Shrs_left(n);
	const ir_node *b  = get_Shrs_right(n);
	ir_tarval     *ta = value_of(a);
	ir_tarval     *tb = value_of(b);

	if (ta != tarval_unknown && tb != tarval_unknown)
		return tarval_shrs(ta, tb);
	return tarval_unknown;
}

bool ir_zero_when_converted(const ir_node *node, ir_mode *dest_mode)
{
	const ir_mode *mode = get_irn_mode(node);
	if (get_mode_arithmetic(mode) != irma_twos_complement
	    || get_mode_arithmetic(dest_mode) != irma_twos_complement)
	    return false;

	ir_tarval *tv = is_shl_const_like(node);
	if (tv != NULL && tarval_is_long(tv)) {
		long shiftval = get_tarval_long(tv);
		long destbits = get_mode_size_bits(dest_mode);
		if (shiftval >= destbits
			&& shiftval < (long)get_mode_modulo_shift(mode))
			return true;
	}
	if (is_And(node)) {
		ir_node *right = get_And_right(node);
		if (is_Const(right)) {
			ir_tarval *tv     = get_Const_tarval(right);
			ir_tarval *conved = tarval_convert_to(tv, dest_mode);
			return tarval_is_null(conved);
		}
	}
	return false;
}

/**
 * Return the value of a Conv.
 */
static ir_tarval *computed_value_Conv(const ir_node *n)
{
	const ir_node *a    = get_Conv_op(n);
	ir_tarval     *ta   = value_of(a);
	ir_mode       *mode = get_irn_mode(n);

	if (ta != tarval_unknown)
		return tarval_convert_to(ta, mode);
	if (ir_zero_when_converted(a, mode))
		return get_mode_null(mode);

	return tarval_unknown;
}

static ir_tarval *computed_value_Bitcast(const ir_node *n)
{
	const ir_node *op = get_Bitcast_op(n);
	ir_tarval     *ta = value_of(op);
	if (ta == tarval_unknown)
		return tarval_unknown;

	ir_mode *mode = get_irn_mode(n);
	return tarval_bitcast(ta, mode);
}

/**
 * Calculate the value of a Mux: can be evaluated, if the
 * sel and the right input are known.
 */
static ir_tarval *computed_value_Mux(const ir_node *n)
{
	const ir_node   *sel = get_Mux_sel(n);
	const ir_tarval *ts  = value_of(sel);

	if (ts == get_tarval_b_true()) {
		ir_node *v = get_Mux_true(n);
		return value_of(v);
	} else if (ts == get_tarval_b_false()) {
		ir_node *v = get_Mux_false(n);
		return value_of(v);
	}
	return tarval_unknown;
}

/**
 * Calculate the value of a Confirm: can be evaluated,
 * if it has the form Confirm(x, '=', Const).
 */
static ir_tarval *computed_value_Confirm(const ir_node *n)
{
	const ir_node     *bound    = get_Confirm_bound(n);
	const ir_node     *value    = get_Confirm_value(n);
	const ir_relation  possible = ir_get_possible_cmp_relations(value, bound);
	const ir_relation  relation = get_Confirm_relation(n);

	switch (possible & relation) {
	case ir_relation_false:
		return tarval_bad;

	case ir_relation_equal: {
		ir_tarval *tv = value_of(bound);
		if (tarval_is_constant(tv))
			return tv;
		break;
	}
	}

	return value_of(value);
}

static ir_node *get_commutative_other_op(const ir_node *const node, const ir_node *const op)
{
	assert(is_op_commutative(get_irn_op(node)));
	ir_node *const l = get_binop_left(node);
	ir_node *const r = get_binop_right(node);
	if (l == op)
		return r;
	if (r == op)
		return l;
	return NULL;
}

/**
 * gives a (conservative) estimation of possible relation when comparing
 * left+right
 */
ir_relation ir_get_possible_cmp_relations(const ir_node *left,
                                          const ir_node *right)
{
	ir_relation      possible = ir_relation_true;
	const ir_tarval *tv_l     = value_of(left);
	const ir_tarval *tv_r     = value_of(right);

	/* both values known - evaluate them */
	if (tv_l != tarval_unknown && tv_r != tarval_unknown) {
		possible = tarval_cmp(tv_l, tv_r);
		/* we can return now, won't get any better */
		return possible;
	}

	/* NaN never compares successfully to anything */
	if (tarval_is_nan(tv_l) || tarval_is_nan(tv_r))
		return ir_relation_unordered;

	/* a == a is never less or greater (but might be equal or unordered) */
	if (left == right)
		possible &= ~ir_relation_less_greater;
	/* unordered results only happen for float compares */
	const ir_mode *mode = get_irn_mode(left);
	if (!mode_is_float(mode))
		possible &= ~ir_relation_unordered;
	/* values can never be less than the least representable number or
	 * greater than the greatest representable number */
	ir_tarval       *min = get_mode_min(mode);
	const ir_tarval *max = get_mode_max(mode);
	if (tv_l == min)
		possible &= ~ir_relation_greater;
	if (tv_l == max)
		possible &= ~ir_relation_less;
	if (tv_r == max)
		possible &= ~ir_relation_greater;
	if (tv_r == min)
		possible &= ~ir_relation_less;

	/* Try to use bit information. */
	const bitinfo *const bl = get_bitinfo(left);
	const bitinfo *const br = get_bitinfo(right);
	if (bl != NULL && br != NULL) {
		ir_tarval *const l_o   = bl->o;
		ir_tarval *const l_z   = bl->z;
		ir_tarval *const r_o   = br->o;
		ir_tarval *const r_z   = br->z;
		if (get_mode_arithmetic(mode) == irma_twos_complement) {
			/* Compute min/max values of operands. */
			ir_tarval *l_max = tarval_and(l_z, tarval_ornot(l_o, min));
			ir_tarval *l_min = tarval_or(l_o, tarval_and(l_z, min));
			ir_tarval *r_max = tarval_and(r_z, tarval_ornot(r_o, min));
			ir_tarval *r_min = tarval_or(r_o, tarval_and(r_z, min));

			if (!(tarval_cmp(l_max, r_min) & ir_relation_greater))
				possible &= ~ir_relation_greater;
			if (!(tarval_cmp(l_min, r_max) & ir_relation_less))
				possible &= ~ir_relation_less;
		}

		if (!tarval_is_null(tarval_andnot(l_o, r_z))
		    || !tarval_is_null(tarval_andnot(r_o, l_z))) {
			possible &= ~ir_relation_equal;
		}
	}

	/* maybe vrp can tell us more */
	possible &= vrp_cmp(left, right);
	/* Alloc nodes never return null (but throw an exception) */
	if (is_Alloc(left) && tarval_is_null(tv_r))
		possible &= ~ir_relation_equal;
	if (is_And(left) && !mode_is_signed(mode) && get_commutative_other_op(left, right))
		possible &= ~ir_relation_greater;
	if (is_And(right) && !mode_is_signed(mode) && get_commutative_other_op(right, left))
		possible &= ~ir_relation_less;
	/* stuff known through confirm nodes */
	if (is_Confirm(left) && get_Confirm_bound(left) == right)
		possible &= get_Confirm_relation(left);
	if (is_Confirm(right) && get_Confirm_bound(right) == left) {
		ir_relation relation = get_Confirm_relation(right);
		relation = get_inversed_relation(relation);
		possible &= relation;
	}

	return possible;
}

/**
 * Checks whether we can use @p relation instead of @p cmp_relation,
 * if only some relations are @p possible.
 *
 * @param relation      The relation to check for
 * @param cmp_relation  The current relation of the Cmp
 * @param possible      The possible relations of the Cmp
 */
static bool is_relation(ir_relation relation, ir_relation cmp_relation, ir_relation possible)
{
	ir_relation min           = cmp_relation & possible;
	ir_relation possible_bits = relation & ~possible;

	return (min | possible_bits) == relation;
}

/**
 * Check whether we can use @p relation or its ordered complement instead of @p
 * cmp_relation, if only some relations are @p possible.
 *
 * @param relation      The relation to check for
 * @param cmp_relation  The current relation of the Cmp
 * @param possible      The possible relations of the Cmp
 * @return The replacement relation or ir_relation_false, if neither is
 *         possible.
 */
static ir_relation get_complementary_relations(ir_relation const relation, ir_relation const cmp_relation, ir_relation const possible)
{
	assert(possible != ir_relation_false);
	if (is_relation(relation, cmp_relation, possible))
		return relation;
	ir_relation const compl = relation ^ ir_relation_less_equal_greater;
	if (is_relation(compl, cmp_relation, possible))
		return compl;
	return ir_relation_false;
}

/**
 * Return the value of a Cmp.
 *
 * The basic idea here is to determine which relations are possible and which
 * one are definitely impossible.
 */
static ir_tarval *computed_value_Cmp(const ir_node *cmp)
{
	ir_node     *left     = get_Cmp_left(cmp);
	ir_node     *right    = get_Cmp_right(cmp);
	ir_relation  possible = ir_get_possible_cmp_relations(left, right);
	ir_relation  relation = get_Cmp_relation(cmp);

	/* if none of the requested relations is possible, return false */
	if ((possible & relation) == ir_relation_false)
		return tarval_b_false;
	/* if possible relations are a subset of the requested ones return true */
	if ((possible & ~relation) == ir_relation_false)
		return tarval_b_true;

	/* we have some special rules for == 0 and != 0 */
	ir_relation const rel_eq = get_complementary_relations(ir_relation_equal, relation, possible);
	if (rel_eq != ir_relation_false && is_irn_null(right) && value_not_null(left, NULL))
		return rel_eq != ir_relation_equal ? tarval_b_true : tarval_b_false;

	return computed_value_Cmp_Confirm(left, right, relation);
}

/**
 * some people want to call compute_cmp directly, in this case we have to
 * test the constant folding flag again
 */
static ir_tarval *compute_cmp_ext(const ir_node *cmp)
{
	if (!get_opt_constant_folding())
		return tarval_unknown;
	return computed_value_Cmp(cmp);
}

static ir_tarval *computed_value_Proj_Builtin(ir_node const *const proj)
{
	long           val;
	ir_node *const builtin = get_Proj_pred(proj);
	switch (get_Builtin_kind(builtin)) {
	case ir_bk_clz: {
		ir_node const *const op = get_Builtin_param(builtin, 0);
		bitinfo const *const b  = get_bitinfo(op);
		if (b) {
			val = get_tarval_highest_bit(b->z);
			if (val != -1 && tarval_get_bit(b->o, val)) {
				val = get_mode_size_bits(get_irn_mode(op)) - val - 1;
				goto make_val;
			}
		}
		break;
	}

	case ir_bk_ctz: {
		ir_node const *const op = get_Builtin_param(builtin, 0);
		bitinfo const *const b  = get_bitinfo(op);
		if (b) {
			val = get_tarval_lowest_bit(b->z);
			if (val != -1 && tarval_get_bit(b->o, val))
				goto make_val;
		}
		break;
	}

	case ir_bk_ffs: {
		ir_node const *const op = get_Builtin_param(builtin, 0);
		bitinfo const *const b  = get_bitinfo(op);
		if (b) {
			val = get_tarval_lowest_bit(b->z);
			if (val == -1 || tarval_get_bit(b->o, val)) {
				++val;
				goto make_val;
			}
		}
		break;
	}

	case ir_bk_parity: {
		ir_node *const op = get_Builtin_param(builtin, 0);
		if (is_Const(op)) {
			ir_tarval *const tv = get_Const_tarval(op);
			val = get_tarval_popcount(tv) & 1;
			goto make_val;
		}
		break;
	}

	case ir_bk_popcount: {
		ir_node *const op = get_Builtin_param(builtin, 0);
		if (is_Const(op)) {
			ir_tarval *const tv = get_Const_tarval(op);
			val = get_tarval_popcount(tv);
			goto make_val;
		}
		break;
	}

	default:
		break;
	}

	return tarval_unknown;

make_val:;
	ir_mode *const mode = get_irn_mode(proj);
	return new_tarval_from_long(val, mode);
}

/**
 * Calculate the value of an integer Div.
 * Special case: 0 / b
 */
static ir_tarval *do_computed_value_Div(const ir_node *a, const ir_node *b)
{
	ir_tarval *ta = value_of(a);

	/* cannot optimize 0 / b = 0 because of NaN */
	if (tarval_is_null(ta)) {
		ir_mode *mode = get_irn_mode(a);
		if (get_mode_arithmetic(mode) == irma_twos_complement
		    && value_not_null(b, NULL)) {
			return ta;  /* 0 / b == 0 if b != 0 */
		}
	}
	if (a == b) {
		ir_mode *mode = get_irn_mode(a);
		/* a/a => 1 */
		if (get_mode_arithmetic(mode) == irma_twos_complement)
			return get_mode_one(mode);
	}
	ir_tarval *tb = value_of(b);
	if (ta != tarval_unknown && tb != tarval_unknown)
		return tarval_div(ta, tb);
	return tarval_unknown;
}

/**
 * Calculate the value of an integer Mod of two nodes.
 * Special case: a % 1
 */
static ir_tarval *do_computed_value_Mod(const ir_node *a, const ir_node *b)
{
	ir_tarval *tb = value_of(b);
	/* a % 1 == 0 */
	/* a % -1 == 0 */
	ir_mode *mode = get_irn_mode(a);
	if (tarval_is_one(tb) || (mode_is_signed(mode) && tarval_is_all_one(tb)))
		return get_mode_null(mode);

	/* constant folding */
	ir_tarval *ta = value_of(a);
	if (ta != tarval_unknown && tb != tarval_unknown)
		return tarval_mod(ta, tb);

	/* 0 % b == 0 if b != 0 */
	/* a % a == 0 if a != 0 */
	if (tarval_is_null(ta) || a == b) {
		assert(get_mode_arithmetic(mode) == irma_twos_complement);
		if (value_not_null(b, NULL))
			return get_mode_null(mode);
	}

	return tarval_unknown;
}

/**
 * Return the value of a Proj(Div).
 */
static ir_tarval *computed_value_Proj_Div(const ir_node *n)
{
	unsigned proj_nr = get_Proj_num(n);
	if (proj_nr != pn_Div_res)
		return tarval_unknown;

	const ir_node *div   = get_Proj_pred(n);
	const ir_node *left  = get_Div_left(div);
	const ir_node *right = get_Div_right(div);
	return do_computed_value_Div(left, right);
}

/**
 * Return the value of a Proj(Mod).
 */
static ir_tarval *computed_value_Proj_Mod(const ir_node *n)
{
	unsigned proj_nr = get_Proj_num(n);

	if (proj_nr == pn_Mod_res) {
		const ir_node *mod = get_Proj_pred(n);
		return do_computed_value_Mod(get_Mod_left(mod), get_Mod_right(mod));
	}
	return tarval_unknown;
}

/**
 * Return the value of a Proj.
 */
static ir_tarval *computed_value_Proj(const ir_node *proj)
{
	const ir_node *n = get_Proj_pred(proj);

	if (n->op->ops.computed_value_Proj != NULL)
		return n->op->ops.computed_value_Proj(proj);
	return tarval_unknown;
}

/**
 * If the parameter n can be computed, return its value, else tarval_unknown.
 * Performs constant folding.
 *
 * @param n  The node this should be evaluated
 */
ir_tarval *computed_value(const ir_node *n)
{
	const vrp_attr *vrp = vrp_get_info(n);
	if (vrp != NULL && vrp->bits_set == vrp->bits_not_set)
		return vrp->bits_set;

	if (n->op->ops.computed_value)
		return n->op->ops.computed_value(n);
	return tarval_unknown;
}

/**
 * Optimize operations that are commutative and have a neutral element.
 * Example: a + 0 = 0 + a = a.
 */
static ir_node *equivalent_node_neutral_element(ir_node *n,
                                                ir_tarval *neutral_element)
{
	/* if right operand is neutral element return the left one.
	 * Beware: We may have Add(NULL,3) in which case the 3 has not pointer type
	 * and this transformation would give us a type mismatch. */
	ir_node *a = get_binop_left(n);
	ir_node *b = get_binop_right(n);
	if (value_of(b) == neutral_element && get_irn_mode(a) == get_irn_mode(n))
		return a;
	if (value_of(a) == neutral_element && get_irn_mode(b) == get_irn_mode(n))
		return b;
	return n;
}

static ir_node *equivalent_node_neutral_zero(ir_node *n)
{
	ir_tarval *null = get_mode_null(get_irn_mode(n));
	return equivalent_node_neutral_element(n, null);
}

/**
 * Eor is commutative and has neutral 0.
 */
static ir_node *equivalent_node_Eor(ir_node *n)
{
	ir_node *oldn = n;

	n = equivalent_node_neutral_zero(n);
	if (n != oldn)
		return n;

	ir_node *a = get_Eor_left(n);
	ir_node *b = get_Eor_right(n);

	ir_node *x;
	ir_node *z;
	if (is_Eor(a) || is_Eor_Add(a)) {
		x = b;
		z = a;
		goto inverse;
	}
	if (is_Eor(b) || is_Eor_Add(b)) {
		x = a;
		z = b;
inverse:;
		ir_node *const y = get_commutative_other_op(z, x);
		if (y) {
			/* x ^ (x ^ y) -> y */
			n = y;
			DBG_OPT_ALGSIM1(oldn, a, b, n);
			return n;
		}
	}
	return n;
}

/*
 * Optimize a - 0 and (a - x) + x (for modes with wrap-around).
 *
 * The second one looks strange, but this construct
 * is used heavily in the LCC sources :-).
 *
 * Beware: The Mode of an Add may be different than the mode of its
 * predecessors, so we could not return a predecessors in all cases.
 */
static ir_node *equivalent_node_Add(ir_node *n)
{
	ir_node *oldn = n;

	/* these optimizations are imprecise for floating point ops */
	ir_node   *right      = get_Add_right(n);
	ir_mode   *right_mode = get_irn_mode(right);
	ir_tarval *neutral    = get_mode_null(right_mode);
	ir_mode   *mode       = get_irn_mode(n);
	if (mode_is_float(mode)) {
		neutral = tarval_neg(neutral);
		/* X + -0.0 -> X */
		n = equivalent_node_neutral_element(n, neutral);
		if (n != oldn)
			return n;
		if (!ir_imprecise_float_transforms_allowed())
			return n;
	}

	n = equivalent_node_neutral_element(n, neutral);
	if (n != oldn)
		return n;

	ir_node *left = get_Add_left(n);

	if (is_Sub(left) && get_Sub_right(left) == right) {
		/* (a - x) + x -> a */
		n = get_Sub_left(left);
		DBG_OPT_ALGSIM1(oldn, left, right, n);
		return n;
	}
	if (is_Sub(right) && get_Sub_right(right) == left) {
		/* x + (a - x) -> a */
		n = get_Sub_left(right);
		DBG_OPT_ALGSIM1(oldn, left, right, n);
		return n;
	}
	return n;
}

/**
 * optimize operations that are not commutative but have right neutral 0,
 * so a op 0 = a.
 */
static ir_node *equivalent_node_right_zero(ir_node *n)
{
	ir_node         *oldn = n;
	ir_node         *b    = get_binop_right(n);
	const ir_tarval *tb   = value_of(b);

	if (tarval_is_null(tb)) {
		ir_node *a = get_binop_left(n);
		n = a;

		DBG_OPT_ALGSIM1(oldn, a, b, n);
	}
	return n;
}

/**
 * Optimize a - 0 and (a + x) - x (for modes with wrap-around).
 *
 * The second one looks strange, but this construct
 * is used heavily in the LCC sources :-).
 *
 * Beware: The Mode of a Sub may be different than the mode of its
 * predecessors, so we could not return a predecessors in all cases.
 */
static ir_node *equivalent_node_Sub(ir_node *n)
{
	ir_node         *oldn = n;
	ir_mode         *mode = get_irn_mode(n);
	ir_node         *a    = get_Sub_left(n);
	ir_node         *b    = get_Sub_right(n);
	const ir_tarval *tb   = value_of(b);

	/* Beware: modes might be different */
	if (tarval_is_null(tb) && mode == get_irn_mode(a)) {
		n = a;
		DBG_OPT_ALGSIM1(oldn, a, b, n);
		return n;
	}

	if (is_Const(a) && is_Or(b)) {
		ir_tarval *ta        = get_Const_tarval(a);
		ir_tarval *all_one   = get_mode_all_one(mode);
		ir_tarval *expect_a  = tarval_shl_unsigned(all_one, 1);
		ir_tarval *expect_bb = tarval_shr_unsigned(all_one, 1);
		ir_node   *bb        = get_Or_right(b);
		if (ta == expect_a && is_Const(bb) && get_Const_tarval(bb) == expect_bb) {
			/* 0xF...FE - (x | 0x7F...F) -> x | 0x7F...F */
			return b;
		}
	}

	return n;
}


/**
 * Optimize an "self-inverse unary op", i.e. op(op(n)) = n.
 *
 * @todo
 *   -(-a) == a, but might overflow two times.
 *   We handle it anyway here but the better way would be a
 *   flag. This would be needed for Pascal for instance.
 */
static ir_node *equivalent_node_involution(ir_node *n, int input)
{
	ir_node *oldn = n;
	ir_node *pred = get_irn_n(n, input);
	if (get_irn_op(pred) == get_irn_op(n)) {
		n = get_irn_n(pred, input);
		DBG_OPT_ALGSIM2(oldn, pred, n);
	}
	return n;
}

static ir_node *equivalent_node_Minus(ir_node *n)
{
	ir_node *oldn = n;
	n = equivalent_node_involution(n, n_Minus_op);
	if (n != oldn)
		return n;

	/* If all bits except the highest bit are zero the Minus is superfluous. */
	ir_mode *const mode = get_irn_mode(n);
	if (get_mode_arithmetic(mode) == irma_twos_complement) {
		ir_node       *const op = get_Minus_op(n);
		const bitinfo *const b  = get_bitinfo(op);

		if (b) {
			ir_tarval *const shl = tarval_shl_unsigned(b->z, 1U);
			if (tarval_is_null(shl))
				n = op;
		}
	}

	return n;
}

static ir_node *equivalent_node_Not(ir_node *n)
{
	return equivalent_node_involution(n, n_Not_op);
}

/**
 * Optimize a * 1 = 1 * a = a.
 */
static ir_node *equivalent_node_Mul(ir_node *n)
{
	ir_mode *mode = get_irn_mode(n);
	return equivalent_node_neutral_element(n, get_mode_one(mode));
}

/**
 * Use algebraic simplification a | a = a | 0 = 0 | a = a.
 */
static ir_node *equivalent_node_Or(ir_node *n)
{
	ir_node *oldn = n;
	ir_node *a    = get_Or_left(n);
	ir_node *b    = get_Or_right(n);

	if (a == b) {
		n = a;    /* idempotence */
		DBG_OPT_ALGSIM0(oldn, n);
		return n;
	}

	const bitinfo *const ba = get_bitinfo(a);
	const bitinfo *const bb = get_bitinfo(b);
	if (ba != NULL && bb != NULL) {
		if (tarval_is_null(tarval_andnot(ba->z, bb->o))) {
			n = b;
			DBG_OPT_ALGSIM1(oldn, a, b, n);
			return n;
		}
		if (tarval_is_null(tarval_andnot(bb->z, ba->o))) {
			n = a;
			DBG_OPT_ALGSIM1(oldn, a, b, n);
			return n;
		}
	}

	n = equivalent_node_neutral_zero(n);
	if (n != oldn)
		return n;

	/* (a & X) | a => a */
	if (is_And(a) && get_commutative_other_op(a, b)) {
		n = b;
		DBG_OPT_ALGSIM1(oldn, a, b, n);
		return n;
	}
	/* a | (a & X) => a */
	if (is_And(b) && get_commutative_other_op(b, a)) {
		n = a;
		DBG_OPT_ALGSIM1(oldn, a, b, n);
		return n;
	}

	return n;
}

/**
 * Optimize a & 0b1...1 = 0b1...1 & a = a & a = (a|X) & a = a.
 */
static ir_node *equivalent_node_And(ir_node *n)
{
	ir_node *oldn = n;
	ir_node*       const a  = get_And_left(n);
	ir_node*       const b  = get_And_right(n);
	bitinfo const* const ba = get_bitinfo(a);
	bitinfo const* const bb = get_bitinfo(b);
	if (ba != NULL && bb != NULL) {
		if (tarval_is_null(tarval_andnot(bb->z, ba->o))) {
			n = b;
			DBG_OPT_ALGSIM1(oldn, a, b, n);
			return n;
		}
		if (tarval_is_null(tarval_andnot(ba->z, bb->o))) {
			n = a;
			DBG_OPT_ALGSIM1(oldn, a, b, n);
			return n;
		}
	}

	if (a == b) {
		n = a;    /* idempotence */
		DBG_OPT_ALGSIM0(oldn, n);
		return n;
	}

	ir_mode *mode = get_irn_mode(n);
	n = equivalent_node_neutral_element(n, get_mode_all_one(mode));
	if (n != oldn)
		return n;

	/* (a|X) & a => a*/
	if ((is_Or(a) || is_Or_Eor_Add(a)) && get_commutative_other_op(a, b)) {
		n = b;
		DBG_OPT_ALGSIM1(oldn, a, b, n);
		return n;
	}
	/* a & (a|X) => a*/
	if ((is_Or(b) || is_Or_Eor_Add(b)) && get_commutative_other_op(b, a)) {
		n = a;
		DBG_OPT_ALGSIM1(oldn, a, b, n);
		return n;
	}
	return n;
}

/**
 * Try to remove useless Conv's:
 */
static ir_node *equivalent_node_Conv(ir_node *n)
{
	ir_node       *oldn   = n;
	ir_node       *a      = get_Conv_op(n);
	const ir_mode *n_mode = get_irn_mode(n);
	ir_mode       *a_mode = get_irn_mode(a);

	if (n_mode == a_mode) { /* No Conv necessary */
		n = a;
		DBG_OPT_ALGSIM0(oldn, n);
		return n;
	} else if (is_Conv(a)) { /* Conv(Conv(b)) */
		ir_node *b      = get_Conv_op(a);
		ir_mode *b_mode = get_irn_mode(b);

		if (n_mode == b_mode) {
			if (values_in_mode(b_mode, a_mode)) {
				n = b;
				DBG_OPT_ALGSIM1(oldn, a, b, n);
				return n;
			}

			const bitinfo *const bb = get_bitinfo(b);
			if (bb != NULL && smaller_mode(a_mode, b_mode) &&
			    get_mode_arithmetic(a_mode) == irma_twos_complement &&
			    get_mode_arithmetic(b_mode) == irma_twos_complement) {
				ir_tarval  *const bz          = bb->z;
				const long        highest_bit = get_tarval_highest_bit(bz);
				const long        mode_bits   = get_mode_size_bits(a_mode);
				const int         is_sext     = mode_is_signed(a_mode);
				/* If it is a sign extension, the highest bit of the smaller mode
				 * must be zero to ensure that no sign extension occurs. */
				if (highest_bit + is_sext < mode_bits) {
					n = b;
					DBG_OPT_ALGSIM1(oldn, a, b, n);
					return n;
				}
			}
		}
	}
	return n;
}

static ir_node *equivalent_node_Bitcast(ir_node *n)
{
	ir_node *op = get_Bitcast_op(n);
	while (is_Bitcast(op)) {
		ir_node *prev_op = get_Bitcast_op(op);
		if (get_irn_mode(prev_op) == get_irn_mode(n))
			return prev_op;
		op = prev_op;
	}
	return n;
}

/**
 * - fold Phi-nodes, iff they have only one predecessor except
 *   themselves.
 */
static ir_node *equivalent_node_Phi(ir_node *n)
{
	if (!get_optimize() &&
	    !irg_is_constrained(get_irn_irg(n), IR_GRAPH_CONSTRAINT_CONSTRUCTION))
		return n;

	/* Phi of dead Region without predecessors. */
	int n_preds = get_Phi_n_preds(n);
	if (n_preds == 0)
		return n;

	/* Determine whether the Phi is trivial, i.e. it only references itself and
	 * one other value. */
	bool     had_self_loop = false;
	ir_node *first_val     = NULL;
	foreach_irn_in(n, i, pred) {
		if (pred == n) {
			had_self_loop = true;
		} else if (pred != first_val) {
			/* more than 1 unique value found? abort */
			if (first_val)
				return n;
			/* First non-self-referencing input */
			first_val = pred;
		}
	}

	/* if we are here then all inputs are either self-loops or first_val */
	if (is_Dummy(first_val))
		return n;
	/* Subtle special case: (Potentially) endless loops are observable behaviour
	 * and must be part of the memory chain. If there are no other memory
	 * operations in a loop we still are not allowed to remove the PhiM unless
	 * we can prove that the loop terminates. */
	if (get_Phi_loop(n)) {
		assert(get_irn_mode(n) == mode_M);
		/* We currently assume that PhiMs that have a keep-alive edge are in a
		 * potentially endless loops. PhiM without a keep alive edge is a sign
		 * that we are sure that the loop terminates. */
		if (had_self_loop)
			return n;
		/* The PhiM will be removed, we can remove keep-alive edges to it as
		 * well. */
		remove_keep_alive(n);
	}

	DBG_OPT_PHI(n, first_val);
	return first_val;
}

/**
 * Optimizes Sync nodes that have only one operand.
 */
static ir_node *equivalent_node_Sync(ir_node *n)
{
	ir_node *op = NULL;
	foreach_irn_in(n, i, pred) {
		if (pred == op) {
			continue;
		} else if (!op) {
			op = pred;
		} else if (is_NoMem(pred)) {
			continue;
		} else if (is_NoMem(op)) {
			op = pred;
		} else {
			return n;
		}
	}

	assert(op);
	return op;
}

static ir_node *equivalent_node_Pin(ir_node *n)
{
	ir_node *const op = get_Pin_op(n);
	/* Pin(Pin(x)) -> Pin(x) */
	if (is_Pin(op) && get_nodes_block(op) == get_nodes_block(n))
		return op;
	return n;
}

/**
 * Optimize Proj(Tuple).
 */
static ir_node *equivalent_node_Proj_Tuple(ir_node *proj)
{
	ir_node *oldn  = proj;
	ir_node *tuple = get_Proj_pred(proj);

	/* Remove the Tuple/Proj combination. */
	proj = get_Tuple_pred(tuple, get_Proj_num(proj));
	DBG_OPT_TUPLE(oldn, tuple, proj);

	return proj;
}

/**
 * Optimize a / 1 = a.
 */
static ir_node *equivalent_node_Proj_Div(ir_node *proj)
{
	ir_node         *oldn = proj;
	const ir_node   *div  = get_Proj_pred(proj);
	const ir_node   *b    = get_Div_right(div);
	const ir_tarval *tb   = value_of(b);

	/* Div is not commutative. */
	if (tarval_is_one(tb)) { /* div(x, 1) == x */
		switch (get_Proj_num(proj)) {
		case pn_Div_M:
			proj = get_Div_mem(div);
			DBG_OPT_ALGSIM0(oldn, proj);
			return proj;

		case pn_Div_res:
			proj = get_Div_left(div);
			DBG_OPT_ALGSIM0(oldn, proj);
			return proj;

		default:
			/* we cannot replace the exception Proj's here, this is done in
			   transform_node_Proj_Div() */
			return proj;
		}
	}
	return proj;
}

/**
 * Optimize Store-after-Load
 */
static ir_node *equivalent_node_Proj_Store(ir_node *proj)
{
	const ir_node *store = get_Proj_pred(proj);

	if (get_Store_volatility(store) == volatility_is_volatile ||
	    ir_throws_exception(store)) {
		return proj;
	}

	ir_node       *store_mem   = get_Store_mem(store);
	const ir_node *store_value = get_Store_value(store);
	const ir_node *store_ptr   = get_Store_ptr(store);

	if (is_Proj(store_mem) && is_Proj(store_value)) {
		const ir_node *load = get_Proj_pred(store_mem);
		if (is_Load(load) && get_Proj_pred(store_value) == load &&
		    get_Load_ptr(load) == store_ptr) {
			return store_mem;
		}
	}
	return proj;
}

/**
 * Optimize CopyB(mem, x, x) into a Nop.
 */
static ir_node *equivalent_node_CopyB(ir_node *copyb)
{
	const ir_type *type = get_CopyB_type(copyb);
	if (get_type_size(type) == 0) {
		return get_CopyB_mem(copyb);
	}
	if (get_CopyB_volatility(copyb) == volatility_is_volatile) {
		return copyb;
	}
	const ir_node *a = get_CopyB_dst(copyb);
	const ir_node *b = get_CopyB_src(copyb);
	if (a == b) {
		return get_CopyB_mem(copyb);
	}
	return copyb;
}

/**
 * Does all optimizations on nodes that must be done on its Projs
 * because of creating new nodes.
 */
static ir_node *equivalent_node_Proj(ir_node *proj)
{
	const ir_node *n = get_Proj_pred(proj);
	if (n->op->ops.equivalent_node_Proj)
		return n->op->ops.equivalent_node_Proj(proj);
	return proj;
}

/**
 * Remove Id's.
 */
static ir_node *equivalent_node_Id(ir_node *n)
{
	ir_node *oldn = n;

	do {
		n = get_Id_pred(n);
	} while (is_Id(n));

	DBG_OPT_ID(oldn, n);
	return n;
}

/**
 * Optimize a Mux.
 */
static ir_node *equivalent_node_Mux(ir_node *n)
{
	ir_node         *oldn = n;
	const ir_node   *sel  = get_Mux_sel(n);
	const ir_tarval *ts   = value_of(sel);

	/* try again with a direct call to compute_cmp, as we don't care
	 * about the MODEB_LOWERED flag here */
	if (!tarval_is_constant(ts) && is_Cmp(sel))
		ts = compute_cmp_ext(sel);

	/* Mux(true, f, t) == t */
	if (ts == tarval_b_true) {
		n = get_Mux_true(n);
		DBG_OPT_ALGSIM0(oldn, n);
		return n;
	}
	/* Mux(false, f, t) == f */
	if (ts == tarval_b_false) {
		n = get_Mux_false(n);
		DBG_OPT_ALGSIM0(oldn, n);
		return n;
	}
	ir_node *n_t = get_Mux_true(n);
	ir_node *n_f = get_Mux_false(n);

	/* Mux(v, x, x) == x */
	if (n_t == n_f) {
		n = n_t;
		DBG_OPT_ALGSIM0(oldn, n);
		return n;
	}
	if (is_Cmp(sel) && !mode_has_signed_zero(get_irn_mode(n))) {
		ir_relation  relation = get_Cmp_relation(sel);
		ir_node     *f        = get_Mux_false(n);
		ir_node     *t        = get_Mux_true(n);

		/*
		 * Note further that these optimization work even for floating point
		 * with NaN's because -NaN == NaN.
		 * However, if +0 and -0 is handled differently, we cannot use the first one.
		 */
		const ir_node     *const cmp_l                              = get_Cmp_left(sel);
		const ir_node     *const cmp_r                              = get_Cmp_right(sel);
		const ir_relation        possible                           = ir_get_possible_cmp_relations(cmp_l, cmp_r);
		const bool               is_relation_equal                  = is_relation(ir_relation_equal, relation, possible);
		const bool               is_relation_less_greater           = is_relation(ir_relation_less_greater, relation, possible);
		const bool               is_relation_unordered_less_greater = is_relation(ir_relation_unordered_less_greater, relation, possible);

		if ((cmp_l == t && cmp_r == f) || (cmp_l == f && cmp_r == t)) {
			if (is_relation_equal) {
				/* Mux(t == f, t, f) -> f */
				/* Mux(f == t, t, f) -> f */
				n = f;
				DBG_OPT_ALGSIM0(oldn, n);
				return n;
			}

			if (is_relation_less_greater || is_relation_unordered_less_greater) {
				/* Mux(t != f, t, f) -> t */
				/* Mux(f != t, t, f) -> t */
				n = t;
				DBG_OPT_ALGSIM0(oldn, n);
				return n;
			}
		}

		/*
		 * Note: normalization puts the constant on the right side,
		 * so we check only one case.
		 */
		if (is_irn_null(f) && is_relation_less_greater &&
		    get_mode_arithmetic(get_irn_mode(cmp_l)) == irma_twos_complement &&
		    (is_Eor(t) || is_Sub(t))) {
			ir_node *t_l = get_binop_left(t);
			ir_node *t_r = get_binop_right(t);

			if ((t_l == cmp_l && t_r == cmp_r) || (t_l == cmp_r && t_r == cmp_l)) {
				/* Mux((a != b, 0, a - b) => a - b */
				/* Mux((a != b, 0, a ^ b) => a ^ b */
				return t;
			}
		}

		/*
		 * Note: normalization puts the constant on the right side,
		 * so we check only one case.
		 */
		if (cmp_l == t && tarval_is_null(value_of(cmp_r))) {
			/* Mux(t CMP 0, X, t) */
			if (is_Minus(f) && get_Minus_op(f) == t) {
				/* Mux(t CMP 0, -t, t) */
				if (is_relation_equal) {
					/* Mux(t == 0, -t, t)  ==>  -t */
					n = f;
					DBG_OPT_ALGSIM0(oldn, n);
				} else if (is_relation_less_greater || is_relation_unordered_less_greater) {
					/* Mux(t != 0, -t, t)  ==> t */
					n = t;
					DBG_OPT_ALGSIM0(oldn, n);
				}
			}
		}

		/* Mux(a == 0, 0, -a) ==> 0 */
		if (is_irn_null(f) && is_Minus(t) && is_relation_equal && is_irn_null(cmp_r) && cmp_l == get_Minus_op(t)) {
			return f;
		}

	}

	return n;
}

/**
 * Remove Confirm nodes if setting is on.
 * Replace Confirms(x, '=', Constlike) by Constlike.
 */
static ir_node *equivalent_node_Confirm(ir_node *n)
{
	ir_node     *pred     = get_Confirm_value(n);
	ir_relation  relation = get_Confirm_relation(n);

	/* Confirm(x, ==, b) => b
	 * Note: irconsconfirm.c has a shortcut for this case, but there may be
	 * cases where other localopts have to reveal this case first. */
	if (relation == ir_relation_equal)
		return pred;

	while (is_Confirm(pred) && relation == get_Confirm_relation(pred)) {
		/*
		 * rare case: two identical Confirms one after another,
		 * replace the second one with the first.
		 */
		n    = pred;
		pred = get_Confirm_value(n);
	}
	return n;
}

/**
 * equivalent_node() returns a node equivalent to input n. It skips all nodes that
 * perform no actual computation, as, e.g., the Id nodes.  It does not create
 * new nodes.  It is therefore safe to free n if the node returned is not n.
 * If a node returns a Tuple we can not just skip it.  If the size of the
 * in array fits, we transform n into a tuple (e.g., Div).
 */
ir_node *equivalent_node(ir_node *n)
{
	if (n->op->ops.equivalent_node)
		return n->op->ops.equivalent_node(n);
	return n;
}

/** Returns true if we can be sure that @p node only has a single read user. */
static bool only_one_user(const ir_node *node)
{
	ir_graph *irg = get_irn_irg(node);
	if (!edges_activated(irg))
		return false;
	return get_irn_n_edges(node) <= 1;
}

/**
 * Returns non-zero if a node is a Phi node
 * with all predecessors constant.
 */
static bool is_const_Phi(ir_node *n)
{
	if (!is_Phi(n) || get_irn_arity(n) == 0)
		return false;
	foreach_irn_in_r(n, i, pred) {
		if (!is_Const(pred))
			return false;
	}
	return true;
}

/**
 * Returns non-zero if a node is a Mux node
 * with true and false predecessors constant.
 */
static int is_const_Mux(ir_node *n)
{
	return is_Mux(n) && is_Const(get_Mux_false(n)) && is_Const(get_Mux_true(n));
}

typedef ir_tarval *(*tarval_binop)(ir_tarval const *a, ir_tarval const *b);

/**
 * Apply an evaluator on a binop with a constant operators (and one Phi).
 *
 * @param phi    the Phi node
 * @param other  the other operand
 * @param eval   an evaluator function
 * @param mode   the mode of the result, may be different from the mode of the Phi!
 * @param left   if true, other is the left operand, else the right
 *
 * @return a new Phi node if the conversion was successful, NULL else
 */
static ir_node *apply_binop_on_phi(ir_node *phi, ir_tarval *other,
                                   tarval_binop eval, ir_mode *mode, bool left)
{
	int         n   = get_irn_arity(phi);
	ir_tarval **tvs = ALLOCAN(ir_tarval*, n);
	if (left) {
		foreach_irn_in(phi, i, pred) {
			ir_tarval *tv        = get_Const_tarval(pred);
			ir_tarval *evaluated = eval(other, tv);

			/* abort if folding failed */
			if (!tarval_is_constant(evaluated))
				return NULL;
			tvs[i] = evaluated;
		}
	} else {
		foreach_irn_in(phi, i, pred) {
			ir_tarval *tv        = get_Const_tarval(pred);
			ir_tarval *evaluated = eval(tv, other);

			/* abort if folding failed */
			if (!tarval_is_constant(evaluated))
				return NULL;
			tvs[i] = evaluated;
		}
	}
	ir_graph  *irg = get_irn_irg(phi);
	ir_node  **res = ALLOCAN(ir_node*, n);
	for (int i = 0; i < n; ++i) {
		res[i] = new_r_Const(irg, tvs[i]);
	}
	ir_node *block = get_nodes_block(phi);
	return new_r_Phi(block, n, res, mode);
}

/**
 * Apply an evaluator on a binop with two constant Phi.
 *
 * @param a      the left Phi node
 * @param b      the right Phi node
 * @param eval   an evaluator function
 * @param mode   the mode of the result, may be different from the mode of the Phi!
 *
 * @return a new Phi node if the conversion was successful, NULL else
 */
static ir_node *apply_binop_on_2_phis(ir_node *a, ir_node *b,
                                      tarval_binop eval, ir_mode *mode)
{
	if (get_nodes_block(a) != get_nodes_block(b))
		return NULL;

	int         n   = get_irn_arity(a);
	ir_tarval **tvs = ALLOCAN(ir_tarval*, n);
	for (int i = 0; i < n; ++i) {
		const ir_node *pred_a = get_irn_n(a, i);
		ir_tarval     *tv_l   = get_Const_tarval(pred_a);
		const ir_node *pred_b = get_irn_n(b, i);
		ir_tarval     *tv_r   = get_Const_tarval(pred_b);
		ir_tarval     *tv     = eval(tv_l, tv_r);

		/* abort if folding failed */
		if (!tarval_is_constant(tv))
			return NULL;
		tvs[i] = tv;
	}
	ir_graph  *irg = get_irn_irg(a);
	ir_node  **res = ALLOCAN(ir_node*, n);
	for (int i = 0; i < n; ++i) {
		res[i] = new_r_Const(irg, tvs[i]);
	}
	ir_node *block = get_nodes_block(a);
	return new_r_Phi(block, n, res, mode);
}

typedef ir_tarval *(*tarval_unop)(ir_tarval const *a);

/**
 * Apply an evaluator on a unop with a constant operator (a Phi).
 *
 * @param phi    the Phi node
 * @param eval   an evaluator function
 *
 * @return a new Phi node if the conversion was successful, NULL else
 */
static ir_node *apply_unop_on_phi(ir_node *phi, tarval_unop eval)
{
	int         n   = get_irn_arity(phi);
	ir_tarval **tvs = ALLOCAN(ir_tarval*, n);
	foreach_irn_in(phi, i, pred) {
		ir_tarval *tv = get_Const_tarval(pred);
		tv = eval(tv);

		/* abort if folding failed */
		if (!tarval_is_constant(tv))
			return NULL;
		tvs[i] = tv;
	}
	ir_graph  *irg = get_irn_irg(phi);
	ir_node  **res = ALLOCAN(ir_node*, n);
	for (int i = 0; i < n; ++i) {
		res[i] = new_r_Const(irg, tvs[i]);
	}
	ir_node *block = get_nodes_block(phi);
	ir_mode *mode  = get_irn_mode(phi);
	return new_r_Phi(block, n, res, mode);
}

/**
 * Apply a conversion on a constant operator (a Phi).
 *
 * @param phi    the Phi node
 *
 * @return a new Phi node if the conversion was successful, NULL else
 */
static ir_node *apply_conv_on_phi(ir_node *phi, ir_mode *mode)
{
	int         n   = get_irn_arity(phi);
	ir_tarval **tvs = ALLOCAN(ir_tarval*, n);
	foreach_irn_in(phi, i, pred) {
		ir_tarval *tv = get_Const_tarval(pred);
		tv = tarval_convert_to(tv, mode);

		/* abort if folding failed */
		if (!tarval_is_constant(tv))
			return NULL;
		tvs[i] = tv;
	}
	ir_graph  *irg = get_irn_irg(phi);
	ir_node  **res = ALLOCAN(ir_node*, n);
	for (int i = 0; i < n; ++i) {
		res[i] = new_r_Const(irg, tvs[i]);
	}
	ir_node *block = get_nodes_block(phi);
	return new_r_Phi(block, n, res, mode);
}

/**
 * Apply an evaluator on a binop with a constant operators (and one Mux).
 *
 * @param mux    the Mux node
 * @param other  the other operand
 * @param eval   an evaluator function
 * @param left   if true, other is the left operand, else the right
 *
 * @return a new Mux node if the conversion was successful, NULL else
 */
static ir_node *apply_binop_on_mux(ir_node *mux, ir_tarval *other, tarval_binop eval, bool left)
{
	if (!only_one_user(mux))
		return NULL;

	ir_tarval *true_val  = get_Const_tarval(get_Mux_true(mux));
	ir_tarval *false_val = get_Const_tarval(get_Mux_false(mux));

	ir_tarval *new_true, *new_false;
	if (left) {
		new_true  = eval(other, true_val);
		new_false = eval(other, false_val);
	} else {
		new_true  = eval(true_val, other);
		new_false = eval(false_val, other);
	}

	if (!tarval_is_constant(new_true) || !tarval_is_constant(new_false))
		return NULL;

	ir_node  *sel       = get_Mux_sel(mux);
	ir_graph *irg       = get_irn_irg(mux);
	ir_node  *irn_true  = new_r_Const(irg, new_true);
	ir_node  *irn_false = new_r_Const(irg, new_false);
	ir_node  *block     = get_nodes_block(mux);
	return new_r_Mux(block, sel, irn_false, irn_true);
}

/**
 * Apply an evaluator on a binop with two constant Mux.
 *
 * @param a      the left Mux node
 * @param b      the right Mux node
 * @param eval   an evaluator function
 *
 * @return a new Mux node if the conversion was successful, NULL else
 */
static ir_node *apply_binop_on_2_muxs(ir_node *a, ir_node *b, tarval_binop eval)
{
	if (!only_one_user(a) || !only_one_user(b))
		return NULL;
	if (get_nodes_block(a) != get_nodes_block(b))
		return NULL;

	ir_node *sel_a = get_Mux_sel(a);
	ir_node *sel_b = get_Mux_sel(b);

	// More complex logical analysis could be added here:
	if (sel_a == sel_b) {
		// a's and b's sel nodes are logically equivalent
		// ==> Either both false or both true
		ir_tarval *false_a = get_Const_tarval(get_Mux_false(a));
		ir_tarval *false_b = get_Const_tarval(get_Mux_false(b));
		ir_tarval *true_a  = get_Const_tarval(get_Mux_true(a));
		ir_tarval *true_b  = get_Const_tarval(get_Mux_true(b));

		ir_tarval *new_false = eval(false_a, false_b);
		ir_tarval *new_true  = eval(true_a, true_b);

		if (!tarval_is_constant(new_true) || !tarval_is_constant(new_false))
			return NULL;

		ir_graph *irg       = get_irn_irg(a);
		ir_node  *irn_false = new_r_Const(irg, new_false);
		ir_node  *irn_true  = new_r_Const(irg, new_true);
		ir_node  *block     = get_nodes_block(a);
		return new_r_Mux(block, sel_a, irn_false, irn_true);
	} else {
		return NULL;
	}
}

/**
 * Apply an evaluator on a unop with a constant operator (a Mux).
 *
 * @param mux    the Mux node
 * @param eval   an evaluator function
 *
 * @return a new Mux node if the conversion was successful, NULL else
 */
static ir_node *apply_unop_on_mux(ir_node *mux, tarval_unop eval)
{
	if (!only_one_user(mux))
		return NULL;

	ir_tarval *true_val  = get_Const_tarval(get_Mux_true(mux));
	ir_tarval *false_val = get_Const_tarval(get_Mux_false(mux));

	ir_tarval *new_true  = eval(true_val);
	ir_tarval *new_false = eval(false_val);

	if (!tarval_is_constant(new_true) || !tarval_is_constant(new_false))
		return NULL;

	ir_node  *sel       = get_Mux_sel(mux);
	ir_graph *irg       = get_irn_irg(mux);
	ir_node  *irn_true  = new_r_Const(irg, new_true);
	ir_node  *irn_false = new_r_Const(irg, new_false);
	ir_node  *block     = get_nodes_block(mux);
	return new_r_Mux(block, sel, irn_false, irn_true);
}

/**
 * Apply a conversion on a constant operator (a Mux).
 *
 * @param mux    the Mux node
 *
 * @return a new Mux node if the conversion was successful, NULL else
 */
static ir_node *apply_conv_on_mux(ir_node *mux, ir_mode *mode)
{
	if (!only_one_user(mux))
		return NULL;

	ir_tarval *true_val  = get_Const_tarval(get_Mux_true(mux));
	ir_tarval *false_val = get_Const_tarval(get_Mux_false(mux));

	ir_tarval *new_true  = tarval_convert_to(true_val, mode);
	ir_tarval *new_false = tarval_convert_to(false_val, mode);

	if (!tarval_is_constant(new_true) || !tarval_is_constant(new_false))
		return NULL;

	ir_node  *sel       = get_Mux_sel(mux);
	ir_graph *irg       = get_irn_irg(mux);
	ir_node  *irn_true  = new_r_Const(irg, new_true);
	ir_node  *irn_false = new_r_Const(irg, new_false);
	ir_node  *block     = get_nodes_block(mux);
	return new_r_Mux(block, sel, irn_false, irn_true);
}

/*
 * Macros to include the constant folding optimizations for nodes with
 * a choice of data (i.e. Phi and Mux).
 */

#define HANDLE_BINOP_CHOICE(eval, a, b, c, mode)                  \
  do {                                                            \
  c = NULL;                                                       \
  if (is_Const(b) && is_const_Phi(a)) {                           \
    /* check for Op(Phi, Const) */                                \
    c = apply_binop_on_phi(a, get_Const_tarval(b), eval, mode, 0);\
  } else if (is_Const(a) && is_const_Phi(b)) {                    \
    /* check for Op(Const, Phi) */                                \
    c = apply_binop_on_phi(b, get_Const_tarval(a), eval, mode, 1);\
  } else if (is_const_Phi(a) && is_const_Phi(b)) {                \
    /* check for Op(Phi, Phi) */                                  \
    c = apply_binop_on_2_phis(a, b, eval, mode);                  \
  } else if (is_Const(b) && is_const_Mux(a)) {                    \
    /* check for Op(Mux, Const) */                                \
    c = apply_binop_on_mux(a, get_Const_tarval(b), eval, 0);      \
  } else if (is_Const(a) && is_const_Mux(b)) {                    \
    /* check for Op(Const, Phi) */                                \
    c = apply_binop_on_mux(b, get_Const_tarval(a), eval, 1);      \
  } else if (is_const_Mux(a) && is_const_Mux(b)) {                \
    /* check for Op(Mux, Mux) */                                  \
    c = apply_binop_on_2_muxs(a, b, eval);                        \
  }                                                               \
  if (c) {                                                        \
    DBG_OPT_ALGSIM0(oldn, c);                                     \
    return c;                                                     \
  }                                                               \
  } while(0)

#define HANDLE_UNOP_CHOICE(eval, a, c)            \
  do {                                            \
  c = NULL;                                       \
  if (is_const_Phi(a)) {                          \
    /* check for Op(Phi) */                       \
    c = apply_unop_on_phi(a, eval);               \
  } else if (is_const_Mux(a)) {                   \
    /* check for Op(Mux) */                       \
    c = apply_unop_on_mux(a, eval);               \
  }                                               \
  if (c) {                                        \
    DBG_OPT_ALGSIM0(oldn, c);                     \
    return c;                                     \
  }                                               \
  } while(0)

static ir_node *create_bool_const(ir_graph *const irg, bool const val)
{
	ir_tarval *const tv = val ? get_tarval_b_true() : get_tarval_b_false();
	return new_r_Const(irg, tv);
}

static bool is_shiftop(const ir_node *n)
{
	return is_Shl(n) || is_Shr(n) || is_Shrs(n);
}

/* the order of the values is important! */
typedef enum const_class {
	const_const = 0,
	const_like  = 1,
	const_other = 2
} const_class;

static const_class classify_const(const ir_node* n)
{
	if (is_Const(n))         return const_const;
	if (is_irn_constlike(n)) return const_like;
	return const_other;
}

/**
 * Determines whether r is more constlike or has a larger index (in that order)
 * than l.
 */
static bool operands_are_normalized(const ir_node *l, const ir_node *r)
{
	const const_class l_order = classify_const(l);
	const const_class r_order = classify_const(r);
	return l_order > r_order
	    || (l_order == r_order && get_irn_idx(l) <= get_irn_idx(r));
}

static bool is_cmp_unequal(const ir_node *node)
{
	const ir_node *left     = get_Cmp_left(node);
	const ir_node *right    = get_Cmp_right(node);
	ir_relation    relation = get_Cmp_relation(node);
	ir_relation    possible = ir_get_possible_cmp_relations(left, right);

	return is_relation(ir_relation_less_greater, relation, possible);
}

/**
 * returns true for Cmp(x == 0) or Cmp(x != 0)
 */
static bool is_cmp_equality_zero(const ir_node *left, const ir_node *right, ir_relation relation)
{
	if (!is_irn_null(right))
		return false;

	ir_relation possible = ir_get_possible_cmp_relations(left, right);

	return get_complementary_relations(ir_relation_equal, relation, possible) != ir_relation_false;
}

static bool is_binop_const(ir_node *const irn, ir_node *const left, ir_tarval *const val)
{
	if (!irn || get_binop_left(irn) != left)
		return false;
	ir_node *const r = get_binop_right(irn);
	return is_Const(r) && get_Const_tarval(r) == val;
}

/**
 * Optimizes a chain of bitwise operations with constants.
 *
 * In general, we can reduce each chain down to two operations.
 * For instance, we transform OR(XOR(AND(x, c1), c2), c3)
 * into XOR(AND(x, c4), c5) with some magic constants c4 and c5.
 */
static ir_node *transform_bitop_chain(ir_node *const irn)
{
	ir_mode *const mode = get_irn_mode(irn);

	/* These tarvals have a bit set iff the corresponding action takes place. */
	ir_tarval *keep = get_mode_all_one(mode); /* And mask. */
	ir_tarval *flip = get_mode_null(mode);    /* Eor mask. */

	/* Walk the chain and adapt the tarvals.
	 * Note that the current operation is performed *before* the old ones. */
	ir_node *current = irn;
	/* Remember the highest node of these kinds. They may be reused later on. */
	ir_node *top_and = NULL;
	ir_node *top_eor = NULL;
	ir_node *top_not = NULL;
	ir_node *top_or  = NULL;
	for (;;) {
		if (is_Not(current)) {
			/* ~x & keep ^ flip -> x & keep ^ (flip ^ keep) */
			flip    = tarval_eor(flip, keep);
			top_not = current;
			current = get_Not_op(current);
		} else if (is_binop(current)) {
			ir_node *const right = get_binop_right(current);
			if (!is_Const(right))
				goto chain_end;

			ir_tarval *const c = get_Const_tarval(right);
			switch (get_irn_opcode(current)) {
			case iro_And:
				/* (x & c) & keep ^ flip -> x & (keep & c) ^ flip */
				keep    = tarval_and(keep, c);
				top_and = current;
				break;

			case iro_Add:
				if (!is_Eor_Add(current))
					goto chain_end;
				/* FALLTHROUGH */
			case iro_Eor:
				/* (x ^ c) & keep ^ flip -> x & keep ^ (flip ^ (keep & c)) */
				flip    = tarval_eor(flip, tarval_and(keep, c));
				top_eor = current;
				break;

			case iro_Or:
				/* (x | c) & keep ^ flip -> x & (keep & ~c) ^ (flip ^ (keep & c)) */
				flip   = tarval_eor(flip, tarval_and(keep, c));
				keep   = tarval_andnot(keep, c);
				top_or = current;
				break;

			default:
				goto chain_end;
			}
			current = get_binop_left(current);
		} else {
			goto chain_end;
		}
	}
chain_end:;

	ir_node        *res   = current;
	dbg_info *const dbgi  = get_irn_dbg_info(irn);
	ir_node  *const block = get_nodes_block(irn);
	ir_graph *const irg   = get_irn_irg(irn);
	if (!tarval_is_all_one(keep)) {
		if (tarval_is_all_one(tarval_eor(keep, flip))) {
			/* Chain is an Or. */
			if (is_binop_const(top_or, res, flip)) {
				res = top_or;
			} else {
				ir_node *const orc = new_r_Const(irg, flip);
				res = new_rd_Or(dbgi, block, res, orc);
			}
		} else {
			if (keep == flip) {
				/* Chain starts with a Not. */
				if (top_not && get_Not_op(top_not) == res) {
					res = top_not;
				} else {
					res = new_rd_Not(dbgi, block, res);
				}
				flip = get_mode_null(mode);
			}

			/* Chain starts with an And. */
			if (is_binop_const(top_and, res, keep)) {
				res = top_and;
			} else {
				ir_node *const andc = new_r_Const(irg, keep);
				res = new_rd_And(dbgi, block, res, andc);
			}
			goto flip;
		}
	} else {
flip:
		if (tarval_is_all_one(flip)) {
			/* Chain ends with a Not. */
			if (top_not && get_Not_op(top_not) == res) {
				res = top_not;
			} else {
				res = new_rd_Not(dbgi, block, res);
			}
		} else if (!tarval_is_null(flip)) {
			/* Chain ends with an Eor. */
			if (is_binop_const(top_eor, res, flip)) {
				res = top_eor;
			} else {
				ir_node *const eorc = new_r_Const(irg, flip);
				res = new_rd_Eor(dbgi, block, res, eorc);
			}
		}
	}

	return res;
}

static ir_node *new_binop(ir_node *const old, ir_node *const block, ir_node *const l, ir_node *const r)
{
	dbg_info *const dbgi = get_irn_dbg_info(old);
	ir_graph *const irg  = get_irn_irg(block);
	ir_op    *const op   = get_irn_op(old);
	ir_mode  *const mode = get_irn_mode(l /* sic */);
	ir_node  *const in[] = { l, r };
	ir_node  *const n    = new_ir_node(dbgi, irg, block, op, mode, ARRAY_SIZE(in), in);
	return optimize_node(n);
}

/**
 * makes use of distributive laws for and, or, eor
 *     and(a OP c, b OP c) -> and(a, b) OP c
 * note, might return a different op than n
 */
static ir_node *transform_bitwise_distributive(ir_node *n)
{
	ir_node     *oldn    = n;
	ir_node     *a       = get_binop_left(n);
	ir_node     *b       = get_binop_right(n);
	const ir_op *op      = get_irn_op(a);
	const ir_op *op_root = get_irn_op(n);

	if (op != get_irn_op(b))
		return n;

	if (!only_one_user(a) && !only_one_user(b))
		return n;

	/* and(conv(a), conv(b)) -> conv(and(a,b)) */
	if (op == op_Conv) {
		ir_node       *a_op   = get_Conv_op(a);
		ir_node       *b_op   = get_Conv_op(b);
		ir_mode       *a_mode = get_irn_mode(a_op);
		const ir_mode *b_mode = get_irn_mode(b_op);
		if (a_mode == b_mode && mode_is_int(a_mode)) {
			ir_node *blk = get_nodes_block(n);
			n = new_binop(n, blk, a_op, b_op);
			n = new_r_Conv(blk, n, get_irn_mode(oldn));

			DBG_OPT_ALGSIM1(oldn, a, b, n);
			return n;
		}
	}

	if (op == op_Eor)
		return n; /* nothing to gain here */

	if (op == op_Shrs || op == op_Shr || op == op_Shl
	    || op == op_And || op == op_Or || op == op_Eor) {
		ir_node *a_left  = get_binop_left(a);
		ir_node *a_right = get_binop_right(a);
		ir_node *b_left  = get_binop_left(b);
		ir_node *b_right = get_binop_right(b);
		ir_node *c       = NULL;
		ir_node *op1     = NULL;
		ir_node *op2     = NULL;

		if (is_op_commutative(op)) {
			if (a_left == b_left) {
				c   = a_left;
				op1 = a_right;
				op2 = b_right;
			} else if (a_left == b_right) {
				c   = a_left;
				op1 = a_right;
				op2 = b_left;
			} else if (a_right == b_left) {
				c   = a_right;
				op1 = a_left;
				op2 = b_right;
			}
		}
		if (a_right == b_right) {
			c   = a_right;
			op1 = a_left;
			op2 = b_left;
		}

		if (c != NULL) {
			/* (a sop c) & (b sop c) => (a & b) sop c */
			ir_node *const blk   = get_nodes_block(n);
			ir_node *const new_n = new_binop(n, blk, op1, op2);

			if (op_root == op_Eor && op == op_Or) {
				dbg_info *dbgi = get_irn_dbg_info(n);
				c = new_rd_Not(dbgi, blk, c);
				n = new_rd_And(dbgi, blk, new_n, c);
			} else {
				n = new_binop(a, blk, new_n, c);
				add_identities(n);
			}

			DBG_OPT_ALGSIM1(oldn, a, b, n);
			return n;
		}
	}

	return n;
}

/**
 * normalization: (x >> c1) & c2   to   (x & (c2<<c1)) >> c1
 *  (we can use:
 *    - and, or, xor    instead of &
 *    - Shl, Shr, Shrs  instead of >>
 *    (with a special case for Or/Xor + Shrs)
 *
 * This normalization is usually good for the backend since << C can often be
 * matched as address-mode.
 */
static ir_node *transform_node_bitop_shift(ir_node *n)
{
	ir_graph *irg = get_irn_irg(n);

	if (!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_NORMALISATION2))
		return n;

	const ir_node *left  = get_binop_left(n);
	const ir_node *right = get_binop_right(n);
	assert(is_And(n) || is_Or(n) || is_Eor(n) || is_Eor_Add(n));
	if (!is_Const(right) || !is_shiftop(left))
		return n;

	ir_node *shift_right = get_binop_right(left);
	if (!is_Const(shift_right))
		return n;

	/* doing it with Shrs is not legal if the Or/Eor affects the topmost bit */
	/* TODO this could be improved */
	if (is_Shrs(left))
		return n;

	ir_node   *shift_left = get_binop_left(left);
	ir_node   *block      = get_nodes_block(n);
	dbg_info  *dbg_bitop  = get_irn_dbg_info(n);
	dbg_info  *dbg_shift  = get_irn_dbg_info(left);
	ir_tarval *tv1        = get_Const_tarval(shift_right);
	ir_tarval *tv2        = get_Const_tarval(right);
	ir_tarval *tv_bitop;
	assert(get_tarval_mode(tv2) == get_irn_mode(n));

	if (is_Shl(left)) {
		tv_bitop = tarval_shr(tv2, tv1);

		/* Check whether we have lost some bits during the right shift. */
		if (!is_And(n)) {
			ir_tarval *tv_back_again = tarval_shl(tv_bitop, tv1);

			if (tv_back_again != tv2)
				return n;
		}
	} else {
		assert(is_Shr(left));
		/* TODO this can be improved by checking whether
		 *      the left shift produces an overflow */
		if (!is_And(n))
			return n;
		tv_bitop = tarval_shl(tv2, tv1);
	}
	ir_node *new_bitop;
	ir_node *new_shift;
	ir_node *new_const = new_r_Const(irg, tv_bitop);

	if (is_And(n)) {
		new_bitop = new_rd_And(dbg_bitop, block, shift_left, new_const);
	} else if (is_Or(n) || is_Or_Eor_Add(n)) {
		new_bitop = new_rd_Or(dbg_bitop, block, shift_left, new_const);
	} else {
		assert(is_Eor(n) || is_Eor_Add(n));
		new_bitop = new_rd_Eor(dbg_bitop, block, shift_left, new_const);
	}

	if (is_Shl(left)) {
		new_shift = new_rd_Shl(dbg_shift, block, new_bitop, shift_right);
	} else {
		assert(is_Shr(left));
		new_shift = new_rd_Shr(dbg_shift, block, new_bitop, shift_right);
	}

	return new_shift;
}

/**
 * for associative operations fold:
 *   op(op(x, c0), c1) to op(x, op(c0, c1)) with constants folded.
 * This is a "light" version of the reassociation phase
 */
static ir_node *fold_constant_associativity(ir_node *node, tarval_binop fold)
{
	const ir_node *right = get_binop_right(node);
	if (!is_Const(right))
		return node;

	const ir_op   *op   = get_irn_op(node);
	const ir_node *left = get_binop_left(node);
	if (get_irn_op(left) != op)
		return node;

	const ir_node *left_right = get_binop_right(left);
	if (!is_Const(left_right))
		return node;

	ir_node   *left_left = get_binop_left(left);
	ir_tarval *c0        = get_Const_tarval(left_right);
	ir_tarval *c1        = get_Const_tarval(right);
	ir_graph  *irg       = get_irn_irg(node);
	if (get_tarval_mode(c0) != get_tarval_mode(c1))
		return node;
	ir_tarval *new_c = fold(c0, c1);
	if (!tarval_is_constant(new_c))
		return node;
	ir_node *new_const = new_r_Const(irg, new_c);
	ir_node *block     = get_nodes_block(node);
	ir_node *new_node  = new_binop(node, block, left_left, new_const);
	return new_node;
}

/**
 * Transform an Or.
 */
static ir_node *transform_node_Or_(ir_node *n)
{
	ir_node *oldn = n;
	ir_node *a    = get_binop_left(n);
	ir_node *b    = get_binop_right(n);

	n = fold_constant_associativity(n, tarval_or);
	if (n != oldn)
		return n;

	if (is_Not(a) && is_Not(b) && (only_one_user(a) || only_one_user(b))) {
		/* ~a | ~b = ~(a&b) */
		ir_node *block = get_nodes_block(n);

		a = get_Not_op(a);
		b = get_Not_op(b);
		n = new_rd_And(get_irn_dbg_info(n), block, a, b);
		n = new_rd_Not(get_irn_dbg_info(n), block, n);
		DBG_OPT_ALGSIM0(oldn, n);
		return n;
	}

	ir_mode *mode = get_irn_mode(n);
	if (is_Minus(a) && is_Const(b)
	    && get_mode_arithmetic(mode) == irma_twos_complement) {
		const bitinfo *const ba   = get_bitinfo(a);
		ir_tarval     *      mask;
		if (ba) {
			ir_tarval *const z   = ba->z;
			ir_tarval *const one = get_mode_one(mode);

			/* Create mask for rightmost may-1-bit and the trailing 0's. */
			mask = tarval_eor(z, tarval_sub(z, one));
		} else {
			mask = get_mode_one(mode);
		}

		ir_tarval *const tv = get_Const_tarval(b);
		if (tarval_is_all_one(tarval_or(tv, mask))) {
			/* -(a=??????00) | 11111000 => a | 11111000 */
			dbg_info *const dbgi     = get_irn_dbg_info(n);
			ir_node  *const block    = get_nodes_block(n);
			ir_node  *const minus_op = get_Minus_op(a);
			return new_rd_Or(dbgi, block, minus_op, b);
		}
	}

	if (is_Eor(a) || is_Eor_Add(a)) {
		ir_node *const o = get_commutative_other_op(a, b);
		if (o) {
			/* (x ^ y) | y => x | y */
			dbg_info *dbgi  = get_irn_dbg_info(n);
			ir_node  *block = get_nodes_block(n);
			return new_rd_Or(dbgi, block, o, b);
		}
	}
	if (is_Eor(b) || is_Eor_Add(b)) {
		ir_node *const o = get_commutative_other_op(b, a);
		if (o) {
			/* x | (x ^ y) => x | y */
			dbg_info *dbgi  = get_irn_dbg_info(n);
			ir_node  *block = get_nodes_block(n);
			return new_rd_Or(dbgi, block, a, o);
		}
	}

	/* we can combine the relations of two compares with the same operands */
	if (is_Cmp(a) && is_Cmp(b)) {
		ir_node *a_left  = get_Cmp_left(a);
		ir_node *a_right = get_Cmp_right(a);
		ir_node *b_left  = get_Cmp_left(b);
		ir_node *b_right = get_Cmp_right(b);
		if (a_left == b_left && b_left == b_right) {
			dbg_info   *dbgi         = get_irn_dbg_info(n);
			ir_node    *block        = get_nodes_block(n);
			ir_relation a_relation   = get_Cmp_relation(a);
			ir_relation b_relation   = get_Cmp_relation(b);
			ir_relation new_relation = a_relation | b_relation;
			return new_rd_Cmp(dbgi, block, a_left, a_right, new_relation);
		}

		/* Cmp(a!=b) or Cmp(c!=d) => Cmp((a^b)|(c^d) != 0) */
		const ir_mode *cmp_mode_left  = get_irn_mode(a_left);
		const ir_mode *cmp_mode_right = get_irn_mode(b_left);
		if (is_cmp_unequal(a) && is_cmp_unequal(b)
		    && !mode_is_float(cmp_mode_left)
		    && !mode_is_float(cmp_mode_right)) {
			if (mode_is_reference(cmp_mode_left)) {
				ir_mode  *omode = get_reference_offset_mode(cmp_mode_left);
				dbg_info *dbgi  = get_irn_dbg_info(a_left);
				ir_node  *block = get_nodes_block(n);
				a_left  = new_rd_Conv(dbgi, block, a_left,  omode);
				a_right = new_rd_Conv(dbgi, block, a_right, omode);
			}
			if (mode_is_reference(cmp_mode_right)) {
				ir_mode  *omode = get_reference_offset_mode(cmp_mode_right);
				dbg_info *dbgi  = get_irn_dbg_info(b_left);
				ir_node  *block = get_nodes_block(n);
				b_left  = new_rd_Conv(dbgi, block, b_left,  omode);
				b_right = new_rd_Conv(dbgi, block, b_right, omode);
			}
			ir_mode *a_mode = get_irn_mode(a_left);
			ir_mode *b_mode = get_irn_mode(b_left);
			if (values_in_mode(a_mode, b_mode)) {
				ir_graph *irg   = get_irn_irg(n);
				dbg_info *dbgi  = get_irn_dbg_info(n);
				ir_node  *block = get_nodes_block(n);
				ir_node  *xora  = new_rd_Eor(dbgi, block, a_left, a_right);
				ir_node  *xorb  = new_rd_Eor(dbgi, block, b_left, b_right);
				ir_node  *conv  = new_rd_Conv(dbgi, block, xora, b_mode);
				ir_node  *orn   = new_rd_Or(dbgi, block, conv, xorb);
				ir_node  *zero  = new_r_Const_null(irg, b_mode);
				return new_rd_Cmp(dbgi, block, orn, zero, ir_relation_less_greater);
			}
			if (values_in_mode(b_mode, a_mode)) {
				ir_graph *irg   = get_irn_irg(n);
				dbg_info *dbgi  = get_irn_dbg_info(n);
				ir_node  *block = get_nodes_block(n);
				ir_node  *xora  = new_rd_Eor(dbgi, block, a_left, a_right);
				ir_node  *xorb  = new_rd_Eor(dbgi, block, b_left, b_right);
				ir_node  *conv  = new_rd_Conv(dbgi, block, xorb, a_mode);
				ir_node  *orn   = new_rd_Or(dbgi, block, xora, conv);
				ir_node  *zero  = new_r_Const_null(irg, a_mode);
				return new_rd_Cmp(dbgi, block, orn, zero, ir_relation_less_greater);
			}
		}
	}

	if (is_Shrs(a) && is_Const(b) && only_one_user(a)) {
		ir_node *ab = get_Shrs_right(a);
		if (is_Const(ab)) {
			ir_tarval *tv_or        = get_Const_tarval(b);
			ir_tarval *tv_not       = tarval_not(tv_or);
			long       highest_bit  = get_tarval_highest_bit(tv_not);
			long       shift_amount = get_Const_long(ab);
			unsigned   bit_size     = get_mode_size_bits(mode);
			if (highest_bit + shift_amount < (long)bit_size) {
				/* (x >>s 16) | 0xFFFF0000 => (x >> 16) | 0xFFFF0000 */
				ir_node  *const aa         = get_Shrs_left(a);
				dbg_info *const shrs_dbgi  = get_irn_dbg_info(a);
				ir_node  *const shrs_block = get_nodes_block(a);
				ir_node  *const shr        = new_rd_Shr(shrs_dbgi, shrs_block, aa, ab);
				dbg_info *const dbgi       = get_irn_dbg_info(n);
				ir_node  *const block      = get_nodes_block(n);
				return new_rd_Or(dbgi, block, shr, b);
			}
		}
	}

	ir_node *c;
	HANDLE_BINOP_CHOICE(tarval_or, a, b, c, mode);

	n = transform_bitop_chain(n);
	if (n != oldn)
		return n;

	n = transform_bitwise_distributive(n);
	if (n != oldn)
		return n;
	n = transform_node_bitop_shift(n);
	if (n != oldn)
		return n;

	return n;
}

static ir_node *transform_node_Or(ir_node *n)
{
	if (is_Or_Eor_Add(n)) {
		dbg_info *dbgi  = get_irn_dbg_info(n);
		ir_node  *block = get_nodes_block(n);
		ir_node  *left  = get_Or_left(n);
		ir_node  *right = get_Or_right(n);
		return new_rd_Add(dbgi, block, left, right);
	}
	return transform_node_Or_(n);
}

/**
 * Transform an Eor.
 */
static ir_node *transform_node_Eor_(ir_node *n)
{
	ir_node *oldn = n;
	ir_node *a    = get_binop_left(n);
	ir_node *b    = get_binop_right(n);
	ir_mode *mode = get_irn_mode(n);

	n = fold_constant_associativity(n, tarval_eor);
	if (n != oldn)
		return n;

	/* we can combine the relations of two compares with the same operands */
	if (is_Cmp(a) && is_Cmp(b)) {
		ir_node       *a_left  = get_Cmp_left(a);
		ir_node       *a_right = get_Cmp_right(a);
		const ir_node *b_left  = get_Cmp_left(b);
		const ir_node *b_right = get_Cmp_right(b);
		if (a_left == b_left && b_left == b_right) {
			dbg_info   *dbgi         = get_irn_dbg_info(n);
			ir_node    *block        = get_nodes_block(n);
			ir_relation a_relation   = get_Cmp_relation(a);
			ir_relation b_relation   = get_Cmp_relation(b);
			ir_relation new_relation = a_relation ^ b_relation;
			return new_rd_Cmp(dbgi, block, a_left, a_right, new_relation);
		}
	}

	ir_node *c;
	HANDLE_BINOP_CHOICE(tarval_eor, a, b, c, mode);

	/* normalize not nodes... ~a ^ b <=> a ^ ~b */
	if (is_Not(a) && only_one_user(a) &&
	    operands_are_normalized(get_Not_op(a), b)) {
		dbg_info *dbg      = get_irn_dbg_info(n);
		ir_node  *block    = get_nodes_block(n);
		ir_node  *new_not  = new_rd_Not(dbg, block, b);
		ir_node  *new_left = get_Not_op(a);
		n = new_rd_Eor(dbg, block, new_left, new_not);
		DBG_OPT_ALGSIM0(oldn, n);
		return n;
	} else if (is_Not(b) && only_one_user(b) &&
	           !operands_are_normalized(a, get_Not_op(b))) {
		dbg_info *dbg       = get_irn_dbg_info(n);
		ir_node  *block     = get_nodes_block(n);
		ir_node  *new_not   = new_rd_Not(dbg, block, a);
		ir_node  *new_right = get_Not_op(b);
		n = new_rd_Eor(dbg, block, new_not, new_right);
		DBG_OPT_ALGSIM0(oldn, n);
		return n;
	}

	if (is_Const(b)) {
		if (is_Const_all_one(b)) {
			/* x ^ 1...1 -> ~x */
			dbg_info *dbgi  = get_irn_dbg_info(n);
			ir_node  *block = get_nodes_block(n);
			n = new_rd_Not(dbgi, block, a);
			DBG_OPT_ALGSIM0(oldn, n);
			return n;
		}

		if (get_mode_arithmetic(mode) == irma_twos_complement) {
			ir_tarval *tb       = get_Const_tarval(b);
			ir_tarval *all_one  = get_mode_all_one(mode);
			ir_tarval *expected = tarval_shr_unsigned(all_one, 1);
			if (tb == expected) {
				if (is_Add(a)) {
					ir_node *ab = get_Add_right(a);
					if (is_Const(ab)) {
						/* (x + c) ^ 0x7F...F -> (0x7F...F - c) - x */
						dbg_info  *dbgi  = get_irn_dbg_info(n);
						ir_graph  *irg   = get_irn_irg(n);
						ir_node   *block = get_nodes_block(n);
						ir_node   *aa    = get_Add_left(a);
						ir_tarval *tab   = get_Const_tarval(ab);
						ir_tarval *tc    = tarval_sub(tb, tab);
						ir_node   *c     = new_rd_Const(dbgi, irg, tc);
						return new_rd_Sub(dbgi, block, c, aa);
					}
				} else if (is_Sub(a)) {
					ir_node *aa = get_Sub_left(a);
					if (is_Const(aa)) {
						/* (c - x) ^ 0x7F...F -> x + (0x7F...F - c) */
						dbg_info  *dbgi  = get_irn_dbg_info(n);
						ir_graph  *irg   = get_irn_irg(n);
						ir_node   *block = get_nodes_block(n);
						ir_node   *ab    = get_Sub_right(a);
						ir_tarval *taa   = get_Const_tarval(aa);
						ir_tarval *tc    = tarval_sub(tb, taa);
						ir_node   *c     = new_rd_Const(dbgi, irg, tc);
						return new_rd_Add(dbgi, block, ab, c);
					}
				}
			}

			if (is_Minus(a)) {
				ir_tarval *tv   = get_Const_tarval(b);
				int        bits = (int)get_mode_size_bits(mode);
				if (get_tarval_popcount(tarval_shl_unsigned(tv, 1)) == bits - 1) {
					/* -x ^ ?1...1 => x + ?1...1 */
					dbg_info *dbgi  = get_irn_dbg_info(n);
					ir_node  *block = get_nodes_block(n);
					ir_node  *op    = get_Minus_op(a);
					return new_rd_Add(dbgi, block, op, b);
				}
			}
		}
	}

	if (is_And(a) && only_one_user(a)) {
		ir_node *const o = get_commutative_other_op(a, b);
		if (o) {
			/* (x & y) ^ y => ~x & y */
			dbg_info *dbgi  = get_irn_dbg_info(n);
			ir_node  *block = get_nodes_block(n);
			ir_node  *not   = new_rd_Not(dbgi, block, o);
			return new_rd_And(dbgi, block, not, b);
		}
	}
	if (is_And(b) && only_one_user(b)) {
		ir_node *const o = get_commutative_other_op(b, a);
		if (o) {
			/* x ^ (x & y) => x & ~y */
			dbg_info *dbgi  = get_irn_dbg_info(n);
			ir_node  *block = get_nodes_block(n);
			ir_node  *not   = new_rd_Not(dbgi, block, o);
			return new_rd_And(dbgi, block, a, not);
		}
	}

	if (is_Or(a) && only_one_user(a)) {
		ir_node *const o = get_commutative_other_op(a, b);
		if (o) {
			/* (x | y) ^ y => x & ~y */
			dbg_info *dbgi  = get_irn_dbg_info(n);
			ir_node  *block = get_nodes_block(n);
			ir_node  *not   = new_rd_Not(dbgi, block, b);
			return new_rd_And(dbgi, block, o, not);
		}
	}
	if (is_Or(b) && only_one_user(b)) {
		ir_node *const o = get_commutative_other_op(b, a);
		if (o) {
			/* x ^ (x | y) => ~x & y */
			dbg_info *dbgi  = get_irn_dbg_info(n);
			ir_node  *block = get_nodes_block(n);
			ir_node  *not   = new_rd_Not(dbgi, block, a);
			return new_rd_And(dbgi, block, not, o);
		}
	}


	if (is_Or(a) && is_And(b) && only_one_user(a) && only_one_user(b)) {
		ir_node *al = get_binop_left(a);
		ir_node *ar = get_binop_right(a);
		ir_node *bl = get_binop_left(b);
		ir_node *br = get_binop_right(b);

		if ((al == bl && ar == br) || (al == br && ar == bl)) {
			/* (x | y) ^ (x & y) -> x ^ y */
			dbg_info *dbgi  = get_irn_dbg_info(n);
			ir_node  *block = get_nodes_block(n);
			return new_rd_Eor(dbgi, block, al, ar);
		}
	}
	if (is_And(a) && is_Or(b) && only_one_user(a) && only_one_user(b)) {
		ir_node *al = get_binop_left(a);
		ir_node *ar = get_binop_right(a);
		ir_node *bl = get_binop_left(b);
		ir_node *br = get_binop_right(b);

		if ((al == bl && ar == br) || (al == br && ar == bl)) {
			/* (x & y) ^ (x | y) -> x ^ y */
			dbg_info *dbgi  = get_irn_dbg_info(n);
			ir_node  *block = get_nodes_block(n);
			return new_rd_Eor(dbgi, block, al, ar);
		}
	}


	// Eor elimination
	ir_node *ta   = a;
	ir_node *tb   = b;
	int      nots = 0;
	if (is_Not(ta) && only_one_user(ta)) {
		nots++;
		ta = get_Not_op(ta);
	}
	if (is_Not(tb) && only_one_user(tb)) {
		nots++;
		tb = get_Not_op(tb);
	}

	if (is_And(tb) || is_Or(tb)) {
		ir_node *l = get_binop_left(tb);
		ir_node *r = get_binop_right(tb);

		if (ta == r || (is_Not(r) && ta == get_Not_op(r))) {
			ir_node *t = l;
			l = r;
			r = t;
		}

		if (is_Not(l) && ta == get_Not_op(l)) {
			nots++;
			ta = l;
		}

		if (ta == l && only_one_user(tb)) {
			nots = nots % 2;

			dbg_info *dbgi  = get_irn_dbg_info(n);
			ir_node  *block = get_nodes_block(n);
			ir_node  *t;
			if(is_And(tb)) {
				t = new_rd_And(dbgi, block, l, new_rd_Not(dbgi, block, r));
			} else {
				assert(is_Or(tb));
				t = new_rd_And(dbgi, block, new_rd_Not(dbgi, block, l), r);
			}
			if (nots)
				t = new_rd_Not(dbgi, block, t);
			return t;
		}
	}

	if (is_And(ta) || is_Or(ta)) {
		ir_node *l = get_binop_left(ta);
		ir_node *r = get_binop_right(ta);

		if (tb == r || (is_Not(r) && tb == get_Not_op(r))) {
			ir_node *t = l;
			l = r;
			r = t;
		}

		if (is_Not(l) && tb == get_Not_op(l)) {
			nots++;
			tb = l;
		}

		if (tb == l && only_one_user(ta)) {
			nots = nots % 2;

			dbg_info *dbgi  = get_irn_dbg_info(n);
			ir_node  *block = get_nodes_block(n);
			ir_node  *t;
			if(is_And(ta)) {
				t = new_rd_And(dbgi, block, l, new_rd_Not(dbgi, block, r));
			} else {
				assert(is_Or(ta));
				t = new_rd_And(dbgi, block, new_rd_Not(dbgi, block, l), r);
			}
			if (nots)
				t = new_rd_Not(dbgi, block, t);
			return t;
		}
	}

	n = transform_bitop_chain(n);
	if (n != oldn)
		return n;

	n = transform_bitwise_distributive(n);
	if (n != oldn)
		return n;

	return transform_node_bitop_shift(n);
}

static ir_node *transform_node_Eor(ir_node *n)
{
	/* normalize */
	if (is_Eor_Add(n)) {
		dbg_info *dbgi  = get_irn_dbg_info(n);
		ir_node  *block = get_nodes_block(n);
		ir_node  *left  = get_Eor_left(n);
		ir_node  *right = get_Eor_right(n);
		return new_rd_Add(dbgi, block, left, right);
	}
	return transform_node_Eor_(n);
}

/**
 * Do the AddSub optimization, then Transform
 *   Constant folding on Phi
 *   Add(a,a)          -> Mul(a, 2)
 *   Add(Mul(a, x), a) -> Mul(a, x+1)
 * if the mode is integer or float.
 * Transform Add(a,-b) into Sub(a,b).
 * Reassociation might fold this further.
 */
static ir_node *transform_node_Add(ir_node *n)
{
	ir_node *oldn = n;

	n = fold_constant_associativity(n, tarval_add);
	if (n != oldn)
		return n;

	ir_node *a    = get_Add_left(n);
	ir_node *b    = get_Add_right(n);
	ir_mode *mode = get_irn_mode(n);

	if (mode_is_reference(mode)) {
		const ir_mode *lmode = get_irn_mode(a);

		if (is_irn_null(b) && mode_is_int(lmode)) {
			/* an Add(a, NULL) is a hidden Conv */
			dbg_info *dbg = get_irn_dbg_info(n);
			return new_rd_Conv(dbg, get_nodes_block(n), a, mode);
		}
		if (is_Const(b) && is_Add(a)) {
			ir_node *ab = get_Add_right(a);
			if (is_Address(ab)) {
				/* (aa + Address) + Const -> aa + (Address + Const) */
				ir_node *aa = get_Add_left(a);
				dbg_info *const dbgi  = get_irn_dbg_info(n);
				ir_node  *const block = get_nodes_block(n);
				n = new_rd_Add(dbgi, block, ab, b);
				n = new_rd_Add(dbgi, block, aa, n);
				DBG_OPT_ALGSIM0(oldn, n);
				return n;
			}
		}
	}

	if (is_Const(b)) {
		if (is_Confirm(a)) {
			const ir_node *bound  = get_Confirm_bound(a);
			ir_tarval     *tbound = value_of(bound);
			ir_relation    rel    = get_Confirm_relation(a);
			ir_tarval     *tb     = get_Const_tarval(b);
			ir_tarval     *max    = get_mode_max(mode);

			if (rel == ir_relation_less_equal && tarval_is_constant(tbound)
			&& tarval_cmp(tbound, tarval_sub(max, tb)) == ir_relation_less) {
				ir_tarval *tv = tarval_add(tbound, tb);
				if (tarval_is_constant(tv)) {
					ir_node  *value = get_Confirm_value(a);
					dbg_info *dbgi  = get_irn_dbg_info(n);
					ir_graph *irg   = get_irn_irg(n);
					ir_node  *block = get_nodes_block(n);
					ir_node  *cnst  = new_r_Const(irg, tv);
					ir_node  *add   = new_rd_Add(dbgi, block, value, b);
					return new_rd_Confirm(dbgi, block, add, cnst, ir_relation_less_equal);
				}
			}
		} else if (is_Sub(a)) {
			const ir_node *sub_l = get_Sub_left(a);
			ir_tarval     *ts    = value_of(sub_l);

			if (tarval_is_constant(ts)) {
				ir_tarval *tb = get_Const_tarval(b);
				ir_tarval *tv = tarval_add(ts, tb);

				if (tarval_is_constant(tv)) {
					/* (C1 - x) + C2 = (C1 + C2) - x */
					dbg_info *dbgi  = get_irn_dbg_info(n);
					ir_graph *irg   = get_irn_irg(n);
					ir_node  *block = get_nodes_block(n);
					ir_node  *cnst  = new_r_Const(irg, tv);
					ir_node  *sub_r = get_Sub_right(a);
					return new_rd_Sub(dbgi, block, cnst, sub_r);
				}
			}
		}
	}

	ir_node *c;
	HANDLE_BINOP_CHOICE(tarval_add, a, b, c, mode);

	/* these optimizations are imprecise for floating-point ops */
	if (mode_is_float(mode) && !ir_imprecise_float_transforms_allowed())
		return n;

	if (mode_is_num(mode)) {
		ir_graph *irg = get_irn_irg(n);
		/* the following code leads to endless recursion when Mul are replaced
		 * by a simple instruction chain */
		if (a == b) {
			dbg_info *const dbgi  = get_irn_dbg_info(n);
			ir_node  *const block = get_nodes_block(n);
			if (!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_ARCH_DEP)) {
				/* a + a -> a * 2 */
				ir_tarval *const tv_one = get_mode_one(mode);
				ir_tarval *const tv_two = tarval_add(tv_one, tv_one);
				ir_node   *const two    = new_r_Const(irg, tv_two);
				n = new_rd_Mul(dbgi, block, a, two);
				DBG_OPT_ALGSIM0(oldn, n);
				return n;
			} else if (get_mode_arithmetic(mode) == irma_twos_complement) {
				/* a + a -> a << 1 */
				ir_node *const one = new_r_Const_one(irg, mode_Iu);
				n = new_rd_Shl(dbgi, block, a, one);
				return n;
			}
		}
		if (is_Minus(a)) {
			/* -a + b -> b - a */
			dbg_info *const dbgi  = get_irn_dbg_info(n);
			ir_node  *const block = get_nodes_block(n);
			n = new_rd_Sub(dbgi, block, b, get_Minus_op(a));
			DBG_OPT_ALGSIM0(oldn, n);
			return n;
		}
		if (is_Minus(b)) {
			/* a + -b -> a - b */
			dbg_info *const dbgi  = get_irn_dbg_info(n);
			ir_node  *const block = get_nodes_block(n);
			n = new_rd_Sub(dbgi, block, a, get_Minus_op(b));
			DBG_OPT_ALGSIM0(oldn, n);
			return n;
		}
		if (get_mode_arithmetic(mode) == irma_twos_complement) {
			/* Here we rely on constants be on the RIGHT side */
			if (is_Not(a)) {
				ir_node *op = get_Not_op(a);

				if (is_Const(b)) {
					ir_tarval *const tv  = get_Const_tarval(b);
					ir_tarval *const one = get_mode_one(mode);
					ir_tarval *const add = tarval_sub(tv, one);

					if (tarval_is_constant(add)) {
						/* ~x + C = (C - 1) - x */
						dbg_info *const dbgi  = get_irn_dbg_info(n);
						ir_node  *const block = get_nodes_block(n);
						ir_node  *const c     = new_rd_Const(dbgi, irg, add);
						n = new_rd_Sub(dbgi, block, c, op);
						DBG_OPT_ALGSIM0(oldn, n);
						return n;
					}
				}
			}
			if (is_Or(a) && is_Const(b)) {
				ir_node *const ab = get_Or_right(a);
				if (is_Const(ab)) {
					ir_tarval *const tab = get_Const_tarval(ab);
					ir_tarval *const tb  = get_Const_tarval(b);
					if (tarval_is_null(tarval_add(tab, tb))) {
						/* (x | c) + -c -> x & ~c */
						dbg_info  *const dbgi  = get_irn_dbg_info(n);
						ir_graph  *const irg   = get_irn_irg(n);
						ir_node   *const block = get_nodes_block(n);
						ir_node   *const aa    = get_Or_left(a);
						ir_tarval *const tc    = tarval_not(tab);
						ir_node   *const c     = new_rd_Const(dbgi, irg, tc);
						return new_rd_And(dbgi, block, aa, c);
					}
				}
			}
			if (!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_ARCH_DEP)) {
				ir_node *x;
				ir_node *y;
				ir_node *z;
				if (is_Mul(a)) {
					x = b;
					y = get_commutative_other_op(a, x);
					if (y && !is_Const(b)) /* (x * y) + x -> x * (y + 1) */
						goto mul_y_plus_1;
					if (is_Mul(b)) {
						ir_node *const al = get_Mul_left(a);
						ir_node *const ar = get_Mul_right(a);
						x = al;
						y = ar;
						z = get_commutative_other_op(b, x);
						if (z) /* (x * y) + (x * z) -> x * (y + z) */
							goto mul_y_plus_z;
						x = ar;
						y = al;
						z = get_commutative_other_op(b, y);
						if (z) /* (y * x) + (x * z) -> x * (y + z) */
							goto mul_y_plus_z;
					}
				}
				if (is_Mul(b)) {
					x = a;
					y = get_commutative_other_op(b, x);
					if (y) {
						/* x + (x * y) -> x * (y + 1) */
mul_y_plus_1:;
						z = new_r_Const_one(irg, mode);
mul_y_plus_z:;
						dbg_info *const dbgi  = get_irn_dbg_info(n);
						ir_node  *const block = get_nodes_block(n);
						ir_node  *const sum   = new_rd_Add(dbgi, block, y, z);
						return new_rd_Mul(dbgi, block, x, sum);
					}
				}
			}

			ir_node *conv_op;
			ir_node *other_op;
			ir_mode *ref_mode;
			if (is_Conv(a)) {
				conv_op  = get_Conv_op(a);
				other_op = b;
				ref_mode = get_irn_mode(conv_op);
				if (mode_is_reference(ref_mode)
				 && get_mode_size_bits(ref_mode) == get_mode_size_bits(mode))
					goto normalize_p;
			}
			if (is_Conv(b)) {
				conv_op  = get_Conv_op(b);
				other_op = a;
				ref_mode = get_irn_mode(conv_op);
				if (mode_is_reference(ref_mode)
				 && get_mode_size_bits(ref_mode) == get_mode_size_bits(mode)) {
normalize_p:;
					/* AddI(ConvI(op_P), b) -> ConvI(AddP(op_P, ConvI(b)))
					 * Note: The Convs are all same size and no-ops. */
					dbg_info *const dbgi  = get_irn_dbg_info(n);
					ir_node  *const block = get_nodes_block(n);
					ir_mode  *const offset_mode
						= get_reference_offset_mode(ref_mode);
					ir_node  *const conv_o  = new_r_Conv(block, other_op,
					                                     offset_mode);
					ir_node  *const new_add = new_rd_Add(dbgi, block, conv_op, conv_o);
					ir_node  *const conv = new_r_Conv(block, new_add, mode);
					return conv;
				}
			}
		}
	}

	if (is_Or_Eor_Add(n)) {
		n = transform_node_Or_(n);
		if (n != oldn)
			return n;
	}
	if (is_Eor_Add(n)) {
		n = transform_node_Eor_(n);
		if (n != oldn)
			return n;
	}

	return n;
}

/**
 * returns -cnst or NULL if impossible
 */
static ir_node *const_negate(ir_node *cnst)
{
	ir_tarval *tv = tarval_neg(get_Const_tarval(cnst));
	if (!tarval_is_constant(tv))
		return NULL;

	dbg_info *dbgi = get_irn_dbg_info(cnst);
	ir_graph *irg  = get_irn_irg(cnst);
	return new_rd_Const(dbgi, irg, tv);
}

/**
 * Do the AddSub optimization, then Transform
 *   Constant folding on Phi
 *   Sub(0,a)          -> Minus(a)
 *   Sub(Mul(a, x), a) -> Mul(a, x-1)
 *   Sub(Sub(x, y), b) -> Sub(x, Add(y,b))
 *   Sub(Add(a, x), x) -> a
 *   Sub(x, Add(x, a)) -> -a
 *   Sub(x, Const)     -> Add(x, -Const)
 */
static ir_node *transform_node_Sub(ir_node *n)
{
	ir_node *oldn = n;

	ir_node *a    = get_Sub_left(n);
	ir_node *b    = get_Sub_right(n);
	ir_mode *mode = get_irn_mode(n);

	if (mode_is_int(mode)) {
		const ir_mode *lmode = get_irn_mode(a);

		if (is_irn_null(b) && mode_is_reference(lmode)) {
			/* a Sub(a, NULL) is a hidden Conv */
			dbg_info *dbg = get_irn_dbg_info(n);
			n = new_rd_Conv(dbg, get_nodes_block(n), a, mode);
			DBG_OPT_ALGSIM0(oldn, n);
			return n;
		}

		if (mode == lmode                                     &&
		    get_mode_arithmetic(mode) == irma_twos_complement &&
		    is_Const(a)) {
			ir_tarval *ta = get_Const_tarval(a);
			if (tarval_is_all_one(ta)) {
				/* -1 - x -> ~x */
				dbg_info *dbgi  = get_irn_dbg_info(n);
				ir_node  *block = get_nodes_block(n);
				n = new_rd_Not(dbgi, block, b);
				DBG_OPT_ALGSIM0(oldn, n);
				return n;
			}

			ir_tarval *all_one  = get_mode_all_one(mode);
			ir_tarval *expected = tarval_shr_unsigned(all_one, 1);
			if (tarval_and(ta, expected) == expected) {
				/* 0x7F...F - x -> x ^ 0x7F...F */
				dbg_info *dbgi  = get_irn_dbg_info(n);
				ir_node  *block = get_nodes_block(n);
				return new_rd_Eor(dbgi, block, b, a);
			}
		}
	}

	ir_node *c;
	HANDLE_BINOP_CHOICE(tarval_sub, a, b, c, mode);

	/* these optimizations are imprecise for floating-point ops */
	if (mode_is_float(mode) && !ir_imprecise_float_transforms_allowed())
		return n;

	if (is_Const(b) && !mode_is_reference(get_irn_mode(b))) {
		/* a - C -> a + (-C) */
		ir_node *cnst = const_negate(b);
		if (cnst != NULL) {
			ir_node  *block = get_nodes_block(n);
			dbg_info *dbgi  = get_irn_dbg_info(n);

			n = new_rd_Add(dbgi, block, a, cnst);
			DBG_OPT_ALGSIM0(oldn, n);
			return n;
		}
	}

	if (is_Const(a) && is_Add(b)) {
		const ir_node *add_r = get_Add_right(b);
		ir_tarval     *tr    = value_of(add_r);

		if (tarval_is_constant(tr)) {
			ir_tarval *ta = get_Const_tarval(a);
			ir_tarval *tv = tarval_sub(ta, tr);

			if (tarval_is_constant(tv)) {
				/* C1 - (x + C2) = (C1 - C2) - x*/
				dbg_info *dbgi  = get_irn_dbg_info(n);
				ir_graph *irg   = get_irn_irg(n);
				ir_node  *block = get_nodes_block(n);
				ir_node  *cnst  = new_r_Const(irg, tv);
				ir_node  *add_l = get_Add_left(b);
				return new_rd_Sub(dbgi, block, cnst, add_l);
			}
		}
	}

	/* (-a) - b -> -(a + b) */
	if (is_Minus(a) && only_one_user(a)) {
		dbg_info *dbg   = get_irn_dbg_info(n);
		ir_node  *block = get_nodes_block(n);
		ir_node  *left  = get_Minus_op(a);
		ir_node  *add   = new_rd_Add(dbg, block, left, b);

		n = new_rd_Minus(dbg, block, add);
		DBG_OPT_ALGSIM0(oldn, n);
		return n;
	}

	/* a - (-b) -> a + b */
	if (is_Minus(b)) {
		dbg_info *dbg   = get_irn_dbg_info(n);
		ir_node  *block = get_nodes_block(n);
		ir_node  *right = get_Minus_op(b);

		n = new_rd_Add(dbg, block, a, right);
		DBG_OPT_ALGSIM0(oldn, n);
		return n;
	}

	/* a - (b - c) -> a + (c - b)
	 *             -> (a - b) + c iff (b - c) is a pointer */
	if (is_Sub(b)) {
		dbg_info *s_dbg   = get_irn_dbg_info(b);
		ir_node  *s_left  = get_Sub_left(b);
		ir_node  *s_right = get_Sub_right(b);
		ir_mode  *s_mode  = get_irn_mode(b);
		if (mode_is_reference(s_mode)) {
			ir_node  *lowest_block = get_nodes_block(n); /* a and b are live here */
			ir_node  *sub          = new_rd_Sub(s_dbg, lowest_block, a, s_left);
			dbg_info *a_dbg        = get_irn_dbg_info(n);

			if (s_mode != mode)
				s_right = new_r_Conv(lowest_block, s_right, mode);
			n = new_rd_Add(a_dbg, lowest_block, sub, s_right);
		} else {
			ir_node  *s_block = get_nodes_block(b);
			ir_node  *sub     = new_rd_Sub(s_dbg, s_block, s_right, s_left);
			dbg_info *a_dbg   = get_irn_dbg_info(n);
			ir_node  *a_block = get_nodes_block(n);

			n = new_rd_Add(a_dbg, a_block, a, sub);
		}
		DBG_OPT_ALGSIM0(oldn, n);
		return n;
	}

	/* Beware of Sub(P, P) which cannot be optimized into a simple Minus ... */
	if (mode_is_num(mode) && mode == get_irn_mode(a) && is_irn_null(a)) {
		n = new_rd_Minus(get_irn_dbg_info(n), get_nodes_block(n), b);
		DBG_OPT_ALGSIM0(oldn, n);
		return n;
	}
	if ((is_Add(a) || is_Or_Eor_Add(a)) && mode_wrap_around(mode)) {
		ir_node *left  = get_binop_left(a);
		ir_node *right = get_binop_right(a);

		/* FIXME: Does the Conv's work only for two complement or generally? */
		if (left == b) {
			/* This Sub is an effective Cast */
			if (mode != get_irn_mode(right))
				right = new_r_Conv(get_nodes_block(n), right, mode);
			n = right;
			DBG_OPT_ALGSIM1(oldn, a, b, n);
			return n;
		} else if (right == b) {
			/* This Sub is an effective Cast */
			if (mode != get_irn_mode(left))
				left = new_r_Conv(get_nodes_block(n), left, mode);
			n = left;
			DBG_OPT_ALGSIM1(oldn, a, b, n);
			return n;
		}
	}
	if ((is_Add(b) || is_Or_Eor_Add(b)) && mode_wrap_around(mode)) {
		ir_node *left  = get_binop_left(b);
		ir_node *right = get_binop_right(b);

		/* FIXME: Does the Conv's work only for two complement or generally? */
		if (left == a) {
			n = new_r_Minus(get_nodes_block(n), right);
			/* This Sub is an effective Cast */
			if (mode != get_irn_mode(right))
				n = new_r_Conv(get_nodes_block(n), n, mode);
			DBG_OPT_ALGSIM1(oldn, a, b, n);
			return n;
		} else if (right == a) {
			n = new_r_Minus(get_nodes_block(n), left);
			/* This Sub is an effective Cast */
			if (mode != get_irn_mode(left))
				n = new_r_Conv(get_nodes_block(n), n, mode);
			DBG_OPT_ALGSIM1(oldn, a, b, n);
			return n;
		}
	}
	if (mode_is_int(mode) && is_Conv(a) && is_Conv(b)) {
		ir_mode *mode = get_irn_mode(a);

		if (mode == get_irn_mode(b)) {
			ir_node *op_a = get_Conv_op(a);
			ir_node *op_b = get_Conv_op(b);

			/* check if it's allowed to skip the conv */
			ir_mode *ma = get_irn_mode(op_a);
			if (mode_is_reference(ma) && ma == get_irn_mode(op_b)) {
				unsigned const mode_size = get_mode_size_bits(mode);
				unsigned const ma_size   = get_mode_size_bits(ma);
				if (ma_size == mode_size) {
					/* SubInt(ConvInt(aP), ConvInt(bP)) -> SubInt(aP,bP) */
					dbg_info *const dbgi  = get_irn_dbg_info(n);
					ir_node  *const block = get_nodes_block(n);
					ir_node  *const new_sub
						= new_rd_Sub(dbgi, block, op_a, op_b);
					ir_node  *const conv
						= new_rd_Conv(dbgi, block, new_sub, mode);
					return conv;
				}
			}
		}
	}
	if (is_Mul(a)) {
		ir_node *const x = b;
		ir_node *const y = get_commutative_other_op(a, x);
		if (y) {
			/* (x * y) - x -> x * (y - 1) */
			dbg_info *const dbg = get_irn_dbg_info(n);
			ir_node  *const blk = get_nodes_block(n);
			ir_graph *const irg = get_irn_irg(n);
			ir_node  *const one = new_r_Const_one(irg, mode);
			ir_node  *const sub = new_rd_Sub(dbg, blk, y, one);
			n = new_rd_Mul(dbg, blk, x, sub);
			DBG_OPT_ALGSIM0(oldn, n);
			return n;
		}
	}
	if (is_Sub(a)) { /* (x - y) - b -> x - (y + b) */
		ir_node *y   = get_Sub_right(a);
		ir_node *x   = get_Sub_left(a);
		ir_node *blk = get_nodes_block(n);
		ir_node *add = new_r_Add(blk, y, b);

		n = new_rd_Sub(get_irn_dbg_info(n), blk, x, add);
		DBG_OPT_ALGSIM0(oldn, n);
		return n;
	}

	if (get_mode_arithmetic(mode) == irma_twos_complement) {
		if (is_Const(a)) {
			ir_tarval *const tv = get_Const_tarval(a);

			if (is_Not(b)) {
				/* c - ~X = X + (c+1) */
				ir_tarval *const add = tarval_add(tv, get_mode_one(mode));
				if (tarval_is_constant(add)) {
					ir_node  *blk = get_nodes_block(n);
					ir_graph *irg = get_irn_irg(n);
					ir_node  *c   = new_r_Const(irg, add);
					n = new_rd_Add(get_irn_dbg_info(n), blk, get_Not_op(b), c);
					DBG_OPT_ALGSIM0(oldn, n);
					return n;
				}
			} else if (is_Or(b)) {
				ir_node *other = get_commutative_other_op(b, a);
				if (other) {
					/* C - (x | C) => -(x & ~C) */
					dbg_info  *const dbgi   = get_irn_dbg_info(n);
					ir_node   *const block  = get_nodes_block(n);
					ir_graph  *const irg    = get_irn_irg(n);
					ir_tarval *const tv_not = tarval_not(tv);
					ir_node   *const not_c  = new_rd_Const(dbgi, irg, tv_not);
					ir_node   *const and    = new_rd_And(dbgi, block, other, not_c);
					ir_node   *const minus  = new_rd_Minus(dbgi, block, and);
					return minus;
				}
			}
		}
		/* ~x - y = ~(x + y) */
		if (is_Not(a)) {
			dbg_info *dbgi  = get_irn_dbg_info(n);
			ir_node  *block = get_nodes_block(n);
			ir_node  *add   = new_rd_Add(dbgi, block, get_Not_op(a), b);
			ir_node  *not   = new_rd_Not(dbgi, block, add);
			return not;
		}
		/* (x & 0x7F...F) - x -> x & 0x80...0 */
		if (is_And(a) && get_And_left(a) == b) {
			ir_node *ab = get_And_right(a);
			if (is_Const(ab)) {
				ir_tarval *tv       = get_Const_tarval(ab);
				ir_tarval *all_one  = get_mode_all_one(mode);
				ir_tarval *expected = tarval_shr_unsigned(all_one, 1);
				if (tv == expected) {
					dbg_info  *const dbgi   = get_irn_dbg_info(n);
					ir_node   *const block  = get_nodes_block(n);
					ir_graph  *const irg    = get_irn_irg(n);
					ir_tarval *const tv_not = tarval_not(tv);
					ir_node   *const not_c  = new_rd_Const(dbgi, irg, tv_not);
					return new_rd_And(dbgi, block, b, not_c);
				}
			}
		}
		if (is_And(b)) {
			ir_node *const y = get_commutative_other_op(b, a);
			if (y) {
				/* a - (a & y) = a & ~y */
				dbg_info *dbgi  = get_irn_dbg_info(n);
				ir_node  *block = get_nodes_block(n);
				ir_node  *notn  = new_rd_Not(dbgi, block, y);
				ir_node  *andn  = new_rd_And(dbgi, block, a, notn);
				return andn;
			}
		}
		if (is_Conv(a) && mode_is_int(mode)) {
			ir_node *const conv_op  = get_Conv_op(a);
			ir_mode *const ref_mode = get_irn_mode(conv_op);
			if (mode_is_reference(ref_mode)
			 && get_mode_size_bits(ref_mode) == get_mode_size_bits(mode)) {
				/* SubI(ConvI(op_P), b) -> ConvI(SubP(op_P, ConvI(b)))
				 * Note: The Convs are all same size and no-ops. */
				dbg_info *const dbgi  = get_irn_dbg_info(n);
				ir_node  *const block = get_nodes_block(n);
				ir_mode  *const offset_mode
					= get_reference_offset_mode(ref_mode);
				ir_node  *const conv_b  = new_r_Conv(block, b,
				                                     offset_mode);
				ir_node  *const new_sub = new_rd_Sub(dbgi, block, conv_op, conv_b);
				ir_node  *const conv = new_r_Conv(block, new_sub, mode);
				return conv;
			}
		}
	}
	return n;
}

/**
 * Returns a negated version of @p node if it is possible to get one
 * without any additional operations.
 */
static ir_node *can_negate_cheaply(dbg_info *const dbgi, ir_node *const node)
{
	/* -C => eval(-C) */
	if (is_Const(node)) {
		ir_graph  *irg   = get_irn_irg(node);
		ir_tarval *tv    = get_Const_tarval(node);
		ir_tarval *negtv = tarval_neg(tv);
		return new_r_Const(irg, negtv);
	}
	/* -(-X) => X */
	if (is_Minus(node))
		return get_Minus_op(node);
	/* -(a-b) => b-a */
	ir_mode  *mode  = get_irn_mode(node);
	if (is_Sub(node)) {
		ir_node  *block = get_nodes_block(node);
		ir_node  *left  = get_Sub_left(node);
		ir_node  *right = get_Sub_right(node);
		return new_rd_Sub(dbgi, block, right, left);
	}
	/* -(a*const) => a * -const */
	if (is_Mul(node)) {
		ir_node *right = get_Mul_right(node);
		if (is_Const(right)) {
			ir_node   *left  = get_Mul_left(node);
			ir_tarval *tv    = get_Const_tarval(right);
			ir_tarval *negtv = tarval_neg(tv);
			ir_graph  *irg   = get_irn_irg(node);
			ir_node   *newc  = new_r_Const(irg, negtv);
			ir_node   *block = get_nodes_block(node);
			return new_rd_Mul(dbgi, block, left, newc);
		}
	}
	/* -(a + C) => -C - a */
	if (is_Add(node)) {
		ir_node *right = get_Add_right(node);
		if (is_Const(right)) {
			ir_node   *left  = get_Add_left(node);
			ir_tarval *tv    = get_Const_tarval(right);
			ir_tarval *negtv = tarval_neg(tv);
			ir_graph  *irg   = get_irn_irg(node);
			ir_node   *newc  = new_r_Const(irg, negtv);
			ir_node   *block = get_nodes_block(node);
			return new_rd_Sub(dbgi, block, newc, left);
		}
	}
	/* -(~x) => x+1 */
	if (is_Not(node)) {
		assert(get_mode_arithmetic(mode) == irma_twos_complement);
		ir_graph  *irg   = get_irn_irg(node);
		ir_node   *c1    = new_r_Const_one(irg, mode);
		ir_node   *block = get_nodes_block(node);
		ir_node   *op    = get_Not_op(node);
		return new_rd_Add(dbgi, block, op, c1);
	}
	/* -(a >>u (size-1)) = a >>s (size-1)
	 * -(a >>s (size-1)) = a >>u (size-1) */
	if (is_Shr(node)) {
		assert(get_mode_arithmetic(mode) == irma_twos_complement);
		ir_node *right = get_Shr_right(node);
		if (is_size_minus_1(right, mode)) {
			ir_node *const left  = get_Shr_left(node);
			ir_node *const block = get_nodes_block(node);
			return new_rd_Shrs(dbgi, block, left, right);
		}
	} else if (is_Shrs(node)) {
		assert(get_mode_arithmetic(mode) == irma_twos_complement);
		ir_node *right = get_Shrs_right(node);
		if (is_size_minus_1(right, mode)) {
			ir_node *const left  = get_Shrs_left(node);
			ir_node *const block = get_nodes_block(node);
			return new_rd_Shr(dbgi, block, left, right);
		}
	}

	/* we don't know a way to negate the node cheaply */
	return NULL;
}

static bool is_zero_or_all_one(ir_node const *const n)
{
	if (is_Shrs(n)) {
		ir_node *const r = get_Shrs_right(n);
		if (is_size_minus_1(r, get_irn_mode(n)))
			return true;
	}
	return false;
}

static ir_node *transform_node_shl_shr(ir_node *n);

static ir_node *transform_node_Mul(ir_node *n)
{
	ir_node *oldn = n;

	n = fold_constant_associativity(n, tarval_mul);
	if (n != oldn)
		return n;

	ir_mode *mode = get_irn_mode(n);
	ir_node *a    = get_Mul_left(n);
	ir_node *b    = get_Mul_right(n);
	ir_node *c;
	HANDLE_BINOP_CHOICE(tarval_mul, a, b, c, mode);

	ir_mode_arithmetic arith = get_mode_arithmetic(mode);
	if (is_Const(b)) {
		ir_tarval *c1 = get_Const_tarval(b);

		/* Normalize (x+C1) * C2 => x*C2 + (C1*C2),
		 * but only do this if we can be sure that the add has only 1 user */
		if (is_Add(a) && only_one_user(a)) {
			ir_node *add_right = get_Add_right(a);
			if (is_Const(add_right) && arith == irma_twos_complement) {
				ir_tarval *c2       = get_Const_tarval(add_right);
				ir_node   *add_left = get_Add_left(a);

				dbg_info  *dbgi  = get_irn_dbg_info(n);
				ir_node   *block = get_nodes_block(n);
				ir_node   *mul   = new_rd_Mul(dbgi, block, add_left, b);
				ir_tarval *c1_c2 = tarval_mul(c1, c2);
				ir_graph  *irg   = get_irn_irg(n);
				ir_node   *cnst  = new_r_Const(irg, c1_c2);
				ir_node   *add   = new_rd_Add(dbgi, block, mul, cnst);
				return add;
			}
		}

		/* x*-1 => -x */
		if ((arith == irma_twos_complement && tarval_is_all_one(c1))
		  || (mode_is_float(mode) && tarval_is_minus_one(c1))) {
			dbg_info *dbgi  = get_irn_dbg_info(n);
			ir_node  *block = get_nodes_block(n);
			return new_rd_Minus(dbgi, block, a);
		} else if (arith == irma_twos_complement
		        && get_tarval_popcount(c1) == 1) {
			/* multiplication behaves Shl-like */
			n = transform_node_shl_shr(n);
			if (n != oldn)
				return n;
		}
	}

	if (mode_is_int(mode)) {
		bitinfo *ba    = get_bitinfo(a);
		bitinfo *bb    = get_bitinfo(b);
		ir_node *bit   = NULL;
		ir_node *other = NULL;
		if (ba && tarval_is_one(ba->z)) {
			bit   = a;
			other = b;
		} else if (bb && tarval_is_one(bb->z)) {
			bit   = b;
			other = a;
		}

		if (bit) {
			/* a * (b & 1) -> a & -(b & 1) */
			dbg_info *dbgi  = get_irn_dbg_info(n);
			ir_node  *block = get_nodes_block(n);
			ir_node  *minus = new_rd_Minus(dbgi, block, bit);
			return new_rd_And(dbgi, block, other, minus);
		}

		if (is_zero_or_all_one(a) || is_zero_or_all_one(b)) {
			/* a * b [-1 <= b <= 0] -> -(a & b) */
			dbg_info *const dbgi  = get_irn_dbg_info(n);
			ir_node  *const block = get_nodes_block(n);
			ir_node  *const and   = new_rd_And(dbgi, block, a, b);
			return new_rd_Minus(dbgi, block, and);
		}
	}

	/* distribute minus:
	 * -x * y => x * (-y) if -y can be computed cheaply
	 */
	if (is_Minus(a)) {
		ir_node *neg_b = can_negate_cheaply(NULL, b);
		if (neg_b != NULL) {
			dbg_info *dbgi  = get_irn_dbg_info(n);
			ir_node  *block = get_nodes_block(n);
			return new_rd_Mul(dbgi, block, get_Minus_op(a), neg_b);
		}
	}
	/* distribute minus: x * -y => -x * y */
	if (is_Minus(b)) {
		ir_node *neg_a = can_negate_cheaply(NULL, a);
		if (neg_a != NULL) {
			dbg_info *dbgi  = get_irn_dbg_info(n);
			ir_node  *block = get_nodes_block(n);
			return new_rd_Mul(dbgi, block, neg_a, get_Minus_op(b));
		}
	}
	if (is_Shl(a)) {
		ir_node *const shl_l = get_Shl_left(a);
		if (is_irn_one(shl_l)) {
			/* (1 << x) * b -> b << x */
			dbg_info *const dbgi  = get_irn_dbg_info(n);
			ir_node  *const block = get_nodes_block(n);
			ir_node  *const shl_r = get_Shl_right(a);
			n = new_rd_Shl(dbgi, block, b, shl_r);
			return n;
		}
	} else if (is_Shl(b)) {
		ir_node *const shl_l = get_Shl_left(b);
		if (is_irn_one(shl_l)) {
			/* a * (1 << x) -> a << x */
			dbg_info *const dbgi  = get_irn_dbg_info(n);
			ir_node  *const block = get_nodes_block(n);
			ir_node  *const shl_r = get_Shl_right(b);
			n = new_rd_Shl(dbgi, block, a, shl_r);
			return n;
		}
	}
	ir_graph *irg = get_irn_irg(n);
	if ((arith == irma_ieee754 || arith == irma_x86_extended_float)
	    && irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_ARCH_DEP)) {
		if (is_Const(a)) {
			const ir_tarval *tv = get_Const_tarval(a);
			if (tarval_get_exponent(tv) == 1 && tarval_zero_mantissa(tv)
			    && !tarval_is_negative(tv)) {
				/* 2.0 * b = b + b */
				n = new_rd_Add(get_irn_dbg_info(n), get_nodes_block(n), b, b);
				DBG_OPT_ALGSIM1(oldn, a, b, n);
				return n;
			}
		} else if (is_Const(b)) {
			const ir_tarval *tv = get_Const_tarval(b);
			if (tarval_get_exponent(tv) == 1 && tarval_zero_mantissa(tv)
			    && !tarval_is_negative(tv)) {
				/* a * 2.0 = a + a */
				n = new_rd_Add(get_irn_dbg_info(n), get_nodes_block(n), a, a);
				DBG_OPT_ALGSIM1(oldn, a, b, n);
				return n;
			}
		}
	}
	/* (a/b)*b == a-(a%b) */
	if (arith == irma_twos_complement) {
		ir_node *div;
		ir_node *div_proj;
		ir_node *div_right;
		if (is_Proj(a)) {
			div_proj = a;
			div      = get_Proj_pred(div_proj);
			if (is_Div(div)) {
				div_right = get_Div_right(div);
				if (div_right == b)
					goto transform_div_mul;
			}
		}
		if (is_Proj(b)) {
			div_proj = b;
			div      = get_Proj_pred(div_proj);
			if (is_Div(div)) {
				div_right = get_Div_right(div);
				if (div_right == a) {
transform_div_mul:
					if (only_one_user(div_proj)) {
						dbg_info    *dbgi      = get_irn_dbg_info(n);
						ir_node     *div_block = get_nodes_block(div);
						ir_node     *div_left  = get_Div_left(div);
						ir_node     *div_mem   = get_Div_mem(div);
						op_pin_state pinned    = get_irn_pinned(div);
						ir_node     *mod       = new_rd_Mod(dbgi, div_block, div_mem, div_left, div_right, pinned);
						/* we are crazy and exchange the Div with the Mod. We
						 * can only do this because: We know we are the only
						 * user and the Proj numbers match up */
						assert((int)pn_Div_res       == (int)pn_Mod_res);
						assert((int)pn_Div_M         == (int)pn_Mod_M);
						assert((int)pn_Div_X_regular == (int)pn_Mod_X_regular);
						assert((int)pn_Div_X_except  == (int)pn_Mod_X_except);
						exchange(div, mod);
						ir_node  *block = get_nodes_block(n);
						return new_rd_Sub(dbgi, block, div_left, div_proj);
					}
				}
			}
		}
	}

	return arch_dep_replace_mul_with_shifts(n);
}

/**
 * Transform a Div Node.
 */
static ir_node *transform_node_Div(ir_node *n)
{
	ir_mode *mode  = get_Div_resmode(n);
	ir_node *a     = get_Div_left(n);
	ir_node *b     = get_Div_right(n);
	ir_node *value = n;

	if (mode_is_int(mode)) {
		if (is_Const(b) && is_const_Phi(a)) {
			/* check for Div(Phi, Const) */
			value = apply_binop_on_phi(a, get_Const_tarval(b), tarval_div, mode, 0);
			if (value) {
				DBG_OPT_ALGSIM0(n, value);
				goto make_tuple;
			}
		} else if (is_Const(a) && is_const_Phi(b)) {
			/* check for Div(Const, Phi) */
			value = apply_binop_on_phi(b, get_Const_tarval(a), tarval_div, mode, 1);
			if (value) {
				DBG_OPT_ALGSIM0(n, value);
				goto make_tuple;
			}
		} else if (is_const_Phi(a) && is_const_Phi(b)) {
			/* check for Div(Phi, Phi) */
			value = apply_binop_on_2_phis(a, b, tarval_div, mode);
			if (value) {
				DBG_OPT_ALGSIM0(n, value);
				goto make_tuple;
			}
		}

		ir_tarval *tv = do_computed_value_Div(a, b);
		/* constant folding possible -> create tuple */
		if (tarval_is_constant(tv)) {
			ir_graph *irg = get_irn_irg(n);
			value = new_r_Const(irg, tv);
			DBG_OPT_CSTEVAL(n, value);
			goto make_tuple;
		} else {
			if (mode_is_signed(mode) && is_Const(b)) {
				ir_tarval *tv = get_Const_tarval(b);

				if (tarval_is_all_one(tv)) {
					/* a / -1 */
					value = new_rd_Minus(get_irn_dbg_info(n), get_nodes_block(n), a);
					DBG_OPT_CSTEVAL(n, value);
					goto make_tuple;
				}
			}
			/* Try architecture dependent optimization */
			value = arch_dep_replace_div_by_const(n);
		}
	} else {
		assert(mode_is_float(mode));

		/* Optimize x/c to x*(1/c) */
		ir_tarval *tv = value_of(b);

		if (tarval_is_constant(tv)) {
			tv = tarval_div(get_mode_one(mode), tv);

			/* Do the transformation if the result is either exact or we are
			   not using strict rules. */
			if (tarval_is_constant(tv)
			    && (tarval_ieee754_get_exact()
				    || ir_imprecise_float_transforms_allowed())) {
				ir_node  *block = get_nodes_block(n);
				ir_graph *irg   = get_irn_irg(block);
				ir_node  *c     = new_r_Const(irg, tv);
				dbg_info *dbgi  = get_irn_dbg_info(n);
				value = new_rd_Mul(dbgi, block, a, c);

				goto make_tuple;
			}
		}
	}

	if (value != n) {
make_tuple:;
		/* Turn Div into a tuple (mem, jmp, bad, value) */
		ir_node  *mem = get_Div_mem(n);
		ir_node  *blk = get_nodes_block(n);
		ir_graph *irg = get_irn_irg(blk);

		/* skip a potential Pin */
		mem = skip_Pin(mem);
		ir_node *in[pn_Div_max+1] = {
			[pn_Div_M]         = mem,
			[pn_Div_res]       = value,
		};
		int n_in = 2;
		assert(pn_Div_M == 0 && pn_Div_res == 1);
		if (ir_throws_exception(n)) {
			in[pn_Div_X_regular] = new_r_Jmp(blk);
			in[pn_Div_X_except]  = new_r_Bad(irg, mode_X);
			n_in = 4;
		}
		turn_into_tuple(n, n_in, in);
	}
	return n;
}

/**
 * Transform a Mod node.
 */
static ir_node *transform_node_Mod(ir_node *n)
{
	ir_mode *mode  = get_Mod_resmode(n);
	ir_node *a     = get_Mod_left(n);
	ir_node *b     = get_Mod_right(n);
	ir_node *value;

	if (is_Const(b) && is_const_Phi(a)) {
		/* check for Div(Phi, Const) */
		value = apply_binop_on_phi(a, get_Const_tarval(b), tarval_mod, mode, 0);
		if (value) {
			DBG_OPT_ALGSIM0(n, value);
			goto make_tuple;
		}
	} else if (is_Const(a) && is_const_Phi(b)) {
		/* check for Div(Const, Phi) */
		value = apply_binop_on_phi(b, get_Const_tarval(a), tarval_mod, mode, 1);
		if (value) {
			DBG_OPT_ALGSIM0(n, value);
			goto make_tuple;
		}
	} else if (is_const_Phi(a) && is_const_Phi(b)) {
		/* check for Div(Phi, Phi) */
		value = apply_binop_on_2_phis(a, b, tarval_mod, mode);
		if (value) {
			DBG_OPT_ALGSIM0(n, value);
			goto make_tuple;
		}
	}

	ir_tarval *tv  = do_computed_value_Mod(a, b);
	ir_graph  *irg = get_irn_irg(n);
	if (tarval_is_constant(tv)) {
		value = new_r_Const(irg, tv);

		DBG_OPT_CSTEVAL(n, value);
		goto make_tuple;
	}

	if (is_Const(b)) {
		ir_tarval *tv = get_Const_tarval(b);
		assert(get_mode_arithmetic(mode) == irma_twos_complement);

		if (is_Proj(a) && only_one_user(a)) {
			ir_node *mod = get_Proj_pred(a);
			if (is_Mod(mod)) {
				ir_node *mod_b = get_Mod_right(mod);
				if (is_Const(mod_b)) {
					ir_tarval *tv_mod = get_Const_tarval(mod_b);
					if (tarval_is_null(tarval_mod(tv_mod, tv))) {
						/* (a % c1) % c2 -> a % c2 */
						ir_node  *mod_a  = get_Mod_left(mod);
						dbg_info *dbgi   = get_irn_dbg_info(n);
						ir_node  *block  = get_nodes_block(n);
						ir_node  *mem    = get_Mod_mem(mod);
						int       pinned = get_irn_pinned(n);
						return new_rd_Mod(dbgi, block, mem, mod_a, b, pinned);
					}
				}
			}
		}

		bitinfo *ba = get_bitinfo(a);
		if (ba != NULL) {
			ir_tarval *const baz  = ba->z;
			ir_tarval *const bao  = ba->o;
			ir_tarval *const divz = tarval_div(baz, tv);
			ir_tarval *const divo = tarval_div(bao, tv);
			ir_tarval *const min  = get_mode_min(mode);
			ir_tarval *const beor = tarval_eor(baz, bao);
			ir_tarval *const sign = tarval_and(beor, min);

			if (divz == divo && tarval_is_constant(divz) && tarval_is_null(sign)) {
				/* a/b and b are constant, so use equation a % b = a - (a/b)*b */
				ir_tarval *tv_mul = tarval_mul(divz, tv);
				dbg_info  *dbgi   = get_irn_dbg_info(n);
				ir_node   *c      = new_rd_Const(dbgi, irg, tv_mul);
				ir_node   *block  = get_nodes_block(n);

				value = new_rd_Sub(dbgi, block, a, c);
				goto make_tuple;
			}
		}
	}

	/* Try architecture dependent optimization */
	value = arch_dep_replace_mod_by_const(n);

	if (value != n) {
make_tuple:;
		/* Turn Mod into a tuple (mem, jmp, bad, value) */
		ir_node  *mem = get_Mod_mem(n);
		ir_node  *blk = get_nodes_block(n);
		ir_graph *irg = get_irn_irg(blk);

		/* skip a potential Pin */
		mem = skip_Pin(mem);
		ir_node *in[pn_Mod_max+1] = {
			[pn_Mod_M]         = mem,
			[pn_Mod_res]       = value
		};
		int n_in = 2;
		assert(pn_Mod_M == 0 && pn_Mod_res == 1);
		if (ir_throws_exception(n)) {
			in[pn_Mod_X_regular] = new_r_Jmp(blk);
			in[pn_Mod_X_except]  = new_r_Bad(irg, mode_X);
			n_in = 4;
		}
		turn_into_tuple(n, n_in, in);
	}
	return n;
}

/**
 * Transform a Cond node.
 *
 * Replace the Cond by a Jmp if it branches on a constant
 * condition.
 */
static ir_node *transform_node_Cond(ir_node *n)
{
	ir_graph *irg = get_irn_irg(n);

	/* we need block info which is not available in floating irgs */
	if (get_irg_pinned(irg) == op_pin_state_floats)
		return n;

	ir_node   *a  = get_Cond_selector(n);
	ir_tarval *ta = value_of(a);
	/* try again with a direct call to compute_cmp, as we don't care
	 * about the MODEB_LOWERED flag here */
	if (!tarval_is_constant(ta) && is_Cmp(a))
		ta = compute_cmp_ext(a);

	if (tarval_is_constant(ta)) {
		/* It's branching on a boolean constant.
		   Replace it by a tuple (Bad, Jmp) or (Jmp, Bad) */
		ir_node  *const blk  = get_nodes_block(n);
		dbg_info *const dbgi = get_irn_dbg_info(n);
		ir_node  *const jmp  = new_rd_Jmp(dbgi, blk);
		ir_node  *const bad  = new_r_Bad(irg, mode_X);
		bool      const cond = ta == tarval_b_true;
		ir_node  *const in[] = {
			[pn_Cond_false] = cond ? bad : jmp,
			[pn_Cond_true]  = cond ? jmp : bad,
		};
		turn_into_tuple(n, ARRAY_SIZE(in), in);

		clear_irg_properties(irg, IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE
		                        | IR_GRAPH_PROPERTY_NO_BADS
		                        | IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE
		                        | IR_GRAPH_PROPERTY_CONSISTENT_POSTDOMINANCE
		                        | IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE_FRONTIERS
		                        | IR_GRAPH_PROPERTY_CONSISTENT_OUTS);
	}
	return n;
}

/**
 * Transforms a Confirm node.
 *
 * This sharpens the relation of a Confirm node.
 */
static ir_node *transform_node_Confirm(ir_node *n)
{
	ir_node     *value    = get_Confirm_value(n);
	ir_node     *bound    = get_Confirm_bound(n);
	ir_relation  relation = get_Confirm_relation(n);
	ir_relation  possible = ir_get_possible_cmp_relations(value, bound);

	/* mask out impossible relations */
	ir_relation new_relation = relation & possible;
	if (new_relation != relation) {
		dbg_info *dbgi  = get_irn_dbg_info(n);
		ir_node  *block = get_nodes_block(n);
		return new_rd_Confirm(dbgi, block, value, bound, new_relation);
	}

	return n;
}

static ir_node *transform_node_Switch(ir_node *n)
{
	unsigned n_outs = get_Switch_n_outs(n);
	/* switch with just default proj is a Jmp */
	if (n_outs == 1) {
		dbg_info *dbgi  = get_irn_dbg_info(n);
		ir_node  *block = get_nodes_block(n);
		ir_node  *in[]  = { new_rd_Jmp(dbgi, block) };
		return new_r_Tuple(block, ARRAY_SIZE(in), in);
	}

	/* evaluate constant switch input */
	ir_node         *op  = get_Switch_selector(n);
	const ir_tarval *val = value_of(op);
	if (tarval_is_constant(val)) {
		const ir_switch_table *table     = get_Switch_table(n);
		size_t                 n_entries = ir_switch_table_get_n_entries(table);
		unsigned               jmp_pn    = 0;
		for (size_t i = 0; i < n_entries; ++i) {
			const ir_switch_table_entry *entry
				= ir_switch_table_get_entry_const(table, i);
			if (entry->pn == 0)
				continue;

			if (tarval_in_range(entry->min, val, entry->max)) {
			    jmp_pn = entry->pn;
			    break;
			}
		}

		dbg_info  *dbgi   = get_irn_dbg_info(n);
		ir_graph  *irg    = get_irn_irg(n);
		ir_node   *block  = get_nodes_block(n);
		ir_node   *bad    = new_r_Bad(irg, mode_X);
		ir_node  **in     = XMALLOCN(ir_node*, n_outs);
		for (unsigned o = 0; o < n_outs; ++o) {
			if (o == jmp_pn) {
				in[o] = new_rd_Jmp(dbgi, block);
			} else {
				in[o] = bad;
			}
		}
		return new_r_Tuple(block, (int)n_outs, in);
	}
	return n;
}

/**
 * Normalization: (x & c1) >> c2   to   (x >> c2) & (c1 >> c2)
 *  (we can use:
 *    - and, or, xor          instead of &
 *    - Shl, Shr, Shrs, rotl  instead of >>
 *    (with a special case for Or/Xor + Shrs)
 *
 * This normalization is good for things like x-(x&y) esp. in 186.crafty.
 */
static ir_node *transform_node_shift_bitop(ir_node *n)
{
	ir_graph  *irg   = get_irn_irg(n);

	if (irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_NORMALISATION2))
		return n;

	assert(is_shiftop(n));

	ir_node *right = get_binop_right(n);
	if (!is_Const(right))
		return n;

	ir_node *left    = get_binop_left(n);
	ir_op   *op_left = get_irn_op(left);
	if (op_left != op_And && op_left != op_Or && op_left != op_Eor)
		return n;

	/* doing it with Shrs is not legal if the Or/Eor affects the topmost bit */
	/* TODO: test if sign bit is affected */
	if (is_Shrs(n) && (op_left == op_Or || op_left == op_Eor))
		return n;

	ir_node *bitop_right = get_binop_right(left);
	if (!is_Const(bitop_right))
		return n;

	ir_node   *bitop_left = get_binop_left(left);
	ir_node   *block      = get_nodes_block(n);
	dbg_info  *dbgi       = get_irn_dbg_info(n);
	ir_tarval *tv1        = get_Const_tarval(bitop_right);
	ir_tarval *tv2        = get_Const_tarval(right);

	assert(get_tarval_mode(tv1) == get_irn_mode(n));

	ir_node   *new_shift;
	ir_tarval *tv_shift;
	ir_tarval *shl_like = is_shl_const_like(n);
	if (shl_like != NULL) {
		tv2       = shl_like;
		new_shift = new_rd_Shl(dbgi, block, bitop_left, right);
		tv_shift  = tarval_shl(tv1, tv2);
	} else if (is_Shr(n)) {
		new_shift = new_rd_Shr(dbgi, block, bitop_left, right);
		tv_shift  = tarval_shr(tv1, tv2);
	} else {
		assert(is_Shrs(n));
		new_shift = new_rd_Shrs(dbgi, block, bitop_left, right);
		tv_shift  = tarval_shrs(tv1, tv2);
	}

	assert(get_tarval_mode(tv_shift) == get_irn_mode(n));

	ir_node *new_const = new_r_Const(irg, tv_shift);
	ir_node *new_bitop;
	if (op_left == op_And) {
		new_bitop = new_rd_And(dbgi, block, new_shift, new_const);
	} else if (op_left == op_Or) {
		new_bitop = new_rd_Or(dbgi, block, new_shift, new_const);
	} else {
		assert(op_left == op_Eor);
		new_bitop = new_rd_Eor(dbgi, block, new_shift, new_const);
	}

	return new_bitop;
}

/**
 * Transform an And.
 */
static ir_node *transform_node_And(ir_node *n)
{
	ir_node *oldn = n;

	n = fold_constant_associativity(n, tarval_and);
	if (n != oldn)
		return n;

	ir_node *a = get_And_left(n);
	ir_node *b = get_And_right(n);
	if (is_Cmp(a) && is_Cmp(b)) {
		ir_node    *a_left     = get_Cmp_left(a);
		ir_node    *a_right    = get_Cmp_right(a);
		ir_node    *b_left     = get_Cmp_left(b);
		ir_node    *b_right    = get_Cmp_right(b);
		ir_relation a_relation = get_Cmp_relation(a);
		ir_relation b_relation = get_Cmp_relation(b);
		/* we can combine the relations of two compares with the same
		 * operands */
		if (a_left == b_left && b_left == b_right) {
			dbg_info   *dbgi         = get_irn_dbg_info(n);
			ir_node    *block        = get_nodes_block(n);
			ir_relation new_relation = a_relation & b_relation;
			return new_rd_Cmp(dbgi, block, a_left, a_right, new_relation);
		}

		ir_relation a_possible = ir_get_possible_cmp_relations(a_left, a_right);
		ir_relation b_possible = ir_get_possible_cmp_relations(b_left, b_right);

		/* Cmp(a==b) and Cmp(c==d) can be optimized to Cmp((a^b)|(c^d)==0) */
		const ir_mode *cmp_mode_left  = get_irn_mode(a_left);
		const ir_mode *cmp_mode_right = get_irn_mode(b_left);
		if (is_relation(ir_relation_equal, a_relation, a_possible)
		    && is_relation(ir_relation_equal, b_relation, b_possible)
		    && !mode_is_float(cmp_mode_left)
		    && !mode_is_float(cmp_mode_right)) {
			if (mode_is_reference(cmp_mode_left)) {
				ir_mode  *omode = get_reference_offset_mode(cmp_mode_left);
				dbg_info *dbgi  = get_irn_dbg_info(a_left);
				ir_node  *block = get_nodes_block(n);
				a_left  = new_rd_Conv(dbgi, block, a_left,  omode);
				a_right = new_rd_Conv(dbgi, block, a_right, omode);
			}
			if (mode_is_reference(cmp_mode_right)) {
				ir_mode  *omode = get_reference_offset_mode(cmp_mode_right);
				dbg_info *dbgi  = get_irn_dbg_info(b_left);
				ir_node  *block = get_nodes_block(n);
				b_left  = new_rd_Conv(dbgi, block, b_left,  omode);
				b_right = new_rd_Conv(dbgi, block, b_right, omode);
			}
			ir_mode *a_mode = get_irn_mode(a_left);
			ir_mode *b_mode = get_irn_mode(b_left);
			if (values_in_mode(a_mode, b_mode)) {
				dbg_info *dbgi  = get_irn_dbg_info(n);
				ir_node  *block = get_nodes_block(n);
				ir_node  *xora  = new_rd_Eor(dbgi, block, a_left, a_right);
				ir_node  *xorb  = new_rd_Eor(dbgi, block, b_left, b_right);
				ir_node  *conv  = new_rd_Conv(dbgi, block, xora, b_mode);
				ir_node  *orn   = new_rd_Or(dbgi, block, conv, xorb);
				ir_graph *irg   = get_irn_irg(n);
				ir_node  *zero  = new_r_Const_null(irg, b_mode);
				return new_rd_Cmp(dbgi, block, orn, zero, ir_relation_equal);
			}
			if (values_in_mode(b_mode, a_mode)) {
				dbg_info *dbgi  = get_irn_dbg_info(n);
				ir_node  *block = get_nodes_block(n);
				ir_node  *xora  = new_rd_Eor(dbgi, block, a_left, a_right);
				ir_node  *xorb  = new_rd_Eor(dbgi, block, b_left, b_right);
				ir_node  *conv  = new_rd_Conv(dbgi, block, xorb, a_mode);
				ir_node  *orn   = new_rd_Or(dbgi, block, xora, conv);
				ir_graph *irg   = get_irn_irg(n);
				ir_node  *zero  = new_r_Const_null(irg, a_mode);
				return new_rd_Cmp(dbgi, block, orn, zero, ir_relation_equal);
			}
		}
	}

	ir_node *c;
	ir_mode *mode = get_irn_mode(n);
	HANDLE_BINOP_CHOICE(tarval_and, a, b, c, mode);

	if (is_Or(a) || is_Or_Eor_Add(a)) {
		ir_node *or_left  = get_binop_left(a);
		ir_node *or_right = get_binop_right(a);
		if (complement_values(or_left, b)) {
			/* (a|b) & ~a => b & ~a */
			dbg_info *dbgi    = get_irn_dbg_info(n);
			ir_node  *block   = get_nodes_block(n);
			return new_rd_And(dbgi, block, or_right, b);
		} else if (complement_values(or_right, b)) {
			/* (a|b) & ~b => a & ~b */
			dbg_info *dbgi    = get_irn_dbg_info(n);
			ir_node  *block   = get_nodes_block(n);
			return new_rd_And(dbgi, block, or_left, b);
		} else if (is_Not(b)) {
			ir_node *op = get_Not_op(b);
			if (is_And(op)) {
				ir_node *ba = get_And_left(op);
				ir_node *bb = get_And_right(op);

				/* it's enough to test the following cases due to normalization! */
				if (or_left == ba && or_right == bb) {
					/* (a|b) & ~(a&b) = a^b */
					ir_node *block = get_nodes_block(n);

					n = new_rd_Eor(get_irn_dbg_info(n), block, ba, bb);
					DBG_OPT_ALGSIM1(oldn, a, b, n);
					return n;
				}
			}
		}
	}
	if (is_Or(b) || is_Or_Eor_Add(b)) {
		ir_node *or_left  = get_binop_left(b);
		ir_node *or_right = get_binop_right(b);
		if (complement_values(or_left, a)) {
			/* (a|b) & ~a => b & ~a */
			dbg_info *dbgi    = get_irn_dbg_info(n);
			ir_node  *block   = get_nodes_block(n);
			return new_rd_And(dbgi, block, or_right, a);
		} else if (complement_values(or_right, a)) {
			/* (a|b) & ~b => a & ~b */
			dbg_info *dbgi    = get_irn_dbg_info(n);
			ir_node  *block   = get_nodes_block(n);
			return new_rd_And(dbgi, block, or_left, a);
		} else if (is_Not(a)) {
			ir_node *op = get_Not_op(a);
			if (is_And(op)) {
				ir_node *aa = get_And_left(op);
				ir_node *ab = get_And_right(op);

				/* it's enough to test the following cases due to normalization! */
				if (or_left == aa && or_right == ab) {
					/* (a|b) & ~(a&b) = a^b */
					ir_node *block = get_nodes_block(n);

					n = new_rd_Eor(get_irn_dbg_info(n), block, aa, ab);
					DBG_OPT_ALGSIM1(oldn, a, b, n);
					return n;
				}
			}
		}
	}
	ir_node *x;
	ir_node *y;
	if ((is_Eor(a) || is_Eor_Add(a)) && only_one_user(a)) {
		x = b;
		y = get_commutative_other_op(a, x);
		if (y) /* (x ^ y) & x -> ~y & x */
			goto absorb;
	}
	if ((is_Eor(b) || is_Eor_Add(b)) && only_one_user(b)) {
		x = a;
		y = get_commutative_other_op(b, x);
		if (y) {
			/* x & (x ^ y) -> ~y & x */
absorb:;
			dbg_info *const dbg   = get_irn_dbg_info(n);
			ir_node  *const block = get_nodes_block(n);
			ir_node  *const noty  = new_rd_Not(dbg, block, y);
			n = new_rd_And(dbg, block, noty, x);
			DBG_OPT_ALGSIM0(oldn, n);
			return n;
		}
	}
	if (is_Not(a) && is_Not(b) && (only_one_user(a) || only_one_user(b))) {
		/* ~a & ~b = ~(a|b) */
		ir_node *block = get_nodes_block(n);

		a = get_Not_op(a);
		b = get_Not_op(b);
		n = new_rd_Or(get_irn_dbg_info(n), block, a, b);
		n = new_rd_Not(get_irn_dbg_info(n), block, n);
		DBG_OPT_ALGSIM0(oldn, n);
		return n;
	}

	if (is_Minus(a) && is_Const(b)
	    && get_mode_arithmetic(mode) == irma_twos_complement) {
		const bitinfo *const ba   = get_bitinfo(a);
		ir_tarval     *      mask;
		if (ba) {
			ir_tarval *const z   = ba->z;
			ir_tarval *const one = get_mode_one(mode);

			/* Create mask for rightmost may-1-bit and the trailing 0's. */
			mask = tarval_eor(z, tarval_sub(z, one));
		} else {
			mask = get_mode_one(mode);
		}

		ir_tarval *const tv = get_Const_tarval(b);
		if (tarval_and(tv, mask) == tv) {
			/* -(a=??????00) & 00000111 => a & 00000111 */
			dbg_info *const dbgi     = get_irn_dbg_info(n);
			ir_node  *const block    = get_nodes_block(n);
			ir_node  *const minus_op = get_Minus_op(a);
			return new_rd_And(dbgi, block, minus_op, b);
		}
	}

	if (is_Shrs(a) && is_Const(b) && only_one_user(a)) {
		ir_node *ab = get_Shrs_right(a);
		if (is_Const(ab)) {
			ir_tarval *tv_and       = get_Const_tarval(b);
			long       highest_bit  = get_tarval_highest_bit(tv_and);
			long       shift_amount = get_Const_long(ab);
			unsigned   bit_size     = get_mode_size_bits(mode);
			if (highest_bit + shift_amount < (long)bit_size) {
				/* (x >>s 16) & 0xFFFF => (x >> 16) & 0xFFFF */
				ir_node  *const aa         = get_Shrs_left(a);
				dbg_info *const shrs_dbgi  = get_irn_dbg_info(a);
				ir_node  *const shrs_block = get_nodes_block(a);
				ir_node  *const shr        = new_rd_Shr(shrs_dbgi, shrs_block, aa, ab);
				dbg_info *const dbgi       = get_irn_dbg_info(n);
				ir_node  *const block      = get_nodes_block(n);
				return new_rd_And(dbgi, block, shr, b);
			}
		}
	}

	n = transform_bitop_chain(n);
	if (n != oldn)
		return n;

	n = transform_bitwise_distributive(n);
	if (is_And(n))
		n = transform_node_bitop_shift(n);

	return n;
}

/**
 * Transform a Not.
 */
static ir_node *transform_node_Not(ir_node *n)
{
	ir_node *oldn = n;
	ir_node *a    = get_Not_op(n);

	ir_node *c;
	HANDLE_UNOP_CHOICE(tarval_not,a,c);

	/* check for a boolean Not */
	if (is_Cmp(a)) {
		dbg_info *dbgi  = get_irn_dbg_info(a);
		ir_node  *block = get_nodes_block(a);
		ir_relation relation = get_Cmp_relation(a);
		relation = get_negated_relation(relation);
		n = new_rd_Cmp(dbgi, block, get_Cmp_left(a), get_Cmp_right(a), relation);
		DBG_OPT_ALGSIM0(oldn, n);
		return n;
	}

	ir_mode *mode = get_irn_mode(n);

	/* Try De Morgan's laws. */
	if (is_And(a) || is_Or(a)) {
		ir_node *left  = get_binop_left(a);
		ir_node *right = get_binop_right(a);
		if (is_Not(left) && only_one_user(a)) {
			dbg_info *dbgi   = get_irn_dbg_info(n);
			ir_node  *block  = get_nodes_block(n);
			ir_node  *not_op = get_Not_op(left);
			ir_node  *not    = new_rd_Not(dbgi, block, right);
			if (is_And(a)) {
				/* ~(~a & b) => a | ~b */
				n = new_rd_Or(dbgi, block, not_op, not);
			} else {
				/* ~(~a | b) => a & ~b */
				n = new_rd_And(dbgi, block, not_op, not);
			}
			DBG_OPT_ALGSIM0(oldn, n);
			return n;
		}
		if (is_Not(right) && only_one_user(a)) {
			dbg_info *dbgi   = get_irn_dbg_info(n);
			ir_node  *block  = get_nodes_block(n);
			ir_node  *not_op = get_Not_op(right);
			ir_node  *not    = new_rd_Not(dbgi, block, left);
			if (is_And(a)) {
				/* ~(a & ~b) => ~a | b */
				n = new_rd_Or(dbgi, block, not, not_op);
			} else {
				/* ~(a | ~b) => ~a & b */
				n = new_rd_And(dbgi, block, not, not_op);
			}
			DBG_OPT_ALGSIM0(oldn, n);
			return n;
		}
	}

	/* normalize ~(a ^ b) => a ^ ~b */
	if ((is_Eor(a) || is_Or_Eor_Add(a)) && only_one_user(a)) {
		dbg_info *dbg       = get_irn_dbg_info(n);
		ir_node  *block     = get_nodes_block(n);
		ir_node  *eor_right = get_binop_right(a);
		ir_node  *eor_left  = get_binop_left(a);
		eor_right = new_rd_Not(dbg, block, eor_right);

		return new_rd_Eor(dbg, block, eor_left, eor_right);
	}

	if (get_mode_arithmetic(mode) == irma_twos_complement) {
		if (is_Minus(a)) { /* ~-x -> x + -1 */
			dbg_info *dbg   = get_irn_dbg_info(n);
			ir_graph *irg   = get_irn_irg(n);
			ir_node  *block = get_nodes_block(n);
			ir_node  *add_l = get_Minus_op(a);
			ir_node  *add_r = new_rd_Const(dbg, irg, get_mode_all_one(mode));
			return new_rd_Add(dbg, block, add_l, add_r);
		}
		if (is_Add(a) || is_Or_Eor_Add(a)) {
			ir_node   *add_r = get_binop_right(a);
			ir_tarval *tr    = value_of(add_r);

			if (tarval_is_constant(tr)) {
				ir_tarval *tv = tarval_not(tr);

				if (tarval_is_constant(tv)) {
					/* ~(x + C) = (~C) - x */
					ir_graph *irg   = get_irn_irg(n);
					ir_node  *cnst  = new_r_Const(irg, tv);
					dbg_info *dbg   = get_irn_dbg_info(a);
					ir_node  *block = get_nodes_block(a);
					ir_node  *add_l = get_binop_left(a);

					return new_rd_Sub(dbg, block, cnst, add_l);
				}
			}
		} else if (is_Sub(a)) {
			ir_node   *sub_l = get_Sub_left(a);
			ir_tarval *tr    = value_of(sub_l);

			if (tarval_is_constant(tr)) {
				ir_tarval *tv = tarval_not(tr);

				if (tarval_is_constant(tv)) {
					/* ~(C - x) = x + (~C) */
					ir_graph *irg   = get_irn_irg(n);
					ir_node  *cnst  = new_r_Const(irg, tv);
					dbg_info *dbg   = get_irn_dbg_info(a);
					ir_node  *block = get_nodes_block(a);
					ir_node  *sub_r = get_Sub_right(a);

					return new_rd_Add(dbg, block, sub_r, cnst);
				}
			}
		}
	}

	n = transform_bitop_chain(n);
	return n;
}

/**
 * Transform a Minus.
 * Optimize:
 *   -(~x) = x + 1
 *   -(a-b) = b - a
 *   -(a >>u (size-1)) = a >>s (size-1)
 *   -(a >>s (size-1)) = a >>u (size-1)
 *   -(a * const) -> a * -const
 */
static ir_node *transform_node_Minus(ir_node *n)
{
	ir_node *oldn = n;
	ir_node *op   = get_Minus_op(n);

	ir_node *c;
	HANDLE_UNOP_CHOICE(tarval_neg, op, c);

	dbg_info *const dbgi       = get_irn_dbg_info(n);
	ir_node  *const negated_op = can_negate_cheaply(dbgi, op);
	if (negated_op != NULL)
		return negated_op;

	if (is_Add(op) && only_one_user(op)) {
		ir_node  *l         = get_Add_left(op);
		ir_node  *r         = get_Add_right(op);
		ir_node  *negated_l = can_negate_cheaply(NULL, l);
		if (negated_l != NULL) {
			/* -((a - b) + c) -> (b - a) - c */
			ir_node *block = get_nodes_block(n);
			return new_rd_Sub(dbgi, block, negated_l, r);
		}
		ir_node *negated_r = can_negate_cheaply(NULL, r);
		if (negated_r != NULL) {
			/* -(a + (b - c)) -> (c - b) - a */
			ir_node *block = get_nodes_block(n);
			return new_rd_Sub(dbgi, block, negated_r, l);
		}
	}
	if (is_And(op) && only_one_user(op)) {
		ir_node *l = get_And_left(op);
		ir_node *r = get_And_right(op);
		if (is_Minus(r) && get_Minus_op(r) == l) {
			/* -(a & (-a)) -> a | (-a) */
			ir_node *block = get_nodes_block(n);
			return new_rd_Or(dbgi, block, l, r);
		}
		if (is_Minus(l) && get_Minus_op(l) == r) {
			/* -((-a) & a) -> (-a) | a */
			ir_node *block = get_nodes_block(n);
			return new_rd_Or(dbgi, block, l, r);
		}
	}
	if (is_Or(op) && only_one_user(op)) {
		ir_node *l = get_Or_left(op);
		ir_node *r = get_Or_right(op);
		if (is_Minus(r) && get_Minus_op(r) == l) {
			/* -(a | (-a)) -> a & (-a) */
			ir_node *block = get_nodes_block(n);
			return new_rd_And(dbgi, block, l, r);
		}
		if (is_Minus(l) && get_Minus_op(l) == r) {
			/* -((-a) | a) -> (-a) & a */
			ir_node *block = get_nodes_block(n);
			return new_rd_And(dbgi, block, l, r);
		}
	}

	if (is_Mul(op) && only_one_user(op)) {
		ir_node  *l         = get_Mul_left(op);
		ir_node  *r         = get_Mul_right(op);
		ir_node  *negated_l = can_negate_cheaply(NULL, l);
		if (negated_l != NULL) {
			/* -((a - b) * c) -> (b - a) * c */
			ir_node *block = get_nodes_block(n);
			return new_rd_Mul(dbgi, block, negated_l, r);
		}
		ir_node *negated_r = can_negate_cheaply(NULL, r);
		if (negated_r != NULL) {
			/* -(a * (b - c)) -> a * (c - b) */
			ir_node *block = get_nodes_block(n);
			return new_rd_Mul(dbgi, block, l, negated_r);
		}
	}

	if (is_Shl(op) && only_one_user(op)) {
		ir_mode *mode = get_irn_mode(n);
		if (get_mode_arithmetic(mode) == irma_twos_complement) {
			ir_node *l         = get_Shl_left(op);
			ir_node *negated_l = can_negate_cheaply(NULL, l);
			if (negated_l != NULL) {
				/* -((a - b) << c) -> (b - a) << c */
				ir_node *block = get_nodes_block(n);
				ir_node *r     = get_Shl_right(op);
				return new_rd_Shl(dbgi, block, negated_l, r);
			}
		}
	}

	return n;
}

/**
 * Transform a Proj(Load) with a non-null address.
 */
static ir_node *transform_node_Proj_Load(ir_node *proj)
{
	if (get_irn_mode(proj) == mode_X) {
		ir_node *load = get_Proj_pred(proj);

		/* get the Load address */
		const ir_node *addr = get_Load_ptr(load);
		const ir_node *confirm;

		if (value_not_null(addr, &confirm)) {
			if (get_Proj_num(proj) == pn_Load_X_except) {
				ir_graph *irg = get_irn_irg(proj);
				return new_r_Bad(irg, mode_X);
			} else {
				ir_node *blk = get_nodes_block(load);
				return new_r_Jmp(blk);
			}
		}
	}
	return proj;
}

/**
 * Transform a Proj(Store) with a non-null address.
 */
static ir_node *transform_node_Proj_Store(ir_node *proj)
{
	if (get_irn_mode(proj) == mode_X) {
		ir_node *store = get_Proj_pred(proj);

		/* get the load/store address */
		const ir_node *addr = get_Store_ptr(store);
		const ir_node *confirm;

		if (value_not_null(addr, &confirm)) {
			if (get_Proj_num(proj) == pn_Store_X_except) {
				ir_graph *irg = get_irn_irg(proj);
				return new_r_Bad(irg, mode_X);
			} else {
				ir_node *blk = get_nodes_block(store);
				return new_r_Jmp(blk);
			}
		}
	}
	return proj;
}

static ir_node *transform_node_Proj_Builtin(ir_node *proj)
{
	ir_node *builtin = get_Proj_pred(proj);
	if (get_Builtin_kind(builtin) != ir_bk_saturating_increment)
		return proj;

	assert(get_Builtin_n_params(builtin) == 1);
	ir_node *op = get_Builtin_param(builtin, 0);
	bitinfo *b  = get_bitinfo(op);
	if (b && !tarval_is_all_one(b->z)) {
		/* Increment cannot overflow -> use a simple add. */
		ir_graph  *const irg   = get_irn_irg(proj);
		dbg_info  *const dbgi  = get_irn_dbg_info(proj);
		ir_node   *const block = get_nodes_block(proj);
		ir_mode   *const mode  = get_irn_mode(proj);
		ir_tarval *const one   = get_mode_one(mode);
		ir_node   *const c     = new_rd_Const(dbgi, irg, one);
		return new_rd_Add(dbgi, block, op, c);
	}

	return proj;
}

/**
 * Transform a Proj(Div) with a non-zero value.
 * Removes the exceptions and routes the memory to the NoMem node.
 */
static ir_node *transform_node_Proj_Div(ir_node *proj)
{
	ir_node       *div     = get_Proj_pred(proj);
	const ir_node *b       = get_Div_right(div);
	const ir_node *confirm;

	if (value_not_null(b, &confirm)) {
		/* div(x, y) && y != 0 */
		if (confirm == NULL) {
			/* we are sure we have a Const != 0 */
			ir_node *new_mem = get_Div_mem(div);
			new_mem = skip_Pin(new_mem);
			set_Div_mem(div, new_mem);
			set_irn_pinned(div, false);
		}

		unsigned proj_nr = get_Proj_num(proj);
		switch (proj_nr) {
		case pn_Div_X_regular:
			return new_r_Jmp(get_nodes_block(div));

		case pn_Div_X_except: {
			ir_graph *irg = get_irn_irg(proj);
			/* we found an exception handler, remove it */
			return new_r_Bad(irg, mode_X);
		}

		case pn_Div_M: {
			ir_graph *irg     = get_irn_irg(proj);
			ir_node  *res     = get_Div_mem(div);
			ir_node  *new_mem = get_irg_no_mem(irg);

			/* This node can only float up to the Confirm block */
			if (confirm)
				new_mem = new_r_Pin(get_nodes_block(confirm), new_mem);

			set_irn_pinned(div, false);
			/* this is a Div without exception, we can remove the memory edge */
			set_Div_mem(div, new_mem);
			return res;
		}
		}
	}
	return proj;
}

/**
 * Transform a Proj(Mod) with a non-zero value.
 * Removes the exceptions and routes the memory to the NoMem node.
 */
static ir_node *transform_node_Proj_Mod(ir_node *proj)
{
	ir_node       *mod     = get_Proj_pred(proj);
	ir_node       *b       = get_Mod_right(mod);
	const ir_node *confirm;

	if (value_not_null(b, &confirm)) {
		/* mod(x, y) && y != 0 */

		if (confirm == NULL) {
			/* we are sure we have a Const != 0 */
			ir_node *new_mem = get_Mod_mem(mod);
			new_mem = skip_Pin(new_mem);
			set_Mod_mem(mod, new_mem);
			set_irn_pinned(mod, false);
		}

		unsigned proj_nr = get_Proj_num(proj);
		switch (proj_nr) {

		case pn_Mod_X_regular:
			return new_r_Jmp(get_nodes_block(mod));

		case pn_Mod_X_except: {
			ir_graph *irg = get_irn_irg(proj);
			/* we found an exception handler, remove it */
			return new_r_Bad(irg, mode_X);
		}

		case pn_Mod_M: {
			ir_graph *irg     = get_irn_irg(proj);
			ir_node  *res     = get_Mod_mem(mod);
			ir_node  *new_mem = get_irg_no_mem(irg);

			/* This node can only float up to the Confirm block */
			if (confirm)
				new_mem = new_r_Pin(get_nodes_block(confirm), new_mem);

			/* this is a Mod without exception, we can remove the memory edge */
			set_Mod_mem(mod, new_mem);
			return res;
		}
		case pn_Mod_res:
			if (get_Mod_left(mod) == b) {
				/* a % a = 0 if a != 0 */
				ir_graph *irg  = get_irn_irg(proj);
				ir_mode  *mode = get_irn_mode(proj);
				ir_node  *res  = new_r_Const_null(irg, mode);

				DBG_OPT_CSTEVAL(mod, res);
				return res;
			}
		}
	}
	return proj;
}

/**
 * return true if the operation returns a value with exactly 1 bit set
 */
static bool is_single_bit(const ir_node *node)
{
	/* a first implementation, could be extended with vrp and others... */
	if (is_Shl(node)) {
		ir_node *shl_l  = get_Shl_left(node);
		ir_mode *mode   = get_irn_mode(node);
		int      modulo = get_mode_modulo_shift(mode);
		/* this works if we shift a 1 and we have modulo shift */
		if (is_irn_one(shl_l) && 0 < modulo && modulo <= (int)get_mode_size_bits(mode)) {
			return true;
		}
	} else if (is_Const(node)) {
		ir_tarval *tv = get_Const_tarval(node);
		return get_tarval_popcount(tv) == 1;
	}
	return false;
}

/**
 * checks if node just flips a bit in another node and returns that other node
 * if so. @p tv should be a value having just 1 bit set
 */
static ir_node *flips_bit(const ir_node *node, ir_tarval *tv)
{
	if (is_Not(node))
		return get_Not_op(node);
	if (is_Eor(node)) {
		ir_node *right = get_Eor_right(node);
		if (is_Const(right)) {
			ir_tarval *right_tv = get_Const_tarval(right);
			if (!tarval_is_null(tarval_and(right_tv, tv)))
				return get_Eor_left(node);
		}
	}
	return NULL;
}

/**
 * Normalizes and optimizes Cmp nodes.
 */
static ir_node *transform_node_Cmp(ir_node *n)
{
	ir_graph    *irg      = get_irn_irg(n);
	ir_node     *left     = get_Cmp_left(n);
	ir_node     *right    = get_Cmp_right(n);
	ir_relation  relation = get_Cmp_relation(n);
	bool         changed  = false;

restart:
	if (relation == ir_relation_false || relation == ir_relation_true) {
		return create_bool_const(irg, relation == ir_relation_true);
	}

	ir_mode     *mode     = get_irn_mode(left);
	ir_relation  possible = ir_get_possible_cmp_relations(left, right);

	/* mask out impossible relations */
	ir_relation new_relation = relation & possible;
	if (new_relation != relation) {
		relation = new_relation;
		changed  = true;
		goto restart;
	}

	/*
	 * First step: remove similar operations on both sides of the Cmp.
	 */

	/* Remove unnecessary conversions */
	if (!mode_is_float(mode) || ir_target.mode_float_arithmetic == NULL) {
		if (is_Conv(left) && is_Conv(right)) {
			ir_node *op_left    = get_Conv_op(left);
			ir_node *op_right   = get_Conv_op(right);
			ir_mode *mode_left  = get_irn_mode(op_left);
			ir_mode *mode_right = get_irn_mode(op_right);
			if (smaller_mode(mode_left, mode) && smaller_mode(mode_right, mode)) {
				ir_node *block = get_nodes_block(n);
				if (mode_left == mode_right) {
					left    = op_left;
					right   = op_right;
					changed = true;
					DBG_OPT_ALGSIM0(n, n);
					goto restart;
				} else if (smaller_mode(mode_left, mode_right)) {
					left    = new_r_Conv(block, op_left, mode_right);
					right   = op_right;
					changed = true;
					DBG_OPT_ALGSIM0(n, n);
					goto restart;
				} else if (smaller_mode(mode_right, mode_left)) {
					left    = op_left;
					right   = new_r_Conv(block, op_right, mode_left);
					changed = true;
					DBG_OPT_ALGSIM0(n, n);
					goto restart;
				}
			}
		}
	}

	/*
	 * Optimize -a CMP -b into b CMP a.
	 * This works only for modes where unary Minus cannot Overflow.
	 * Note that two-complement integers can Overflow so it will NOT work.
	 */
	if (!mode_overflow_on_unary_Minus(mode) && is_Minus(left) && is_Minus(right)) {
		left     = get_Minus_op(left);
		right    = get_Minus_op(right);
		relation = get_inversed_relation(relation);
		changed  = true;
		DBG_OPT_ALGSIM0(n, n);
		goto restart;
	}

	/* remove operation on both sides if possible */
	ir_relation rel_eq = get_complementary_relations(ir_relation_equal, relation, possible);
	/* The following operations are NOT safe for floating point operations, for instance
	 * 1.0 + inf == 2.0 + inf, =/=> x == y */
	if (rel_eq != ir_relation_false && (mode_is_int(mode) || mode_is_reference(mode))) {
		unsigned const lop = get_irn_opcode(left);
		if (lop == get_irn_opcode(right)) {
			/* same operation on both sides, try to remove */
			switch (lop) {
			case iro_Not:
			case iro_Minus:
				/* ~a CMP ~b => a CMP b, -a CMP -b ==> a CMP b */
				assert((int)n_Minus_op == (int)n_Not_op);
				left    = get_irn_n(left, n_Minus_op);
				right   = get_irn_n(right, n_Not_op);
				changed = true;
				DBG_OPT_ALGSIM0(n, n);
				goto restart;

			case iro_Add:
			case iro_Eor: {
				ir_node *const ll = get_binop_left(left);
				ir_node *const lr = get_binop_right(left);
				ir_node *const r0 = get_commutative_other_op(right, ll);
				if (r0) {
					/* X op a CMP X op b ==> a CMP b */
					left    = lr;
					right   = r0;
					changed = true;
					DBG_OPT_ALGSIM0(n, n);
					goto restart;
				} else {
					ir_node *const r1 = get_commutative_other_op(right, lr);
					if (r1) {
						/* a op X CMP X op b ==> a CMP b */
						left    = ll;
						right   = r1;
						changed = true;
						DBG_OPT_ALGSIM0(n, n);
						goto restart;
					}
				}
				break;
			}

			case iro_Sub: {
				ir_node *const ll = get_Sub_left(left);
				ir_node *const lr = get_Sub_right(left);
				ir_node *const rl = get_Sub_left(right);
				ir_node *const rr = get_Sub_right(right);
				if (ll == rl) {
					/* X - a CMP X - b ==> a CMP b */
					left    = lr;
					right   = rr;
					changed = true;
					DBG_OPT_ALGSIM0(n, n);
					goto restart;
				} else if (lr == rr) {
					/* a - X CMP b - X ==> a CMP b */
					left    = ll;
					right   = rl;
					changed = true;
					DBG_OPT_ALGSIM0(n, n);
					goto restart;
				}
				break;
			}

			default:
				break;
			}
		}

		/* X+A == A, A+X == A, X^A == A, A^X == A, A-X == A -> X == 0 */
		ir_node *x;
		if (is_Add(left) || is_Eor(left) || is_Sub(left) || is_Or_Eor_Add(left)) {
			if (is_Sub(left)) {
				x = get_Sub_left(left) == right ? get_Sub_right(left) : NULL;
			} else {
				x = get_commutative_other_op(left, right);
			}
			if (x)
				goto cmp_x_eq_0;
		}
		if (is_Add(right) || is_Eor(right) || is_Sub(right) || is_Or_Eor_Add(right)) {
			if (is_Sub(right)) {
				x = get_Sub_left(right) == left ? get_Sub_right(right) : NULL;
			} else {
				x = get_commutative_other_op(right, left);
			}
			if (x) {
cmp_x_eq_0:
				mode     = get_irn_mode(x);
				left     = x;
				right    = new_r_Const_null(irg, mode);
				relation = rel_eq;
				changed  = true;
				DBG_OPT_ALGSIM0(n, n);
				goto restart;
			}
		}
	}

	if (((is_Shr(left) && is_Shr(right)) || (is_Shrs(left) && is_Shrs(right))) &&
	    get_mode_arithmetic(mode) == irma_twos_complement) {
		ir_node *const lr = get_binop_right(left);
		ir_node *const rr = get_binop_right(right);
		if (lr == rr && is_Const(lr)) {
			bitinfo *const bl = get_bitinfo(left);
			bitinfo *const br = get_bitinfo(right);
			if (bl != NULL && br != NULL) {
				ir_tarval *const lz  = bl->z;
				ir_tarval *const lo  = bl->o;
				ir_tarval *const rz  = br->z;
				ir_tarval *const ro  = br->o;
				ir_tarval *const min = get_mode_min(mode);
				ir_tarval *const leq = tarval_eor(lz, lo);
				ir_tarval *const req = tarval_eor(rz, ro);
				if (tarval_is_null(tarval_and(tarval_or(leq, req), min))) {
					ir_tarval *const c     = get_Const_tarval(lr);
					ir_tarval *const one   = get_mode_one(mode);
					ir_tarval *const mask  = tarval_sub(tarval_shl(one, c), one);
					ir_tarval *const lmask = tarval_and(leq, mask);
					ir_tarval *const rmask = tarval_and(req, mask);
					if (tarval_is_null(tarval_or(lmask, rmask)) &&
					    tarval_is_null(tarval_eor(lz, rz))) {
						/* Cmp(x >> c, y >> c) -> Cmp(x,y) */
						left  = get_binop_left(left);
						right = get_binop_left(right);
						goto restart;
					}
				}
			}
		}
	}

	/*
	 * Second step: normalize the compare op
	 * by placing the constant on the right side
	 * or moving the lower address node to the left.
	 */
	if (!operands_are_normalized(left, right)) {
		ir_node *t = left;
		left  = right;
		right = t;

		relation = get_inversed_relation(relation);
		changed  = true;
		goto restart;
	}

	/*
	 * Third step: remove operations on the left-hand side.
	 */

	/* Remove unnecessary conversions */
	if (is_Conv(left) && is_Const(right)
	    && (!mode_is_float(mode) || ir_target.mode_float_arithmetic == NULL)) {
		ir_node *op_left   = get_Conv_op(left);
		ir_mode *mode_left = get_irn_mode(op_left);
		if (smaller_mode(mode_left, mode)) {
			ir_tarval *tv = get_Const_tarval(right);
			int old_wrap_on_overflow = tarval_get_wrap_on_overflow();
			tarval_set_wrap_on_overflow(false);
			ir_tarval *new_tv = tarval_convert_to(tv, mode_left);
			tarval_set_wrap_on_overflow(old_wrap_on_overflow);
			if (tarval_is_constant(new_tv)) {
				left    = op_left;
				right   = new_r_Const(irg, new_tv);
				changed = true;
				DBG_OPT_ALGSIM0(n, n);
				goto restart;
			}
		}
	}

	if (is_Proj(left) && is_irn_null(right) && rel_eq != ir_relation_false) {
		ir_node *op = get_Proj_pred(left);

		if (is_Mod(op) && get_Proj_num(left) == pn_Mod_res) {
			ir_node *c = get_binop_right(op);

			if (is_Const(c)) {
				ir_tarval *tv = get_Const_tarval(c);

				if (get_tarval_popcount(tv) == 1) {
					/* special case: (x % 2^n) CMP 0 ==> x & (2^n-1) CMP 0 */
					ir_node *v    = get_binop_left(op);
					ir_node *blk  = get_nodes_block(op);
					ir_mode *mode = get_irn_mode(v);

					tv      = tarval_sub(tv, get_mode_one(mode));
					left    = new_rd_And(get_irn_dbg_info(op), blk, v, new_r_Const(irg, tv));
					changed = true;
					DBG_OPT_ALGSIM0(n, n);
					goto restart;
				}
			}
		}
	}

	/* Cmp(Eor(x, y), 0) <=> Cmp(Sub(x, y), 0) <=> Cmp(x, y)
	 * at least for the ==0, !=0 cases */
	if (mode_is_int(mode) && is_cmp_equality_zero(left, right, relation) &&
	    (is_Eor(left) || is_Sub(left) || is_Or_Eor_Add(left))) {
		right    = get_binop_right(left);
		left     = get_binop_left(left);
		relation = rel_eq;
		changed  = true;
		goto restart;
	}

	if (is_And(left) && rel_eq != ir_relation_false) {
		if (is_Const(right)) {
			ir_node *ll = get_And_left(left);
			ir_node *lr = get_And_right(left);
			if (is_Shr(ll) && is_Const(lr)) {
				/* Cmp((x >>u c1) & c2, c3) = Cmp(x & (c2 << c1), c3 << c1) */
				ir_node *llr = get_Shr_right(ll);

				if (is_Const(llr)) {
					ir_tarval *c1 = get_Const_tarval(llr);
					ir_tarval *c2 = get_Const_tarval(lr);
					ir_tarval *c3 = get_Const_tarval(right);

					long l1   = get_tarval_long(c1);
					long h2   = get_tarval_highest_bit(c2);
					long h3   = get_tarval_highest_bit(c3);
					long bits = get_mode_size_bits(mode);

					if (l1 + h2 < bits && l1 + h3 < bits) {
						dbg_info *dbg   = get_irn_dbg_info(left);
						ir_node  *block = get_nodes_block(n);

						ir_tarval *mask  = tarval_shl(c2, c1);
						ir_tarval *value = tarval_shl(c3, c1);

						left     = new_rd_And(dbg, block, get_Shr_left(ll), new_r_Const(irg, mask));
						right    = new_r_Const(irg, value);
						relation = rel_eq;
						changed  = true;
						goto restart;
					}
				}
			}
		}

		/* a complicated Cmp(And(1bit, val), 1bit) "bit-testing" can be replaced
		 * by the simpler Cmp(And(1bit, val), 0) negated pnc */
		if (get_commutative_other_op(left, right) && is_single_bit(right)) {
			relation = rel_eq ^ ir_relation_less_equal_greater;
			right    = new_r_Const_null(irg, mode);
			changed  = true;
			goto restart;
		}

		if (is_irn_null(right)) {
			/* instead of flipping the bit before the bit-test operation negate
			 * pnc */
			ir_node *and0 = get_And_left(left);
			ir_node *and1 = get_And_right(left);
			if (is_Const(and1)) {
				ir_tarval *tv = get_Const_tarval(and1);
				if (get_tarval_popcount(tv) == 1) {
					ir_node *flipped = flips_bit(and0, tv);
					if (flipped != NULL) {
						dbg_info *dbgi  = get_irn_dbg_info(left);
						ir_node  *block = get_nodes_block(left);
						relation = get_negated_relation(relation);
						left     = new_rd_And(dbgi, block, flipped, and1);
						changed  = true;
						goto restart;
					}
				}
			}
		}
	}

	/*
	 * Fourth step: Try to reduce the magnitude
	 * of a constant. This may help to generate better code
	 * later and may help to normalize more compares.
	 * Of course this is only possible for integer values.
	 */
	ir_tarval *tv = value_of(right);
	if (tarval_is_constant(tv)) {
		/* cmp(mux(x, cf, ct), c2) can be eliminated:
		 *   cmp(ct,c2) | cmp(cf,c2) | result
		 *   -----------|------------|--------
		 *   true       | true       | True
		 *   false      | false      | False
		 *   true       | false      | x
		 *   false      | true       | not(x)
		 */
		if (is_Mux(left)) {
			ir_node *mux_true  = get_Mux_true(left);
			ir_node *mux_false = get_Mux_false(left);
			if (is_Const(mux_true) && is_Const(mux_false)) {
				/* we can fold true/false constant separately */
				ir_tarval   *tv_true  = get_Const_tarval(mux_true);
				ir_tarval   *tv_false = get_Const_tarval(mux_false);
				ir_relation  r_true   = tarval_cmp(tv_true, tv);
				ir_relation  r_false  = tarval_cmp(tv_false, tv);
				if (r_true != ir_relation_false
				    || r_false != ir_relation_false) {
					bool     rel_true  = (r_true &relation)  != 0;
					bool     rel_false = (r_false &relation) != 0;
					ir_node *cond      = get_Mux_sel(left);
					if (rel_true == rel_false) {
						relation = rel_true ? ir_relation_true
						                    : ir_relation_false;
						goto restart;
					} else if (rel_true) {
						return cond;
					} else {
						dbg_info *dbgi  = get_irn_dbg_info(n);
						ir_node  *block = get_nodes_block(n);
						ir_node  *notn  = new_rd_Not(dbgi, block, cond);
						return notn;
					}
				}
			}
		}

		/* TODO extend to arbitrary constants */
		if (is_Conv(left) && tarval_is_null(tv)) {
			ir_node *op      = get_Conv_op(left);
			ir_mode *op_mode = get_irn_mode(op);

			/*
			 * UpConv(x) REL 0  ==> x REL 0
			 * Don't do this for float values as it's unclear whether it is a
			 * win. (on the other side it makes detection/creation of fabs hard)
			 */
			if (get_mode_size_bits(mode) > get_mode_size_bits(op_mode) &&
			    (rel_eq != ir_relation_false || mode_is_signed(mode) || !mode_is_signed(op_mode)) &&
			    !mode_is_float(op_mode)) {
				if (!mode_is_signed(mode) && mode_is_signed(op_mode))
					relation = rel_eq;
				tv      = get_mode_null(op_mode);
				left    = op;
				right   = new_r_Const(irg, tv);
				changed = true;
				DBG_OPT_ALGSIM0(n, n);
				goto restart;
			}
		}

		/* the following optimization is possible on modes without Overflow
		 * on Unary Minus or on == and !=:
		 * -a CMP c  ==>  a swap(CMP) -c
		 *
		 * Beware: for two-complement Overflow may occur, so only == and != can
		 * be optimized, see this:
		 * -MININT < 0 =/=> MININT > 0 !!!
		 */
		if (is_Minus(left)) {
			ir_node   *op  = get_Minus_op(left);
			bitinfo   *b   = get_bitinfo(op);
			ir_tarval *max = get_mode_max(mode);
			if (!mode_overflow_on_unary_Minus(mode) ||
			    (mode_is_int(mode) && rel_eq != ir_relation_false) ||
			    (get_mode_arithmetic(mode) == irma_twos_complement && b != NULL &&
			     (!tarval_is_all_one(tarval_or(max, b->z)) ||
			      !tarval_is_null(tarval_and(b->o, max))))) {
				tv = tarval_neg(tv);
				assert(tarval_is_constant(tv));

				left = op;
				if (mode_is_int(mode) && rel_eq != ir_relation_false) {
					relation = rel_eq;
				} else {
					relation = get_inversed_relation(relation);
				}
				right   = new_r_Const(irg, tv);
				changed = true;
				DBG_OPT_ALGSIM0(n, n);
				goto restart;
			}
		} else if (is_Not(left) && rel_eq != ir_relation_false) {
			/* Not(a) ==/!= c  ==>  a ==/!= Not(c) */
			tv = tarval_not(tv);
			assert(tarval_is_constant(tv));

			left     = get_Not_op(left);
			right    = new_r_Const(irg, tv);
			relation = rel_eq;
			changed  = true;
			DBG_OPT_ALGSIM0(n, n);
			goto restart;
		}

		/* for integer modes, we have more */
		if (mode_is_int(mode) && !is_Const(left)) {
			if (!is_relation(ir_relation_false, relation, possible) &&
			    !is_relation(ir_relation_true,  relation, possible)) {
				if ((relation == ir_relation_less || relation == ir_relation_greater_equal) &&
					tarval_cmp(tv, get_mode_null(mode)) == ir_relation_greater) {
					bitinfo const *const bl = get_bitinfo(left);
					if (bl) {
						ir_tarval *const uneq = tarval_or(tarval_andnot(tv, bl->z), tarval_andnot(bl->o, tv));
						int        const hi   = get_tarval_highest_bit(uneq);
						if (hi >= 0) {
							/* Example: 0b????0101 < 0b11001000 -> 0b????0101 <= 0b11000101
							 * It is possible that o <= tv <= z and it is known that left and
							 * tv differ in at least one bit.  Reduce the constant by the
							 * value of the highest differing bit and set all bits below to
							 * the maximum possible value of the left hand side, i.e. z.
							 * This converges in O(n) in comparison to l < c -> l <= c - 1,
							 * which converges in O(2**n). */
							ir_tarval *const one   = get_mode_one(mode);
							ir_tarval *const hibit = tarval_shl_unsigned(one, hi);
							ir_tarval *const mask  = tarval_sub(hibit, one);
							tv = tarval_or(tarval_andnot(tarval_sub(tv, hibit), mask), tarval_and(bl->z, mask));
							goto reduced_tv;
						}
					}

					/* c > 0 : a < c  ==>  a <= (c - 1)    a >= c  ==>  a > (c - 1) */
					tv = tarval_sub(tv, get_mode_one(mode));
					goto reduced_tv;
				} else if ((relation == ir_relation_greater || relation == ir_relation_less_equal) &&
					tarval_cmp(tv, get_mode_null(mode)) == ir_relation_less) {
					bitinfo const *const bl = get_bitinfo(left);
					if (bl) {
						ir_tarval *const uneq = tarval_or(tarval_andnot(tv, bl->z), tarval_andnot(bl->o, tv));
						int        const hi   = get_tarval_highest_bit(uneq);
						if (hi >= 0) {
							/* Example: 0b????0101 > 0b11001000 -> 0b????0101 >= 0b11010101
							 * It is possible that o <= tv <= z and it is known that left and
							 * tv differ in at least one bit.  Increase the constant by the
							 * value of the highest differing bit and set all bits below to
							 * the maximum possible value of the left hand side, i.e. z.
							 * This converges in O(n) in comparison to l > c -> l >= c + 1,
							 * which converges in O(2**n). */
							ir_tarval *const one   = get_mode_one(mode);
							ir_tarval *const hibit = tarval_shl_unsigned(one, hi);
							ir_tarval *const mask  = tarval_sub(hibit, one);
							tv = tarval_or(tarval_andnot(tarval_add(tv, hibit), mask), tarval_and(bl->o, mask));
							goto reduced_tv;
						}
					}

					/* c < 0 : a > c  ==>  a >= (c + 1)    a <= c  ==>  a < (c + 1) */
					tv = tarval_add(tv, get_mode_one(mode));

reduced_tv:
					assert(tarval_is_constant(tv));
					relation ^= ir_relation_equal;
					right     = new_r_Const(irg, tv);
					changed   = true;
					DBG_OPT_ALGSIM0(n, n);
					goto restart;
				}
			}

			/* the following reassociations work only for == and != */
			if (rel_eq != ir_relation_false) {
				if (tarval_is_constant(tv)) {
					if (is_Sub(left)) {
						/* a - c1 ==/!= c2  ==>  a ==/!= c2 + c1 */
						ir_node   *c1  = get_Sub_right(left);
						ir_tarval *tv2 = value_of(c1);

						if (tarval_is_constant(tv2)) {
							tv2 = tarval_add(tv, value_of(c1));

							if (tarval_is_constant(tv2)) {
								left     = get_Sub_left(left);
								right    = new_r_Const(irg, tv2);
								relation = rel_eq;
								changed  = true;
								DBG_OPT_ALGSIM0(n, n);
								goto restart;
							}
						}
					} else if ((is_Add(left) || is_Or_Eor_Add(left))
					           && only_one_user(left)) {
						/* a + c1 ==/!= c2  ==>  a ==/!= c2 - c1 */
						ir_tarval *tv2 = value_of(get_binop_right(left));
						if (tarval_is_constant(tv2)) {
							tv2 = tarval_sub(tv, tv2);
							if (tarval_is_constant(tv2)) {
								left     = get_binop_left(left);
								right    = new_r_Const(irg, tv2);
								relation = rel_eq;
								changed  = true;
								DBG_OPT_ALGSIM0(n, n);
								goto restart;
							}
						}
					} else if (is_Minus(left)) {
						/* -a ==/!= c ==> a ==/!= -c */
						ir_tarval *tv2 = tarval_neg(tv);

						if (tarval_is_constant(tv2)) {
							left     = get_Minus_op(left);
							right    = new_r_Const(irg, tv2);
							relation = rel_eq;
							changed  = true;
							DBG_OPT_ALGSIM0(n, n);
							goto restart;
						}
					}
				}
			}
		}

		if (rel_eq != ir_relation_false) {
			switch (get_irn_opcode(left)) {
				ir_node *c1;

			case iro_And:
				c1 = get_And_right(left);
				if (is_Const(c1)) {
					/* And(x, C1) == C2 ==> FALSE if C2 & C1 != C2
					 * And(x, C1) != C2 ==> TRUE if C2 & C1 != C2 */
					ir_tarval *tv1  = get_Const_tarval(c1);
					ir_tarval *mask = tarval_and(tv1, tv);
					if (mask != tv) {
						/* TODO: move to constant evaluation */
						c1 = create_bool_const(irg, rel_eq != ir_relation_equal);
						DBG_OPT_CSTEVAL(n, c1);
						return c1;
					}

					if (get_tarval_popcount(tv) == 1) {
						/* optimization for AND:
						 * Optimize:
						 *   And(x, C) == C  ==>  And(x, C) != 0
						 *   And(x, C) != C  ==>  And(X, C) == 0
						 *
						 * if C is a single Bit constant.  */

						/* check for Constant's match. We have check hare the tarvals,
						   because our const might be changed */
						if (get_Const_tarval(c1) == tv) {
							/* fine: do the transformation */
							tv = get_mode_null(get_tarval_mode(tv));
							right     = new_r_Const(irg, tv);
							relation ^= ir_relation_less_equal_greater;
							changed   = true;
							DBG_OPT_ALGSIM0(n, n);
							goto restart;
						}
					}

					if (mode_is_signed(mode) &&
					    get_mode_arithmetic(mode) == irma_twos_complement &&
					    get_mode_min(mode) == tv1 && tarval_is_null(tv)) {
						/* And(x, 0x80000000) == 0 ==> x >= 0
						 * And(x, 0x80000000) != 0 ==> x <  0 */
						left     = get_And_left(left);
						relation = rel_eq != ir_relation_equal ? ir_relation_less : ir_relation_greater_equal;
						changed  = true;
						goto restart;
					}

				}
				break;
			case iro_Or:
				c1 = get_Or_right(left);
				if (is_Const(c1) && tarval_is_null(tv)) {
					/* Or(x, C) == 0  && C != 0 ==> FALSE
					 * Or(x, C) != 0  && C != 0 ==> TRUE */
					if (! tarval_is_null(get_Const_tarval(c1))) {
						/* TODO: move to constant evaluation */
						c1 = create_bool_const(irg, rel_eq != ir_relation_equal);
						DBG_OPT_CSTEVAL(n, c1);
						return c1;
					}
				}
				break;
			case iro_Shl:
				/*
				 * optimize x << c1 == c into x & (-1 >>u c1) == c >> c1  if  c & (-1 << c1) == c
				 *                             FALSE                       else
				 * optimize x << c1 != c into x & (-1 >>u c1) != c >> c1  if  c & (-1 << c1) == c
				 *                             TRUE                        else
				 */
				c1 = get_Shl_right(left);
				if (is_Const(c1)) {
					ir_tarval *tv1    = get_Const_tarval(c1);
					ir_tarval *minus1 = get_mode_all_one(mode);
					ir_tarval *cmask  = tarval_shl(minus1, tv1);

					if (tarval_and(tv, cmask) != tv) {
						/* condition not met */
						c1 = create_bool_const(irg, relation != ir_relation_equal);
						DBG_OPT_CSTEVAL(n, c1);
						return c1;
					}
					ir_tarval *amask = tarval_shr(minus1, tv1);
					ir_node   *sl    = get_Shl_left(left);
					ir_node   *blk   = get_nodes_block(n);
					tv       = tarval_shr(tv, tv1);
					left     = new_rd_And(get_irn_dbg_info(left), blk, sl, new_r_Const(irg, amask));
					right    = new_r_Const(irg, tv);
					relation = rel_eq;
					changed  = true;
					DBG_OPT_ALGSIM0(n, n);
					goto restart;
				}
				break;
			case iro_Shr:
				/*
				 * optimize x >>u c1 == c into x & (-1 << c1) == c << c1  if  c & (-1 >>u c1) == c
				 *                             FALSE                       else
				 * optimize x >>u c1 != c into x & (-1 << c1) != c << c1  if  c & (-1 >>u c1) == c
				 *                             TRUE                        else
				 */
				c1 = get_Shr_right(left);
				if (is_Const(c1)) {
					ir_tarval *tv1    = get_Const_tarval(c1);
					ir_tarval *minus1 = get_mode_all_one(mode);
					ir_tarval *cmask  = tarval_shr(minus1, tv1);

					if (tarval_and(tv, cmask) != tv) {
						/* condition not met */
						c1 = create_bool_const(irg, rel_eq != ir_relation_equal);
						DBG_OPT_CSTEVAL(n, c1);
						return c1;
					}
					ir_tarval *amask = tarval_shl(minus1, tv1);
					ir_node   *sl    = get_Shr_left(left);
					ir_node   *blk   = get_nodes_block(n);
					tv       = tarval_shl(tv, tv1);
					left     = new_rd_And(get_irn_dbg_info(left), blk, sl, new_r_Const(irg, amask));
					right    = new_r_Const(irg, tv);
					relation = rel_eq;
					changed  = true;
					DBG_OPT_ALGSIM0(n, n);
					goto restart;
				}
				break;
			case iro_Shrs:
				/*
				 * optimize x >>s c1 == c into x & (-1 << c1) == c << c1  if  (c >>s ((BITS - 1) - c1)) \in {0,-1}
				 *                             FALSE                       else
				 * optimize x >>s c1 != c into x & (-1 << c1) != c << c1  if  (c >>s ((BITS - 1) - c1)) \in {0,-1}
				 *                             TRUE                        else
				 */
				c1 = get_Shrs_right(left);
				if (is_Const(c1)) {
					ir_tarval *tv1     = get_Const_tarval(c1);
					ir_tarval *all_one = get_mode_all_one(mode);
					ir_tarval *cond    = new_tarval_from_long(get_mode_size_bits(mode) - 1, get_tarval_mode(tv1));

					cond = tarval_sub(cond, tv1);
					cond = tarval_shrs(tv, cond);

					if (!tarval_is_all_one(cond) && !tarval_is_null(cond)) {
						/* condition not met */
						c1 = create_bool_const(irg, relation != ir_relation_equal);
						DBG_OPT_CSTEVAL(n, c1);
						return c1;
					}
					ir_tarval *amask = tarval_shl(all_one, tv1);
					ir_node   *sl    = get_Shrs_left(left);
					ir_node   *blk   = get_nodes_block(n);
					tv       = tarval_shl(tv, tv1);
					left     = new_rd_And(get_irn_dbg_info(left), blk, sl, new_r_Const(irg, amask));
					right    = new_r_Const(irg, tv);
					relation = rel_eq;
					changed  = true;
					DBG_OPT_ALGSIM0(n, n);
					goto restart;
				}
				break;
			}
		}
	}

	if (changed) {
		dbg_info *dbgi  = get_irn_dbg_info(n);
		ir_node  *block = get_nodes_block(n);

		/* create a new compare */
		n = new_rd_Cmp(dbgi, block, left, right, relation);
	}

	return n;
}

/**
 * Does all optimizations on nodes that must be done on its Projs
 * because of creating new nodes.
 */
static ir_node *transform_node_Proj(ir_node *proj)
{
	ir_node *n = get_Proj_pred(proj);

	if (n->op->ops.transform_node_Proj)
		return n->op->ops.transform_node_Proj(proj);
	return proj;
}

/**
 * Test whether a block is unreachable
 * Note: That this only returns true when
 * IR_GRAPH_CONSTRAINT_OPTIMIZE_UNREACHABLE_CODE is set.
 * This is important, as you easily end up producing invalid constructs in the
 * unreachable code when optimizing away edges into the unreachable code.
 * So only set this flag when you iterate localopts to the fixpoint.
 * When you reach the fixpoint then all unreachable code is dead
 * (= can't be reached by firm edges) and you won't see the invalid constructs
 * anymore.
 */
static bool is_block_unreachable(const ir_node *block)
{
	const ir_graph *irg = get_irn_irg(block);
	if (!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_OPTIMIZE_UNREACHABLE_CODE))
		return false;
	return get_Block_dom_depth(block) < 0;
}

static ir_node *transform_node_Block(ir_node *block)
{
	ir_graph *irg = get_irn_irg(block);

	if (!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_OPTIMIZE_UNREACHABLE_CODE))
		return block;

	int      arity = get_irn_arity(block);
	ir_node *bad   = NULL;
	for (int i = 0; i < arity; ++i) {
		ir_node *const pred = get_Block_cfgpred(block, i);
		if (is_Bad(pred)) {
			continue;
		} else if (is_block_unreachable(get_nodes_block(pred))) {
			goto bad;
		} else if (is_IJmp(pred)) {
			ir_node *const target = get_IJmp_target(pred);
			if (is_Address(target)) {
				ir_entity *const entity = get_Address_entity(target);
				if (entity == get_Block_entity(block)) {
					/* BB[x](..., IJmp(Address[x]), ...)  ==>  BB[x](..., Jmp, ...) */
					dbg_info *const dbgi = get_irn_dbg_info(pred);
					ir_node*  const jmp  = new_rd_Jmp(dbgi, get_nodes_block(pred));
					set_irn_n(block, i, jmp);
				} else {
					/* BB[x](..., IJmp(Address[y]), ...), x != y  ==>  BB[x](..., Bad, ...) */
bad:
					if (!bad)
						bad = new_r_Bad(irg, mode_X);
					set_irn_n(block, i, bad);
				}
			}
		}
	}

	return block;
}

static ir_node *transform_node_Phi(ir_node *phi)
{
	int       n     = get_irn_arity(phi);
	ir_mode  *mode  = get_irn_mode(phi);
	ir_node  *block = get_nodes_block(phi);
	ir_graph *irg   = get_irn_irg(phi);
	ir_node  *bad   = NULL;

	/* do not optimize Phi0 */
	if (n == 0)
		return phi;

	/* Set phi-operands for bad-block inputs to bad */
	for (int i = 0; i < n; ++i) {
		if (!is_Bad(get_Phi_pred(phi, i))) {
			ir_node *pred = get_Block_cfgpred(block, i);
			if (is_Bad(pred) || is_block_unreachable(get_nodes_block(pred))) {
				if (bad == NULL)
					bad = new_r_Bad(irg, mode);
				set_irn_n(phi, i, bad);
			}
		}
	}

	if (mode_is_reference(mode)) {
		/* Move Confirms down through Phi nodes. */
		ir_node *pred = get_irn_n(phi, 0);
		if (!is_Confirm(pred))
			return phi;

		ir_node      *bound       = get_Confirm_bound(pred);
		bool          has_confirm = false;
		ir_relation   relation    = get_Confirm_relation(pred);
		ir_node     **in          = ALLOCAN(ir_node*, n);
		in[0] = get_Confirm_value(pred);

		for (int i = 1; i < n; ++i) {
			pred = get_irn_n(phi, i);

			if (is_Confirm(pred) &&
			    get_Confirm_bound(pred) == bound &&
			    get_Confirm_relation(pred) == relation) {
				in[i]       = get_Confirm_value(pred);
				has_confirm = true;
			} else if (is_Bad(pred)) {
				in[i] = pred;
			} else {
				return phi;
			}
		}

		if (!has_confirm)
			return phi;

		/* move the Confirm nodes "behind" the Phi */
		ir_node *new_phi = new_r_Phi(block, n, in, get_irn_mode(phi));
		return new_r_Confirm(block, new_phi, bound, relation);
	}
	return phi;
}

/**
 * Optimize (a >> c1) >> c2, works for Shr, Shrs, Shl.
 *
 * Should be moved to reassociation?
 */
static ir_node *transform_node_shift(ir_node *n)
{
	ir_node *left = get_binop_left(n);

	/* different operations */
	if (get_irn_op(left) != get_irn_op(n))
		return n;

	ir_node   *right = get_binop_right(n);
	ir_tarval *tv1   = value_of(right);
	if (!tarval_is_constant(tv1))
		return n;

	ir_tarval *tv2 = value_of(get_binop_right(left));
	if (!tarval_is_constant(tv2))
		return n;

	ir_mode *count_mode = get_tarval_mode(tv1);
	/* TODO: search bigger mode or something and convert... */
	if (get_tarval_mode(tv2) != count_mode)
		return n;

	ir_mode  *mode       = get_irn_mode(n);
	unsigned  modulo_shf = get_mode_modulo_shift(mode);

	if (modulo_shf > 0) {
		ir_tarval *modulo_mask = new_tarval_from_long(modulo_shf - 1U, count_mode);

		/* I'm not so sure what happens in one complement... */
		assert(get_mode_arithmetic(count_mode) == irma_twos_complement);
		/* modulo shifts should always be a power of 2 (otherwise modulo_mask
		 * above will be invalid) */
		assert(is_po2_or_zero(modulo_shf));

		tv1 = tarval_and(tv1, modulo_mask);
		tv2 = tarval_and(tv2, modulo_mask);
	}
	ir_tarval *res = tarval_add(tv1, tv2);
	ir_graph  *irg = get_irn_irg(n);

	/* beware: a simple replacement works only, if res < modulo shift */
	long       bits      = get_mode_size_bits(mode);
	ir_tarval *mode_size = new_tarval_from_long(bits, count_mode);

	/* shifting too much */
	if (!(tarval_cmp(res, mode_size) & ir_relation_less)) {
		if (is_Shrs(n)) {
			ir_node  *block = get_nodes_block(n);
			dbg_info *dbgi  = get_irn_dbg_info(n);
			ir_mode  *smode = get_irn_mode(right);
			ir_node  *cnst  = new_r_Const_long(irg, smode, get_mode_size_bits(mode) - 1);
			return new_rd_Shrs(dbgi, block, get_binop_left(left), cnst);
		}

		return new_r_Const_null(irg, mode);
	}

	/* ok, we can replace it */
	assert(modulo_shf >= get_mode_size_bits(mode));
	ir_node *const block = get_nodes_block(n);
	ir_node *const val   = get_binop_left(left);
	ir_node *const amt   = new_r_Const(irg, res);
	ir_node *const irn   = new_binop(n, block, val, amt);

	DBG_OPT_ALGSIM0(n, irn);

	return irn;
}

/**
 * normalization:
 *    (x << c1) >> c2  <=>  x OP (c2-c1) & ((-1 << c1) >> c2)
 *    also:
 *    (x >> c1) << c2  <=>  x OP (c2-c1) & ((-1 >> c1) << c2)
 *      (also with x >>s c1  when c1>=c2)
 */
static ir_node *transform_node_shl_shr(ir_node *n)
{
	ir_node *right = get_binop_right(n);
	if (!is_Const(right))
		return n;

	ir_node   *left      = get_binop_left(n);
	ir_node   *x;
	ir_mode   *mode      = get_irn_mode(n);
	ir_tarval *tv_shr;
	ir_tarval *tv_mask;
	bool       need_shrs = false;
	ir_tarval *tv_shl    = is_shl_const_like(n);
	if (tv_shl != NULL && (is_Shr(left) || is_Shrs(left))) {
		ir_node *shr_right = get_binop_right(left);
		if (!is_Const(shr_right))
			return n;

		x      = get_binop_left(left);
		tv_shr = get_Const_tarval(shr_right);

		if (is_Shrs(left)) {
			/* shrs variant only allowed if c1 >= c2 */
			if (! (tarval_cmp(tv_shl, tv_shr) & ir_relation_greater_equal))
				return n;

			tv_mask = tarval_shrs(get_mode_all_one(mode), tv_shr);
			need_shrs = true;
		} else {
			tv_mask = tarval_shr(get_mode_all_one(mode), tv_shr);
		}
		tv_mask = tarval_shl(tv_mask, tv_shl);
	} else if (is_Shr(n)) {
		tv_shl = is_shl_const_like(left);
		if (tv_shl == NULL)
			return n;
		x      = get_binop_left(left);
		tv_shr = get_Const_tarval(right);

		tv_mask = tarval_shl(get_mode_all_one(mode), tv_shl);
		tv_mask = tarval_shr(tv_mask, tv_shr);
	} else {
		return n;
	}

	tv_shl = tarval_convert_to(tv_shl, get_tarval_mode(tv_shr));

	assert(tarval_is_constant(tv_mask));
	assert(get_tarval_mode(tv_mask) == mode);

	ir_node     *block    = get_nodes_block(n);
	ir_graph    *irg      = get_irn_irg(block);
	dbg_info    *dbgi     = get_irn_dbg_info(n);
	ir_relation  relation = tarval_cmp(tv_shl, tv_shr);

	/* Reusing the original shift operand is still cheaper. */
	if (relation != ir_relation_equal && !only_one_user(left))
		return n;

	ir_node *new_shift;
	if (relation == ir_relation_less || relation == ir_relation_equal) {
		ir_tarval *tv_shift  = tarval_sub(tv_shr, tv_shl);
		ir_node   *new_const = new_r_Const(irg, tv_shift);
		if (need_shrs) {
			new_shift = new_rd_Shrs(dbgi, block, x, new_const);
		} else {
			new_shift = new_rd_Shr(dbgi, block, x, new_const);
		}
	} else {
		assert(relation == ir_relation_greater);
		ir_tarval *tv_shift  = tarval_sub(tv_shl, tv_shr);
		ir_node   *new_const = new_r_Const(irg, tv_shift);
		new_shift = new_rd_Shl(dbgi, block, x, new_const);
	}

	ir_node *new_const = new_r_Const(irg, tv_mask);
	ir_node *new_and   = new_rd_And(dbgi, block, new_shift, new_const);

	return new_and;
}

static ir_tarval *get_modulo_tv_value(ir_tarval *tv, int modulo_val)
{
	ir_mode   *mode      = get_tarval_mode(tv);
	ir_tarval *modulo_tv = new_tarval_from_long(modulo_val, mode);
	return tarval_mod(tv, modulo_tv);
}

typedef ir_node*(*new_shift_func)(dbg_info *dbgi, ir_node *block, ir_node *left, ir_node *right);

/**
 * Normalization: if we have a shl/shr with modulo_shift behavior
 * then we can use that to minimize the value of Add(x, const) or
 * Sub(Const, x). In particular this often avoids 1 instruction in some
 * backends for the Shift(x, Sub(Const, y)) case because it can be replaced
 * by Shift(x, Minus(y)) which does not need an explicit Const constructed.
 */
static ir_node *transform_node_shift_modulo(ir_node *n,
                                            new_shift_func new_shift)
{
	ir_mode *mode   = get_irn_mode(n);
	int      modulo = get_mode_modulo_shift(mode);

	if (modulo == 0)
		return n;
	if (get_mode_arithmetic(mode) != irma_twos_complement)
		return n;
	if (!is_po2_or_zero(modulo))
		return n;

	ir_graph *irg   = get_irn_irg(n);
	ir_node  *block = get_nodes_block(n);
	ir_node  *right = get_binop_right(n);
	ir_node  *newop = NULL;
	if (is_Const(right)) {
		ir_tarval *tv     = get_Const_tarval(right);
		ir_tarval *tv_mod = get_modulo_tv_value(tv, modulo);
		if (!tarval_is_constant(tv_mod) || tv_mod == tv)
			return n;

		newop = new_r_Const(irg, tv_mod);
	} else if (is_Add(right) || is_Or_Eor_Add(right)) {
		ir_node *add_right = get_binop_right(right);
		if (is_Const(add_right)) {
			ir_tarval *tv     = get_Const_tarval(add_right);
			ir_tarval *tv_mod = get_modulo_tv_value(tv, modulo);
			if (!tarval_is_constant(tv_mod) || tv_mod == tv)
				return n;

			dbg_info *dbgi     = get_irn_dbg_info(right);
			ir_node  *newconst = new_r_Const(irg, tv_mod);
			newop = new_rd_Add(dbgi, block, get_binop_left(right), newconst);
		}
	} else if (is_Sub(right)) {
		ir_node *sub_left = get_Sub_left(right);
		if (is_Const(sub_left)) {
			ir_tarval *tv     = get_Const_tarval(sub_left);
			ir_tarval *tv_mod = get_modulo_tv_value(tv, modulo);
			if (!tarval_is_constant(tv_mod) || tv_mod == tv)
				return n;

			dbg_info *dbgi     = get_irn_dbg_info(right);
			ir_node  *newconst = new_r_Const(irg, tv_mod);
			newop = new_rd_Sub(dbgi, block, newconst, get_Sub_right(right));
		}
	} else {
		return n;
	}

	if (newop != NULL) {
		dbg_info *dbgi = get_irn_dbg_info(n);
		ir_node  *left = get_binop_left(n);
		return new_shift(dbgi, block, left, newop);
	}
	return n;
}

static bool has_all_bits_set(ir_node *n, ir_tarval *mask)
{
	bitinfo *bi = get_bitinfo(n);
	if (bi == NULL) {
		return false;
	}

	return tarval_and(bi->o, mask) == mask;
}

/**
 * If the shift operations have modulo_shift behaviour, we can discard
 * an explicit modulo operation before them, such as in this code (where
 * modulo_shift == 32): result = x SHIFT (y & 0x1f)
 */
static ir_node *transform_node_shift_and(ir_node *n, new_shift_func new_shift)
{
	ir_mode *mode         = get_irn_mode(n);
	unsigned modulo_shift = get_mode_modulo_shift(mode);
	if (modulo_shift == 0)
		return n;

	ir_node *amount = get_binop_right(n);
	if (!is_And(amount))
		return n;

	assert(is_po2_or_zero(modulo_shift));
	ir_mode   *amount_mode = get_irn_mode(amount);
	ir_tarval *all_one     = get_mode_all_one(amount_mode);
	unsigned   shift       = get_mode_size_bits(amount_mode)
	                         - (32-nlz(modulo_shift-1));
	ir_tarval *modulo_mask = tarval_shr_unsigned(all_one, shift);
	ir_node   *and_l       = get_And_left(amount);
	ir_node   *and_r       = get_And_right(amount);
	ir_node   *new_amount  = NULL;

	if (has_all_bits_set(and_r, modulo_mask)) {
		new_amount = and_l;
	} else if (has_all_bits_set(and_l, modulo_mask)) {
		new_amount = and_r;
	} else {
		return n;
	}

	dbg_info *dbgi  = get_irn_dbg_info(n);
	ir_node  *block = get_nodes_block(n);
	ir_node  *left  = get_binop_left(n);
	return new_shift(dbgi, block, left, new_amount);
}

/**
 * Transform a Shr.
 */
static ir_node *transform_node_Shr(ir_node *n)
{
	ir_node *oldn  = n;
	ir_node *left  = get_Shr_left(n);
	ir_node *right = get_Shr_right(n);
	ir_mode *mode  = get_irn_mode(n);

	/* a >>s b >>u (n - 1) -> a >>u (n - 1)
	 * This is a common pattern when replacing division with constant by
	 * multiplication. */
	if (is_Shrs(left) && is_size_minus_1(right, mode)) {
		dbg_info *const dbgi  = get_irn_dbg_info(n);
		ir_node  *const block = get_nodes_block(n);
		ir_node  *const new_l = get_Shrs_left(left);
		return new_rd_Shr(dbgi, block, new_l, right);
	}

	ir_node *c;
	HANDLE_BINOP_CHOICE(tarval_shr, left, right, c, mode);
	n = transform_node_shift(n);

	if (is_Shr(n))
		n = transform_node_shift_modulo(n, new_rd_Shr);
	if (is_Shr(n))
		n = transform_node_shl_shr(n);
	if (is_Shr(n))
		n = transform_node_shift_bitop(n);
	if (is_Shr(n))
		n = transform_node_shift_and(n, new_rd_Shr);

	return n;
}

/**
 * Transform a Shrs.
 */
static ir_node *transform_node_Shrs(ir_node *n)
{
	ir_node *oldn = n;
	ir_node *a    = get_Shrs_left(n);
	ir_node *b    = get_Shrs_right(n);
	ir_mode *mode = get_irn_mode(n);

	if (is_oversize_shift(n)) {
		ir_node  *block = get_nodes_block(n);
		dbg_info *dbgi  = get_irn_dbg_info(n);
		ir_mode  *cmode = get_irn_mode(b);
		long      val   = get_mode_size_bits(cmode)-1;
		ir_graph *irg   = get_irn_irg(n);
		ir_node  *cnst  = new_r_Const_long(irg, cmode, val);
		return new_rd_Shrs(dbgi, block, a, cnst);
	}

	ir_node *c;
	HANDLE_BINOP_CHOICE(tarval_shrs, a, b, c, mode);
	n = transform_node_shift(n);
	if (n != oldn)
		return n;

	n = transform_node_shift_modulo(n, new_rd_Shrs);
	if (n != oldn)
		return n;
	n = transform_node_shift_bitop(n);
	if (n != oldn)
		return n;
	n = transform_node_shift_and(n, new_rd_Shrs);
	if (n != oldn)
		return n;

	/* Normalization: use Shr when sign bit is guaranteed to be cleared */
	const bitinfo *const bn = get_bitinfo(n);
	if (bn != NULL &&
	    get_mode_arithmetic(mode) == irma_twos_complement) {
		unsigned const mode_bits = get_mode_size_bits(mode);
		if (tarval_get_bit(bn->z, mode_bits - 1) == 0) {
			dbg_info *const dbgi  = get_irn_dbg_info(n);
			ir_node  *const block = get_nodes_block(n);
			n = new_rd_Shr(dbgi, block, a, b);
			return n;
		}
	}

	return n;
}

/**
 * Transform a Shl.
 */
static ir_node *transform_node_Shl(ir_node *n)
{
	ir_node *oldn = n;
	ir_node *a    = get_Shl_left(n);
	ir_node *b    = get_Shl_right(n);
	ir_mode *mode = get_irn_mode(n);
	ir_node *c;

	ir_graph *const irg = get_irn_irg(n);
	if (!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_ARCH_DEP)) {
		if (is_Const(b)) {
			/* Normalisation: x << c -> x * (1 << c) */
			dbg_info  *const dbgi  = get_irn_dbg_info(n);
			ir_node   *const block = get_nodes_block(n);
			ir_mode   *const mode  = get_irn_mode(n);
			ir_tarval *const one   = get_mode_one(mode);
			ir_tarval *const val   = tarval_shl(one, get_Const_tarval(b));
			ir_node   *const cnst  = new_r_Const(irg, val);
			return new_rd_Mul(dbgi, block, a, cnst);
		}
	}

	HANDLE_BINOP_CHOICE(tarval_shl, a, b, c, mode);
	n = transform_node_shift(n);

	if (is_Shl(n))
		n = transform_node_shift_modulo(n, new_rd_Shl);
	if (is_Shl(n))
		n = transform_node_shl_shr(n);
	if (is_Shl(n))
		n = transform_node_shift_bitop(n);
	if (is_Shl(n))
		n = transform_node_shift_and(n, new_rd_Shl);

	return n;
}

/**
 * returns mode size for may_leave_out_middle_mode
 */
static unsigned get_significand_size(ir_mode *mode)
{
	const ir_mode_arithmetic arithmetic = get_mode_arithmetic(mode);
	switch (arithmetic) {
	case irma_ieee754:
	case irma_x86_extended_float:
		return get_mode_mantissa_size(mode) + 1;
	case irma_twos_complement:
		return get_mode_size_bits(mode);
	case irma_none:
		panic("Conv node with irma_none mode");
	}
	panic("unexpected mode_arithmetic in get_significand_size");
}

/**
 * Returns true if a conversion from mode @p m0 to @p m2 has the same effect
 * as converting from @p m0 to @p m1 and then to @p m2.
 * Classifying the 3 modes as the big(b), middle(m) and small(s) mode this
 * gives the following truth table:
 * s -> b -> m  : true
 * s -> m -> b  : !signed(s) || signed(m)
 * m -> b -> s  : true
 * m -> s -> b  : false
 * b -> s -> m  : false
 * b -> m -> s  : true
 *
 * s -> b -> b  : true
 * s -> s -> b  : false
 *
 * additional float constraints:
 * F -> F -> F: fine
 * F -> I -> I: signedness of Is must match
 * I -> F -> I: signedness of Is must match
 * I -> I -> F: signedness of Is must match
 * F -> I -> F: bad
 * I -> F -> F: fine
 * F -> F -> I: fine
 * at least 1 float involved: signedness must match
 */
bool may_leave_out_middle_conv(ir_mode *m0, ir_mode *m1, ir_mode *m2)
{
	int n_floats = mode_is_float(m0) + mode_is_float(m1) + mode_is_float(m2);
	if (n_floats == 1) {
		int n_signed = mode_is_signed(m0) + mode_is_signed(m1) + mode_is_signed(m2);
		/* we assume that float modes are always signed */
		if ((n_signed & 1) != 1)
			return false;
	} else if (n_floats == 2 && !mode_is_float(m1)) {
		return false;
	}

	unsigned size0 = get_significand_size(m0);
	unsigned size1 = get_significand_size(m1);
	unsigned size2 = get_significand_size(m2);
	if (size1 < size2 && size0 >= size1)
		return false;
	if (size1 >= size2)
		return true;
	return !mode_is_signed(m0) || mode_is_signed(m1);
}

/**
 * Transform a Conv.
 */
static ir_node *transform_node_Conv(ir_node *n)
{
	ir_node *oldn = n;
	ir_mode *mode = get_irn_mode(n);
	ir_node *a    = get_Conv_op(n);

	if (is_Conv(a)) {
		ir_mode *a_mode = get_irn_mode(a);
		ir_node *b      = get_Conv_op(a);
		ir_mode *b_mode = get_irn_mode(b);
		if (may_leave_out_middle_conv(b_mode, a_mode, mode)) {
			dbg_info *dbgi  = get_irn_dbg_info(n);
			ir_node  *block = get_nodes_block(n);
			return new_rd_Conv(dbgi, block, b, mode);
		}
	}

	ir_node *c = NULL;
	if (is_const_Phi(a)) {
		c = apply_conv_on_phi(a, mode);
	} else if (is_const_Mux(a)) {
		c = apply_conv_on_mux(a, mode);
	}
	if (c) {
		DBG_OPT_ALGSIM0(oldn, c);
		return c;
	}

	return n;
}

static ir_node *transform_node_Bitcast(ir_node *n)
{
	ir_node *orig_op = get_Bitcast_op(n);
	ir_node *new_op  = orig_op;

	/* skip other Bitcasts or Conversions that don't touch the bits */
again:
	if (is_Bitcast(new_op)) {
		new_op = get_Bitcast_op(new_op);
		goto again;
	}
	if (is_Conv(new_op)) {
		ir_node *conv_op = get_Conv_op(new_op);
		ir_mode *op_mode = get_irn_mode(conv_op);
		ir_mode *dst     = get_irn_mode(new_op);
		if (get_mode_arithmetic(op_mode) == irma_twos_complement
		    && get_mode_arithmetic(dst) == irma_twos_complement
		    && get_mode_size_bits(op_mode) == get_mode_size_bits(dst)) {
			new_op = conv_op;
			goto again;
		}
	}

	if (new_op != orig_op) {
		dbg_info *dbgi    = get_irn_dbg_info(n);
		ir_node  *block   = get_nodes_block(n);
		ir_mode  *mode    = get_irn_mode(n);
		ir_mode  *op_mode = get_irn_mode(new_op);
		if (get_mode_arithmetic(mode) == get_mode_arithmetic(op_mode)) {
			return new_rd_Conv(dbgi, block, new_op, mode);
		}
		return new_rd_Bitcast(dbgi, block, new_op, mode);
	}
	return n;
}

/**
 * Remove dead blocks and nodes in dead blocks
 * in keep alive list.  We do not generate a new End node.
 */
static ir_node *transform_node_End(ir_node *n)
{
	int       j;
	int       n_keepalives = get_End_n_keepalives(n);
	ir_node **in           = ALLOCAN(ir_node*, n_keepalives);

	for (int i = j = 0; i < n_keepalives; ++i) {
		ir_node *ka = get_End_keepalive(n, i);
		/* no need to keep Bad */
		if (is_Bad(ka))
			continue;
		/* do not keep unreachable code */
		ir_node *const block = get_block(ka);
		if (is_block_unreachable(block))
			continue;
		in[j++] = ka;
	}
	if (j != n_keepalives)
		set_End_keepalives(n, j, in);
	return n;
}

int ir_is_negated_value(const ir_node *a, const ir_node *b)
{
	if (is_Minus(a) && get_Minus_op(a) == b)
		return true;
	if (is_Minus(b) && get_Minus_op(b) == a)
		return true;
	if (is_Sub(a) && is_Sub(b)) {
		ir_node *a_left  = get_Sub_left(a);
		ir_node *a_right = get_Sub_right(a);
		ir_node *b_left  = get_Sub_left(b);
		ir_node *b_right = get_Sub_right(b);

		if (a_left == b_right && a_right == b_left)
			return true;
	}

	return false;
}

static const ir_node *skip_upconv(const ir_node *node)
{
	while (is_Conv(node)) {
		ir_mode       *mode    = get_irn_mode(node);
		const ir_node *op      = get_Conv_op(node);
		ir_mode       *op_mode = get_irn_mode(op);
		if (!smaller_mode(op_mode, mode))
			break;
		node = op;
	}
	return node;
}

int ir_mux_is_abs(const ir_node *sel, const ir_node *mux_false,
                  const ir_node *mux_true)
{
	if (!is_Cmp(sel))
		return 0;

	/**
	 * Note further that these optimization work even for floating point
	 * with NaN's because -NaN == NaN.
	 * However, if +0 and -0 is handled differently, we cannot use the Abs/-Abs
	 * transformations.
	 */
	ir_mode *mode = get_irn_mode(mux_true);
	if (mode_has_signed_zero(mode))
		return 0;

	/* must be <, <=, >=, > */
	ir_relation relation = get_Cmp_relation(sel);
	if ((relation & ir_relation_less_greater) == 0)
		return 0;

	if (!ir_is_negated_value(mux_true, mux_false))
		return 0;

	mux_true  = skip_upconv(mux_true);
	mux_false = skip_upconv(mux_false);

	/* must be x cmp 0 */
	ir_node *cmp_right = get_Cmp_right(sel);
	if (!is_irn_null(cmp_right))
		return 0;

	ir_node *cmp_left = get_Cmp_left(sel);
	if (cmp_left == mux_false) {
		if (relation & ir_relation_less) {
			return 1;
		} else {
			assert(relation & ir_relation_greater);
			return -1;
		}
	} else if (cmp_left == mux_true) {
		if (relation & ir_relation_less) {
			return -1;
		} else {
			assert(relation & ir_relation_greater);
			return 1;
		}
	}

	return 0;
}

ir_node *ir_get_abs_op(const ir_node *sel, ir_node *mux_false,
                       ir_node *mux_true)
{
	ir_node *cmp_left = get_Cmp_left(sel);
	return cmp_left == skip_upconv(mux_false) ? mux_false : mux_true;
}

static bool ir_is_optimizable_mux_set(const ir_node *cond, ir_relation relation, const ir_mode *dest_mode)
{
	ir_node     *left     = get_Cmp_left(cond);
	ir_mode     *mode     = get_irn_mode(left);
	ir_node     *right    = get_Cmp_right(cond);
	ir_relation  possible = ir_get_possible_cmp_relations(left, right);
	ir_relation  rel_eq   = get_complementary_relations(ir_relation_equal, relation, possible);
	if (rel_eq != ir_relation_false) {
		if (rel_eq == ir_relation_less_greater) {
			bitinfo *bl = get_bitinfo(left);
			bitinfo *br = get_bitinfo(right);

			if (bl != NULL && br != NULL) {
				ir_tarval *left_zeros     = bl->z;
				int        left_low_bit   = get_tarval_lowest_bit(left_zeros);
				int        left_high_bit  = get_tarval_highest_bit(left_zeros);
				ir_tarval *right_zeros    = br->z;
				int        right_low_bit  = get_tarval_lowest_bit(right_zeros);
				int        right_high_bit = get_tarval_highest_bit(right_zeros);

				/* Check that there is only one position where the operands may have a bit set. */
				if (left_low_bit == left_high_bit && right_low_bit == right_high_bit &&
				    (left_low_bit == -1 || right_low_bit == -1 || left_low_bit == right_low_bit)) {
					/* (a & (1 << c)) != (b & (1 << c)) <=> (a ^ b) >> c */
					return true;
				}
			}
		}
		if (is_irn_null(right) && is_And(left)) {
			ir_node *and_right = get_And_right(left);
			if (is_Const(and_right)) {
				ir_tarval *tv = get_Const_tarval(and_right);
				if (get_tarval_popcount(tv) == 1) {
					/* (a & (1 << c)) == 0 <=> (~a >> c) & 1 */
					/* (a & (1 << c)) != 0 <=> ( a >> c) & 1 */
					return true;
				}
			}
		}
	}

	ir_relation const rel_lt = get_complementary_relations(ir_relation_less,    relation, possible);
	ir_relation const rel_gt = get_complementary_relations(ir_relation_greater, relation, possible);

	if (get_mode_size_bits(mode) >= get_mode_size_bits(dest_mode)) {
		/* Due to possible overflows, we can only transform compares with special constants. */
		if (!mode_is_signed(mode) || !is_Const(right))
			return false;

		if ((rel_lt == ir_relation_false || !is_Const_null(right)) &&
		    (rel_gt == ir_relation_false || !is_Const_all_one(right)))
			return false;
	} else if (!mode_is_signed(dest_mode)) {
		return false;
	}

	if (rel_lt != ir_relation_false) {
		/* a <  b <=>                (a - b) < 0  <=>  (a - b) >> 31
		 * a >= b <=> !(a < b) <=> !((a - b) < 0) <=> ~(a - b) >> 31 */
		return true;
	} else if (rel_gt != ir_relation_false) {
		/* a >  b <=>                (b - a) < 0  <=>  (b - a) >> 31 *
		 * a <= b <=> !(a > b) <=> !((b - a) < 0) <=> ~(b - a) >> 31 */
		return true;
	} else {
		return false;
	}
}

int ir_is_optimizable_mux(const ir_node *sel, const ir_node *mux_false,
                          const ir_node *mux_true)
{
	/* this code should return true each time transform_node_Mux would
	 * optimize the Mux completely away */

	ir_mode *const mode = get_irn_mode(mux_false);
	/* abs(x) = x^(x >>s 31) - (x >>s 31) */
	if (get_mode_arithmetic(mode) == irma_twos_complement
	 && ir_mux_is_abs(sel, mux_false, mux_true))
		return true;

	ir_node const *f = mux_false;
	ir_node const *t = mux_true;

	if (is_Cmp(sel)) {
		const ir_node *cmp_l    = get_Cmp_left(sel);
		const ir_node *cmp_r    = get_Cmp_right(sel);
		ir_relation    relation = get_Cmp_relation(sel);

		if (is_Const(t)) {
			/* first normalization step: try to move a constant to the false side,
			 * 0 preferred on false side too */
			if (!is_Const(f) || (is_Const_null(t) && !is_Const_null(f))) {
				/* Mux(x, a, b) => Mux(not(x), b, a) */
				t        = mux_false;
				f        = mux_true;
				relation = get_negated_relation(relation);
			}
		}

		ir_mode *cmp_mode = get_irn_mode(cmp_l);
		if (is_irn_null(f) &&
		    get_mode_arithmetic(mode) == irma_twos_complement &&
		    get_mode_arithmetic(cmp_mode) == irma_twos_complement) {
			if (is_Const(t)) {
				if (is_Const_one(t)) {
					if (ir_is_optimizable_mux_set(sel, relation, mode))
						return true;
				} else if (is_Const_all_one(t) && is_irn_null(cmp_r)) {
					ir_relation possible = ir_get_possible_cmp_relations(cmp_l, cmp_r);
					if (get_complementary_relations(ir_relation_less, relation, possible) != ir_relation_false) {
						/* Mux(a >= 0, 0, 0xFFFFFFFF) => ~a >>s 31 */
						/* Mux(a <  0, 0, 0xFFFFFFFF) =>  a >>s 31 */
						return true;
					}
				}
			}

			if (mode_is_int(mode) && is_Const(cmp_r) && (is_Const_null(cmp_r) || is_Const_one(cmp_r))) {
				bitinfo *bl = get_bitinfo(cmp_l);

				if (bl && tarval_is_one(bl->z)) {
					/* Mux((a & 1) != 0, 0, b) => -a & b */
					/* Mux((a & 1) == 0, 0, b) => (a - 1) & b */
					return true;
				}
			}
		}

		if (mode_is_int(mode) && is_cmp_equality_zero(cmp_l, cmp_r, relation) && is_And(cmp_l) && f == cmp_r) {
			const ir_node *and_l = get_And_left(cmp_l);
			const ir_node *and_r = get_And_right(cmp_l);

			if ((and_l == t || and_r == t) && is_single_bit(t)) {
				/* Mux((a & (1<<n)) == 0, 0, (1<<n)) == (a & (1<<n)) xor ((1<<n)) */
				/* Mux((a & (1<<n)) != 0, 0, (1<<n)) ==  a & (1<<n) */
				return true;
			}
		}
	}

	ir_graph *irg  = get_irn_irg(sel);
	if (!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_MODEB_LOWERED) && mode == mode_b) {
		/* note: after normalization, false can only happen on default */
		if (is_Const(t)) {
			ir_tarval *tv_t = get_Const_tarval(t);
			if (tv_t == tarval_b_true) {
				/* Muxb(sel, true, false) = sel */
				/* Muxb(sel, true, x)     = Or(sel, x) */
				return true;
			}
		} else if (is_Const(f)) {
			/* Muxb(sel, x, true)  = Or(Not(sel), x) */
			/* Muxb(sel, x, false) = And(sel, x) */
			return true;
		}
	}

	return false;
}

/**
 * Optimize a Mux(c, 0, 1) node (sometimes called a "set" instruction)
 */
static ir_node *transform_Mux_set(ir_node *n, ir_relation relation)
{
	ir_node     *cond      = get_Mux_sel(n);
	ir_node     *left      = get_Cmp_left(cond);
	ir_mode     *mode      = get_irn_mode(left);
	ir_mode     *dest_mode = get_irn_mode(n);
	ir_node     *right     = get_Cmp_right(cond);
	ir_relation  possible  = ir_get_possible_cmp_relations(left, right);
	ir_relation  rel_eq    = get_complementary_relations(ir_relation_equal, relation, possible);
	if (rel_eq != ir_relation_false) {
		if (rel_eq == ir_relation_less_greater) {
			bitinfo *bl = get_bitinfo(left);
			bitinfo *br = get_bitinfo(right);

			if (bl != NULL && br != NULL) {
				ir_tarval *left_zeros     = bl->z;
				int        left_low_bit   = get_tarval_lowest_bit(left_zeros);
				int        left_high_bit  = get_tarval_highest_bit(left_zeros);
				ir_tarval *right_zeros    = br->z;
				int        right_low_bit  = get_tarval_lowest_bit(right_zeros);
				int        right_high_bit = get_tarval_highest_bit(right_zeros);

				/* Check that there is only one position where the operands may have a bit set. */
				if (left_low_bit == left_high_bit && right_low_bit == right_high_bit &&
				    (left_low_bit == -1 || right_low_bit == -1 || left_low_bit == right_low_bit)) {
					/* (a & (1 << c)) != (b & (1 << c)) <=> (a ^ b) >> c */
					dbg_info *dbgi         = get_irn_dbg_info(n);
					ir_node  *block        = get_nodes_block(n);
					ir_graph *irg          = get_irn_irg(block);
					ir_node  *eor          = new_rd_Eor(dbgi, block, left, right);
					unsigned  shift_amount = MAX(left_low_bit, right_low_bit);
					ir_node  *shift_cnt    = new_rd_Const_long(dbgi, irg, mode_Iu, shift_amount);
					ir_node  *shift        = new_rd_Shr(dbgi, block, eor, shift_cnt);
					if (mode != dest_mode)
						shift = new_rd_Conv(dbgi, block, shift, dest_mode);

					return shift;
				}
			}
		}

		if (is_irn_null(right) && is_And(left)) {
			ir_node *and_right = get_And_right(left);
			if (is_Const(and_right)) {
				ir_tarval *tv = get_Const_tarval(and_right);
				if (get_tarval_popcount(tv) == 1) {
					/* (a & (1 << c)) == 0 <=> (~a >> c) & 1 */
					/* (a & (1 << c)) != 0 <=> ( a >> c) & 1 */
					dbg_info *dbgi      = get_irn_dbg_info(n);
					ir_node  *block     = get_nodes_block(n);
					ir_node  *a         = get_And_left(left);
					ir_mode  *calc_mode = mode;
					if (get_mode_size_bits(mode) < get_mode_size_bits(dest_mode)) {
						a         = new_rd_Conv(dbgi, block, a, dest_mode);
						calc_mode = dest_mode;
					}

					if (rel_eq == ir_relation_equal)
						a = new_rd_Not(dbgi, block, a);

					ir_graph  *irg          = get_irn_irg(block);
					unsigned   shift_amount = get_tarval_highest_bit(tv);
					ir_node   *shift_cnt    = new_rd_Const_long(dbgi, irg, mode_Iu, shift_amount);
					ir_node   *shift        = new_rd_Shr(dbgi, block, a, shift_cnt);
					ir_node   *c            = new_rd_Const_one(dbgi, irg, calc_mode);
					ir_node   *and          = new_rd_And(dbgi, block, shift, c);
					if (calc_mode != dest_mode)
						and = new_rd_Conv(dbgi, block, and, dest_mode);

					return and;
				}
			}
		}
	}

	ir_relation const rel_lt = get_complementary_relations(ir_relation_less,    relation, possible);
	ir_relation const rel_gt = get_complementary_relations(ir_relation_greater, relation, possible);

	if (get_mode_size_bits(mode) >= get_mode_size_bits(dest_mode)) {
		/* Due to possible overflows, we can only transform compares with special constants. */
		if (!mode_is_signed(mode) || !is_Const(right))
			return n;

		if ((rel_lt == ir_relation_false || !is_Const_null(right)) &&
		    (rel_gt == ir_relation_false || !is_Const_all_one(right)))
			return n;
	} else if (!mode_is_signed(dest_mode)) {
		return n;
	}

	bool     need_not;
	ir_node *a;
	ir_node *b;
	if (rel_lt != ir_relation_false) {
		/* a <  b              <=>   (a - b) < 0  <=>  (a - b) >> 31
		 * a >= b <=> !(a < b) <=> !((a - b) < 0) <=> ~(a - b) >> 31 */
		a        = left;
		b        = right;
		need_not = rel_lt == ir_relation_greater_equal;
	} else if (rel_gt != ir_relation_false) {
		/* a >  b <=>                (b - a) < 0  <=> ( b - a) >> 31 */
		/* a <= b <=> !(a > b) <=> !((b - a) < 0) <=> ~(b - a) >> 31 */
		a        = right;
		b        = left;
		need_not = rel_gt == ir_relation_less_equal;
	} else {
		return n;
	}

	dbg_info *dbgi      = get_irn_dbg_info(n);
	ir_node  *block     = get_nodes_block(n);
	ir_mode  *calc_mode = mode;
	if (get_mode_size_bits(mode) < get_mode_size_bits(dest_mode)) {
		a         = new_rd_Conv(dbgi, block, a, dest_mode);
		b         = new_rd_Conv(dbgi, block, b, dest_mode);
		calc_mode = dest_mode;
	}

	ir_node *sub = new_rd_Sub(dbgi, block, a, b);
	if (need_not)
		sub = new_rd_Not(dbgi, block, sub);

	ir_graph  *irg       = get_irn_irg(block);
	unsigned   bits      = get_mode_size_bits(calc_mode);
	ir_node   *shift_cnt = new_rd_Const_long(dbgi, irg, mode_Iu, bits - 1);
	ir_node   *shift     = new_rd_Shr(dbgi, block, sub, shift_cnt);
	if (calc_mode != dest_mode)
		shift = new_rd_Conv(dbgi, block, shift, dest_mode);

	return shift;
}

/**
 * Optimize a Mux into some simpler cases.
 */
static ir_node *transform_node_Mux(ir_node *n)
{
	ir_node  *oldn = n;
	ir_node  *sel  = get_Mux_sel(n);
	ir_mode  *mode = get_irn_mode(n);
	ir_node  *t    = get_Mux_true(n);
	ir_node  *f    = get_Mux_false(n);
	ir_graph *irg  = get_irn_irg(n);

	/* implement integer abs: abs(x) = x^(x >>s 31) - (x >>s 31) */
	if (get_mode_arithmetic(mode) == irma_twos_complement) {
		int abs = ir_mux_is_abs(sel, f, t);
		if (abs != 0) {
			dbg_info *dbgi       = get_irn_dbg_info(n);
			ir_node  *block      = get_nodes_block(n);
			ir_node  *op         = ir_get_abs_op(sel, f, t);
			unsigned  bits       = get_mode_size_bits(mode);
			ir_node  *shiftconst = new_r_Const_long(irg, mode_Iu, bits-1);
			ir_node  *sext       = new_rd_Shrs(dbgi, block, op, shiftconst);
			ir_node  *xorn       = new_rd_Eor(dbgi, block, op, sext);
			ir_node  *res;
			if (abs > 0) {
				res = new_rd_Sub(dbgi, block, xorn, sext);
			} else {
				res = new_rd_Sub(dbgi, block, sext, xorn);
			}
			return res;
		}
	}

	if (is_Cmp(sel)) {
		ir_node     *cmp_l    = get_Cmp_left(sel);
		ir_node     *cmp_r    = get_Cmp_right(sel);
		ir_relation  relation = get_Cmp_relation(sel);
		bool         inverted = false;

		if (is_Const(t)) {
			/* first normalization step: try to move a constant to the false side,
			 * 0 preferred on false side too */
			if (!is_Const(f) || (is_Const_null(t) && !is_Const_null(f))) {
				/* Mux(x, a, b) => Mux(not(x), b, a) */
				ir_node *tmp = t;
				t        = f;
				f        = tmp;
				relation = get_negated_relation(relation);
				inverted = true;
			}
		}

		ir_mode *cmp_mode = get_irn_mode(cmp_l);
		if (is_irn_null(f) &&
		    get_mode_arithmetic(mode) == irma_twos_complement &&
		    get_mode_arithmetic(cmp_mode) == irma_twos_complement) {
			if (is_Const(t)) {
				if (is_Const_one(t)) {
					n = transform_Mux_set(n, relation);
					if (n != oldn)
						return n;
				} else if (is_Const_all_one(t) && is_irn_null(cmp_r) && mode_is_signed(cmp_mode)) {
					ir_relation const possible = ir_get_possible_cmp_relations(cmp_l, cmp_r);
					ir_relation const rel_lt   = get_complementary_relations(ir_relation_less, relation, possible);
					if (rel_lt != ir_relation_false) {
						ir_node  *block     = get_nodes_block(n);
						dbg_info *dbgi      = get_irn_dbg_info(n);

						/* Mux(a < 0, 0, 0xFFFFFFFF) => a >>s 31 */
						/* Mux(a >= 0, 0, 0xFFFFFFFF) => ~a >>s 31 */
						if (rel_lt == ir_relation_greater_equal)
							cmp_l = new_rd_Not(dbgi, block, cmp_l);

						unsigned  bits = get_mode_size_bits(cmp_mode);
						ir_node  *c    = new_rd_Const_long(dbgi, irg, mode_Iu, bits - 1U);
						ir_node  *shrs = new_rd_Shrs(dbgi, block, cmp_l, c);

						if (cmp_mode != mode)
							shrs = new_rd_Conv(dbgi, block, shrs, mode);

						return shrs;
					}
				}
			}

			if (mode_is_int(mode) && is_Const(cmp_r) && (is_Const_null(cmp_r) || is_Const_one(cmp_r))) {
				bitinfo *bl = get_bitinfo(cmp_l);

				if (bl && tarval_is_one(bl->z)) {
					ir_relation  possible = ir_get_possible_cmp_relations(cmp_l, cmp_r);
					ir_relation  rel_eq   = get_complementary_relations(ir_relation_equal, relation, possible);
					ir_node     *block    = get_nodes_block(n);
					dbg_info    *dbgi     = get_irn_dbg_info(n);
					assert(rel_eq != ir_relation_false);

					if (mode != cmp_mode)
						cmp_l = new_rd_Conv(dbgi, block, cmp_l, mode);

					if (rel_eq == ir_relation_equal ? is_Const_one(cmp_r) : is_Const_null(cmp_r)) {
						/* Mux((a & 1) != 0, 0, b) => -a & b */
						cmp_l = new_rd_Minus(dbgi, block, cmp_l);
					} else {
						/* Mux((a & 1) == 0, 0, b) => (a - 1) & b */
						ir_node *one = new_rd_Const_one(dbgi, irg, mode);
						cmp_l = new_rd_Sub(dbgi, block, cmp_l, one);
					}

					return new_rd_And(dbgi, block, cmp_l, t);
				}
			}
		}

		ir_relation const possible = ir_get_possible_cmp_relations(cmp_l, cmp_r);
		ir_relation const rel_eq   = get_complementary_relations(ir_relation_equal, relation, possible);
		if (rel_eq != ir_relation_false && is_irn_null(cmp_r) && is_And(cmp_l) && f == cmp_r) {
			ir_node *and_l = get_And_left(cmp_l);
			ir_node *and_r = get_And_right(cmp_l);

			if ((and_l == t || and_r == t) && is_single_bit(t)) {
				if (rel_eq == ir_relation_equal) {
					/* Mux((a & (1<<n)) == 0, 0, (1<<n)) == (a & (1<<n)) xor ((1<<n)) */
					ir_node *block = get_nodes_block(n);
					n = new_rd_Eor(get_irn_dbg_info(n), block, cmp_l, t);
					DBG_OPT_ALGSIM1(oldn, sel, sel, n);
				} else {
					/* Mux((a & (1<<n)) != 0, 0, (1<<n)) == a & (1<<n) */
					assert(rel_eq == ir_relation_less_greater);
					n = cmp_l;
					DBG_OPT_ALGSIM1(oldn, sel, sel, n);
				}
				return n;
			}
		}

		/* Normalize Mux. */
		if (inverted) {
			dbg_info *seldbgi = get_irn_dbg_info(sel);
			ir_node  *block   = get_nodes_block(sel);

			/* Mux(x, a, b) => Mux(not(x), b, a) */
			sel = new_rd_Cmp(seldbgi, block, cmp_l, cmp_r, relation);
			return new_rd_Mux(get_irn_dbg_info(n), get_nodes_block(n), sel, f, t);
		}
	}

	/* the following optimizations create new mode_b nodes, so only do them
	 * before mode_b lowering */
	if (!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_MODEB_LOWERED)) {
		if (is_Mux(t)) {
			ir_node  *block = get_nodes_block(n);
			dbg_info *dbgi  = get_irn_dbg_info(n);
			ir_node  *c0    = sel;
			ir_node  *c1    = get_Mux_sel(t);
			ir_node  *t1    = get_Mux_true(t);
			ir_node  *f1    = get_Mux_false(t);
			if (f == f1) {
				/* Mux(cond0, Mux(cond1, x, y), y) => Mux(cond0 && cond1, x, y) */
				ir_node* and = new_r_And(block, c0, c1);
				DBG_OPT_ALGSIM0(oldn, t1);
				return new_rd_Mux(dbgi, block, and, f1, t1);
			} else if (f == t1) {
				/* Mux(cond0, Mux(cond1, x, y), x) */
				ir_node* not_c1  = new_r_Not(block, c1);
				ir_node* and     = new_r_And(block, c0, not_c1);
				DBG_OPT_ALGSIM0(oldn, f1);
				return new_rd_Mux(dbgi, block, and, t1, f1);
			}
		} else if (is_Mux(f)) {
			ir_node  *block = get_nodes_block(n);
			dbg_info *dbgi  = get_irn_dbg_info(n);
			ir_node  *c0    = sel;
			ir_node  *c1    = get_Mux_sel(f);
			ir_node  *t1    = get_Mux_true(f);
			ir_node  *f1    = get_Mux_false(f);
			if (t == t1) {
				/* Mux(cond0, x, Mux(cond1, x, y)) -> typical if (cond0 || cond1) x else y */
				ir_node* or = new_r_Or(block, c0, c1);
				DBG_OPT_ALGSIM0(oldn, f1);
				return new_rd_Mux(dbgi, block, or, f1, t1);
			} else if (t == f1) {
				/* Mux(cond0, x, Mux(cond1, y, x)) */
				ir_node* not_c1  = new_r_Not(block, c1);
				ir_node* or      = new_r_Or(block, c0, not_c1);
				DBG_OPT_ALGSIM0(oldn, t1);
				return new_rd_Mux(dbgi, block, or, t1, f1);
			}
		}

		/* note: after normalization, false can only happen on default */
		if (mode == mode_b) {
			dbg_info *dbg   = get_irn_dbg_info(n);
			ir_node  *block = get_nodes_block(n);

			if (is_Const(t)) {
				ir_tarval *tv_t = get_Const_tarval(t);
				if (tv_t == tarval_b_true) {
					if (is_Const(f)) {
						/* Muxb(sel, true, false) = sel */
						assert(get_Const_tarval(f) == tarval_b_false);
						DBG_OPT_ALGSIM0(oldn, sel);
						return sel;
					} else {
						/* Muxb(sel, true, x) = Or(sel, x) */
						n = new_rd_Or(dbg, block, sel, f);
						DBG_OPT_ALGSIM0(oldn, n);
						return n;
					}
				}
			} else if (is_Const(f)) {
				ir_tarval *tv_f = get_Const_tarval(f);
				if (tv_f == tarval_b_true) {
					/* Muxb(sel, x, true) = Or(Not(sel), x) */
					ir_node* not_sel = new_rd_Not(dbg, block, sel);
					n = new_rd_Or(dbg, block, not_sel, t);
					DBG_OPT_ALGSIM0(oldn, n);
					return n;
				} else {
					/* Muxb(sel, x, false) = And(sel, x) */
					assert(tv_f == tarval_b_false);
					n = new_rd_And(dbg, block, sel, t);
					DBG_OPT_ALGSIM0(oldn, n);
					return n;
				}
			}
		}
	}

	return n;
}

/**
 * optimize Sync nodes that have other syncs as input we simply add the inputs
 * of the other sync to our own inputs
 */
static ir_node *transform_node_Sync(ir_node *n)
{
	int arity = get_Sync_n_preds(n);

	for (int i = 0; i < arity;) {
		ir_node *pred = get_Sync_pred(n, i);

		/* Remove Bad predecessors */
		if (is_Bad(pred)) {
			remove_Sync_n(n, i);
			--arity;
			continue;
		}

		/* Remove duplicate predecessors */
		int j;
		for (j = 0; j < i; ++j) {
			if (get_Sync_pred(n, j) == pred) {
				remove_Sync_n(n, i);
				--arity;
				break;
			}
		}
		if (j < i)
			continue;

		if (!is_Sync(pred)) {
			++i;
			continue;
		}

		remove_Sync_n(n, i);
		--arity;

		int pred_arity = get_Sync_n_preds(pred);
		for (int j = 0; j < pred_arity; ++j) {
			ir_node *pred_pred = get_Sync_pred(pred, j);

			for (int k = 0;; ++k) {
				if (k >= arity) {
					add_irn_n(n, pred_pred);
					++arity;
					break;
				}
				if (get_Sync_pred(n, k) == pred_pred)
					break;
			}
		}
	}

	/* rehash the sync node */
	add_identities(n);
	return n;
}

static ir_node *create_load_replacement_tuple(ir_node *n, ir_node *mem,
                                              ir_node *res)
{
	ir_node  *block = get_nodes_block(n);
	ir_node  *in[pn_Load_max + 1];
	size_t    n_in  = 2;
	in[pn_Load_M]   = mem;
	in[pn_Load_res] = res;
	if (ir_throws_exception(n)) {
		ir_graph *irg = get_irn_irg(n);
		in[pn_Load_X_regular] = new_r_Jmp(block);
		in[pn_Load_X_except]  = new_r_Bad(irg, mode_X);
		n_in                  = 4;
		assert(pn_Load_max == 3);
	}
	return new_r_Tuple(block, n_in, in);
}

static bool sim_store_bitfield(unsigned char *buf, ir_mode *mode, long offset,
                               unsigned bitfield_offset,
                               unsigned bitfield_size, const ir_type *type,
                               const ir_initializer_t *initializer)
{
	if (get_initializer_kind(initializer) == IR_INITIALIZER_NULL)
		return true;
	if (get_initializer_kind(initializer) != IR_INITIALIZER_TARVAL)
		return false;
	ir_tarval *tv = get_initializer_tarval_value(initializer);
	if (tv == tarval_unknown)
		return false;

	ir_mode *type_mode = get_type_mode(type);
	assert(get_tarval_mode(tv) == type_mode);
	assert(bitfield_offset + bitfield_size <= get_mode_size_bits(type_mode));

	ir_tarval *mask = get_mode_all_one(type_mode);
	mask = tarval_shr_unsigned(mask, get_mode_size_bits(type_mode)-bitfield_size);
	mask = tarval_shl_unsigned(mask, bitfield_offset);

	ir_tarval *shifted_value = tarval_shl_unsigned(tv, bitfield_offset);
	ir_tarval *masked_value  = tarval_and(shifted_value, mask);

	unsigned mode_size        = get_mode_size_bytes(mode);
	unsigned initializer_size = get_type_size(type);
	for (unsigned b = (unsigned)MAX(0, offset); b < initializer_size; ++b) {
		if (b > (unsigned)offset + mode_size)
			continue;
		unsigned      idx      = ir_target_big_endian()
		                         ? (initializer_size - 1 - b) : b;
		unsigned char prev     = buf[b-offset];
		unsigned char maskbits = get_tarval_sub_bits(mask, idx);
		unsigned char val      = get_tarval_sub_bits(masked_value, idx);
		unsigned char v        = (prev & ~maskbits) | val;
		buf[b-offset] = v;
	}
	return true;
}

static bool sim_store(unsigned char *buf, ir_mode *mode, long offset,
                      const ir_type *type,
                      const ir_initializer_t *initializer)
{
	unsigned mode_size = get_mode_size_bytes(mode);
	if (offset + (int)mode_size <= 0)
		return true;
	unsigned initializer_size = get_type_size(type);
	assert(initializer_size > 0);
	if (offset >= (long)initializer_size)
		return true;

	ir_tarval *tv;
	switch (get_initializer_kind(initializer)) {
	case IR_INITIALIZER_NULL:
		return true;
	case IR_INITIALIZER_TARVAL:
		tv = get_initializer_tarval_value(initializer);
handle_tv:
		assert(get_type_mode(type) == get_tarval_mode(tv));

		for (unsigned b = (unsigned)MAX(0, offset);
		     b < initializer_size; ++b) {
			if (b > (unsigned)offset + mode_size)
				continue;
			unsigned      idx = ir_target_big_endian()
			                    ? (initializer_size - 1 - b) : b;
			unsigned char v   = get_tarval_sub_bits(tv, idx);
			buf[b-offset] = v;
		}
		return true;
	case IR_INITIALIZER_CONST: {
		ir_node *val = get_initializer_const_value(initializer);
		if (is_Const(val)) {
			tv = get_Const_tarval(val);
			goto handle_tv;
		}
		return false;
	}
	case IR_INITIALIZER_COMPOUND:
		if (is_Array_type(type)) {
			ir_type  *el_type = get_array_element_type(type);
			unsigned  el_size = get_type_size(el_type);
			assert(el_size > 0);
			long   offset0   = MAX(0, offset);
			size_t first_idx = (size_t) ((unsigned)offset0 / el_size);
			size_t last_idx  = (size_t)
				((unsigned)(offset+get_mode_size_bytes(mode)-1) / el_size);
			size_t n_entries
				= get_initializer_compound_n_entries(initializer);
			if (last_idx >= n_entries)
				last_idx = n_entries-1;
			for (size_t i = first_idx; i <= last_idx; ++i) {
				const ir_initializer_t *sub_initializer
					= get_initializer_compound_value(initializer, i);
				long new_offset = offset - (i * el_size);
				bool res
					= sim_store(buf, mode, new_offset, el_type, sub_initializer);
				if (!res)
					return false;
			}
		} else {
			assert(is_compound_type(type));
			/* in case of unions there is no order in the compound offsets,
			 * so we look at all of them in reverse order */
			for (size_t i = 0, n_members = get_compound_n_members(type);
			     i < n_members; ++i) {
				const ir_entity *member      = get_compound_member(type, i);
				const ir_type   *member_type = get_entity_type(member);
				int              member_offs = get_entity_offset(member);
				if (member_offs >= offset + (long)get_mode_size_bytes(mode)
				    || offset >= member_offs + (long)get_type_size(member_type))
				    continue;
				if (i > get_initializer_compound_n_entries(initializer))
					continue;
				const ir_initializer_t *sub_initializer
					= get_initializer_compound_value(initializer, i);
				long     new_offset    = offset - member_offs;
				unsigned bitfield_size = get_entity_bitfield_size(member);
				bool     res;
				if (bitfield_size > 0) {
					unsigned bitfield_offset
						= get_entity_bitfield_offset(member);
					res = sim_store_bitfield(buf, mode, new_offset,
					                         bitfield_offset, bitfield_size,
					                         member_type, sub_initializer);
				} else {
					res = sim_store(buf, mode, new_offset, member_type,
					                sub_initializer);
				}
				if (!res)
					return false;
			}
		}
		return true;
	}
	panic("invalid initializer");
}

static void reverse_bytes(unsigned char *buffer, unsigned buffer_len)
{
	unsigned char *l = buffer;
	unsigned char *r = buffer + buffer_len - 1;
	while (l < r) {
		unsigned char vl = *l;
		unsigned char vr = *r;
		*l++ = vr;
		*r-- = vl;
	}
}

static ir_node *sim_store_load(const ir_type *type,
                               const ir_initializer_t *initializer, long offset,
                               ir_mode *mode, ir_graph *irg)
{
	unsigned       storage_size = get_mode_size_bytes(mode);
	unsigned char *storage      = ALLOCANZ(unsigned char, storage_size);
	if (!sim_store(storage, mode, offset, type, initializer))
		return NULL;
	if (ir_target_big_endian())
		reverse_bytes(storage, storage_size);
	ir_tarval *tv = new_tarval_from_bytes(storage, mode);
	if (tv == tarval_unknown)
		return NULL;
	return new_r_Const(irg, tv);
}

static ir_node *extract_from_initializer(const ir_type *type,
                                         const ir_initializer_t *initializer,
                                         long offset, ir_mode *mode,
                                         dbg_info *dbgi, ir_graph *irg)
{
	if (offset < 0)
		return NULL;
	unsigned size = get_type_size(type);
	if ((unsigned long)offset >= size)
		return NULL;

	ir_tarval *tv;
	switch (get_initializer_kind(initializer)) {
	case IR_INITIALIZER_NULL:
		return new_r_Const_null(irg, mode);

	case IR_INITIALIZER_TARVAL: {
		tv = get_initializer_tarval_value(initializer);
handle_tv:;
		ir_mode *tv_mode = get_tarval_mode(tv);
		/* the following is forbidden, but some frontends construct it anyway */
		if (tv_mode != get_type_mode(type))
			return NULL;
		/* the easy case */
		if (offset == 0
		    && get_mode_arithmetic(tv_mode) == get_mode_arithmetic(mode)
		    && get_mode_size_bits(tv_mode) == get_mode_size_bits(mode)) {
			tv = tarval_convert_to(tv, mode);
			return new_r_Const(irg, tv);
		}
		/* TODO: more advanced variants */
		break;
	}
	case IR_INITIALIZER_CONST: {
		ir_node *val = get_initializer_const_value(initializer);
		if (is_Const(val)) {
			tv = get_Const_tarval(val);
			goto handle_tv;
		}

		ir_mode *val_mode = get_irn_mode(val);
		if (offset == 0
		    && get_mode_arithmetic(val_mode) == get_mode_arithmetic(mode)
		    && get_mode_size_bits(val_mode) == get_mode_size_bits(mode)) {
			ir_node *start_block = get_irg_start_block(irg);
			ir_node *res         = duplicate_subgraph(dbgi, val, start_block);
			if (val_mode != mode)
				res = new_rd_Conv(dbgi, start_block, res, mode);
			return res;
		}
		/* we could do more complicated transformations with the expression tree
		 * here, but it's unclear to me if that improves things */
		return NULL;
	}
	case IR_INITIALIZER_COMPOUND: {
		if (is_Array_type(type)) {
			ir_type  *el_type = get_array_element_type(type);
			unsigned  el_size = get_type_size(el_type);
			assert(el_size > 0);
			unsigned  idx     = (unsigned)offset / el_size;
			if ((size_t)idx >= get_initializer_compound_n_entries(initializer))
				return NULL;
			const ir_initializer_t *sub_initializer
				= get_initializer_compound_value(initializer, (size_t)idx);
			long new_offset = offset - (idx * el_size);
			return extract_from_initializer(el_type, sub_initializer,
			                                new_offset, mode, dbgi, irg);
		} else {
			assert(is_compound_type(type));
			/* in case of unions there is no order in the compound offsets,
			 * so we look at all of them in reverse order */
			for (size_t i = get_compound_n_members(type); i-- > 0; ) {
				const ir_entity *member      = get_compound_member(type, i);
				const ir_type   *member_type = get_entity_type(member);
				int              member_offs = get_entity_offset(member);
				if (member_offs >= offset + (long)get_mode_size_bytes(mode)
				    || offset >= member_offs + (long)get_type_size(member_type))
				    continue;
				if (get_entity_bitfield_size(member) > 0)
					return NULL;
				if (i > get_initializer_compound_n_entries(initializer))
					continue;
				const ir_initializer_t *sub_initializer
					= get_initializer_compound_value(initializer, i);
				if (get_initializer_kind(sub_initializer)
				    == IR_INITIALIZER_NULL)
				    continue;
				long new_offset = offset - member_offs;

				return extract_from_initializer(member_type, sub_initializer,
				                                new_offset, mode, dbgi, irg);
			}
			return new_r_Const_null(irg, mode);
		}
	}
	}
	return NULL;
}

ir_node *predict_load(ir_node *ptr, ir_mode *mode)
{
	long offset = 0;
	if (is_Add(ptr)) {
		ir_node *right = get_Add_right(ptr);
		if (!is_Const(right))
			return NULL;
		ir_tarval *tv = get_Const_tarval(right);
		if (!tarval_is_long(tv))
			return NULL;
		offset = get_tarval_long(tv);
		ptr    = get_Add_left(ptr);
	}
	if (is_Address(ptr)) {
		ir_entity *entity = get_Address_entity(ptr);
		if (get_entity_kind(entity) != IR_ENTITY_NORMAL ||
		    !(get_entity_linkage(entity) & IR_LINKAGE_CONSTANT))
			return NULL;
		const ir_type *type = get_entity_type(entity);
		if (get_type_state(type) != layout_fixed)
			return NULL;
		const ir_initializer_t *initializer = get_entity_initializer(entity);
		if (initializer == NULL)
			initializer = get_initializer_null();
		dbg_info *dbgi = get_irn_dbg_info(ptr);
		ir_graph *irg  = get_irn_irg(ptr);
		/* attempt 1 */
		ir_node  *val  =
			extract_from_initializer(type, initializer, offset, mode, dbgi,
			                         irg);
		if (val != NULL)
			return val;
		ir_node *val2 = sim_store_load(type, initializer, offset, mode, irg);
		return val2;
	}
	return NULL;
}

static ir_node *transform_node_Load(ir_node *n)
{
	/* don't touch volatile loads */
	if (get_Load_volatility(n) == volatility_is_volatile)
		return n;

	/* are we loading from a global constant entity? */
	ir_node *ptr  = get_Load_ptr(n);
	ir_node *mem  = get_Load_mem(n);
	ir_mode *mode = get_Load_mode(n);
	ir_node *val  = predict_load(ptr, mode);
	if (val != NULL)
		return create_load_replacement_tuple(n, mem, val);

	const ir_node *confirm;
	if (value_not_null(ptr, &confirm) && confirm == NULL)
		set_irn_pinned(n, false);

	/* if our memory predecessor is a load from the same address, then reuse the
	 * previous result */
	if (!is_Proj(mem))
		return n;
	ir_node *mem_pred = get_Proj_pred(mem);
	if (is_Load(mem_pred)) {
		ir_node *pred_load = mem_pred;

		/* conservatively compare the 2 loads. TODO: This could be less strict
		 * with fixup code in some situations (like smaller/bigger modes) */
		if (get_Load_ptr(pred_load) != ptr)
			return n;
		ir_mode *const pred_mode = get_Load_mode(pred_load);
		if (get_mode_size_bits(pred_mode) != get_mode_size_bits(mode))
			return n;
		ir_node *      res   = new_r_Proj(pred_load, pred_mode, pn_Load_res);
		ir_node *const block = get_nodes_block(n);
		if (pred_mode != mode) {
			if (get_mode_arithmetic(pred_mode) == get_mode_arithmetic(mode)) {
				res = new_r_Conv(block, res, mode);
			} else {
				res = new_r_Bitcast(block, res, mode);
			}
		}
		/* all combinations of aligned/unaligned pred/n should be fine so we do
		 * not compare the unaligned attribute */
		return create_load_replacement_tuple(n, mem, res);
	} else if (is_Store(mem_pred)) {
		ir_node *pred_store = mem_pred;
		ir_node *value      = get_Store_value(pred_store);

		if (get_Store_ptr(pred_store) != ptr)
			return n;
		ir_mode *const value_mode = get_irn_mode(value);
		if (value_mode != mode) {
			/* we don't handle different sizes currently.
			 * TODO: we could handle the case where the store is bigger
			 * than the load.*/
			if (get_mode_size_bits(value_mode) != get_mode_size_bits(mode))
				return n;
			ir_node *const block = get_nodes_block(n);
			if (get_mode_arithmetic(value_mode) == get_mode_arithmetic(mode)) {
				value = new_r_Conv(block, value, mode);
			} else {
				value = new_r_Bitcast(block, value, mode);
			}
		}
		/* all combinations of aligned/unaligned pred/n should be fine so we do
		 * not compare the unaligned attribute */
		return create_load_replacement_tuple(n, mem, value);
	}

	return n;
}

static ir_node *transform_node_Store(ir_node *n)
{
	ir_node *mem = get_Store_mem(n);
	ir_node *ptr = get_Store_ptr(n);
	/* The store might modify a global value that is used within a loop. */
	if (is_Proj(mem) && only_one_user(mem)) {
		ir_node *pred_store = get_Proj_pred(mem);
		if (is_Store(pred_store) && get_Store_ptr(pred_store) == ptr &&
		    get_Store_volatility(pred_store) != volatility_is_volatile) {
			ir_node  *value          = get_Store_value(n);
			ir_mode  *mode           = get_irn_mode(value);
			unsigned  mode_size      = get_mode_size_bits(mode);
			ir_node  *pred_value     = get_Store_value(pred_store);
			ir_mode  *pred_mode      = get_irn_mode(pred_value);
			unsigned  pred_mode_size = get_mode_size_bits(pred_mode);
			if (pred_mode_size <= mode_size) {
				/* If our memory predecessor is a (smaller) store to the same address,
				 * we can remove the dead predecessor store. */
				ir_node *new_store = exact_copy(n);
				set_Store_mem(new_store, get_Store_mem(pred_store));

				return new_store;
			}
		}
	}

	/* don't touch volatile stores */
	if (get_Store_volatility(n) == volatility_is_volatile)
		return n;

	const ir_node *confirm;
	if (value_not_null(ptr, &confirm) && confirm == NULL)
		set_irn_pinned(n, false);
	return n;
}

static bool always_optimize(unsigned const iro)
{
	return
		iro == iro_Block ||
		iro == iro_Id    ||
		iro == iro_Phi   ||
		iro == iro_Proj;
}

/**
 * Tries several [inplace] [optimizing] transformations and returns an
 * equivalent node.  The difference to equivalent_node() is that these
 * transformations _do_ generate new nodes, and thus the old node must
 * not be freed even if the equivalent node isn't the old one.
 */
static ir_node *transform_node(ir_node *n)
{
restart:;
	ir_node  *old_n = n;
	unsigned  iro   = get_irn_opcode_(n);
	/* constant expression evaluation / constant folding */
	if (get_opt_constant_folding()) {
		/* neither constants nor Tuple values can be evaluated */
		if (iro != iro_Const && get_irn_mode(n) != mode_T) {
			bitinfo *const b = get_bitinfo(n);
			if (b) {
				ir_tarval *z = b->z;
				ir_tarval *o = b->o;

				/* Replace node with constant value by Const. */
				if (z == o && tarval_is_constant(z)) {
					ir_mode *const m = get_irn_mode(n);
					if (mode_is_int(m) || m == mode_b) {
						ir_graph *const irg = get_irn_irg(n);
						n = new_r_Const(irg, z);
						return n;
					}
				}
			}

			/* try to evaluate */
			ir_tarval *tv = computed_value(n);
			if (tarval_is_constant(tv)) {
				/* evaluation was successful -- replace the node. */
				ir_graph *const irg = get_irn_irg(n);

				n = new_r_Const(irg, tv);

				DBG_OPT_CSTEVAL(old_n, n);
				return n;
			}
		}
	}

	/* remove unnecessary nodes */
	if (get_opt_constant_folding() || always_optimize(iro)) {
		n = equivalent_node(n);
		if (n != old_n)
			goto restart;
	}

	/* Some more constant expression evaluation. */
	if (get_opt_algebraic_simplification() ||
		(iro == iro_Cond) ||
		(iro == iro_Proj)) {    /* Flags tested local. */
		if (n->op->ops.transform_node != NULL) {
			n = n->op->ops.transform_node(n);
			if (n != old_n)
				goto restart;
		}
	}

	return n;
}

void ir_register_opt_node_ops(void)
{
	set_op_computed_value(op_Add,      computed_value_Add);
	set_op_computed_value(op_Align,    computed_value_Align);
	set_op_computed_value(op_And,      computed_value_And);
	set_op_computed_value(op_Bitcast,  computed_value_Bitcast);
	set_op_computed_value(op_Cmp,      computed_value_Cmp);
	set_op_computed_value(op_Confirm,  computed_value_Confirm);
	set_op_computed_value(op_Const,    computed_value_Const);
	set_op_computed_value(op_Conv,     computed_value_Conv);
	set_op_computed_value(op_Offset,   computed_value_Offset);
	set_op_computed_value(op_Eor,      computed_value_Eor);
	set_op_computed_value(op_Minus,    computed_value_Minus);
	set_op_computed_value(op_Mul,      computed_value_Mul);
	set_op_computed_value(op_Mux,      computed_value_Mux);
	set_op_computed_value(op_Not,      computed_value_Not);
	set_op_computed_value(op_Or,       computed_value_Or);
	set_op_computed_value(op_Proj,     computed_value_Proj);
	set_op_computed_value(op_Shl,      computed_value_Shl);
	set_op_computed_value(op_Shr,      computed_value_Shr);
	set_op_computed_value(op_Shrs,     computed_value_Shrs);
	set_op_computed_value(op_Size,     computed_value_Size);
	set_op_computed_value(op_Sub,      computed_value_Sub);
	set_op_computed_value_proj(op_Builtin, computed_value_Proj_Builtin);
	set_op_computed_value_proj(op_Div,     computed_value_Proj_Div);
	set_op_computed_value_proj(op_Mod,     computed_value_Proj_Mod);

	set_op_equivalent_node(op_Add,     equivalent_node_Add);
	set_op_equivalent_node(op_And,     equivalent_node_And);
	set_op_equivalent_node(op_Bitcast, equivalent_node_Bitcast);
	set_op_equivalent_node(op_Confirm, equivalent_node_Confirm);
	set_op_equivalent_node(op_Conv,    equivalent_node_Conv);
	set_op_equivalent_node(op_CopyB,   equivalent_node_CopyB);
	set_op_equivalent_node(op_Eor,     equivalent_node_Eor);
	set_op_equivalent_node(op_Id,      equivalent_node_Id);
	set_op_equivalent_node(op_Minus,   equivalent_node_Minus);
	set_op_equivalent_node(op_Mul,     equivalent_node_Mul);
	set_op_equivalent_node(op_Mux,     equivalent_node_Mux);
	set_op_equivalent_node(op_Not,     equivalent_node_Not);
	set_op_equivalent_node(op_Or,      equivalent_node_Or);
	set_op_equivalent_node(op_Phi,     equivalent_node_Phi);
	set_op_equivalent_node(op_Pin,     equivalent_node_Pin);
	set_op_equivalent_node(op_Proj,    equivalent_node_Proj);
	set_op_equivalent_node(op_Shl,     equivalent_node_right_zero);
	set_op_equivalent_node(op_Shr,     equivalent_node_right_zero);
	set_op_equivalent_node(op_Shrs,    equivalent_node_right_zero);
	set_op_equivalent_node(op_Sub,     equivalent_node_Sub);
	set_op_equivalent_node(op_Sync,    equivalent_node_Sync);
	set_op_equivalent_node_proj(op_Div,   equivalent_node_Proj_Div);
	set_op_equivalent_node_proj(op_Tuple, equivalent_node_Proj_Tuple);
	set_op_equivalent_node_proj(op_Store, equivalent_node_Proj_Store);

	set_op_transform_node(op_Add,     transform_node_Add);
	set_op_transform_node(op_And,     transform_node_And);
	set_op_transform_node(op_Bitcast, transform_node_Bitcast);
	set_op_transform_node(op_Block,   transform_node_Block);
	set_op_transform_node(op_Cmp,     transform_node_Cmp);
	set_op_transform_node(op_Cond,    transform_node_Cond);
	set_op_transform_node(op_Confirm, transform_node_Confirm);
	set_op_transform_node(op_Conv,    transform_node_Conv);
	set_op_transform_node(op_Div,     transform_node_Div);
	set_op_transform_node(op_End,     transform_node_End);
	set_op_transform_node(op_Eor,     transform_node_Eor);
	set_op_transform_node(op_Load,    transform_node_Load);
	set_op_transform_node(op_Minus,   transform_node_Minus);
	set_op_transform_node(op_Mod,     transform_node_Mod);
	set_op_transform_node(op_Mul,     transform_node_Mul);
	set_op_transform_node(op_Mux,     transform_node_Mux);
	set_op_transform_node(op_Not,     transform_node_Not);
	set_op_transform_node(op_Or,      transform_node_Or);
	set_op_transform_node(op_Phi,     transform_node_Phi);
	set_op_transform_node(op_Proj,    transform_node_Proj);
	set_op_transform_node(op_Shl,     transform_node_Shl);
	set_op_transform_node(op_Shrs,    transform_node_Shrs);
	set_op_transform_node(op_Shr,     transform_node_Shr);
	set_op_transform_node(op_Store,   transform_node_Store);
	set_op_transform_node(op_Sub,     transform_node_Sub);
	set_op_transform_node(op_Switch,  transform_node_Switch);
	set_op_transform_node(op_Sync,    transform_node_Sync);
	set_op_transform_node_proj(op_Builtin, transform_node_Proj_Builtin);
	set_op_transform_node_proj(op_Div,     transform_node_Proj_Div);
	set_op_transform_node_proj(op_Load,    transform_node_Proj_Load);
	set_op_transform_node_proj(op_Mod,     transform_node_Proj_Mod);
	set_op_transform_node_proj(op_Store,   transform_node_Proj_Store);
}

/** The size of the hash table used, should estimate the number of nodes
 * in a graph. */
#define N_IR_NODES 512

static int identities_cmp(const void *elt, const void *key)
{
	ir_node *a = (ir_node *)elt;
	ir_node *b = (ir_node *)key;

	if (a == b)
		return 0;

	if ((get_irn_op(a) != get_irn_op(b)) ||
	    (get_irn_mode(a) != get_irn_mode(b)))
	    return 1;

	/* compare if a's in and b's in are of equal length */
	int irn_arity_a = get_irn_arity(a);
	if (irn_arity_a != get_irn_arity(b))
		return 1;

	/* blocks are never the same */
	if (is_Block(a))
		return 1;

	if (get_irn_pinned(a)) {
		/* for pinned nodes, the block inputs must be equal */
		if (get_nodes_block(a) != get_nodes_block(b))
			return 1;
	} else {
		ir_node *block_a = get_nodes_block(a);
		ir_node *block_b = get_nodes_block(b);
		if (!get_opt_global_cse()) {
			/* for block-local CSE both nodes must be in the same Block */
			if (block_a != block_b)
				return 1;
		} else {
			/* The optimistic approach would be to do nothing here.
			 * However doing GCSE optimistically produces a lot of partially dead code which appears
			 * to be worse in practice than the missed opportunities.
			 * So we use a very conservative variant here and only CSE if one value dominates the
			 * other or one value postdominates the common dominator. */
			if (!block_dominates(block_a, block_b)
			 && !block_dominates(block_b, block_a)) {
				if (get_Block_dom_depth(block_a) < 0
				 || get_Block_dom_depth(block_b) < 0)
					return 1;

				ir_node *dom = ir_deepest_common_dominator(block_a, block_b);
				if (!block_postdominates(block_a, dom)
				 && !block_postdominates(block_b, dom))
					return 1;
			}
		}
	}

	/* compare a->in[0..ins] with b->in[0..ins] */
	for (int i = 0; i < irn_arity_a; ++i) {
		ir_node *pred_a = get_irn_n(a, i);
		ir_node *pred_b = get_irn_n(b, i);
		if (pred_a != pred_b)
			return 1;
	}

	/* here, we already know that the nodes are identical except their
	 * attributes */
	return !a->op->ops.attrs_equal(a, b);
}

unsigned ir_node_hash(const ir_node *node)
{
	return node->op->ops.hash(node);
}

void new_identities(ir_graph *irg)
{
	del_identities(irg);
	irg->value_table = new_pset(identities_cmp, N_IR_NODES);
}

void del_identities(ir_graph *irg)
{
	if (irg->value_table != NULL)
		del_pset(irg->value_table);
}

static int cmp_node_nr(const void *a, const void *b)
{
	ir_node **p1 = (ir_node**)a;
	ir_node **p2 = (ir_node**)b;
	long      n1 = get_irn_node_nr(*p1);
	long      n2 = get_irn_node_nr(*p2);
	return (n1 > n2) - (n1 < n2);
}

void ir_normalize_node(ir_node *n)
{
	if (is_op_commutative(get_irn_op(n))) {
		ir_node *l = get_binop_left(n);
		ir_node *r = get_binop_right(n);

		/* For commutative operators perform  a OP b == b OP a but keep
		 * constants on the RIGHT side. This helps greatly in some
		 * optimizations.  Moreover we use the idx number to make the form
		 * deterministic. */
		if (!operands_are_normalized(l, r)) {
			set_binop_left(n, r);
			set_binop_right(n, l);
		}
	} else if (is_Sync(n)) {
		/* we assume that most of the time the inputs of a Sync node are already
		 * sorted, so check this first as a shortcut */
		bool           ins_sorted = true;
		int            arity      = get_irn_arity(n);
		const ir_node *last       = get_irn_n(n, 0);
		for (int i = 1; i < arity; ++i) {
			const ir_node *node = get_irn_n(n, i);
			if (get_irn_node_nr(node) < get_irn_node_nr(last)) {
				ins_sorted = false;
				break;
			}
			last = node;
		}

		if (!ins_sorted) {
			ir_node **ins     = get_irn_in(n);
			ir_node **new_ins = XMALLOCN(ir_node*, arity);
			MEMCPY(new_ins, ins, arity);
			QSORT(new_ins, arity, cmp_node_nr);
			set_irn_in(n, arity, new_ins);
			free(new_ins);
		}
	}
}

ir_node *identify_remember(ir_node *n)
{
	ir_graph *irg         = get_irn_irg(n);
	pset     *value_table = irg->value_table;

	if (value_table == NULL)
		return n;

	ir_normalize_node(n);
	/* lookup or insert in hash table with given hash key. */
	ir_node *nn = (ir_node *)pset_insert(value_table, n, ir_node_hash(n));

	/* nn is reachable again */
	if (nn != n)
		edges_node_revival(nn);

	return nn;
}

/**
 * During construction we set the op_pin_state_pinned flag in the graph right
 * when the optimization is performed.  The flag turning on procedure global
 * cse could be changed between two allocations.  This way we are safe.
 *
 * @param n            The node to lookup
 */
static inline ir_node *identify_cons(ir_node *n)
{
	ir_node *old = n;

	n = identify_remember(n);
	if (n != old && get_nodes_block(old) != get_nodes_block(n)) {
		ir_graph *irg = get_irn_irg(n);
		set_irg_pinned(irg, op_pin_state_floats);
	}
	return n;
}

void add_identities(ir_node *node)
{
	if (!get_opt_cse())
		return;
	if (is_Block(node))
		return;

	identify_remember(node);
}

void visit_all_identities(ir_graph *irg, irg_walk_func visit, void *env)
{
	foreach_pset(irg->value_table, ir_node, node) {
		visit(node, env);
	}
}

ir_node *optimize_node(ir_node *n)
{
	ir_node  *oldn = n;
	unsigned  iro  = get_irn_opcode(n);

	/* Always optimize Phi nodes: part of the construction. */
	if (!get_optimize() && (iro != iro_Phi))
		return n;

	ir_graph *irg = get_irn_irg(n);

	/* constant expression evaluation / constant folding */
	if (get_opt_constant_folding()) {
		/* neither constants nor Tuple values can be evaluated */
		if (iro != iro_Const && (get_irn_mode(n) != mode_T)) {
			/* try to evaluate */
			ir_tarval *tv = computed_value(n);
			if (tarval_is_constant(tv)) {
				/* we MUST copy the node here temporarily, because it's still
				 * needed for DBG_OPT_CSTEVAL */
				size_t node_size = offsetof(ir_node, attr) +  n->op->attr_size;
				oldn = (ir_node *)alloca(node_size);

				memcpy(oldn, n, node_size);
				size_t n_in = ARR_LEN(n->in);
				oldn->in = ALLOCAN(ir_node*, n_in);

				/* ARG, copy the in array, we need it for statistics */
				MEMCPY(oldn->in, n->in, n_in);

				/* note the inplace edges module */
				edges_node_deleted(n);

				/* evaluation was successful -- replace the node. */
				irg_kill_node(irg, n);
				ir_node *nw = new_r_Const(irg, tv);

				DBG_OPT_CSTEVAL(oldn, nw);
				return nw;
			}
		}
	}

	/* remove unnecessary nodes */
	if (get_opt_algebraic_simplification() || always_optimize(iro))
		n = equivalent_node(n);

	/* Common Subexpression Elimination.
	 *
	 * Checks whether n is already available.
	 * The block input is used to distinguish different subexpressions. Right
	 * now all nodes are pinned to blocks, i.e., the CSE only finds common
	 * subexpressions within a block.
	 */
	if (get_opt_cse())
		n = identify_cons(n);

	if (n != oldn) {
		edges_node_deleted(oldn);

		/* We found an existing, better node, so we can deallocate the old node. */
		irg_kill_node(irg, oldn);
		return n;
	}

	/* Some more constant expression evaluation that does not allow to
	 * free the node. */
	iro = get_irn_opcode(n);
	if (get_opt_algebraic_simplification() ||
		(iro == iro_Cond) ||
		(iro == iro_Proj)) {    /* Flags tested local. */
		n = transform_node(n);
	}

	/* Now we have a legal, useful node. Enter it in hash table for CSE */
	if (get_opt_cse()) {
		ir_node *o = n;
		n = identify_remember(o);
		if (o != n)
			DBG_OPT_CSE(o, n);
	}

	return n;
}

ir_node *optimize_in_place_2(ir_node *n)
{
	if (!get_optimize() && !is_Phi(n))
		return n;

	if (is_Deleted(n))
		return n;

	/** common subexpression elimination **/
	/* Checks whether n is already available. */
	/* The block input is used to distinguish different subexpressions.
	 * Right now all nodes are pinned to blocks, i.e., the cse
	 * only finds common subexpressions within a block. */
	if (get_opt_cse()) {
		ir_node *o = n;
		n = identify_remember(n);
		if (n != o) {
			DBG_OPT_CSE(o, n);
			/* we have another existing node now, we do not optimize it here */
			return n;
		}
	}

	n = transform_node(n);

#ifdef DEBUG_libfirm
	/* Now we can verify the node, as it has no dead inputs any more. */
	assert(irn_verify(n));
#endif

	/* Now we have a legal, useful node. Enter it in hash table for cse.
	 *
	 * Note: This is only necessary because some of the optimizations
	 * operate in-place (set_XXX_bla, turn_into_tuple, ...) which is considered
	 * bad practice and should be fixed sometime.
	 */
	if (get_opt_cse()) {
		ir_node *o = n;
		n = identify_remember(o);
		if (o != n)
			DBG_OPT_CSE(o, n);
	}

	return n;
}

ir_node *optimize_in_place(ir_node *n)
{
	ir_graph *irg = get_irn_irg(n);

	if (get_opt_global_cse())
		set_irg_pinned(irg, op_pin_state_floats);

	/* FIXME: Maybe we could also test whether optimizing the node can
	   change the control graph. */
	clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);
	return optimize_in_place_2(n);
}
