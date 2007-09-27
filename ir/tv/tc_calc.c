#include "irnode.h"
#include "tc_calc.h"

/* shift bits with sign extension */
#define SHRS(v, c)	((v) < 0 ? ~((~(v)) >> (c)) : (v) >> (c))

#define SIGN(v) ((v)->part[0] & TC_VALUE_PART_SIGN_BIT)

/* the carry and overflow bits */
int tc_Carry, tc_Overflow;

/**
 * Produce a tc_value from a long value
 */
void tc_from_long(tc_value *tcv, long value) {
	int i;

	for (i = NUM_VALUE_PARTS - 1; i >= 0; --i) {
		long part = value & TC_VALUE_PART_MASK;
		tcv->part[i] = (tc_value_part)part;
		value = SHRS(value, TC_VALUE_PART_SIZE);
	}
}

/**
 * Bit complement an tc_value: res = ~a.
 */
void tc_not(const tc_value *a, tc_value *res) {
	int	i;

	for (i = NUM_VALUE_PARTS - 1; i >= 0; --i)
		res->part[i] = ~a->part[i] & TC_VALUE_PART_MAX_UINT;
}

/**
 * Logical AND of two tc_values: res = a & b.
 */
void tc_and(const tc_value *a, const tc_value *b, tc_value *res) {
	int	i;

	for (i = NUM_VALUE_PARTS - 1; i >= 0; --i)
		res->part[i] = a->part[i] & b->part[i];
}

/**
 * Logical OR of two tc_values: res = a | b.
 */
void tc_or(const tc_value *a, const tc_value *b, tc_value *res) {
	int	i;

	for (i = NUM_VALUE_PARTS - 1; i >= 0; --i)
		res->part[i] = a->part[i] | b->part[i];
}

/**
 * Logical exclusive OR of two tc_values: res = a ^ b.
 */
void tc_eor(const tc_value *a, const tc_value *b, tc_value *res) {
	int	i;

	for (i = NUM_VALUE_PARTS - 1; i >= 0; --i)
		res->part[i] = a->part[i] ^ b->part[i];
}


/**
 * Compare two tc_value's.
 */
pn_Cmp tc_cmp(const tc_value *a, const tc_value *b, int signed_cmp) {
	int i;

	if (signed_cmp) {
		int sign_a = SIGN(a);

		if (sign_a != !SIGN(b))
			return sign_a ? pn_Cmp_Lt : pn_Cmp_Gt;
	}
	for (i = 0; i < NUM_VALUE_PARTS; ++i) {
		if (a->part[i] > b->part[i])
			return pn_Cmp_Gt;
		if (a->part[i] < b->part[i])
			return pn_Cmp_Lt;
	}
	return pn_Cmp_Eq;
}

/**
 * Add two tc_values: res = a + b. Sets carry and overflow.
 */
void tc_add(const tc_value *a, const tc_value *b, tc_value *res) {
	int	i, carry = 0, sign_a;

	tc_Carry = tc_Overflow = 0;

	for (i = NUM_VALUE_PARTS - 1; i >= 0; --i) {
		tc_temp t = (tc_temp)a->part[i] + (tc_temp)b->part[i] + carry;
		if (t > TC_VALUE_PART_MAX_UINT) {
			t -= TC_VALUE_PART_MAX_UINT;
			carry = 1;
		} else
			carry = 0;
		res->part[i] = (tc_value_part)t;
	}
	/* Check for overflow. */
	tc_Carry = carry;

	/* A signed overflow occurred if the two operands have the same sign and
       the result has a different sign. */
	sign_a = SIGN(a);
    tc_Overflow = (sign_a == SIGN(b)) && (sign_a != SIGN(res));
}

/**
 * Subtract two tc_values: res = a - b. Sets carry and overflow.
 */
void tc_sub(const tc_value *a, const tc_value *b, tc_value *res) {
	int	    i, sign_a;
	tc_temp borrow = 0;

	tc_Carry = tc_Overflow = 0;
	for (i = NUM_VALUE_PARTS - 1; i >= 0; --i) {
		tc_temp t;
		t = (tc_temp)a->part[i] - (tc_temp)b->part[i] - borrow;
		if (t < 0) {
			t += TC_VALUE_PART_MAX_UINT + 1;
			borrow = 1;
		} else
			borrow = 0;
		res->part[i] = (tc_value_part)t;
	}
	/* Check for underflow or overflow. */
	tc_Carry = borrow;
	/* A signed overflow occurred if the two operands have the same sign and
       the result has a different sign. */
	sign_a = SIGN(a);
    tc_Overflow = (sign_a == SIGN(b)) && (sign_a != SIGN(res));
}

/**
 * Negate an tc_value: res = -a. Sets carry and overflow.
 */
void tc_neg(const tc_value *a, tc_value *res) {
	tc_from_long(res, 0);
	tc_sub(res, a, res);
}

#define NUM_WORK_PARTS (NUM_VALUE_PARTS * 2)

/**
 * Multiply two unsigned tc_values producing a double precision result : w = a * b.
 */
static void _tc_umul(const tc_value *a, const tc_value *b, tc_value_part *w) {
	const tc_value_part *u = a->part;
	const tc_value_part *v = b->part;

	int i, j;

	for (i = 0; i < NUM_WORK_PARTS; ++i)
		w[i] = 0;

	for (j = NUM_VALUE_PARTS - 1; j >= 0; --j) {
		tc_temp k = 0;
		for (i = NUM_VALUE_PARTS - 1; i >= 0; --i) {
			tc_temp t = u[i] * v[j] + k;
			w[i + j] = t & TC_VALUE_PART_MASK;
			k = t >> 16;
		}
		w[j + NUM_VALUE_PARTS] = k & TC_VALUE_PART_MASK;
	}
}

/**
 * Multiply two unsigned tc_values: res = a * b.
 */
void tc_umul(const tc_value *a, const tc_value *b, tc_value *res) {
	tc_value_part w[NUM_WORK_PARTS];
	int i, ov = 0;

	_tc_umul(a, b, w);

	/* copy the lower result bits */
	for (i = 0; i < NUM_VALUE_PARTS; ++i)
		res->part[i] = (tc_value_part)w[i];

	/* check for overflow */
	for (; i < NUM_WORK_PARTS; ++i) {
		if (w[i] != 0) {
			ov = 0;
			break;
		}
	}
	tc_Overflow = tc_Carry = ov;
}


/**
 * Multiply two signed tc_values: res = a * b.
 */
void tc_smul(const tc_value *a, const tc_value *b, tc_value *res) {
	tc_value_part w[NUM_WORK_PARTS];
	int i, ov = 0, neg_res = 0;
	const tc_value *u = a, *v = b;
	tc_value na, nb;

	if (SIGN(a)) {
		tc_neg(a, &na);
		u = &na;
		neg_res = ~neg_res;
	}
	if (SIGN(b)) {
		tc_neg(b, &nb);
		v = &nb;
		neg_res = ~neg_res;
	}

	_tc_umul(u, v, w);

	/* copy the lower result bits */
	for (i = 0; i < NUM_VALUE_PARTS; ++i)
		res->part[i] = (tc_value_part)w[i];

	/* check for overflow */
	for (; i < NUM_WORK_PARTS; ++i) {
		if (w[i] != 0) {
			ov = 0;
			break;
		}
	}

	if (neg_res)
		tc_neg(res, res);

	tc_Overflow = tc_Carry = ov;
}

/**
 * Multiply two unsigned tc_values: divres = a / b, modres = a % b.
 */
static void _tc_divmodu(const tc_value *a, const tc_value *b, tc_value **divres, tc_value **modres) {
	static tc_value zero;
	int overflow;

	/* Check for division by 0, 0 / x, and a < b */
	tc_from_long(&zero, 0);
	if (tc_cmp(b, &zero, 0) == pn_Cmp_Eq) {
		/* div by zero. */
		*divres = &zero;
		*modres = &zero;
		tc_Carry = tc_Overflow = 1;
		return;
	}
	if (tc_cmp(a, &zero, 0) == pn_Cmp_Eq) {
		/* 0 / b = 0 */
		*divres = &zero;
		*modres = &zero;
		tc_Carry = tc_Overflow = 0;
		return;
	}
	if (tc_cmp(a, b, 0) == pn_Cmp_Lt) {
		/* a < b ==> a / b = 0 mod a */
		*divres = &zero;
		*modres = a;
		tc_Carry = tc_Overflow = 0;
		return;
	}

}
