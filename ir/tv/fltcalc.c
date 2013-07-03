/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    tarval floating point calculations
 * @date     2003-2013
 * @author   Mathias Heil, Michael Beck, Matthias Braun
 */
#include "fltcalc.h"
#include "strcalc.h"
#include "error.h"

#include <math.h>
#include <inttypes.h>
#include <float.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <stdbool.h>

#include "xmalloc.h"

static long double string_to_long_double(const char *str)
{
#if __STDC_VERSION__ >= 199901L || _POSIX_C_SOURCE >= 200112L
	return strtold(str, NULL);
#else
	return strtod(str, NULL);
#endif
}

static bool my_isnan(long double val)
{
#if __STDC_VERSION__ >= 199901L
	return isnan(val);
#else
	/* hopefully the compiler does not optimize aggressively (=incorrect) */
	return val != val;
#endif
}

static bool my_isinf(long double val)
{
#if __STDC_VERSION__ >= 199901L
	return isinf(val);
#else
	/* hopefully the compiler does not optimize aggressively (=incorrect) */
	return my_isnan(val-val) && !my_isnan(val);
#endif
}

/** The number of extra precision rounding bits */
#define ROUNDING_BITS 2

typedef union {
	volatile struct {
#ifdef WORDS_BIGENDIAN
		uint32_t high;
#else
		uint32_t low;
#endif
		uint32_t mid;
#ifdef WORDS_BIGENDIAN
		uint32_t low;
#else
		uint32_t high;
#endif
	} val_ld12;
	volatile struct {
#ifdef WORDS_BIGENDIAN
		uint32_t high;
#else
		uint32_t low;
#endif
#ifdef WORDS_BIGENDIAN
		uint32_t low;
#else
		uint32_t high;
#endif
	} val_ld8;
	volatile long double d;
} value_t;

#define CLEAR_BUFFER(buffer) memset(buffer, 0, calc_buffer_size)

/* our floating point value */
struct fp_value {
	float_descriptor_t desc;
	unsigned char      clss;
	char               sign;
	char               value[];  /* exp[value_size] + mant[value_size] */
};

#define _exp(a)  &((a)->value[0])
#define _mant(a) &((a)->value[value_size])

#define _save_result(x) memcpy((x), sc_get_buffer(), value_size)
#define _shift_right(x, y, res) sc_shr((x), (y), value_size*4, 0, (res))
#define _shift_left(x, y, res) sc_shl((x), (y), value_size*4, 0, (res))

/** A temporary buffer. */
static fp_value *calc_buffer = NULL;

/** Current rounding mode.*/
static fc_rounding_mode_t rounding_mode;

static int calc_buffer_size;
static int value_size;
static int max_precision;

/** Exact flag. */
static bool fc_exact = true;

static float_descriptor_t long_double_desc;

/** pack machine-like */
static void *pack(const fp_value *int_float, void *packed)
{
	switch ((value_class_t)int_float->clss) {
	case FC_NAN: {
		fp_value *val_buffer = (fp_value*) alloca(calc_buffer_size);
		fc_get_qnan(&int_float->desc, val_buffer);
		int_float = val_buffer;
		break;
	}

	case FC_INF: {
		fp_value *val_buffer = (fp_value*) alloca(calc_buffer_size);
		fc_get_plusinf(&int_float->desc, val_buffer);
		val_buffer->sign = int_float->sign;
		int_float = val_buffer;
		break;
	}

	default:
		break;
	}
	assert(int_float->desc.explicit_one <= 1);

	/* pack sign: move it to the left after exponent AND mantissa */
	char *temp = ALLOCAN(char, value_size);
	sc_val_from_ulong(int_float->sign, temp);

	int pos = int_float->desc.exponent_size + int_float->desc.mantissa_size + int_float->desc.explicit_one;
	sc_val_from_ulong(pos, NULL);
	_shift_left(temp, sc_get_buffer(), packed);

	/* pack exponent: move it to the left after mantissa */
	pos = int_float->desc.mantissa_size + int_float->desc.explicit_one;
	char *shift_val = ALLOCAN(char, value_size);
	sc_val_from_ulong(pos, shift_val);
	_shift_left(_exp(int_float), shift_val, temp);

	/* combine sign|exponent */
	sc_or(temp, packed, packed);

	/* extract mantissa */
	/* remove rounding bits */
	sc_val_from_ulong(ROUNDING_BITS, shift_val);
	_shift_right(_mant(int_float), shift_val, temp);

	/* remove leading 1 (or 0 if denormalized) */
	sc_max_from_bits(pos, 0, shift_val); /* all mantissa bits are 1's */
	sc_and(temp, shift_val, temp);

	/* combine sign|exponent|mantissa */
	sc_or(temp, packed, packed);

	return packed;
}

/**
 * Normalize a fp_value.
 *
 * @return true if result is exact
 */
static bool normalize(const fp_value *in_val, fp_value *out_val, bool sticky)
{
	/* save rounding bits at the end */
	int hsb = ROUNDING_BITS + in_val->desc.mantissa_size - sc_get_highest_set_bit(_mant(in_val)) - 1;

	if (in_val != out_val) {
		out_val->sign = in_val->sign;
		out_val->desc = in_val->desc;
	}

	out_val->clss = FC_NORMAL;

	/* mantissa all zeros, so zero exponent (because of explicit one) */
	if (hsb == ROUNDING_BITS + in_val->desc.mantissa_size) {
		sc_val_from_ulong(0, _exp(out_val));
		hsb = -1;
	}

	/* shift the first 1 into the left of the radix point (i.e. hsb == -1) */
	bool  exact = true;
	char *temp  = (char*) alloca(value_size);
	if (hsb < -1) {
		/* shift right */
		sc_val_from_ulong(-hsb-1, temp);

		_shift_right(_mant(in_val), temp, _mant(out_val));

		/* remember if some bits were shifted away */
		if (sc_had_carry()) {
			exact  = false;
			sticky = true;
		}
		sc_add(_exp(in_val), temp, _exp(out_val));
	} else if (hsb > -1) {
		/* shift left */
		sc_val_from_ulong(hsb+1, temp);

		_shift_left(_mant(in_val), temp, _mant(out_val));

		sc_sub(_exp(in_val), temp, _exp(out_val));
	}

	/* check for exponent underflow */
	if (sc_is_negative(_exp(out_val)) || sc_is_zero(_exp(out_val))) {
		/* exponent underflow */
		/* shift the mantissa right to have a zero exponent */
		sc_val_from_ulong(1, temp);
		sc_sub(temp, _exp(out_val), NULL);

		_shift_right(_mant(out_val), sc_get_buffer(), _mant(out_val));
		if (sc_had_carry()) {
			exact  = false;
			sticky = true;
		}
		/* denormalized means exponent of zero */
		sc_val_from_ulong(0, _exp(out_val));

		out_val->clss = FC_SUBNORMAL;
	}

	/* perform rounding by adding a value that clears the guard bit and the round bit
	 * and either causes a carry to round up or not */
	/* get the last 3 bits of the value */
	char lsb       = sc_sub_bits(_mant(out_val), out_val->desc.mantissa_size + ROUNDING_BITS, 0) & 0x7;
	char guard     = (lsb&0x2)>>1;
	char round     = lsb&0x1;
	char round_dir = 0;
	switch (rounding_mode) {
	case FC_TONEAREST:
		/* round to nearest representable value, if in doubt choose the version
		 * with lsb == 0 */
		round_dir = guard && (sticky || round || lsb>>2);
		break;
	case FC_TOPOSITIVE:
		/* if positive: round to one if the exact value is bigger, else to zero */
		round_dir = (!out_val->sign && (guard || round || sticky));
		break;
	case FC_TONEGATIVE:
		/* if negative: round to one if the exact value is bigger, else to zero */
		round_dir = (out_val->sign && (guard || round || sticky));
		break;
	case FC_TOZERO:
		/* always round to 0 (chopping mode) */
		round_dir = 0;
		break;
	}

	if (round_dir == 1) {
		guard = (round^guard)<<1;
		lsb = !(round || guard)<<2 | guard | round;
	} else {
		lsb = -((guard<<1) | round);
	}

	/* add the rounded value */
	if (lsb != 0) {
		sc_val_from_long(lsb, temp);
		sc_add(_mant(out_val), temp, _mant(out_val));
		exact = false;
	}

	/* could have rounded down to zero */
	if (sc_is_zero(_mant(out_val)) && (out_val->clss == FC_SUBNORMAL))
		out_val->clss = FC_ZERO;

	/* check for rounding overflow */
	hsb = ROUNDING_BITS + out_val->desc.mantissa_size - sc_get_highest_set_bit(_mant(out_val)) - 1;
	if ((out_val->clss != FC_SUBNORMAL) && (hsb < -1)) {
		sc_val_from_ulong(1, temp);
		_shift_right(_mant(out_val), temp, _mant(out_val));
		if (exact && sc_had_carry())
			exact = false;
		sc_add(_exp(out_val), temp, _exp(out_val));
	} else if ((out_val->clss == FC_SUBNORMAL) && (hsb == -1)) {
		/* overflow caused the mantissa to be normal again,
		 * so adapt the exponent accordingly */
		sc_val_from_ulong(1, temp);
		sc_add(_exp(out_val), temp, _exp(out_val));

		out_val->clss = FC_NORMAL;
	}
	/* no further rounding is needed, because rounding overflow means
	 * the carry of the original rounding was propagated all the way
	 * up to the bit left of the radix point. This implies the bits
	 * to the right are all zeros (rounding is +1) */

	/* check for exponent overflow */
	sc_val_from_ulong((1 << out_val->desc.exponent_size) - 1, temp);
	if (sc_comp(_exp(out_val), temp) != ir_relation_less) {
		/* exponent overflow, reaction depends on rounding method:
		 *
		 * mode        | sign of value |  result
		 *--------------------------------------------------------------
		 * TO_NEAREST  |      +        |   +inf
		 *             |      -        |   -inf
		 *--------------------------------------------------------------
		 * TO_POSITIVE |      +        |   +inf
		 *             |      -        |   smallest representable value
		 *--------------------------------------------------------------
		 * TO_NEAGTIVE |      +        |   largest representable value
		 *             |      -        |   -inf
		 *--------------------------------------------------------------
		 * TO_ZERO     |      +        |   largest representable value
		 *             |      -        |   smallest representable value
		 *--------------------------------------------------------------*/
		if (out_val->sign == 0) {
			/* value is positive */
			switch (rounding_mode) {
			case FC_TONEAREST:
			case FC_TOPOSITIVE:
				out_val->clss = FC_INF;
				break;

			case FC_TONEGATIVE:
			case FC_TOZERO:
				fc_get_max(&out_val->desc, out_val);
			}
		} else {
			/* value is negative */
			switch (rounding_mode) {
			case FC_TONEAREST:
			case FC_TONEGATIVE:
				out_val->clss = FC_INF;
				break;

			case FC_TOPOSITIVE:
			case FC_TOZERO:
				fc_get_min(&out_val->desc, out_val);
			}
		}
	}
	return exact;
}

/**
 * Operations involving NaN's must return NaN.
 * They are NOT exact.
 */
#define handle_NAN(a, b, result) \
do {                                                      \
  if (a->clss == FC_NAN) {                                \
    if (a != result) memcpy(result, a, calc_buffer_size); \
    fc_exact = false;                                     \
    return;                                               \
  }                                                       \
  if (b->clss == FC_NAN) {                                \
    if (b != result) memcpy(result, b, calc_buffer_size); \
    fc_exact = false;                                     \
    return;                                               \
  }                                                       \
} while (0)


/**
 * calculate a + b, where a is the value with the bigger exponent
 */
static void _fadd(const fp_value *a, const fp_value *b, fp_value *result)
{
	fc_exact = true;

	handle_NAN(a, b, result);

	/* make sure result has a descriptor */
	if (result != a && result != b)
		result->desc = a->desc;

	/* determine if this is an addition or subtraction */
	char sign = a->sign ^ b->sign;

	/* produce NaN on inf - inf */
	if (sign && (a->clss == FC_INF) && (b->clss == FC_INF)) {
		fc_exact = false;
		fc_get_qnan(&a->desc, result);
		return;
	}

	char *temp     = ALLOCAN(char, value_size);
	char *exp_diff = ALLOCAN(char, value_size);

	/* get exponent difference */
	sc_sub(_exp(a), _exp(b), exp_diff);

	/* initially set sign to be the sign of a, special treatment of subtraction
	 * when exponents are equal is required though.
	 * Also special care about the sign is needed when the mantissas are equal
	 * (+/- 0 ?) */
	char res_sign;
	if (sign && sc_val_to_long(exp_diff) == 0) {
		switch (sc_comp(_mant(a), _mant(b))) {
		case ir_relation_greater:  /* a > b */
			res_sign = a->sign;  /* abs(a) is bigger and a is negative */
			break;
		case ir_relation_equal:  /* a == b */
			res_sign = (rounding_mode == FC_TONEGATIVE);
			break;
		case ir_relation_less: /* a < b */
			res_sign = b->sign; /* abs(b) is bigger and b is negative */
			break;
		default:
			panic("invalid comparison result");
		}
	} else {
		res_sign = a->sign;
	}
	result->sign = res_sign;

	/* sign has been taken care of, check for special cases */
	if (a->clss == FC_ZERO || b->clss == FC_INF) {
		if (b != result)
			memcpy(result, b, calc_buffer_size);
		fc_exact = b->clss == FC_NORMAL;
		result->sign = res_sign;
		return;
	}
	if (b->clss == FC_ZERO || a->clss == FC_INF) {
		if (a != result)
			memcpy(result, a, calc_buffer_size);
		fc_exact = a->clss == FC_NORMAL;
		result->sign = res_sign;
		return;
	}

	/* shift the smaller value to the right to align the radix point */
	/* subnormals have their radix point shifted to the right,
	 * take care of this first */
	if ((b->clss == FC_SUBNORMAL) && (a->clss != FC_SUBNORMAL)) {
		sc_val_from_ulong(1, temp);
		sc_sub(exp_diff, temp, exp_diff);
	}

	_shift_right(_mant(b), exp_diff, temp);
	bool sticky = sc_had_carry();
	fc_exact &= !sticky;

	if (sticky && sign) {
		/* if subtracting a little more than the represented value or adding a
		 * little more than the represented value to a negative value this, in
		 * addition to the still set sticky bit, takes account of the
		 * 'little more' */
		char *temp1 = ALLOCAN(char, value_size);
		sc_val_from_ulong(1, temp1);
		sc_add(temp, temp1, temp);
	}

	if (sign) {
		if (sc_comp(_mant(a), temp) == ir_relation_less)
			sc_sub(temp, _mant(a), _mant(result));
		else
			sc_sub(_mant(a), temp, _mant(result));
	} else {
		sc_add(_mant(a), temp, _mant(result));
	}

	/* _normalize expects a 'normal' radix point, adding two subnormals
	 * results in a subnormal radix point -> shifting before normalizing */
	if ((a->clss == FC_SUBNORMAL) && (b->clss == FC_SUBNORMAL)) {
		sc_val_from_ulong(1, NULL);
		_shift_left(_mant(result), sc_get_buffer(), _mant(result));
	}

	/* resulting exponent is the bigger one */
	memmove(_exp(result), _exp(a), value_size);

	fc_exact &= normalize(result, result, sticky);
}

/**
 * calculate a * b
 */
static void _fmul(const fp_value *a, const fp_value *b, fp_value *result)
{
	fc_exact = true;

	handle_NAN(a, b, result);

	if (result != a && result != b)
		result->desc = a->desc;

	char res_sign;
	result->sign = res_sign = a->sign ^ b->sign;

	/* produce NaN on 0 * inf */
	if (a->clss == FC_ZERO) {
		if (b->clss == FC_INF) {
			fc_get_qnan(&a->desc, result);
			fc_exact = false;
		} else {
			if (a != result)
				memcpy(result, a, calc_buffer_size);
			result->sign = res_sign;
		}
		return;
	}
	if (b->clss == FC_ZERO) {
		if (a->clss == FC_INF) {
			fc_get_qnan(&a->desc, result);
			fc_exact = false;
		} else {
			if (b != result)
				memcpy(result, b, calc_buffer_size);
			result->sign = res_sign;
		}
		return;
	}

	if (a->clss == FC_INF) {
		fc_exact = false;
		if (a != result)
			memcpy(result, a, calc_buffer_size);
		result->sign = res_sign;
		return;
	}
	if (b->clss == FC_INF) {
		fc_exact = false;
		if (b != result)
			memcpy(result, b, calc_buffer_size);
		result->sign = res_sign;
		return;
	}

	/* exp = exp(a) + exp(b) - excess */
	sc_add(_exp(a), _exp(b), _exp(result));

	char *temp = ALLOCAN(char, value_size);
	sc_val_from_ulong((1 << (a->desc.exponent_size - 1)) - 1, temp);
	sc_sub(_exp(result), temp, _exp(result));

	/* mixed normal, subnormal values introduce an error of 1, correct it */
	if ((a->clss == FC_SUBNORMAL) ^ (b->clss == FC_SUBNORMAL)) {
		sc_val_from_ulong(1, temp);
		sc_add(_exp(result), temp, _exp(result));
	}

	sc_mul(_mant(a), _mant(b), _mant(result));

	/* realign result: after a multiplication the digits right of the radix
	 * point are the sum of the factors' digits after the radix point. As all
	 * values are normalized they both have the same amount of these digits,
	 * which has to be restored by proper shifting
	 * because of the rounding bits */
	sc_val_from_ulong(ROUNDING_BITS + result->desc.mantissa_size, temp);

	_shift_right(_mant(result), temp, _mant(result));
	bool sticky = sc_had_carry();
	fc_exact &= !sticky;

	fc_exact &= normalize(result, result, sticky);
}

/**
 * calculate a / b
 */
static void _fdiv(const fp_value *a, const fp_value *b, fp_value *result)
{
	fc_exact = true;

	handle_NAN(a, b, result);

	if (result != a && result != b)
		result->desc = a->desc;

	char res_sign;
	result->sign = res_sign = a->sign ^ b->sign;

	/* produce FC_NAN on 0/0 and inf/inf */
	if (a->clss == FC_ZERO) {
		if (b->clss == FC_ZERO) {
			/* 0/0 -> NaN */
			fc_get_qnan(&a->desc, result);
			fc_exact = false;
		} else {
			/* 0/x -> a */
			if (a != result)
				memcpy(result, a, calc_buffer_size);
			result->sign = res_sign;
		}
		return;
	}

	if (b->clss == FC_INF) {
		fc_exact = false;
		if (a->clss == FC_INF) {
			/* inf/inf -> NaN */
			fc_get_qnan(&a->desc, result);
		} else {
			/* x/inf -> 0 */
			sc_val_from_ulong(0, NULL);
			_save_result(_exp(result));
			_save_result(_mant(result));
			result->clss = FC_ZERO;
		}
		return;
	}

	if (a->clss == FC_INF) {
		fc_exact = false;
		/* inf/x -> inf */
		if (a != result)
			memcpy(result, a, calc_buffer_size);
		result->sign = res_sign;
		return;
	}
	if (b->clss == FC_ZERO) {
		fc_exact = false;
		/* division by zero */
		if (result->sign)
			fc_get_minusinf(&a->desc, result);
		else
			fc_get_plusinf(&a->desc, result);
		return;
	}

	/* exp = exp(a) - exp(b) + excess - 1*/
	char *temp = ALLOCAN(char, value_size);
	sc_sub(_exp(a), _exp(b), _exp(result));
	sc_val_from_ulong((1 << (a->desc.exponent_size - 1)) - 2, temp);
	sc_add(_exp(result), temp, _exp(result));

	/* mixed normal, subnormal values introduce an error of 1, correct it */
	if ((a->clss == FC_SUBNORMAL) ^ (b->clss == FC_SUBNORMAL)) {
		sc_val_from_ulong(1, temp);
		sc_add(_exp(result), temp, _exp(result));
	}

	/* mant(res) = mant(a) / 1/2mant(b) */
	/* to gain more bits of precision in the result the dividend could be
	 * shifted left, as this operation does not loose bits. This would not
	 * fit into the integer precision, but due to the rounding bits (which
	 * are always zero because the values are all normalized) the divisor
	 * can be shifted right instead to achieve the same result */
	sc_val_from_ulong(ROUNDING_BITS + result->desc.mantissa_size, temp);

	char *dividend = ALLOCAN(char, value_size);
	_shift_left(_mant(a), temp, dividend);

	char *divisor = ALLOCAN(char, value_size);
	sc_val_from_ulong(1, divisor);
	_shift_right(_mant(b), divisor, divisor);
	sc_div(dividend, divisor, _mant(result));
	bool sticky = sc_had_carry();
	fc_exact &= !sticky;

	fc_exact &= normalize(result, result, sticky);
}

/**
 * Truncate the fractional part away.
 *
 * This does not clip to any integer range.
 */
static void _trunc(const fp_value *a, fp_value *result)
{
	/* When exponent == 0 all bits left of the radix point
	 * are the integral part of the value. For 15bit exp_size
	 * this would require a left shift of max. 16383 bits which
	 * is too much.
	 * But it is enough to ensure that no bit right of the radix
	 * point remains set. This restricts the interesting
	 * exponents to the interval [0, mant_size-1].
	 * Outside this interval the truncated value is either 0 or
	 * it does not have fractional parts. */

	/* fixme: can be exact */
	fc_exact = false;

	if (a != result) {
		result->desc = a->desc;
		result->clss = a->clss;
	}

	int exp_bias = (1 << (a->desc.exponent_size - 1)) - 1;
	int exp_val  = sc_val_to_long(_exp(a)) - exp_bias;
	if (exp_val < 0) {
		sc_val_from_ulong(0, NULL);
		_save_result(_exp(result));
		_save_result(_mant(result));
		result->clss = FC_ZERO;
		return;
	}

	if (exp_val > (long)a->desc.mantissa_size) {
		if (a != result)
			memcpy(result, a, calc_buffer_size);
		return;
	}

	/* set up a proper mask to delete all bits right of the
	 * radix point if the mantissa had been shifted until exp == 0 */
	char *temp = ALLOCAN(char, value_size);
	sc_max_from_bits(1 + exp_val, 0, temp);
	sc_val_from_long(a->desc.mantissa_size - exp_val + 2, NULL);
	_shift_left(temp, sc_get_buffer(), temp);

	/* and the mask and return the result */
	sc_and(_mant(a), temp, _mant(result));

	if (a != result) {
		memcpy(_exp(result), _exp(a), value_size);
		result->sign = a->sign;
	}
}

/********
 * functions defined in fltcalc.h
 ********/
const void *fc_get_buffer(void)
{
	return calc_buffer;
}

int fc_get_buffer_length(void)
{
	return calc_buffer_size;
}

void *fc_val_from_str(const char *str, size_t len, void *result)
{
	char *buffer = alloca(len + 1);
	memcpy(buffer, str, len);
	buffer[len] = '\0';
	long double val = string_to_long_double(buffer);
	return fc_val_from_ieee754(val, result);
}

fp_value *fc_val_from_ieee754(long double l, fp_value *result)
{
	value_t  srcval;
	srcval.d = l;
	int  bias_res = ((1 << (long_double_desc.exponent_size - 1)) - 1);
	int  bias_val;
	int  mant_val;
	char sign;
	uint32_t exponent;
	uint32_t mantissa0;
	uint32_t mantissa1;
	if (long_double_desc.exponent_size == 11 && long_double_desc.mantissa_size == 52) {
		assert(sizeof(long double) == 8);
		mant_val  = 52;
		bias_val  = 0x3ff;
		sign      = (srcval.val_ld8.high & 0x80000000) != 0;
		exponent  = (srcval.val_ld8.high & 0x7FF00000) >> 20;
		mantissa0 = srcval.val_ld8.high & 0x000FFFFF;
		mantissa1 = srcval.val_ld8.low;
	} else if (long_double_desc.exponent_size == 15 && long_double_desc.mantissa_size == 63) {
		/* we assume an x86-like 80bit representation of the value... */
		assert(sizeof(long double) == 12 || sizeof(long double) == 16);
		mant_val  = 63;
		bias_val  = 0x3fff;
		sign      = (srcval.val_ld12.high & 0x00008000) != 0;
		exponent  = (srcval.val_ld12.high & 0x00007FFF) ;
		mantissa0 = srcval.val_ld12.mid;
		mantissa1 = srcval.val_ld12.low;
	} else {
		panic("unsupported long double format");
	}

	if (result == NULL)
		result = calc_buffer;

	/* CLEAR the buffer, else some bits might be uninitialized */
	memset(result, 0, fc_get_buffer_length());

	result->desc = long_double_desc;
	result->clss = FC_NORMAL;
	result->sign = sign;

	/* sign and flag suffice to identify NaN or inf, no exponent/mantissa
	 * encoding is needed. the function can return immediately in these cases */
	if (my_isnan(l)) {
		result->clss = FC_NAN;
		return result;
	} else if (my_isinf(l)) {
		result->clss = FC_INF;
		return result;
	}

	/* build exponent, because input and output exponent and mantissa sizes may
	 * differ this looks more complicated than it is:
	 * unbiased input exponent + output bias, minus the mantissa difference
	 * which is added again later when the output float becomes normalized */
	sc_val_from_long((exponent - bias_val + bias_res) - (mant_val - long_double_desc.mantissa_size), _exp(result));

	/* build mantissa representation */
	char *temp = ALLOCAN(char, value_size);
	if (exponent != 0) {
		/* insert the hidden bit */
		sc_val_from_ulong(1, temp);
		sc_val_from_ulong(mant_val + ROUNDING_BITS, NULL);
		_shift_left(temp, sc_get_buffer(), NULL);
	} else {
		sc_val_from_ulong(0, NULL);
	}

	_save_result(_mant(result));

	/* bits from the upper word */
	sc_val_from_ulong(mantissa0, temp);
	sc_val_from_ulong(34, NULL);
	_shift_left(temp, sc_get_buffer(), temp);
	sc_or(_mant(result), temp, _mant(result));

	/* bits from the lower word */
	sc_val_from_ulong(mantissa1, temp);
	sc_val_from_ulong(ROUNDING_BITS, NULL);
	_shift_left(temp, sc_get_buffer(), temp);
	sc_or(_mant(result), temp, _mant(result));

	/* _normalize expects the radix point to be normal, so shift mantissa of
	 * subnormal origin one to the left */
	if (exponent == 0) {
		sc_val_from_ulong(1, NULL);
		_shift_left(_mant(result), sc_get_buffer(), _mant(result));
	}

	normalize(result, result, 0);

	return result;
}

long double fc_val_to_ieee754(const fp_value *val)
{
	unsigned mantissa_size
		= long_double_desc.mantissa_size + long_double_desc.explicit_one;

	fp_value *temp  = (fp_value*) alloca(calc_buffer_size);
	fp_value *value = fc_cast(val, &long_double_desc, temp);

	uint32_t sign = value->sign;

	/* @@@ long double exponent is 15bit, so the use of sc_val_to_long should
	 * not lead to wrong results */
	uint32_t exponent = sc_val_to_long(_exp(value)) ;

	sc_val_from_ulong(ROUNDING_BITS, NULL);
	_shift_right(_mant(value), sc_get_buffer(), _mant(value));

	uint32_t mantissa0 = 0;
	uint32_t mantissa1 = 0;

	unsigned byte_offset;
	for (byte_offset = 0; byte_offset < 4; byte_offset++)
		mantissa1 |= sc_sub_bits(_mant(value), mantissa_size, byte_offset) << (byte_offset << 3);

	for (; (byte_offset<<3) < long_double_desc.mantissa_size; byte_offset++)
		mantissa0 |= sc_sub_bits(_mant(value), mantissa_size, byte_offset) << ((byte_offset - 4) << 3);

	value_t buildval;
	if (long_double_desc.exponent_size == 11 && long_double_desc.mantissa_size == 52) {
		assert(sizeof(long double) == 8);
		mantissa0 &= 0x000FFFFF;  /* get rid of garbage */
		buildval.val_ld8.high = sign << 31;
		buildval.val_ld8.high |= exponent << 20;
		buildval.val_ld8.high |= mantissa0;
		buildval.val_ld8.low = mantissa1;
	} else if (long_double_desc.exponent_size == 15 && long_double_desc.mantissa_size == 63) {
		/* we assume an x86-like 80bit representation of the value... */
		assert(sizeof(long double) == 12 || sizeof(long double) == 16);
		buildval.val_ld12.high = sign << 15;
		buildval.val_ld12.high |= exponent;
		buildval.val_ld12.mid = mantissa0;
		buildval.val_ld12.low = mantissa1;
	} else {
		panic("unsupported long double format");
	}

	return buildval.d;
}

fp_value *fc_cast(const fp_value *value, const float_descriptor_t *desc,
                  fp_value *result)
{
	if (result == NULL)
		result = calc_buffer;
	assert(value != result);

	if (value->desc.exponent_size == desc->exponent_size &&
		value->desc.mantissa_size == desc->mantissa_size &&
		value->desc.explicit_one  == desc->explicit_one) {
		if (value != result)
			memcpy(result, value, calc_buffer_size);
		return result;
	}

	if (value->clss == FC_NAN) {
		if (sc_get_highest_set_bit(_mant(value)) == value->desc.mantissa_size + 1)
			return fc_get_qnan(desc, result);
		else
			return fc_get_snan(desc, result);
	}
	else if (value->clss == FC_INF) {
		if (value->sign == 0)
			return fc_get_plusinf(desc, result);
		else
			return fc_get_minusinf(desc, result);
	}

	/* set the descriptor of the new value */
	result->desc = *desc;
	result->clss = value->clss;
	result->sign = value->sign;

	/* when the mantissa sizes differ normalizing has to shift to align it.
	 * this would change the exponent, which is unwanted. So calculate this
	 * offset and add it */
	int val_bias = (1 << (value->desc.exponent_size - 1)) - 1;
	int res_bias = (1 << (desc->exponent_size - 1)) - 1;

	int exp_offset = (res_bias - val_bias) - (value->desc.mantissa_size - desc->mantissa_size);
	char *temp = ALLOCAN(char, value_size);
	sc_val_from_long(exp_offset, temp);
	sc_add(_exp(value), temp, _exp(result));

	/* _normalize expects normalized radix point */
	if (value->clss == FC_SUBNORMAL) {
		sc_val_from_ulong(1, NULL);
		_shift_left(_mant(value), sc_get_buffer(), _mant(result));
	} else if (value != result) {
		memcpy(_mant(result), _mant(value), value_size);
	} else {
		memmove(_mant(result), _mant(value), value_size);
	}

	normalize(result, result, 0);
	return result;
}

fp_value *fc_get_max(const float_descriptor_t *desc, fp_value *result)
{
	if (result == NULL)
		result = calc_buffer;

	result->desc = *desc;
	result->clss = FC_NORMAL;
	result->sign = 0;

	sc_val_from_ulong((1 << desc->exponent_size) - 2, _exp(result));

	sc_max_from_bits(desc->mantissa_size + 1, 0, _mant(result));
	sc_val_from_ulong(ROUNDING_BITS, NULL);
	_shift_left(_mant(result), sc_get_buffer(), _mant(result));

	return result;
}

fp_value *fc_get_min(const float_descriptor_t *desc, fp_value *result)
{
	if (result == NULL)
		result = calc_buffer;

	fc_get_max(desc, result);
	result->sign = 1;

	return result;
}

fp_value *fc_get_snan(const float_descriptor_t *desc, fp_value *result)
{
	if (result == NULL)
		result = calc_buffer;

	result->desc = *desc;
	result->clss = FC_NAN;
	result->sign = 0;

	sc_val_from_ulong((1 << desc->exponent_size) - 1, _exp(result));

	/* signaling NaN has non-zero mantissa with msb not set */
	sc_val_from_ulong(1, _mant(result));

	return result;
}

fp_value *fc_get_qnan(const float_descriptor_t *desc, fp_value *result)
{
	if (result == NULL)
		result = calc_buffer;

	result->desc = *desc;
	result->clss = FC_NAN;
	result->sign = 0;

	sc_val_from_ulong((1 << desc->exponent_size) - 1, _exp(result));

	/* quiet NaN has the msb of the mantissa set, so shift one there */
	sc_val_from_ulong(1, _mant(result));
	/* mantissa_size >+< 1 because of two extra rounding bits */
	sc_val_from_ulong(desc->mantissa_size + 1, NULL);
	_shift_left(_mant(result), sc_get_buffer(), _mant(result));

	return result;
}

fp_value *fc_get_plusinf(const float_descriptor_t *desc, fp_value *result)
{
	if (result == NULL)
		result = calc_buffer;

	result->desc = *desc;
	result->clss = FC_INF;
	result->sign = 0;

	sc_val_from_ulong((1 << desc->exponent_size) - 1, _exp(result));

	char *mant = _mant(result);
	sc_val_from_ulong(0, mant);
	if (desc->explicit_one) {
		sc_set_bit_at(mant, result->desc.mantissa_size + ROUNDING_BITS);
	}

	return result;
}

fp_value *fc_get_minusinf(const float_descriptor_t *desc, fp_value *result)
{
	if (result == NULL)
		result = calc_buffer;

	fc_get_plusinf(desc, result);
	result->sign = 1;

	return result;
}

ir_relation fc_comp(fp_value const *const val_a, fp_value const *const val_b)
{
	/*
	 * shortcut: if both values are identical, they are either
	 * Unordered if NaN or equal
	 */
	if (val_a == val_b)
		return val_a->clss == FC_NAN ? ir_relation_unordered : ir_relation_equal;

	/* unordered if one is a NaN */
	if (val_a->clss == FC_NAN || val_b->clss == FC_NAN)
		return ir_relation_unordered;

	/* zero is equal independent of sign */
	if ((val_a->clss == FC_ZERO) && (val_b->clss == FC_ZERO))
		return ir_relation_equal;

	/* different signs make compare easy */
	if (val_a->sign != val_b->sign)
		return val_a->sign == 0 ? ir_relation_greater : ir_relation_less;

	ir_relation const mul = val_a->sign ? ir_relation_less_greater : ir_relation_false;

	/* both infinity means equality */
	if ((val_a->clss == FC_INF) && (val_b->clss == FC_INF))
		return ir_relation_equal;

	/* infinity is bigger than the rest */
	if (val_a->clss == FC_INF)
		return ir_relation_greater ^ mul;
	if (val_b->clss == FC_INF)
		return ir_relation_less ^ mul;

	/* check first exponent, that mantissa if equal */
	ir_relation rel = sc_comp(_exp(val_a), _exp(val_b));
	if (rel == ir_relation_equal)
		rel = sc_comp(_mant(val_a), _mant(val_b));
	if (rel != ir_relation_equal)
		rel ^= mul;
	return rel;
}

bool fc_is_zero(const fp_value *a)
{
	return a->clss == FC_ZERO;
}

bool fc_is_negative(const fp_value *a)
{
	return a->sign;
}

bool fc_is_inf(const fp_value *a)
{
	return a->clss == FC_INF;
}

bool fc_is_nan(const fp_value *a)
{
	return a->clss == FC_NAN;
}

bool fc_is_subnormal(const fp_value *a)
{
	return a->clss == FC_SUBNORMAL;
}

char *fc_print(const fp_value *val, char *buf, int buflen, unsigned base)
{
	long double flt_val;

	switch (base) {
	case FC_DEC:
		switch ((value_class_t)val->clss) {
		case FC_INF:
			snprintf(buf, buflen, "%cINF", val->sign ? '-' : '+');
			break;
		case FC_NAN:
			snprintf(buf, buflen, "NaN");
			break;
		case FC_ZERO:
			snprintf(buf, buflen, "0.0");
			break;
		default:
			flt_val = fc_val_to_ieee754(val);
			/* XXX 30 is arbitrary */
			snprintf(buf, buflen, "%.30LE", flt_val);
		}
		break;

	case FC_HEX:
		switch ((value_class_t)val->clss) {
		case FC_INF:
			snprintf(buf, buflen, "%cINF", val->sign ? '-' : '+');
			break;
		case FC_NAN:
			snprintf(buf, buflen, "NaN");
			break;
		case FC_ZERO:
			snprintf(buf, buflen, "0.0");
			break;
		default:
			flt_val = fc_val_to_ieee754(val);
			snprintf(buf, buflen, "%LA", flt_val);
		}
		break;

	case FC_PACKED:
	default: {
		char *mul_1 = (char*) alloca(calc_buffer_size);
		snprintf(buf, buflen, "%s", sc_print(pack(val, mul_1), value_size*4, SC_HEX, 0));
		buf[buflen - 1] = '\0';
		break;
	}
	}
	return buf;
}

unsigned char fc_sub_bits(const fp_value *value, unsigned num_bits,
                          unsigned byte_ofs)
{
	/* this is used to cache the packed version of the value */
	static char *packed_value = NULL;
	if (packed_value == NULL)
		packed_value = XMALLOCN(char, value_size);

	if (value != NULL)
		pack(value, packed_value);

	return sc_sub_bits(packed_value, num_bits, byte_ofs);
}

bool fc_zero_mantissa(const fp_value *value)
{
	return sc_get_lowest_set_bit(_mant(value)) == ROUNDING_BITS + value->desc.mantissa_size;
}

int fc_get_exponent(const fp_value *value)
{
	int exp_bias = (1 << (value->desc.exponent_size - 1)) - 1;
	return sc_val_to_long(_exp(value)) - exp_bias;
}

bool fc_can_lossless_conv_to(const fp_value *value,
                             const float_descriptor_t *desc)
{
	/* handle some special cases first */
	switch (value->clss) {
	case FC_ZERO:
	case FC_INF:
	case FC_NAN:
		return true;
	default:
		break;
	}

	/* check if the exponent can be encoded: note, 0 and all ones are reserved
	 * for the exponent */
	int exp_bias = (1 << (desc->exponent_size - 1)) - 1;
	int v        = fc_get_exponent(value) + exp_bias;
	if (0 < v && v < (1 << desc->exponent_size) - 1) {
		/* exponent can be encoded, now check the mantissa */
		v = value->desc.mantissa_size + ROUNDING_BITS - sc_get_lowest_set_bit(_mant(value));
		return v <= (int)desc->mantissa_size;
	}
	return false;
}


fc_rounding_mode_t fc_set_rounding_mode(fc_rounding_mode_t mode)
{
	if (mode == FC_TONEAREST || mode == FC_TOPOSITIVE || mode == FC_TONEGATIVE
	 || mode == FC_TOZERO)
		rounding_mode = mode;

	return rounding_mode;
}

fc_rounding_mode_t fc_get_rounding_mode(void)
{
	return rounding_mode;
}

void init_fltcalc(int precision)
{
	if (calc_buffer == NULL) {
		/* does nothing if already init */
		if (precision == 0)
			precision = FC_DEFAULT_PRECISION;

		init_strcalc(precision + 2 + ROUNDING_BITS);

		/* needs additionally rounding bits, one bit as explicit 1., and one for
		 * addition overflow */
		max_precision = sc_get_precision() - (2 + ROUNDING_BITS);
		if (max_precision < precision)
			printf("WARNING: not enough precision available, using %d\n", max_precision);

		rounding_mode    = FC_TONEAREST;
		value_size       = sc_get_buffer_length();
		calc_buffer_size = sizeof(fp_value) + 2*value_size;

		calc_buffer = (fp_value*) xmalloc(calc_buffer_size);
		memset(calc_buffer, 0, calc_buffer_size);

		const size_t long_double_size = sizeof(long double);
#if LDBL_MANT_DIG == 64
		assert(long_double_size == 12 || long_double_size == 16);
		long_double_desc = (float_descriptor_t) { 15, 63, 1 };
#elif LDBL_MANT_DIG == 53
		assert(long_double_size == 8);
		long_double_desc = (float_descriptor_t) { 11, 52, 0 };
#else
	#error "Unsupported long double format"
#endif
	}
}

void finish_fltcalc(void)
{
	free(calc_buffer);
	calc_buffer = NULL;
}

/* definition of interface functions */
fp_value *fc_add(const fp_value *a, const fp_value *b, fp_value *result)
{
	if (result == NULL)
		result = calc_buffer;

	/* make the value with the bigger exponent the first one */
	if (sc_comp(_exp(a), _exp(b)) == ir_relation_less)
		_fadd(b, a, result);
	else
		_fadd(a, b, result);

	return result;
}

fp_value *fc_sub(const fp_value *a, const fp_value *b, fp_value *result)
{
	if (result == NULL)
		result = calc_buffer;

	fp_value *temp = (fp_value*) alloca(calc_buffer_size);
	memcpy(temp, b, calc_buffer_size);
	temp->sign = !b->sign;
	if (sc_comp(_exp(a), _exp(temp)) == ir_relation_less)
		_fadd(temp, a, result);
	else
		_fadd(a, temp, result);

	return result;
}

fp_value *fc_mul(const fp_value *a, const fp_value *b, fp_value *result)
{
	if (result == NULL)
		result = calc_buffer;

	_fmul(a, b, result);

	return result;
}

fp_value *fc_div(const fp_value *a, const fp_value *b, fp_value *result)
{
	if (result == NULL)
		result = calc_buffer;

	_fdiv(a, b, result);

	return result;
}

fp_value *fc_neg(const fp_value *a, fp_value *result)
{
	if (result == NULL)
		result = calc_buffer;

	if (a != result)
		memcpy(result, a, calc_buffer_size);
	result->sign = !a->sign;

	return result;
}

fp_value *fc_int(const fp_value *a, fp_value *result)
{
	if (result == NULL)
		result = calc_buffer;

	_trunc(a, result);

	return result;
}

fp_value *fc_rnd(const fp_value *a, fp_value *result)
{
	(void)a;
	(void)result;
	panic("not yet implemented");
}

bool fc_flt2int(const fp_value *a, void *result, ir_mode *dst_mode)
{
	if (a->clss == FC_NORMAL) {
		if (a->sign && !mode_is_signed(dst_mode)) {
			/* FIXME: for now we cannot convert this */
			return false;
		}

		int tgt_bits = get_mode_size_bits(dst_mode);
		if (mode_is_signed(dst_mode))
			--tgt_bits;

		int exp_bias = (1 << (a->desc.exponent_size - 1)) - 1;
		int exp_val  = sc_val_to_long(_exp(a)) - exp_bias;
		assert(exp_val >= 0 && "floating point value not integral before fc_flt2int() call");
		int mantissa_size = a->desc.mantissa_size + ROUNDING_BITS;
		int shift         = exp_val - mantissa_size;

		if (tgt_bits < mantissa_size + 1)
			tgt_bits = mantissa_size + 1;
		if (shift > 0) {
			sc_shlI(_mant(a),  shift, tgt_bits, 0, result);
		} else {
			sc_shrI(_mant(a), -shift, tgt_bits, 0, result);
		}

		/* check for overflow */
		int highest = sc_get_highest_set_bit(result);

		if (mode_is_signed(dst_mode)) {
			if (highest == sc_get_lowest_set_bit(result)) {
				/* need extra test for MIN_INT */
				if (highest >= (int) get_mode_size_bits(dst_mode)) {
					/* FIXME: handle overflow */
					return false;
				}
			} else {
				if (highest >= (int) get_mode_size_bits(dst_mode) - 1) {
					/* FIXME: handle overflow */
					return false;
				}
			}
		} else {
			if (highest >= (int) get_mode_size_bits(dst_mode)) {
				/* FIXME: handle overflow */
				return false;
			}
		}

		if (a->sign)
			sc_neg(result, result);

		return true;
	} else if (a->clss == FC_ZERO) {
		sc_zero(result);
		return true;
	}
	return false;
}

int fc_is_exact(void)
{
	return fc_exact;
}
