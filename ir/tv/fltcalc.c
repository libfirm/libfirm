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
 * @brief    tarval floating point calculations
 * @date     2003
 * @author   Mathias Heil
 * @version  $Id$
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "fltcalc.h"
#include "strcalc.h"

#include <math.h>    /* need isnan() and isinf() (will be changed)*/
/* undef some reused constants defined by math.h */
#ifdef NAN
#  undef NAN
#endif

#ifdef HAVE_INTTYPES_H
# include <inttypes.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#include <stdio.h>
#include <assert.h>

#include "xmalloc.h"

typedef uint32_t UINT32;

#ifdef HAVE_LONG_DOUBLE
#ifdef WORDS_BIGENDIAN
typedef union {
	struct {
		UINT32 high;
		UINT32 mid;
		UINT32 low;
	} val;
	volatile long double d;
} value_t;
#else
typedef union {
	struct {
		UINT32 low;
		UINT32 mid;
		UINT32 high;
	} val;
	volatile long double d;
} value_t;
#endif
#else
#ifdef WORDS_BIGENDIAN
typedef union {
	struct {
		UINT32 high;
		UINT32 low;
	} val;
	volatile double d;
} value_t;
#else
typedef union {
	struct {
		UINT32 low;
		UINT32 high;
	} val;
	volatile double d;
} value_t;
#endif
#endif

/**
 * possible float states
 */
typedef enum {
	NORMAL,       /**< normal representation, implicit 1 */
	ZERO,         /**< +/-0 */
	SUBNORMAL,    /**< denormals, implicit 0 */
	INF,          /**< +/-oo */
	NAN,          /**< Not A Number */
} value_class_t;

/** A descriptor for an IEEE float value. */
typedef struct {
	unsigned char exponent_size;    /**< size of exponent in bits */
	unsigned char mantissa_size;    /**< size of mantissa in bits */
	value_class_t clss;             /**< state of this float */
} descriptor_t;

#define CLEAR_BUFFER(buffer) memset(buffer, 0, calc_buffer_size)

/* our floating point value */
struct _fp_value {
	descriptor_t desc;
	char sign;
	char value[1];			/* exp[value_size] + mant[value_size] */
};

#define _exp(a)  &((a)->value[0])
#define _mant(a) &((a)->value[value_size])

#define _save_result(x) memcpy((x), sc_get_buffer(), value_size)
#define _shift_right(x, y, b) sc_shr((x), (y), value_size*4, 0, (b))
#define _shift_left(x, y, b) sc_shl((x), (y), value_size*4, 0, (b))


#ifdef FLTCALC_DEBUG
#  define DEBUGPRINTF(x) printf x
#else
#  define DEBUGPRINTF(x) ((void)0)
#endif

#ifdef FLTCALC_TRACE_CALC
#  define TRACEPRINTF(x) printf x
#else
#  define TRACEPRINTF(x) ((void)0)
#endif

/** The immediate precision. */
static unsigned immediate_prec = 0;

/** A temporal buffer. */
static fp_value *calc_buffer = NULL;

/** Current rounding mode.*/
static fc_rounding_mode_t rounding_mode;

static int calc_buffer_size;
static int value_size;
static int max_precision;

/** Exact flag. */
static int fc_exact = 1;

#if 0
static void fail_char(const char *str, unsigned int len, int pos) {
	if (*(str+pos))
		printf("ERROR: Unexpected character '%c'\n", *(str + pos));
	else
		printf("ERROR: Unexpected end of string\n");
	while (len-- && *str) printf("%c", *str++); printf("\n");
	while (pos--) printf(" "); printf("^\n");
	/* the front end has to to check constant strings */
	exit(-1);
}
#endif

/** pack machine-like */
static void *pack(const fp_value *int_float, void *packed) {
	char *shift_val;
	char *temp;
	fp_value *val_buffer;

	temp = alloca(value_size);
	shift_val = alloca(value_size);

	switch (int_float->desc.clss) {
	case NAN:
		val_buffer = alloca(calc_buffer_size);
		fc_get_qnan(int_float->desc.exponent_size, int_float->desc.mantissa_size, val_buffer);
		int_float = val_buffer;
		break;

	case INF:
		val_buffer = alloca(calc_buffer_size);
		fc_get_plusinf(int_float->desc.exponent_size, int_float->desc.mantissa_size, val_buffer);
		val_buffer->sign = int_float->sign;
		int_float = val_buffer;
		break;

	default:
		break;
	}
	/* pack sign */
	sc_val_from_ulong(int_float->sign, temp);

	sc_val_from_ulong(int_float->desc.exponent_size + int_float->desc.mantissa_size, NULL);
	_shift_left(temp, sc_get_buffer(), packed);

	/* extract exponent */
	sc_val_from_ulong(int_float->desc.mantissa_size, shift_val);

	_shift_left(_exp(int_float), shift_val, temp);

	sc_or(temp, packed, packed);

	/* extract mantissa */
	/* remove 2 rounding bits */
	sc_val_from_ulong(2, shift_val);
	_shift_right(_mant(int_float), shift_val, temp);

	/* remove leading 1 (or 0 if denormalized) */
	sc_max_from_bits(int_float->desc.mantissa_size, 0, shift_val); /* all mantissa bits are 1's */
	sc_and(temp, shift_val, temp);

	/* save result */
	sc_or(temp, packed, packed);

	return packed;
}

/**
 * Normalize a fp_value.
 *
 * @return non-zero if result is exact
 */
static int normalize(const fp_value *in_val, fp_value *out_val, int sticky) {
	int exact = 1;
	int hsb;
	char lsb, guard, round, round_dir = 0;
	char *temp = alloca(value_size);

	/* +2: save two rounding bits at the end */
	hsb = 2 + in_val->desc.mantissa_size - sc_get_highest_set_bit(_mant(in_val)) - 1;

	if (in_val != out_val)   {
		out_val->sign = in_val->sign;
		memcpy(&out_val->desc, &in_val->desc, sizeof(out_val->desc));
	}

	out_val->desc.clss = NORMAL;

	/* mantissa all zeros, so zero exponent (because of explicit one) */
	if (hsb == 2 + in_val->desc.mantissa_size)   {
		sc_val_from_ulong(0, _exp(out_val));
		hsb = -1;
	}

	/* shift the first 1 into the left of the radix point (i.e. hsb == -1) */
	if (hsb < -1)   {
		/* shift right */
		sc_val_from_ulong(-hsb-1, temp);

		_shift_right(_mant(in_val), temp, _mant(out_val));

		/* remember if some bits were shifted away */
		if (sc_had_carry()) {
			exact = 0;
			sticky = 1;
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
		DEBUGPRINTF(("Exponent underflow!\n"));
		/* exponent underflow */
		/* shift the mantissa right to have a zero exponent */
		sc_val_from_ulong(1, temp);
		sc_sub(temp, _exp(out_val), NULL);

		_shift_right(_mant(out_val), sc_get_buffer(), _mant(out_val));
		if (sc_had_carry()) {
			exact  = 0;
			sticky = 1;
		}
		/* denormalized means exponent of zero */
		sc_val_from_ulong(0, _exp(out_val));

		out_val->desc.clss = SUBNORMAL;
	}

	/* perform rounding by adding a value that clears the guard bit and the round bit
	 * and either causes a carry to round up or not */
	/* get the last 3 bits of the value */
	lsb = sc_sub_bits(_mant(out_val), out_val->desc.mantissa_size + 2, 0) & 0x7;
	guard = (lsb&0x2)>>1;
	round = lsb&0x1;

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
	DEBUGPRINTF(("Rounding (s%d, l%d, g%d, r%d, s%d) %s\n", out_val->sign, lsb>>2, guard, round, sticky, (round_dir)?"up":"down"));

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
		exact = 0;
	}

	/* could have rounded down to zero */
	if (sc_is_zero(_mant(out_val)) && (out_val->desc.clss == SUBNORMAL))
		out_val->desc.clss = ZERO;

	/* check for rounding overflow */
	hsb = 2 + out_val->desc.mantissa_size - sc_get_highest_set_bit(_mant(out_val)) - 1;
	if ((out_val->desc.clss != SUBNORMAL) && (hsb < -1)) {
		sc_val_from_ulong(1, temp);
		_shift_right(_mant(out_val), temp, _mant(out_val));
		if (exact && sc_had_carry())
			exact = 0;
		sc_add(_exp(out_val), temp, _exp(out_val));
	} else if ((out_val->desc.clss == SUBNORMAL) && (hsb == -1)) {
		/* overflow caused the mantissa to be normal again,
		 * so adapt the exponent accordingly */
		sc_val_from_ulong(1, temp);
		sc_add(_exp(out_val), temp, _exp(out_val));

		out_val->desc.clss = NORMAL;
	}
	/* no further rounding is needed, because rounding overflow means
	 * the carry of the original rounding was propagated all the way
	 * up to the bit left of the radix point. This implies the bits
	 * to the right are all zeros (rounding is +1) */

	/* check for exponent overflow */
	sc_val_from_ulong((1 << out_val->desc.exponent_size) - 1, temp);
	if (sc_comp(_exp(out_val), temp) != -1) {
		DEBUGPRINTF(("Exponent overflow!\n"));
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
				out_val->desc.clss = INF;
				break;

			case FC_TONEGATIVE:
			case FC_TOZERO:
				fc_get_max(out_val->desc.exponent_size, out_val->desc.mantissa_size, out_val);
			}
		} else {
			/* value is negative */
			switch (rounding_mode) {
			case FC_TONEAREST:
			case FC_TONEGATIVE:
				out_val->desc.clss = INF;
				break;

			case FC_TOPOSITIVE:
			case FC_TOZERO:
				fc_get_min(out_val->desc.exponent_size, out_val->desc.mantissa_size, out_val);
			}
		}
	}
	return exact;
}

/**
 * Operations involving NaN's must return NaN
 */
#define handle_NAN(a, b, result) \
do {                                                      \
  if (a->desc.clss == NAN) {                              \
    if (a != result) memcpy(result, a, calc_buffer_size); \
    return;                                               \
  }                                                       \
  if (b->desc.clss == NAN) {                              \
    if (b != result) memcpy(result, b, calc_buffer_size); \
    return;                                               \
  }                                                       \
}while (0)


/**
 * calculate a + b, where a is the value with the bigger exponent
 */
static void _fadd(const fp_value *a, const fp_value *b, fp_value *result) {
	char *temp;
	char *exp_diff;

	char sign, res_sign;
	char sticky;

	fc_exact = 1;

	handle_NAN(a, b, result);

	/* make sure result has a descriptor */
	if (result != a && result != b)
		memcpy(&result->desc, &a->desc, sizeof(descriptor_t));

	/* determine if this is an addition or subtraction */
	sign = a->sign ^ b->sign;

	/* produce NaN on inf - inf */
	if (sign && (a->desc.clss == INF) && (b->desc.clss == INF)) {
		fc_get_qnan(a->desc.exponent_size, b->desc.mantissa_size, result);
		return;
	}

	temp     = alloca(value_size);
	exp_diff = alloca(value_size);

	/* get exponent difference */
	sc_sub(_exp(a), _exp(b), exp_diff);

	/* initially set sign to be the sign of a, special treatment of subtraction
	 * when exponents are equal is required though.
	 * Also special care about the sign is needed when the mantissas are equal
	 * (+/- 0 ?) */
	if (sign && sc_val_to_long(exp_diff) == 0) {
		switch (sc_comp(_mant(a), _mant(b))) {
		case 1:  /* a > b */
			res_sign = a->sign;  /* abs(a) is bigger and a is negative */
			break;
		case 0:  /* a == b */
			res_sign = (rounding_mode == FC_TONEGATIVE);
			break;
		case -1: /* a < b */
			res_sign = b->sign; /* abs(b) is bigger and b is negative */
			break;
		default:
			/* can't be reached */
			res_sign = 0;
			break;
		}
	}
	else
		res_sign = a->sign;
	result->sign = res_sign;

	/* sign has been taken care of, check for special cases */
	if (a->desc.clss == ZERO || b->desc.clss == INF) {
		if (b != result)
			memcpy(result, b, calc_buffer_size);
		result->sign = res_sign;
		return;
	}
	if (b->desc.clss == ZERO || a->desc.clss == INF) {
		if (a != result)
			memcpy(result, a, calc_buffer_size);
		result->sign = res_sign;
		return;
	}

	/* shift the smaller value to the right to align the radix point */
	/* subnormals have their radix point shifted to the right,
	 * take care of this first */
	if ((b->desc.clss == SUBNORMAL) && (a->desc.clss != SUBNORMAL)) {
		sc_val_from_ulong(1, temp);
		sc_sub(exp_diff, temp, exp_diff);
	}

	_shift_right(_mant(b), exp_diff, temp);
	sticky = sc_had_carry();
	fc_exact &= !sticky;

	if (sticky && sign) {
		/* if subtracting a little more than the represented value or adding a little
		 * more than the represented value to a negative value this, in addition to the
		 * still set sticky bit, takes account of the 'little more' */
		char *temp1 = alloca(calc_buffer_size);
		sc_val_from_ulong(1, temp1);
		sc_add(temp, temp1, temp);
	}

	if (sign) {
		if (sc_comp(_mant(a), temp) == -1)
			sc_sub(temp, _mant(a), _mant(result));
		else
			sc_sub(_mant(a), temp, _mant(result));
	} else {
		sc_add(_mant(a), temp, _mant(result));
	}

	/* _normalize expects a 'normal' radix point, adding two subnormals
	 * results in a subnormal radix point -> shifting before normalizing */
	if ((a->desc.clss == SUBNORMAL) && (b->desc.clss == SUBNORMAL)) {
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
static void _fmul(const fp_value *a, const fp_value *b, fp_value *result) {
	int sticky;
	char *temp;
	char res_sign;

	fc_exact = 1;

	handle_NAN(a, b, result);

	temp = alloca(value_size);

	if (result != a && result != b)
		memcpy(&result->desc, &a->desc, sizeof(descriptor_t));

	result->sign = res_sign = a->sign ^ b->sign;

	/* produce NaN on 0 * inf */
	if (a->desc.clss == ZERO) {
		if (b->desc.clss == INF)
			fc_get_qnan(a->desc.exponent_size, a->desc.mantissa_size, result);
		else {
			if (a != result)
				memcpy(result, a, calc_buffer_size);
			result->sign = res_sign;
		}
		return;
	}
	if (b->desc.clss == ZERO) {
		if (a->desc.clss == INF)
			fc_get_qnan(a->desc.exponent_size, a->desc.mantissa_size, result);
		else {
			if (b != result)
				memcpy(result, b, calc_buffer_size);
			result->sign = res_sign;
		}
		return;
	}

	if (a->desc.clss == INF) {
		if (a != result)
			memcpy(result, a, calc_buffer_size);
		result->sign = res_sign;
		return;
	}
	if (b->desc.clss == INF) {
		if (b != result)
			memcpy(result, b, calc_buffer_size);
		result->sign = res_sign;
		return;
	}

	/* exp = exp(a) + exp(b) - excess */
	sc_add(_exp(a), _exp(b), _exp(result));

	sc_val_from_ulong((1 << (a->desc.exponent_size - 1)) - 1, temp);
	sc_sub(_exp(result), temp, _exp(result));

	/* mixed normal, subnormal values introduce an error of 1, correct it */
	if ((a->desc.clss == SUBNORMAL) ^ (b->desc.clss == SUBNORMAL)) {
		sc_val_from_ulong(1, temp);
		sc_add(_exp(result), temp, _exp(result));
	}

	sc_mul(_mant(a), _mant(b), _mant(result));

	/* realign result: after a multiplication the digits right of the radix
	 * point are the sum of the factors' digits after the radix point. As all
	 * values are normalized they both have the same amount of these digits,
	 * which has to be restored by proper shifting
	 * +2 because of the two rounding bits */
	sc_val_from_ulong(2 + result->desc.mantissa_size, temp);

	_shift_right(_mant(result), temp, _mant(result));
	sticky = sc_had_carry();
	fc_exact &= !sticky;

	fc_exact &= normalize(result, result, sticky);
}

/**
 * calculate a / b
 */
static void _fdiv(const fp_value *a, const fp_value *b, fp_value *result) {
	int sticky;
	char *temp, *dividend;
	char res_sign;

	fc_exact = 1;

	handle_NAN(a, b, result);

	temp = alloca(value_size);
	dividend = alloca(value_size);

	if (result != a && result != b)
		memcpy(&result->desc, &a->desc, sizeof(descriptor_t));

	result->sign = res_sign = a->sign ^ b->sign;

	/* produce NAN on 0/0 and inf/inf */
	if (a->desc.clss == ZERO) {
		if (b->desc.clss == ZERO)
			/* 0/0 -> nan */
			fc_get_qnan(a->desc.exponent_size, a->desc.mantissa_size, result);
		else {
			/* 0/x -> a */
			if (a != result)
				memcpy(result, a, calc_buffer_size);
			result->sign = res_sign;
		}
		return;
	}

	if (b->desc.clss == INF) {
		if (a->desc.clss == INF)
			/* inf/inf -> nan */
			fc_get_qnan(a->desc.exponent_size, a->desc.mantissa_size, result);
		else {
			/* x/inf -> 0 */
			sc_val_from_ulong(0, NULL);
			_save_result(_exp(result));
			_save_result(_mant(result));
			result->desc.clss = ZERO;
		}
		return;
	}

	if (a->desc.clss == INF) {
		/* inf/x -> inf */
		if (a != result)
			memcpy(result, a, calc_buffer_size);
		result->sign = res_sign;
		return;
	}
	if (b->desc.clss == ZERO) {
		/* division by zero */
		if (result->sign)
			fc_get_minusinf(a->desc.exponent_size, a->desc.mantissa_size, result);
		else
			fc_get_plusinf(a->desc.exponent_size, a->desc.mantissa_size, result);
		return;
	}

	/* exp = exp(a) - exp(b) + excess - 1*/
	sc_sub(_exp(a), _exp(b), _exp(result));
	sc_val_from_ulong((1 << (a->desc.exponent_size - 1)) - 2, temp);
	sc_add(_exp(result), temp, _exp(result));

	/* mixed normal, subnormal values introduce an error of 1, correct it */
	if ((a->desc.clss == SUBNORMAL) ^ (b->desc.clss == SUBNORMAL)) {
		sc_val_from_ulong(1, temp);
		sc_add(_exp(result), temp, _exp(result));
	}

	/* mant(res) = mant(a) / 1/2mant(b) */
	/* to gain more bits of precision in the result the dividend could be
	 * shifted left, as this operation does not loose bits. This would not
	 * fit into the integer precision, but due to the rounding bits (which
	 * are always zero because the values are all normalized) the divisor
	 * can be shifted right instead to achieve the same result */
	sc_val_from_ulong(2 + result->desc.mantissa_size, temp);

	_shift_left(_mant(a), temp, dividend);

	{
		char *divisor = alloca(calc_buffer_size);
		sc_val_from_ulong(1, divisor);
		_shift_right(_mant(b), divisor, divisor);
		sc_div(dividend, divisor, _mant(result));
		sticky = sc_had_carry();
		fc_exact &= !sticky;
	}

	fc_exact &= normalize(result, result, sticky);
}

#if 0
static void _power_of_ten(int exp, descriptor_t *desc, char *result) {
	char *build;
	char *temp;

	/* positive sign */
	result->sign = 0;

	/* set new descriptor (else result is supposed to already have one) */
	if (desc != NULL)
		memcpy(&result->desc, desc, sizeof(descriptor_t));

	build = alloca(value_size);
	temp = alloca(value_size);

	sc_val_from_ulong((1 << result->desc.exponent_size)/2-1, _exp(result));

	if (exp > 0) {
		/* temp is value of ten now */
		sc_val_from_ulong(10, NULL);
		_save_result(temp);

		for (exp--; exp > 0; exp--) {
			_save_result(build);
			sc_mul(build, temp, NULL);
		}
		_save_result(build);

		/* temp is amount of left shift needed to put the value left of the radix point */
		sc_val_from_ulong(result->desc.mantissa_size + 2, temp);

		_shift_left(build, temp, _mant(result));

		_normalize(result, result, 0);
	}
}
#endif

/**
 * Truncate the fractional part away.
 *
 * This does not clip to any integer range.
 */
static void _trunc(const fp_value *a, fp_value *result) {
	/*
	 * When exponent == 0 all bits left of the radix point
	 * are the integral part of the value. For 15bit exp_size
	 * this would require a left shift of max. 16383 bits which
	 * is too much.
	 * But it is enough to ensure that no bit right of the radix
	 * point remains set. This restricts the interesting
	 * exponents to the interval [0, mant_size-1].
	 * Outside this interval the truncated value is either 0 or
	 * it does not have fractional parts.
	 */

	int exp_bias, exp_val;
	char *temp;

	/* fixme: can be exact */
	fc_exact = 0;

	temp = alloca(value_size);

	if (a != result)
		memcpy(&result->desc, &a->desc, sizeof(descriptor_t));

	exp_bias = (1 << (a->desc.exponent_size - 1)) - 1;
	exp_val  = sc_val_to_long(_exp(a)) - exp_bias;

	if (exp_val < 0) {
		sc_val_from_ulong(0, NULL);
		_save_result(_exp(result));
		_save_result(_mant(result));
		result->desc.clss = ZERO;

		return;
	}

	if (exp_val > a->desc.mantissa_size) {
		if (a != result)
			memcpy(result, a, calc_buffer_size);

		return;
	}

	/* set up a proper mask to delete all bits right of the
	 * radix point if the mantissa had been shifted until exp == 0 */
	sc_max_from_bits(1 + exp_val, 0, temp);
	sc_val_from_long(a->desc.mantissa_size - exp_val + 2, NULL);
	_shift_left(temp, sc_get_buffer(), temp);

	/* and the mask and return the result */
	sc_and(_mant(a), temp, _mant(result));

	if (a != result) memcpy(_exp(result), _exp(a), value_size);

	return;
}

/********
 * functions defined in fltcalc.h
 ********/
const void *fc_get_buffer(void) {
	return calc_buffer;
}

int fc_get_buffer_length(void) {
	return calc_buffer_size;
}

void *fc_val_from_str(const char *str, unsigned int len, char exp_size, char mant_size, void *result) {
#if 0
	enum {
		START,
		LEFT_OF_DOT,
		RIGHT_OF_DOT,
		EXP_START,
		EXPONENT,
		END
	};

	char exp_sign;
	int exp_int, hsb, state;

	const char *old_str;

	int pos;
	char *mant_str, *exp_val, *power_val;

	(void) len;
	if (result == NULL) result = calc_buffer;

	exp_val = alloca(value_size);
	power_val = alloca(calc_buffer_size);
	mant_str = alloca((len)?(len):(strlen(str)));

	result->desc.exponent_size = exp_size;
	result->desc.mantissa_size = mant_size;
	result->desc.clss = NORMAL;

	old_str = str;
	pos = 0;
	exp_int = 0;
	state = START;

	while (len == 0 || str-old_str < len) {
		switch (state) {
		case START:
			switch (*str) {
			case '+':
				result->sign = 0;
				state = LEFT_OF_DOT;
				str++;
				break;

			case '-':
				result->sign = 1;
				state = LEFT_OF_DOT;
				str++;
				break;

			case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
				result->sign = 0;
				state = LEFT_OF_DOT;
				break;

			case '.':
				result->sign = 0;
				state = RIGHT_OF_DOT;
				str++;
				break;

			case 'n':
			case 'N':
			case 'i':
			case 'I':
				break;

			default:
				fail_char(old_str, len, str - old_str);
			}
			break;

		case LEFT_OF_DOT:
			switch (*str) {
			case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
				mant_str[pos++] = *(str++);
				break;

			case '.':
				state = RIGHT_OF_DOT;
				str++;
				break;

			case 'e':
			case 'E':
				state = EXP_START;
				str++;
				break;

			case '\0':
				mant_str[pos] = '\0';
				goto done;

			default:
				fail_char(old_str, len, str - old_str);
			}
			break;

		case RIGHT_OF_DOT:
			switch (*str) {
			case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
				mant_str[pos++] = *(str++);
				exp_int++;
				break;

			case 'e':
			case 'E':
				state = EXP_START;
				str++;
				break;

			case '\0':
				mant_str[pos] = '\0';
				goto done;

			default:
				fail_char(old_str, len, str - old_str);
			}
			break;

		case EXP_START:
			switch (*str) {
			case '-':
				exp_sign = 1;
				/* fall through */
			case '+':
				if (*(str-1) != 'e' && *(str-1) != 'E') fail_char(old_str, len, str - old_str);
				str++;
				break;

			case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
				mant_str[pos] = '\0';
				pos = 1;
				str++;
				state = EXPONENT;
				break;

			default:
				fail_char(old_str, len, str - old_str);
			}
			break;

		case EXPONENT:
			switch (*str) {
			case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
				pos++;
				str++;
				break;

			case '\0': goto done;

			default:
				fail_char(old_str, len, str - old_str);
			}
		}
	} /*  switch(state) */

done:
	sc_val_from_str(mant_str, strlen(mant_str), _mant(result));

	/* shift to put value left of radix point */
	sc_val_from_ulong(mant_size + 2, exp_val);

	_shift_left(_mant(result), exp_val, _mant(result));

	sc_val_from_ulong((1 << exp_size)/2-1, _exp(result));

	_normalize(result, result, 0);

	if (state == EXPONENT) {
		exp_int -= atoi(str-pos);
	}

	_power_of_ten(exp_int, &result->desc, power_val);

	_fdiv(result, power_val, result);

	return result;
#else
	/* XXX excuse of an implementation to make things work */
	LLDBL val;
	fp_value *tmp = alloca(calc_buffer_size);
	(void) len;

#ifdef HAVE_LONG_DOUBLE
	val = strtold(str, NULL);
	DEBUGPRINTF(("val_from_str(%s)\n", str));
	fc_val_from_ieee754(val, 15, 64, tmp);
#else
	val = strtod(str, NULL);
	DEBUGPRINTF(("val_from_str(%s)\n", str));
	fc_val_from_ieee754(val, 11, 52, tmp);
#endif /* HAVE_LONG_DOUBLE */
	return fc_cast(tmp, exp_size, mant_size, result);
#endif
}

fp_value *fc_val_from_ieee754(LLDBL l, char exp_size, char mant_size, fp_value *result) {
	char *temp;
	int bias_res, bias_val, mant_val;
	value_t srcval;
	UINT32 sign, exponent, mantissa0, mantissa1;

	srcval.d = l;
	bias_res = ((1<<exp_size)/2-1);

#ifdef HAVE_LONG_DOUBLE
	mant_val  = 64;
	bias_val  = 0x3fff;
	sign      = (srcval.val.high & 0x00008000) != 0;
	exponent  = (srcval.val.high & 0x00007FFF) ;
	mantissa0 = srcval.val.mid;
	mantissa1 = srcval.val.low;
#else /* no long double */
	mant_val  = 52;
	bias_val  = 0x3ff;
	sign      = (srcval.val.high & 0x80000000) != 0;
	exponent  = (srcval.val.high & 0x7FF00000) >> 20;
	mantissa0 = srcval.val.high & 0x000FFFFF;
	mantissa1 = srcval.val.low;
#endif

#ifdef HAVE_LONG_DOUBLE
	TRACEPRINTF(("val_from_float(%.8X%.8X%.8X)\n", ((int*)&l)[2], ((int*)&l)[1], ((int*)&l)[0]));/* srcval.val.high, srcval.val.mid, srcval.val.low)); */
	DEBUGPRINTF(("(%d-%.4X-%.8X%.8X)\n", sign, exponent, mantissa0, mantissa1));
#else
	TRACEPRINTF(("val_from_float(%.8X%.8X)\n", srcval.val.high, srcval.val.low));
	DEBUGPRINTF(("(%d-%.3X-%.5X%.8X)\n", sign, exponent, mantissa0, mantissa1));
#endif

	if (result == NULL) result = calc_buffer;
	temp = alloca(value_size);

	/* CLEAR the buffer */
	memset(result, 0, fc_get_buffer_length());

	result->desc.exponent_size = exp_size;
	result->desc.mantissa_size = mant_size;

	/* extract sign */
	result->sign = sign;

	/* sign and flag suffice to identify nan or inf, no exponent/mantissa
	 * encoding is needed. the function can return immediately in these cases */
	if (isnan(l)) {
		result->desc.clss = NAN;
		TRACEPRINTF(("val_from_float resulted in NAN\n"));
		return result;
	}
	else if (isinf(l)) {
		result->desc.clss = INF;
		TRACEPRINTF(("val_from_float resulted in %sINF\n", (result->sign == 1) ? "-" : ""));
		return result;
	}

	/* build exponent, because input and output exponent and mantissa sizes may differ
	 * this looks more complicated than it is: unbiased input exponent + output bias,
	 * minus the mantissa difference which is added again later when the output float
	 * becomes normalized */
#ifdef HAVE_EXPLICIT_ONE
	sc_val_from_long((exponent-bias_val+bias_res)-(mant_val-mant_size-1), _exp(result));
#else
	sc_val_from_long((exponent-bias_val+bias_res)-(mant_val-mant_size), _exp(result));
#endif

	/* build mantissa representation */
#ifndef HAVE_EXPLICIT_ONE
	if (exponent != 0) {
		/* insert the hidden bit */
		sc_val_from_ulong(1, temp);
		sc_val_from_ulong(mant_val + 2, NULL);
		_shift_left(temp, sc_get_buffer(), NULL);
	}
	else
#endif
	{
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
	sc_val_from_ulong(2, NULL);
	_shift_left(temp, sc_get_buffer(), temp);
	sc_or(_mant(result), temp, _mant(result));

	/* _normalize expects the radix point to be normal, so shift mantissa of subnormal
	 * origin one to the left */
	if (exponent == 0) {
		sc_val_from_ulong(1, NULL);
		_shift_left(_mant(result), sc_get_buffer(), _mant(result));
	}

	normalize(result, result, 0);

	TRACEPRINTF(("val_from_float results in %s\n", fc_print(result, temp, calc_buffer_size, FC_PACKED)));

	return result;
}

LLDBL fc_val_to_ieee754(const fp_value *val) {
	fp_value *value;
	fp_value *temp = NULL;

	int byte_offset;

	UINT32 sign;
	UINT32 exponent;
	UINT32 mantissa0;
	UINT32 mantissa1;

	value_t buildval;

#ifdef HAVE_LONG_DOUBLE
	char result_exponent = 15;
	char result_mantissa = 64;
#else
	char result_exponent = 11;
	char result_mantissa = 52;
#endif

	temp = alloca(calc_buffer_size);
#ifdef HAVE_EXPLICIT_ONE
	value = fc_cast(val, result_exponent, result_mantissa-1, temp);
#else
	value = fc_cast(val, result_exponent, result_mantissa, temp);
#endif

	sign = value->sign;

	/* @@@ long double exponent is 15bit, so the use of sc_val_to_long should not
	 * lead to wrong results */
	exponent = sc_val_to_long(_exp(value)) ;

	sc_val_from_ulong(2, NULL);
	_shift_right(_mant(value), sc_get_buffer(), _mant(value));

	mantissa0 = 0;
	mantissa1 = 0;

	for (byte_offset = 0; byte_offset < 4; byte_offset++)
		mantissa1 |= sc_sub_bits(_mant(value), result_mantissa, byte_offset) << (byte_offset<<3);

	for (; (byte_offset<<3) < result_mantissa; byte_offset++)
		mantissa0 |= sc_sub_bits(_mant(value), result_mantissa, byte_offset) << ((byte_offset-4)<<3);

#ifdef HAVE_LONG_DOUBLE
	buildval.val.high = sign << 15;
	buildval.val.high |= exponent;
	buildval.val.mid = mantissa0;
	buildval.val.low = mantissa1;
#else /* no long double */
	mantissa0 &= 0x000FFFFF;  /* get rid of garbage */
	buildval.val.high = sign << 31;
	buildval.val.high |= exponent << 20;
	buildval.val.high |= mantissa0;
	buildval.val.low = mantissa1;
#endif

	TRACEPRINTF(("val_to_float: %d-%x-%x%x\n", sign, exponent, mantissa0, mantissa1));
	return buildval.d;
}

fp_value *fc_cast(const fp_value *value, char exp_size, char mant_size, fp_value *result) {
	char *temp;
	int exp_offset, val_bias, res_bias;

	if (result == NULL) result = calc_buffer;
	temp = alloca(value_size);

	if (value->desc.exponent_size == exp_size && value->desc.mantissa_size == mant_size) {
		if (value != result)
			memcpy(result, value, calc_buffer_size);
		return result;
	}

	if (value->desc.clss == NAN) {
		if (sc_get_highest_set_bit(_mant(value)) == value->desc.mantissa_size + 1)
			return fc_get_qnan(exp_size, mant_size, result);
		else
			return fc_get_snan(exp_size, mant_size, result);
	}

	/* set the descriptor of the new value */
	result->desc.exponent_size = exp_size;
	result->desc.mantissa_size = mant_size;
	result->desc.clss = value->desc.clss;

	result->sign = value->sign;

	/* when the mantissa sizes differ normalizing has to shift to align it.
	 * this would change the exponent, which is unwanted. So calculate this
	 * offset and add it */
	val_bias = (1 << (value->desc.exponent_size - 1)) - 1;
	res_bias = (1 << (exp_size - 1)) - 1;

	exp_offset = (res_bias - val_bias) - (value->desc.mantissa_size - mant_size);
	sc_val_from_long(exp_offset, temp);
	sc_add(_exp(value), temp, _exp(result));

	/* _normalize expects normalized radix point */
	if (value->desc.clss == SUBNORMAL) {
		sc_val_from_ulong(1, NULL);
		_shift_left(_mant(value), sc_get_buffer(), _mant(result));
	} else if (value != result) {
		memcpy(_mant(result), _mant(value), value_size);
	} else {
		memmove(_mant(result), _mant(value), value_size);
	}

	normalize(result, result, 0);
	TRACEPRINTF(("Cast results in %s\n", fc_print(result, temp, value_size, FC_PACKED)));
	return result;
}

fp_value *fc_get_max(unsigned int exponent_size, unsigned int mantissa_size, fp_value *result) {
	if (result == NULL) result = calc_buffer;

	result->desc.exponent_size = exponent_size;
	result->desc.mantissa_size = mantissa_size;
	result->desc.clss = NORMAL;

	result->sign = 0;

	sc_val_from_ulong((1<<exponent_size) - 2, _exp(result));

	sc_max_from_bits(mantissa_size + 1, 0, _mant(result));
	sc_val_from_ulong(2, NULL);
	_shift_left(_mant(result), sc_get_buffer(), _mant(result));

	return result;
}

fp_value *fc_get_min(unsigned int exponent_size, unsigned int mantissa_size, fp_value *result) {
	if (result == NULL) result = calc_buffer;

	fc_get_max(exponent_size, mantissa_size, result);
	result->sign = 1;

	return result;
}

fp_value *fc_get_snan(unsigned int exponent_size, unsigned int mantissa_size, fp_value *result) {
	if (result == NULL) result = calc_buffer;

	result->desc.exponent_size = exponent_size;
	result->desc.mantissa_size = mantissa_size;
	result->desc.clss = NAN;

	result->sign = 0;

	sc_val_from_ulong((1<<exponent_size)-1, _exp(result));

	/* signaling NaN has non-zero mantissa with msb not set */
	sc_val_from_ulong(1, _mant(result));

	return result;
}

fp_value *fc_get_qnan(unsigned int exponent_size, unsigned int mantissa_size, fp_value *result) {
	if (result == NULL) result = calc_buffer;

	result->desc.exponent_size = exponent_size;
	result->desc.mantissa_size = mantissa_size;
	result->desc.clss = NAN;

	result->sign = 0;

	sc_val_from_ulong((1<<exponent_size)-1, _exp(result));

	/* quiet NaN has the msb of the mantissa set, so shift one there */
	sc_val_from_ulong(1, _mant(result));
	/* mantissa_size >+< 1 because of two extra rounding bits */
	sc_val_from_ulong(mantissa_size + 1, NULL);
	_shift_left(_mant(result), sc_get_buffer(), _mant(result));

	return result;
}

fp_value *fc_get_plusinf(unsigned int exponent_size, unsigned int mantissa_size, fp_value *result) {
	if (result == NULL) result = calc_buffer;

	result->desc.exponent_size = exponent_size;
	result->desc.mantissa_size = mantissa_size;
	result->desc.clss = NORMAL;

	result->sign = 0;

	sc_val_from_ulong((1<<exponent_size)-1, _exp(result));

	sc_val_from_ulong(0, _mant(result));

	return result;
}

fp_value *fc_get_minusinf(unsigned int exponent_size, unsigned int mantissa_size, fp_value *result) {
	if (result == NULL) result = calc_buffer;

	fc_get_plusinf(exponent_size, mantissa_size, result);
	result->sign = 1;

	return result;
}

int fc_comp(const fp_value *val_a, const fp_value *val_b) {
	int mul = 1;

	/*
	 * shortcut: if both values are identical, they are either
	 * Unordered if NaN or equal
	 */
	if (val_a == val_b)
		return val_a->desc.clss == NAN ? 2 : 0;

	/* unordered if one is a NaN */
	if (val_a->desc.clss == NAN || val_b->desc.clss == NAN)
		return 2;

	/* zero is equal independent of sign */
	if ((val_a->desc.clss == ZERO) && (val_b->desc.clss == ZERO))
		return 0;

	/* different signs make compare easy */
	if (val_a->sign != val_b->sign)
		return (val_a->sign == 0) ? (1) : (-1);

	mul = val_a->sign ? -1 : 1;

	/* both infinity means equality */
	if ((val_a->desc.clss == INF) && (val_b->desc.clss == INF))
		return 0;

	/* infinity is bigger than the rest */
	if (val_a->desc.clss == INF)
		return  1 * mul;
	if (val_b->desc.clss == INF)
		return -1 * mul;

	/* check first exponent, that mantissa if equal */
	switch (sc_comp(_exp(val_a), _exp(val_b))) {
	case -1:
		return -1 * mul;
	case  1:
		return  1 * mul;
	case  0:
		return sc_comp(_mant(val_a), _mant(val_b)) * mul;
	default:
		return 2;
	}
}

int fc_is_zero(const fp_value *a) {
	return a->desc.clss == ZERO;
}

int fc_is_negative(const fp_value *a) {
	return a->sign;
}

int fc_is_inf(const fp_value *a) {
	return a->desc.clss == INF;
}

int fc_is_nan(const fp_value *a) {
	return a->desc.clss == NAN;
}

int fc_is_subnormal(const fp_value *a) {
	return a->desc.clss == SUBNORMAL;
}

char *fc_print(const fp_value *val, char *buf, int buflen, unsigned base) {
	char *mul_1;

	mul_1 = alloca(calc_buffer_size);

	switch (base) {
	case FC_DEC:
		switch (val->desc.clss) {
		case INF:
			if (buflen >= 8 + val->sign) sprintf(buf, "%sINFINITY", val->sign ? "-":"");
			else snprintf(buf, buflen, "%sINF", val->sign ? "-":NULL);
			break;
		case NAN:
			snprintf(buf, buflen, "NAN");
			break;
		case ZERO:
			snprintf(buf, buflen, "0.0");
			break;
		default:
			/* XXX to be implemented */
#ifdef HAVE_LONG_DOUBLE
			/* XXX 30 is arbitrary */
			snprintf(buf, buflen, "%.30LE", fc_val_to_ieee754(val));
#else
			snprintf(buf, buflen, "%.18E", fc_val_to_ieee754(val));
#endif
		}
		break;

	case FC_HEX:
		switch (val->desc.clss) {
		case INF:
			if (buflen >= 8+val->sign) sprintf(buf, "%sINFINITY", val->sign?"-":"");
			else snprintf(buf, buflen, "%sINF", val->sign?"-":NULL);
			break;
		case NAN:
			snprintf(buf, buflen, "NAN");
			break;
		case ZERO:
			snprintf(buf, buflen, "0.0");
			break;
		default:
#ifdef HAVE_LONG_DOUBLE
			snprintf(buf, buflen, "%LA", fc_val_to_ieee754(val));
#else
			snprintf(buf, buflen, "%A", fc_val_to_ieee754(val));
#endif
		}
		break;

	case FC_PACKED:
	default:
		snprintf(buf, buflen, "%s", sc_print(pack(val, mul_1), value_size*4, SC_HEX, 0));
		buf[buflen - 1] = '\0';
		break;
	}
	return buf;
}

unsigned char fc_sub_bits(const fp_value *value, unsigned num_bits, unsigned byte_ofs) {
	/* this is used to cache the packed version of the value */
	static char *packed_value = NULL;

	if (packed_value == NULL) packed_value = xmalloc(value_size);

	if (value != NULL)
		pack(value, packed_value);

	return sc_sub_bits(packed_value, num_bits, byte_ofs);
}

int fc_zero_mantissa(const fp_value *value) {
	return sc_get_lowest_set_bit(_mant(value)) == 2 + value->desc.mantissa_size;
}

int fc_get_exponent(const fp_value *value) {
	int exp_bias = (1 << (value->desc.exponent_size - 1)) - 1;
	return sc_val_to_long(_exp(value)) - exp_bias;
}


fc_rounding_mode_t fc_set_rounding_mode(fc_rounding_mode_t mode) {
	if (mode == FC_TONEAREST || mode == FC_TOPOSITIVE || mode == FC_TONEGATIVE || mode == FC_TOZERO)
		rounding_mode = mode;

	return rounding_mode;
}

fc_rounding_mode_t fc_get_rounding_mode(void) {
	return rounding_mode;
}

void init_fltcalc(int precision) {
	if (calc_buffer == NULL) {
		/* does nothing if already init */
		if (precision == 0) precision = FC_DEFAULT_PRECISION;

		init_strcalc(precision + 4);

		/* needs additionally two bits to round, a bit as explicit 1., and one for
		 * addition overflow */
		max_precision = sc_get_precision() - 4;
		if (max_precision < precision)
			printf("WARNING: not enough precision available, using %d\n", max_precision);

		rounding_mode    = FC_TONEAREST;
		value_size       = sc_get_buffer_length();
		calc_buffer_size = sizeof(fp_value) + 2*value_size - 1;

		calc_buffer = xmalloc(calc_buffer_size);
		memset(calc_buffer, 0, calc_buffer_size);
		DEBUGPRINTF(("init fltcalc:\n\tVALUE_SIZE = %d\ntCALC_BUFFER_SIZE = %d\n\tcalc_buffer = %p\n\n", value_size, calc_buffer_size, calc_buffer));
#ifdef HAVE_LONG_DOUBLE
		DEBUGPRINTF(("\tUsing long double (1-15-64) interface\n"));
#else
		DEBUGPRINTF(("\tUsing double (1-11-52) interface\n"));
#endif
#ifdef WORDS_BIGENDIAN
		DEBUGPRINTF(("\tWord order is big endian\n\n"));
#else
		DEBUGPRINTF(("\tWord order is little endian\n\n"));
#endif
	}
}

void finish_fltcalc (void) {
	free(calc_buffer); calc_buffer = NULL;
}

#ifdef FLTCALC_TRACE_CALC
static char buffer[100];
#endif

/* definition of interface functions */
fp_value *fc_add(const fp_value *a, const fp_value *b, fp_value *result) {
	if (result == NULL) result = calc_buffer;

	TRACEPRINTF(("%s ", fc_print(a, buffer, sizeof(buffer), FC_PACKED)));
	TRACEPRINTF(("+ %s ", fc_print(b, buffer, sizeof(buffer), FC_PACKED)));

	/* make the value with the bigger exponent the first one */
	if (sc_comp(_exp(a), _exp(b)) == -1)
		_fadd(b, a, result);
	else
		_fadd(a, b, result);

	TRACEPRINTF(("= %s\n", fc_print(result, buffer, sizeof(buffer), FC_PACKED)));
	return result;
}

fp_value *fc_sub(const fp_value *a, const fp_value *b, fp_value *result) {
	fp_value *temp;

	if (result == NULL) result = calc_buffer;

	TRACEPRINTF(("%s ", fc_print(a, buffer, sizeof(buffer), FC_PACKED)));
	TRACEPRINTF(("- %s ", fc_print(b, buffer, sizeof(buffer), FC_PACKED)));

	temp = alloca(calc_buffer_size);
	memcpy(temp, b, calc_buffer_size);
	temp->sign = !b->sign;
	if (sc_comp(_exp(a), _exp(temp)) == -1)
		_fadd(temp, a, result);
	else
		_fadd(a, temp, result);

	TRACEPRINTF(("= %s\n", fc_print(result, buffer, sizeof(buffer), FC_PACKED)));
	return result;
}

fp_value *fc_mul(const fp_value *a, const fp_value *b, fp_value *result) {
	if (result == NULL) result = calc_buffer;

	TRACEPRINTF(("%s ", fc_print(a, buffer, sizeof(buffer), FC_PACKED)));
	TRACEPRINTF(("* %s ", fc_print(b, buffer, sizeof(buffer), FC_PACKED)));

	_fmul(a, b, result);

	TRACEPRINTF(("= %s\n", fc_print(result, buffer, sizeof(buffer), FC_PACKED)));
	return result;
}

fp_value *fc_div(const fp_value *a, const fp_value *b, fp_value *result) {
	if (result == NULL) result = calc_buffer;

	TRACEPRINTF(("%s ", fc_print(a, buffer, sizeof(buffer), FC_PACKED)));
	TRACEPRINTF(("/ %s ", fc_print(b, buffer, sizeof(buffer), FC_PACKED)));

	_fdiv(a, b, result);

	TRACEPRINTF(("= %s\n", fc_print(result, buffer, sizeof(buffer), FC_PACKED)));
	return result;
}

fp_value *fc_neg(const fp_value *a, fp_value *result) {
	if (result == NULL) result = calc_buffer;

	TRACEPRINTF(("- %s ", fc_print(a, buffer, sizeof(buffer), FC_PACKED)));

	if (a != result)
		memcpy(result, a, calc_buffer_size);
	result->sign = !a->sign;

	TRACEPRINTF(("= %s\n", fc_print(result, buffer, sizeof(buffer), FC_PACKED)));
	return result;
}

fp_value *fc_int(const fp_value *a, fp_value *result) {
	if (result == NULL) result = calc_buffer;

	TRACEPRINTF(("%s ", fc_print(a, buffer, sizeof(buffer), FC_PACKED)));
	TRACEPRINTF(("truncated to integer "));

	_trunc(a, result);

	TRACEPRINTF(("= %s\n", fc_print(result, buffer, sizeof(buffer), FC_PACKED)));
	return result;
}

fp_value *fc_rnd(const fp_value *a, fp_value *result) {
	if (result == NULL) result = calc_buffer;

	(void) a;
	TRACEPRINTF(("%s ", fc_print(a, buffer, sizeof(buffer), FC_PACKED)));
	TRACEPRINTF(("rounded to integer "));

	assert(!"fc_rnd() not yet implemented");

	TRACEPRINTF(("= %s\n", fc_print(result, buffer, sizeof(buffer), FC_PACKED)));
	return result;
}

/*
 * convert a floating point value into an sc value ...
 */
int fc_flt2int(const fp_value *a, void *result, ir_mode *dst_mode) {
	if (a->desc.clss == NORMAL) {
		int exp_bias = (1 << (a->desc.exponent_size - 1)) - 1;
		int exp_val  = sc_val_to_long(_exp(a)) - exp_bias;
		int shift, highest;

		if (a->sign && !mode_is_signed(dst_mode)) {
			/* FIXME: for now we cannot convert this */
			return 0;
		}

		assert(exp_val >= 0 && "floating point value not integral before fc_flt2int() call");
		shift = exp_val - a->desc.mantissa_size - 2;

		if (shift > 0) {
			sc_shlI(_mant(a),  shift, 64, 0, result);
		} else {
			sc_shrI(_mant(a), -shift, 64, 0, result);
		}

		/* check for overflow */
		highest = sc_get_highest_set_bit(result);

		if (mode_is_signed(dst_mode)) {
			if (highest == sc_get_lowest_set_bit(result)) {
				/* need extra test for MIN_INT */
				if (highest >= get_mode_size_bits(dst_mode)) {
					/* FIXME: handle overflow */
					return 0;
				}
			} else {
				if (highest >= get_mode_size_bits(dst_mode) - 1) {
					/* FIXME: handle overflow */
					return 0;
				}
			}
		} else {
			if (highest >= get_mode_size_bits(dst_mode)) {
				/* FIXME: handle overflow */
				return 0;
			}
		}

		if (a->sign)
			sc_neg(result, result);

		return 1;
	}
	else if (a->desc.clss == ZERO) {
		sc_zero(result);
		return 1;
	}
	return 0;
}


unsigned fc_set_immediate_precision(unsigned bits) {
	unsigned old = immediate_prec;

	immediate_prec = bits;
	return old;
}

int fc_is_exact(void) {
	return fc_exact;
}
