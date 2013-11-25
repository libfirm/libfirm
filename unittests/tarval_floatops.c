#include <assert.h>
#include <float.h>
#include <stdlib.h>
#include <string.h>
#include "tv_t.h"
#include "firm.h"
#include "util.h"

static void check_mode(ir_mode *mode)
{
	ir_tarval *zero       = get_mode_null(mode);
	ir_tarval *one        = get_mode_one(mode);
	ir_tarval *minus_one  = get_mode_minus_one(mode);
	ir_tarval *minus_zero = tarval_neg(zero);
	ir_tarval *two        = new_tarval_from_str("2", 1, mode);
	ir_tarval *half       = new_tarval_from_str("0.5", 3, mode);
	ir_tarval *nan        = get_mode_NAN(mode);
	ir_tarval *inf        = get_mode_infinite(mode);
	ir_tarval *minus_inf  = tarval_neg(inf);
	ir_tarval *small      = get_tarval_small(mode);
	ir_tarval *epsilon    = get_tarval_epsilon(mode);
	ir_tarval *denorm     = tarval_mul(small, epsilon);
	ir_tarval *min        = get_mode_min(mode);
	ir_tarval *max        = get_mode_max(mode);
	ir_tarval *values[] = {
		one,
		two,
		half,
		new_tarval_from_str("42", 2, mode),
		new_tarval_from_str("-13", 3, mode),
		small,
		epsilon,
		tarval_neg(small),
		tarval_neg(epsilon),
		min,
		max,
		zero,
		minus_zero,
		inf,
		minus_inf,
		minus_one,
		nan,
		denorm,
		tarval_neg(denorm),
	};

	/* test neutral elements */
	for (unsigned i = 0; i < ARRAY_SIZE(values); ++i) {
		ir_tarval *val = values[i];
		ir_tarval *mul = tarval_mul(val, one);
		ir_tarval *div = tarval_div(val, one);
		ir_tarval *add = tarval_add(val, zero);
		ir_tarval *sub = tarval_sub(val, zero, mode);
		assert(mul == val);
		assert(div == val);
		assert((val == minus_zero && add == zero)
		    || (val != minus_zero && add == val));
		assert(sub == val);
	}

	/* zero elements */
	for (unsigned i = 0; i < ARRAY_SIZE(values); ++i) {
		ir_tarval *val = values[i];
		ir_tarval *mul = tarval_mul(val, zero);
		assert((tarval_is_negative(val) && mul == minus_zero)
		    || (!tarval_is_negative(val) && mul == zero)
		    || (val == nan && mul == nan)
		    || (val == inf && mul == nan)
		    || (val == minus_inf && tarval_is_nan(mul)));
		assert(!tarval_is_finite(val) || tarval_sub(val, val, mode) == zero);
	}

	/* commutativity */
	for (unsigned a = 0; a < ARRAY_SIZE(values); ++a) {
		ir_tarval *val_a = values[a];
		for (unsigned b = 0; b < ARRAY_SIZE(values); ++b) {
			ir_tarval *val_b = values[b];
			assert(tarval_mul(val_a, val_b) == tarval_mul(val_b, val_a));
			assert(tarval_add(val_a, val_b) == tarval_add(val_b, val_a));
		}
	}

	/* neg(neg(x)) == x */
	for (unsigned i = 0; i < ARRAY_SIZE(values); ++i) {
		ir_tarval *val = values[i];
		assert(tarval_neg(tarval_neg(val)) == val);
	}

	/* test abs */
	for (unsigned i = 0; i < ARRAY_SIZE(values); ++i) {
		ir_tarval *val = values[i];
		if (tarval_is_nan(val))
			continue;
		assert((!tarval_is_negative(val) && tarval_abs(val) == val)
		    || (tarval_is_negative(val) && tarval_abs(val) == tarval_neg(val)));
	}

	/* nan stays nan TODO: check nan payload? */
	for (unsigned i = 0; i < ARRAY_SIZE(values); ++i) {
		ir_tarval *val = values[i];
		assert(tarval_is_nan(tarval_add(val, nan)));
		assert(tarval_is_nan(tarval_sub(val, nan, mode)));
		assert(tarval_is_nan(tarval_mul(val, nan)));
		assert(tarval_is_nan(tarval_div(val, nan)));
	}
	assert(tarval_is_nan(tarval_neg(nan)));
	assert(tarval_is_nan(tarval_abs(nan)));

	/* minus zero */
	assert(tarval_mul(minus_zero, minus_zero) == zero);
	assert(tarval_add(minus_zero, minus_zero) == minus_zero);
	assert(tarval_add(minus_zero, zero) == zero);
	assert(tarval_div(minus_zero, minus_inf) == zero);
	for (unsigned i = 0; i < ARRAY_SIZE(values); ++i) {
		ir_tarval *val = values[i];
		if (!tarval_is_finite(val) || val == zero || val == minus_zero)
			continue;
		assert(tarval_div(minus_zero, tarval_abs(val)) == minus_zero);
	}

	/* some random arithmetics */
	assert(tarval_div(one, two) == half);
	assert(tarval_mul(two, one) == two);

	ir_tarval *int_zero      = get_mode_null(mode_Is);
	ir_tarval *int_one       = get_mode_one(mode_Is);
	ir_tarval *int_minus_one = get_mode_minus_one(mode_Is);
	ir_tarval *int_min       = get_mode_min(mode_Is);
	ir_tarval *int_max       = get_mode_max(mode_Is);
	assert(tarval_convert_to(zero, mode_Is) == int_zero);
	assert(tarval_convert_to(minus_zero, mode_Is) == int_zero);
	assert(tarval_convert_to(one, mode_Is) == int_one);
	assert(tarval_convert_to(minus_one, mode_Is) == int_minus_one);
	assert(tarval_convert_to(min, mode_Is) == int_min);
	assert(tarval_convert_to(max, mode_Is) == int_max);
	assert(tarval_convert_to(inf, mode_Is) == int_max);
	assert(tarval_convert_to(minus_inf, mode_Is) == int_min);

	static const char *const ints[] = {
		"0", "1", "-1", "12345", "2", "4", "8", "16", "32", "64", "128", "256",
		"512", "1024", "2048", "127", "2047"
	};
	for (unsigned i = 0; i < ARRAY_SIZE(ints); ++i) {
		const char *str = ints[i];
		ir_tarval *flt = new_tarval_from_str(str, strlen(str), mode);
		ir_tarval *intt = new_tarval_from_str(str, strlen(str), mode_Is);
		assert(tarval_convert_to(flt, mode_Is) == intt);
		assert(tarval_convert_to(intt, mode) == flt);
	}
}

int main(void)
{
	ir_init();

	check_mode(mode_F);
	check_mode(mode_F);
#if LDBL_MANT_DIG == 64
	ir_mode *mode_E = new_float_mode("E", irma_x86_extended_float, 15, 64,
	                                 ir_overflow_min_max);
	check_mode(mode_E);
#endif
}
