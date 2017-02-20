#include "ident_t.h"
#include "irmode_t.h"
#include "irnode.h"
#include "irprintf.h"
#include "irprog_t.h"
#include "tv_t.h"
#include "type_t.h"
#include "util.h"
#include "xmalloc.h"
#include <assert.h>
#include <limits.h>
#include <stdio.h>

static int result = 0;
static unsigned n_modes;
static ir_mode *const *modes;
static unsigned n_tarvals;
static ir_tarval *const *tarvals;

static const char *context = "";
static const char *op_name = "";

static void compare_int(const char *file, unsigned line,
                        const char *expr0, int v0, const char *expr1, int v1)
{
	if (v0 == v1)
		return;
	fprintf(stderr, "%s:%d [%s%s]: Test failed %s != %s  (%d != %d)\n",
	        file, line, context, op_name, expr0, expr1, v0, v1);
	result = 1;
}
#define COMPARE_INT(val0,val1) compare_int(__FILE__, __LINE__, #val0, val0, #val1, val1)
#define TEST(expr) COMPARE_INT(expr, 1)

static void compare_tv(const char *file, unsigned line,
                       const char *expr0, ir_tarval *tv0, const char *expr1,
                       ir_tarval *tv1)
{
	if (tv0 == tv1)
		return;
	ir_fprintf(stderr, "%s:%d [%s%s]: Test failed %s != %s (%T != %T)\n",
	           file, line, context, op_name, expr0, expr1, tv0, tv1);
	result = 1;
}
#define TVS_EQUAL(val0,val1) compare_tv(__FILE__, __LINE__, #val0, val0, #val1, val1)

typedef ir_tarval* (*binop)(ir_tarval const *op0, ir_tarval const *op1);
typedef ir_tarval* (*unop)(ir_tarval const *op);

/* tarval_div, except that x/0 == 0 */
static ir_tarval *safe_div(ir_tarval const *op0, ir_tarval const *op1)
{
	if (tarval_is_null(op1))
		return (ir_tarval*)op1;
	return tarval_div(op0, op1);
}

static ir_tarval *safe_mod(ir_tarval const *op0, ir_tarval const *op1)
{
	if (tarval_is_null(op1))
		return (ir_tarval*)op1;
	return tarval_mod(op0, op1);
}

static ir_tarval *tarval_id(ir_tarval const *tv)
{
	return (ir_tarval*)tv;
}

static void test_neutral_(binop op, const char *new_op_name,
                          ir_tarval *neutral_element, bool commutative)
{
	op_name = new_op_name;

	for (unsigned i = 0, n = n_tarvals; i < n; ++i) {
		ir_tarval *value = tarvals[i];
		TVS_EQUAL(op(value, neutral_element), value);
		if (commutative)
			TVS_EQUAL(op(neutral_element, value), value);
	}

	op_name = "";
}
#define test_neutral(func, neutral_element, commutative) \
	test_neutral_(func, #func, neutral_element, commutative)

static void test_zero_(binop op, const char *new_op_name,
                       ir_tarval *zero_element, bool left_zero,
                       bool right_zero, ir_tarval *zero)
{
	op_name = new_op_name;

	for (unsigned i = 0, n = n_tarvals; i < n; ++i) {
		ir_tarval *value = tarvals[i];
		if (left_zero)
			TVS_EQUAL(op(zero_element, value), zero);
		if (right_zero)
			TVS_EQUAL(op(value, zero_element), zero);
	}

	op_name = "";
}
#define test_zero(func, zero_element, left_zero, right_zero) \
	test_zero_(func, #func, zero_element, left_zero, right_zero, zero)

static void test_inverse_(binop op, const char *new_op_name,
                          unop create_inverse, ir_tarval *neutral_element)
{
	op_name = new_op_name;

	for (unsigned i = 0, n = n_tarvals; i < n; ++i) {
		ir_tarval *value = tarvals[i];
		TVS_EQUAL(op(value, create_inverse(value)), neutral_element);
	}

	op_name = "";
}
#define test_inverse(func, create_inverse, neutral_element) \
	test_inverse_(func, #func, create_inverse, neutral_element)

static void test_associativity_(binop op, const char *new_op_name)
{
	op_name = new_op_name;
	unsigned n = n_tarvals;
	for (unsigned a = 0; a < n; ++a) {
		ir_tarval *val_a = tarvals[a];
		for (unsigned b = 0; b < n; ++b) {
			ir_tarval *val_b = tarvals[b];
			for (unsigned c = 0; c < n; ++c) {
				ir_tarval *val_c = tarvals[c];
				TVS_EQUAL(op(val_a, op(val_b, val_c)), op(op(val_a, val_b), val_c));
			}
		}
	}
	op_name = "";
}
#define test_associativity(func) test_associativity_(func, #func)

static void test_commutativity_(binop op, const char *new_op_name)
{
	op_name = new_op_name;
	unsigned n = n_tarvals;
	for (unsigned a = 0; a < n; ++a) {
		ir_tarval *val_a = tarvals[a];
		for (unsigned b = 0; b < n; ++b) {
			ir_tarval *val_b = tarvals[b];
			TVS_EQUAL(op(val_a, val_b), op(val_b, val_a));
		}
	}
	op_name = "";
}
#define test_commutativity(func) test_commutativity_(func, #func)

static void test_involution_(unop op, const char *new_op_name)
{
	op_name = new_op_name;
	for (unsigned i = 0, n = n_tarvals; i < n; ++i) {
		ir_tarval *value = tarvals[i];
		TVS_EQUAL(op(op(value)), value);
	}

	op_name = "";
}
#define test_involution(func) test_involution_(func, #func)

static void test_binop_nan_(binop op, const char *new_op_name, ir_tarval *nan)
{
	op_name = new_op_name;
	for (unsigned i = 0, n = n_tarvals; i < n; ++i) {
		ir_tarval *value = tarvals[i];
		if (tarval_is_nan(value))
			continue;
		TVS_EQUAL(op(nan, value), nan);
		TVS_EQUAL(op(value, nan), nan);
	}
	op_name = "";
}
#define test_binop_nan(func, nan) test_binop_nan_(func, #func, nan)

static void test_unop_nan_(unop op, const char *new_op_name, ir_tarval *nan)
{
	op_name = new_op_name;
	TVS_EQUAL(op(nan), nan);
	op_name = "";
}
#define test_unop_nan(func, nan) test_unop_nan_(func, #func, nan)

static void test_bitcast(ir_mode *mode)
{
	for (unsigned m = 0, n = n_modes; m < n; ++m) {
		ir_mode *other_mode = modes[m];
		if (get_mode_size_bits(other_mode) != get_mode_size_bits(mode))
			continue;
		for (unsigned i = 0, ni = n_tarvals; i < ni; ++i) {
			ir_tarval *value  = tarvals[i];
			TVS_EQUAL(value, tarval_bitcast(tarval_bitcast(value, other_mode), mode));
		}
	}
}

static void test_compare(ir_tarval *minus_zero, ir_tarval *zero)
{
	/* assumes the tarvals are in order */
	for (unsigned a = 0; a < n_tarvals; ++a) {
		ir_tarval *val_a = tarvals[a];
		for (unsigned b = 0; b < n_tarvals; ++b) {
			ir_tarval *val_b = tarvals[b];
			TEST(tarval_cmp(val_a, val_b) == get_inversed_relation(tarval_cmp(val_b, val_a)));

			ir_relation expected;
			if (tarval_is_nan(val_a) || tarval_is_nan(val_b))
				expected = ir_relation_unordered;
			else if (val_a == val_b)
				expected = ir_relation_equal;
			else if ((val_a == minus_zero && val_b == zero)
			         || (val_a == zero && val_b == minus_zero))
				expected = ir_relation_equal;
			else if (a < b)
				expected = ir_relation_less;
			else
				expected = ir_relation_greater;
			ir_relation relation = tarval_cmp(val_a, val_b);
			if (relation != expected) {
				ir_fprintf(stderr, "Comparing %+F with %+F failed: got %s, expected %s\n", val_a, val_b, get_relation_string(relation), get_relation_string(expected));
			}
		}
	}
}

static void test_int_tarvals(ir_mode *mode)
{
	char ctxbuf[128];
	snprintf(ctxbuf, sizeof(ctxbuf), "mode %s", get_mode_name(mode));
	context = ctxbuf;

	assert(get_mode_arithmetic(mode) == irma_twos_complement);
	unsigned bits = get_mode_size_bits(mode);
	// Create some values...
	ir_tarval *zero    = get_mode_null(mode);
	ir_tarval *one     = get_mode_one(mode);
	ir_tarval *all_one = get_mode_all_one(mode);
	ir_tarval *min     = get_mode_min(mode);
	ir_tarval *max     = get_mode_max(mode);
	ir_tarval *n_bits  = new_tarval_from_long(bits, mode);

	unsigned char buffer[bits/CHAR_BIT+1];
	for (unsigned i = 0, n = sizeof(buffer); i < n; ++i) {
		buffer[i] = 0x55;
	}
	ir_tarval *oddbits = new_tarval_from_bytes(buffer, mode);
	for (unsigned i = 0, n = sizeof(buffer); i < n; ++i) {
		buffer[i] = 0xaa;
	}
	ir_tarval *evenbits = new_tarval_from_bytes(buffer, mode);

	/* a collection of "interesting" values, make sure they are sorted */
	tarvals = mode_is_signed(mode) ? (ir_tarval*[]) {
		min,
		bits % 2 == 0 ? evenbits : oddbits,
		all_one,
		zero,
		one,
		n_bits,
		bits % 2 == 0 ? oddbits : evenbits,
		max,
		NULL,
	} : (ir_tarval*[]) {
		zero,
		one,
		n_bits,
		bits % 2 == 0 ? oddbits : evenbits,
		bits % 2 == 0 ? evenbits : oddbits,
		all_one,
		NULL,
	};
	n_tarvals = 0;
	while (tarvals[n_tarvals] != NULL)
		++n_tarvals;

	/* unops - involution */
	test_involution(tarval_neg);
	test_involution(tarval_not);

	/* binops - neutral elements */
	test_neutral(tarval_add, zero, true);
	test_neutral(tarval_sub, zero, false);
	test_neutral(tarval_mul, one, true);
	test_neutral(tarval_div, one, false);
	/* tarval_mod has no neutral element */
	test_neutral(tarval_and, all_one, true);
	test_neutral(tarval_andnot, zero, false);
	test_neutral(tarval_or, zero, true);
	test_neutral(tarval_ornot, all_one, false);
	test_neutral(tarval_eor, zero, true);
	test_neutral(tarval_shl, zero, false);
	test_neutral(tarval_shr, zero, false);
	/* TODO: implement this in libfirm! */
	if (bits % 8 == 0)
		test_neutral(tarval_shrs, zero, false);

	/* binops - zero elements */
	test_zero(tarval_mul, zero, true, true);
	test_zero(safe_div, zero, true, true);
	test_zero(safe_mod, zero, true, true);
	test_zero(tarval_and, zero, true, true);
	if (get_mode_modulo_shift(mode) == 0) {
		test_zero(tarval_shl, n_bits, false, true);
		test_zero(tarval_shr, n_bits, false, true);
	}

	/* binops - inverse elements */
	test_inverse(tarval_add, tarval_neg, zero);
	test_inverse(tarval_sub, tarval_id, zero);
	test_inverse(tarval_and, tarval_not, zero);
	test_inverse(tarval_andnot, tarval_id, zero);
	test_inverse(tarval_or, tarval_not, all_one);
	test_inverse(tarval_ornot, tarval_id, all_one);
	test_inverse(tarval_eor, tarval_id, zero);
	test_inverse(tarval_eor, tarval_not, all_one);

	/* binops - commutativity */
	test_commutativity(tarval_add);
	test_commutativity(tarval_mul);
	test_commutativity(tarval_and);
	test_commutativity(tarval_or);
	test_commutativity(tarval_eor);

	/* binops - associativity */
	test_associativity(tarval_add);
	test_associativity(tarval_mul);
	test_associativity(tarval_and);
	test_associativity(tarval_or);
	test_associativity(tarval_eor);

	/* misc logical operation tests on oddbits/evenbits */
	TVS_EQUAL(tarval_or(oddbits, evenbits), all_one);
	TVS_EQUAL(tarval_ornot(oddbits, oddbits), all_one);
	TVS_EQUAL(tarval_ornot(evenbits, evenbits), all_one);
	TVS_EQUAL(tarval_ornot(oddbits, evenbits), oddbits);
	TVS_EQUAL(tarval_and(oddbits, evenbits), zero);
	TVS_EQUAL(tarval_andnot(oddbits, evenbits), oddbits);
	TVS_EQUAL(tarval_andnot(evenbits, oddbits), evenbits);
	TVS_EQUAL(tarval_eor(oddbits, evenbits), all_one);
	TVS_EQUAL(tarval_not(oddbits), evenbits);
	TVS_EQUAL(tarval_not(evenbits), oddbits);

	/* test popcount */
	COMPARE_INT(get_tarval_popcount(zero), 0);
	COMPARE_INT(get_tarval_popcount(one), 1);
	COMPARE_INT(get_tarval_popcount(all_one), bits);
	if (mode_is_signed(mode)) {
		COMPARE_INT(get_tarval_popcount(min), 1);
		COMPARE_INT(get_tarval_popcount(max), bits-1);
	} else {
		COMPARE_INT(get_tarval_popcount(min), 0);
		COMPARE_INT(get_tarval_popcount(max), bits);
	}
	COMPARE_INT(get_tarval_popcount(oddbits), (bits/2)+(bits%2));
	COMPARE_INT(get_tarval_popcount(evenbits), bits/2);

	/* tarval_is_negative */
	TEST(!tarval_is_negative(zero));
	TEST(!tarval_is_negative(one));
	TEST(!tarval_is_negative(max));
	if (mode_is_signed(mode)) {
		TEST(tarval_is_negative(min));
		TEST(tarval_is_negative(all_one));
	} else {
		TEST(!tarval_is_negative(min));
		TEST(!tarval_is_negative(all_one));
	}

	/* tarval_is_XXX, tarval_abs */
	for (unsigned i = 0, n = n_tarvals; i < n; ++i) {
		ir_tarval *value = tarvals[i];
		if (value == zero)
			TEST(tarval_is_null(value));
		else
			TEST(!tarval_is_null(value));

		if (value == one)
			TEST(tarval_is_one(value));
		else
			TEST(!tarval_is_one(value));

		if (value == all_one)
			TEST(tarval_is_all_one(value));
		else
			TEST(!tarval_is_all_one(value));

		TEST(tarval_is_constant(value));

		TVS_EQUAL(tarval_abs(value),
		          tarval_is_negative(value) ? tarval_neg(value) : value);
	}

	test_compare(NULL, NULL);

	test_bitcast(mode);

	context = "";
}

static void test_float_tarvals(ir_mode *mode)
{
	ir_tarval *nan_payload0 = new_tarval_from_str("0xCAFEBABE12345678", 10, mode_Lu);
	ir_tarval *nan_payload1 = new_tarval_from_str("0x12345678DEADBEEF", 10, mode_Lu);

	ir_tarval *zero       = get_mode_null(mode);
	ir_tarval *one        = get_mode_one(mode);
	ir_tarval *minus_one  = tarval_neg(one);
	ir_tarval *minus_zero = tarval_neg(zero);
	ir_tarval *two        = new_tarval_from_str("2", 1, mode);
	ir_tarval *half       = new_tarval_from_str("0.5", 3, mode);
	ir_tarval *qnan       = new_tarval_nan(mode, false, nan_payload0);
	ir_tarval *snan       = new_tarval_nan(mode, true, nan_payload1);
	ir_tarval *inf        = get_mode_infinite(mode);
	ir_tarval *minus_inf  = tarval_neg(inf);
	ir_tarval *small      = get_tarval_small(mode);
	ir_tarval *epsilon    = get_tarval_epsilon(mode);
	ir_tarval *denorm     = tarval_mul(small, epsilon);
	ir_tarval *min        = get_mode_min(mode);
	ir_tarval *max        = get_mode_max(mode);
	ir_tarval *const values[] = {
		/* list of interesting values, keep it sorted */
		minus_inf,
		min,
		new_tarval_from_str("-13", 3, mode),
		minus_one,
		tarval_neg(epsilon),
		tarval_neg(small),
		tarval_neg(denorm),
		minus_zero,
		zero,
		denorm,
		small,
		epsilon,
		half,
		one,
		two,
		new_tarval_from_str("42", 2, mode),
		max,
		inf,
		qnan,
		snan,
	};
	tarvals = values;
	n_tarvals = ARRAY_SIZE(values);

	/* tarval_is_nan */
	for (unsigned i = 0, n = n_tarvals; i < n; ++i) {
		ir_tarval *value = tarvals[i];
		if (value == qnan) {
			TEST(tarval_is_nan(value));
			TEST(tarval_is_quiet_nan(value));
			TEST(!tarval_is_signaling_nan(value));
		} else if (value == snan) {
			TEST(tarval_is_nan(value));
			TEST(!tarval_is_quiet_nan(value));
			TEST(tarval_is_signaling_nan(value));
		} else {
			TEST(!tarval_is_nan(value));
			TEST(!tarval_is_quiet_nan(value));
			TEST(!tarval_is_signaling_nan(value));
		}
	}

	/* tarval_is_finite */
	for (unsigned i = 0, n = n_tarvals; i < n; ++i) {
		ir_tarval *value = tarvals[i];
		if (value == inf || value == minus_inf || value == qnan
		 || value == snan) {
			TEST(!tarval_is_finite(value));
		} else {
			TEST(tarval_is_finite(value));
		}
	}

	/* unops - involution */
	test_involution(tarval_neg);

	/* binops - neutral elements */
	test_neutral(tarval_add, minus_zero, true);
	test_neutral(tarval_sub, zero, false);
	test_neutral(tarval_mul, one, true);
	test_neutral(tarval_div, one, false);
	/* zero is a neutral element for tarval_add, except for the
	 * tarval_add(minus_zero, zero) == zero case.*/
	TVS_EQUAL(tarval_add(minus_zero, zero), zero);
	TVS_EQUAL(tarval_add(zero, minus_zero), zero);
	for (unsigned i = 0, n = n_tarvals; i < n; ++i) {
		ir_tarval *value = tarvals[i];
		if (value == minus_zero)
			continue;
		TVS_EQUAL(tarval_add(value, zero), value);
	}
	TVS_EQUAL(tarval_div(minus_zero, minus_inf), zero);

	/* binops - zero elements */
	for (unsigned i = 0, n = n_tarvals; i < n; ++i) {
		ir_tarval *value = tarvals[i];
		if (tarval_is_nan(value))
			continue;
		if (value == inf || value == minus_inf) {
			TEST(tarval_is_quiet_nan(tarval_mul(value, zero)));
		} else {
			TVS_EQUAL(tarval_mul(value, zero), tarval_is_negative(value) ? minus_zero : zero);
			TVS_EQUAL(tarval_sub(value, value), zero);
		}
	}

	/* binop commutativity */
	test_commutativity(tarval_add);
	test_commutativity(tarval_mul);

	/* NaN stays NaN (and keeps the payload) */
	for (unsigned i = 0, n = 2; i < n; ++i) {
		ir_tarval *nan = i == 0 ? qnan : snan;

		test_binop_nan(tarval_add, nan);
		test_binop_nan(tarval_sub, nan);
		test_binop_nan(tarval_mul, nan);
		test_binop_nan(tarval_div, nan);
		test_unop_nan(tarval_abs, nan);
	}

	/* NaN results */
	TEST(tarval_is_quiet_nan(tarval_div(zero, zero)));
	TEST(tarval_is_quiet_nan(tarval_div(minus_zero, zero)));
	TEST(tarval_is_quiet_nan(tarval_div(minus_zero, minus_zero)));

	TEST(tarval_is_quiet_nan(tarval_div(inf, inf)));
	TEST(tarval_is_quiet_nan(tarval_div(minus_inf, inf)));
	TEST(tarval_is_quiet_nan(tarval_div(minus_inf, minus_inf)));

	TEST(tarval_is_quiet_nan(tarval_mul(zero, inf)));
	TEST(tarval_is_quiet_nan(tarval_mul(minus_zero, inf)));
	TEST(tarval_is_quiet_nan(tarval_mul(zero, minus_inf)));
	TEST(tarval_is_quiet_nan(tarval_mul(minus_zero, minus_inf)));

	TEST(tarval_is_quiet_nan(tarval_add(minus_inf, inf)));
	TEST(tarval_is_quiet_nan(tarval_sub(inf, inf)));

	/* infinity results */
	for (unsigned i = 0, n = n_tarvals; i < n; ++i) {
		ir_tarval *value = tarvals[i];
		if (tarval_is_nan(value) || value == inf || value == minus_inf
		    || value == zero || value == minus_zero)
			continue;
		TVS_EQUAL(tarval_div(value, zero), tarval_is_negative(value) ? minus_inf : inf);
		TVS_EQUAL(tarval_div(value, minus_zero), tarval_is_negative(value) ? inf : minus_inf);
	}

	/* tarval_abs */
	for (unsigned i = 0, n = n_tarvals; i < n; ++i) {
		ir_tarval *value = tarvals[i];
		if (tarval_is_nan(value))
			continue;
		TVS_EQUAL(tarval_abs(value), tarval_is_negative(value) ? tarval_neg(value) : value);
	}

	/* misc */
	TVS_EQUAL(tarval_div(one, two), half);
	TVS_EQUAL(tarval_mul(half, two), one);
	for (unsigned i = 0, n = n_tarvals; i < n; ++i) {
		ir_tarval *value = tarvals[i];
		if (!tarval_is_finite(value))
			continue;
		TVS_EQUAL(tarval_div(value, inf), tarval_is_negative(value) ? minus_zero : zero);
		TVS_EQUAL(tarval_div(value, minus_inf), tarval_is_negative(value) ? zero : minus_zero);
	}

	test_compare(minus_zero, zero);

	/* TODO: check that overflows go to inf/minus_inf correctly */

	test_bitcast(mode);
}

int main(void)
{
	init_ident();
	init_tarval_1();
	init_irprog_1();
	init_mode();
	init_tarval_2();

	ir_mode *const new_modes[] = {
		new_int_mode("uint8",  8,  false, 0),
		new_int_mode("uint16", 16, false, 0),
		new_int_mode("uint24", 24, false, 0),
		new_int_mode("uint32", 32, false, 0),
		new_int_mode("uint64", 64, false, 0),
		new_int_mode("uint6",  6,  false, 0),
		new_int_mode("uint13", 13, false, 0),

		new_int_mode("int8",  8,  true, 0),
		new_int_mode("int16", 16, true, 0),
		new_int_mode("int24", 24, true, 0),
		new_int_mode("int32", 32, true, 0),
		new_int_mode("int64", 64, true, 0),
		new_int_mode("int6",  6,  true, 0),
		new_int_mode("int13", 13, true, 0),

		mode_F,
		mode_D,
		new_float_mode("E", irma_x86_extended_float, 15, 64, ir_overflow_indefinite),
	};
	modes = new_modes;
	n_modes = ARRAY_SIZE(new_modes);
	for (size_t i = 0, n = n_modes; i < n; ++i) {
		ir_mode *mode = modes[i];
		if (get_mode_arithmetic(mode) == irma_twos_complement)
			test_int_tarvals(mode);
		if (mode_is_float(mode))
			test_float_tarvals(mode);
	}

	/* misc */
	TEST(!tarval_is_constant(tarval_bad));
	TEST(!tarval_is_constant(tarval_unknown));

	finish_tarval();
	finish_mode();
	finish_ident();
	return result;
}
