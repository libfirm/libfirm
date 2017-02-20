/*
 * Test some strcalc operations.
 * Note: we use XMALLOCN (instead of ALLOCAN) here so valgrind can detect
 *       buffer overflows more easily.
 */

#include "strcalc.h"

#include "util.h"
#include "xmalloc.h"
#include <assert.h>
#include <limits.h>
#include <stdio.h>

static const unsigned precision = 72; /* some random non-po2 number (but a multiple of SC_BITS),
                                         as strcalc rounds up to multiple of SC_BITS anyway */
static unsigned buflen;

static bool equal(const sc_word *v0, const sc_word *v1)
{
	/* use precision/SC_BITS instead of buflen for now until we don't have these
	 * strange extra precision words anymore. */
	size_t len = precision/SC_BITS;
	return memcmp(v0, v1, len) == 0;
}

static void test_conv_print(unsigned long v, enum base_t base,
                            const char *expected)
{
	sc_word *temp = XMALLOCN(sc_word, buflen);
	sc_val_from_ulong(v, temp);
	char buf[128];
	const char *p = sc_print_buf(buf, sizeof(buf), temp, precision, base, false);
	assert(streq(p, expected));
	assert((unsigned long)sc_val_to_long(temp) == v);
}

static void test_conv(long v)
{
	sc_word *temp = XMALLOCN(sc_word, buflen);
	sc_val_from_long(v, temp);
	long back = sc_val_to_long(temp);
	assert(back == v);
	free(temp);

	char buf[128];
	snprintf(buf, sizeof(buf), "%ld", v);
	bool negative = false;
	const char *p = buf;
	if (*p == '-') {
		negative = true;
		++p;
	}
	sc_word *temp2 = XMALLOCN(sc_word, buflen);
	sc_val_from_str(negative, 10, p, strlen(p), temp2);
	long back2 = sc_val_to_long(temp2);
	assert(back2 == v);
	free(temp2);

	snprintf(buf, sizeof(buf), "%lX", (unsigned long)v);
	sc_word *temp3 = XMALLOCN(sc_word, buflen);
	sc_val_from_str(false, 16, buf, strlen(buf), temp3);
	long back3 = sc_val_to_long(temp3);
	assert(back3 == v);
	free(temp3);
	test_conv_print((unsigned long)v, SC_HEX, buf);
}

typedef void (*binop)(const sc_word *v0, const sc_word *v1, sc_word *dest);

static void check_commutativity(const sc_word *val0, const sc_word *val1,
                                binop op)
{
	sc_word *temp0 = XMALLOCN(sc_word, buflen);
	sc_word *temp1 = XMALLOCN(sc_word, buflen);
	op(val0, val1, temp0);
	op(val1, val0, temp1);
	assert(equal(temp0, temp1));
	free(temp0);
	free(temp1);
}

static void check_associativity(const sc_word *val0, const sc_word *val1,
                                const sc_word *val2, binop op)
{
	sc_word *temp0 = XMALLOCN(sc_word, buflen);
	sc_word *temp1 = XMALLOCN(sc_word, buflen);
	op(val0,  val1, temp0);
	op(temp0, val2, temp0);

	op(val1,  val2, temp1);
	op(val0, temp1, temp1);
	assert(equal(temp0, temp1));
	free(temp0);
	free(temp1);
}

static bool is_zero(const sc_word *val)
{
	return sc_is_zero(val, precision);
}

int main(void)
{
	init_strcalc(precision);
	buflen = sc_get_value_length();

	sc_word *temp  = XMALLOCN(sc_word, buflen);
	sc_word *temp1 = XMALLOCN(sc_word, buflen);

	sc_word *zero = XMALLOCN(sc_word, buflen);
	sc_zero(zero);
	for (unsigned i = 0; i <= precision; ++i) {
		assert(sc_is_zero(zero, i));
	}
	assert(sc_popcount(zero, precision) == 0);
	assert(!sc_is_negative(zero));

	sc_word *all_one = XMALLOCN(sc_word, buflen);
	sc_not(zero, all_one);
	for (unsigned i = 0; i <= precision; ++i) {
		assert(sc_is_all_one(all_one, i));
	}
	assert(sc_popcount(all_one, precision) == precision);
	assert(sc_is_negative(all_one));

	sc_word *one = XMALLOCN(sc_word, buflen);
	sc_zero(one);
	sc_set_bit_at(one, 0);
	for (unsigned i = 1; i <= precision; ++i) {
		assert(sc_popcount(one, i) == 1);
	}
	assert(!sc_is_negative(one));

	sc_add(all_one, one, temp);
	assert(is_zero(temp));
	sc_sub(zero, one, temp);
	assert(equal(temp, all_one));

	sc_neg(zero, temp);
	assert(equal(temp, zero));
	sc_neg(all_one, temp);
	assert(equal(temp, one));
	sc_neg(one, temp);
	assert(equal(temp, all_one));

	/* some tests with values where exactly 1 bit is set */
	for (unsigned i = 0; i < precision; ++i) {
		sc_zero(temp);
		sc_set_bit_at(temp, i);
		assert(sc_popcount(temp, precision) == 1);
		sc_shlI(one, i, temp1);
		assert(equal(temp, temp1));
		sc_shrI(temp, i, temp1);
		assert(equal(temp1, one));
		assert(sc_get_lowest_set_bit(temp) == (int)i);
		assert(sc_get_highest_set_bit(temp) == (int)i);
		// following disabled: internal precision is currently higher than
		// precision...
		//assert(sc_is_negative(temp) == (i == precision-1));
	}

	sc_word *alt0 = XMALLOCN(sc_word, buflen);
	sc_word *alt1 = XMALLOCN(sc_word, buflen);
	sc_zero(alt0);
	sc_zero(alt1);
	for (unsigned i = 0; i < precision; ++i) {
		if ((i & 1) == 0)
			sc_set_bit_at(alt0, i);
		else
			sc_set_bit_at(alt1, i);
	}
	assert(sc_popcount(alt0, precision) == precision/2+(precision&1));
	assert(sc_popcount(alt1, precision) == precision/2);
	sc_shrI(alt1, 1, temp);
	assert(equal(alt0, temp));
	sc_shlI(alt0, 1, temp);
	assert(equal(alt1, temp));

	sc_word *v2    = XMALLOCNZ(sc_word, buflen);
	sc_word *v4    = XMALLOCNZ(sc_word, buflen);
	sc_word *v8    = XMALLOCNZ(sc_word, buflen);
	sc_word *v16   = XMALLOCNZ(sc_word, buflen);
	sc_word *v2048 = XMALLOCNZ(sc_word, buflen);
	sc_set_bit_at(v2, 1);
	sc_set_bit_at(v4, 2);
	sc_set_bit_at(v8, 3);
	sc_set_bit_at(v16, 4);
	sc_set_bit_at(v2048, 11);

	sc_word *highbit = XMALLOCNZ(sc_word, buflen);
	sc_set_bit_at(highbit, precision-1);

	// following disabled: internal precision is currently higher than
	// precision...
	//assert(sc_is_negative(highbit));

	sc_word *cafebabe = XMALLOCN(sc_word, buflen);
	sc_val_from_str(false, 16, "CAFEBABE", 8, cafebabe);

	const sc_word * const vals[] = {
		zero,
		all_one,
		one,
		alt0,
		alt1,
		v2,
		v4,
		v8,
		v16,
		v2048,
		highbit,
		cafebabe
	};
	for (unsigned i = 0; i < ARRAY_SIZE(vals); ++i) {
		/* test neutral elements */
		const sc_word *val = vals[i];
		sc_add(val, zero, temp);
		assert(equal(temp, val));
		sc_add(zero, val, temp);
		assert(equal(temp, val));
		sc_sub(val, zero, temp);
		assert(equal(temp, val));
		sc_mul(val, one, temp);
		assert(equal(temp, val));
		sc_div(val, one, temp);
		assert(equal(temp, val));
		sc_neg(val, temp);
		sc_neg(temp, temp);
		assert(equal(temp, val));
		sc_not(val, temp);
		sc_not(temp, temp);
		assert(equal(temp, val));
		sc_or(val, zero, temp);
		assert(equal(temp, val));
		sc_xor(val, zero, temp);
		assert(equal(temp, val));
		sc_and(val, all_one, temp);
		assert(equal(temp, val));
		sc_andnot(val, zero, temp);
		assert(equal(temp, val));
		sc_shl(val, zero, temp);
		assert(equal(temp, val));
		sc_shr(val, zero, temp);
		assert(equal(temp, val));
		sc_shrs(val, zero, precision, temp);
		assert(equal(temp, val));

		/* test zero */
		sc_sub(val, val, temp);
		assert(is_zero(temp));
		sc_xor(val, val, temp);
		assert(is_zero(temp));
		sc_shlI(val, precision, temp);
		assert(is_zero(temp));

		/* workaround until we don't have this stupid
		 * calc_buffer_size*4 > precision anymore */
		memcpy(temp, val, buflen);
		sc_zero_extend(temp, precision);

		sc_shrI(temp, precision, temp);
		assert(is_zero(temp));
		sc_shrsI(val, precision, precision, temp);
		if (sc_get_bit_at(val, precision-1))
			assert(equal(temp, all_one));
		else
			assert(is_zero(temp));
		sc_neg(val, temp);
		sc_add(val, temp, temp);
		assert(is_zero(temp));
		sc_and(val, zero, temp);
		assert(is_zero(temp));
		sc_andnot(val, all_one, temp);
		assert(is_zero(temp));
		sc_mul(val, zero, temp);
		assert(is_zero(temp));

		/* test if zero/sign extension is equivalent to shl+shr combinations */
		for (unsigned b = 0; b < precision; ++b) {
			sc_shlI(val, b, temp);
			sc_zero_extend(temp, precision); /* higher precision workaround */
			sc_shrI(temp, b, temp);
			memcpy(temp1, val, buflen);
			sc_zero_extend(temp1, precision-b);
			assert(equal(temp, temp1));

			if (b > 0) {
				sc_shlI(val, precision-b, temp);
				sc_zero_extend(temp, precision); /* higher precision workaround */
				sc_shrsI(temp, precision-b, precision, temp);
				memcpy(temp1, val, buflen);
				sc_sign_extend(temp1, b);
				assert(equal(temp, temp1));
			}
		}

		/* test if we shl+shr combinations can isolate a specific bit */
		for (unsigned b = 0; b < precision; ++b) {
			sc_shlI(val, precision-b-1, temp);
			/* workaround internal higher precision... */
			sc_zero_extend(temp, precision);
			sc_shrI(temp, precision-1, temp);
			assert(!is_zero(temp) == sc_get_bit_at(val, b));
		}

		/* misc */
		int high = sc_get_highest_set_bit(val);
		int low  = sc_get_lowest_set_bit(val);
		assert(high >= low);
		assert((int)sc_popcount(val, precision) <= (high-low+1));
	}

	/* test commutativity + associativity */
	for (unsigned i0 = 0; i0 < ARRAY_SIZE(vals); ++i0) {
		const sc_word *val0 = vals[i0];
		for (unsigned i1 = 0; i1 < ARRAY_SIZE(vals); ++i1) {
			const sc_word *val1 = vals[i1];
			check_commutativity(val0, val1, sc_add);
			check_commutativity(val0, val1, sc_mul);
			check_commutativity(val0, val1, sc_or);
			check_commutativity(val0, val1, sc_and);
			check_commutativity(val0, val1, sc_xor);

			for (unsigned i2 = 0; i2 < ARRAY_SIZE(vals); ++i2) {
				const sc_word *val2 = vals[i2];
				check_associativity(val0, val1, val2, sc_add);
				check_associativity(val0, val1, val2, sc_or);
				check_associativity(val0, val1, val2, sc_and);
				check_associativity(val0, val1, val2, sc_xor);
				check_associativity(val0, val1, val2, sc_mul);
			}
		}
	}

	/* test printing/conversion */
	test_conv_print(1, SC_HEX, "1");
	test_conv_print(1, SC_DEC, "1");

	test_conv_print(0xcafebabe, SC_HEX, "CAFEBABE");
	test_conv_print(0xcafebabe, SC_DEC, "3405691582");

	test_conv(0);
	test_conv(-1);
	test_conv(-2);
	test_conv(-4);
	test_conv(-8);
	test_conv(1);
	test_conv(2);
	test_conv(4);
	test_conv(8);
	test_conv(42);
	test_conv(13);
	test_conv(LONG_MAX);
	test_conv(LONG_MIN);

	return 0;
}
