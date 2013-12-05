#include "strcalc.h"

#include <assert.h>
#include "xmalloc.h"
#include "util.h"

static const unsigned precision = 60; /* some random non-po2 number (but a multiple of 4),
                                         as strcalc rounds up to multiple of 4 anyway */
static unsigned buflen;

static bool equal(const sc_word *v0, const sc_word *v1)
{
	/* use precision/4 instead of buflen for now until we don't have these
	 * strange extra precision words anymore. */
	size_t len = precision/4;
	return memcmp(v0, v1, len) == 0;
}

typedef void (*binop)(const sc_word *v0, const sc_word *v1, sc_word *dest);

static void check_commutativity(const sc_word *val0, const sc_word *val1,
                                binop op)
{
	sc_word *temp0 = ALLOCAN(sc_word, buflen);
	sc_word *temp1 = ALLOCAN(sc_word, buflen);
	op(val0, val1, temp0);
	op(val1, val0, temp1);
	assert(equal(temp0, temp1));
}

static void check_associativity(const sc_word *val0, const sc_word *val1,
                                const sc_word *val2, binop op)
{
	sc_word *temp0 = ALLOCAN(sc_word, buflen);
	sc_word *temp1 = ALLOCAN(sc_word, buflen);
	op(val0,  val1, temp0);
	op(temp0, val2, temp0);

	op(val1,  val2, temp1);
	op(val0, temp1, temp1);
	assert(equal(temp0, temp1));
}

static bool is_zero(const sc_word *val)
{
	return sc_is_zero(val, precision);
}

int main(void)
{
	init_strcalc(precision);
	buflen = sc_get_buffer_length() + 1;

	sc_word *temp = ALLOCAN(sc_word, buflen);

	sc_word *zero = ALLOCAN(sc_word, buflen);
	sc_zero(zero);
	for (unsigned i = 0; i <= precision; ++i) {
		assert(sc_is_zero(zero, i));
	}
	assert(sc_popcount(zero, precision) == 0);
	assert(!sc_is_negative(zero));

	sc_word *all_one = ALLOCAN(sc_word, buflen);
	sc_not(zero, all_one);
	for (unsigned i = 0; i <= precision; ++i) {
		assert(sc_is_all_one(all_one, i));
	}
	assert(sc_popcount(all_one, precision) == precision);
	assert(sc_is_negative(all_one));

	sc_word *one = ALLOCAN(sc_word, buflen);
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

	for (unsigned i = 0; i < precision; ++i) {
		sc_zero(temp);
		sc_set_bit_at(temp, i);
		assert(sc_popcount(temp, precision) == 1);
		// following disabled: internal precision is currently higher than
		// precision...
		//assert(sc_is_negative(temp) == (i == precision-1));
	}

	sc_word *alt0 = ALLOCAN(sc_word, buflen);
	sc_word *alt1 = ALLOCAN(sc_word, buflen);
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

	sc_word *v2    = ALLOCANZ(sc_word, buflen);
	sc_word *v4    = ALLOCANZ(sc_word, buflen);
	sc_word *v8    = ALLOCANZ(sc_word, buflen);
	sc_word *v16   = ALLOCANZ(sc_word, buflen);
	sc_word *v2048 = ALLOCANZ(sc_word, buflen);
	sc_set_bit_at(v2, 1);
	sc_set_bit_at(v4, 2);
	sc_set_bit_at(v8, 3);
	sc_set_bit_at(v16, 4);
	sc_set_bit_at(v2048, 11);

	sc_word *highbit = ALLOCANZ(sc_word, buflen);
	sc_set_bit_at(highbit, precision-1);

	// following disabled: internal precision is currently higher than
	// precision...
	//assert(sc_is_negative(highbit));

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
		highbit
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
		sc_shl(val, zero, precision, false, temp);
		assert(equal(temp, val));
		sc_shr(val, zero, precision, false, temp);
		assert(equal(temp, val));
		sc_shrs(val, zero, precision, false, temp);
		assert(equal(temp, val));

		/* test zero */
		sc_sub(val, val, temp);
		assert(is_zero(temp));
		sc_xor(val, val, temp);
		assert(is_zero(temp));
		sc_shlI(val, precision, precision, false, temp);
		assert(is_zero(temp));
		sc_shrI(val, precision, precision, false, temp);
		assert(is_zero(temp));
		sc_shrsI(val, precision, precision, false, temp);
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

	sc_print(all_one, precision, SC_HEX, false);

	return 0;
}
