/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Representation of and static computations on target machine
 *           values.
 * @date     2003
 * @author   Mathias Heil
 * @brief
 *
 * Values are stored in a format depending upon chosen arithmetic
 * module. Default uses strcalc and fltcalc.
 * This implementation assumes:
 *  - target has IEEE-754 floating-point arithmetic.
 */
#include "tv_t.h"

#include "bitfiddle.h"
#include "entity_t.h"
#include "firm_common.h"
#include "fltcalc.h"
#include "hashptr.h"
#include "hashptr.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "irprintf.h"
#include "panic.h"
#include "set.h"
#include "strcalc.h"
#include "util.h"
#include "xmalloc.h"
#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <stdlib.h>
#include <string.h>

/** Size of hash tables.  Should correspond to average number of distinct
 * constant target values */
#define N_CONSTANTS 2048

/** A set containing all existing tarvals. */
static struct set *tarvals = NULL;

static unsigned sc_value_length;
static unsigned fp_value_size;

/** The integer overflow mode. */
static bool wrap_on_overflow = true;

/** Hash a tarval. */
static unsigned hash_tv(ir_tarval const *const tv)
{
	return hash_combine(hash_ptr(tv->mode), hash_data(tv->value, tv->length));
}

static int cmp_tv(const void *p1, const void *p2, size_t n)
{
	(void)n;
	ir_tarval const *const tv1 = (ir_tarval const*)p1;
	ir_tarval const *const tv2 = (ir_tarval const*)p2;
	if (tv1->mode != tv2->mode)
		return 1;
	assert(tv1->length == tv2->length);
	return memcmp(tv1->value, tv2->value, tv1->length);
}

static ir_tarval *identify_tarval(ir_tarval const *const tv)
{
	unsigned hash = hash_tv(tv);
	return set_insert(ir_tarval, tarvals, tv, sizeof(ir_tarval) + tv->length,
	                  hash);
}

static ir_tarval *get_fp_tarval(const fp_value *value, ir_mode *mode)
{
	ir_tarval *const tv = ALLOCAF(ir_tarval, value, fp_value_size);
	tv->kind   = k_tarval;
	tv->mode   = mode;
	tv->length = fp_value_size;
	memcpy(tv->value, value, fp_value_size);
	return identify_tarval(tv);
}

static ir_tarval *get_int_tarval(const sc_word *value, ir_mode *mode)
{
	unsigned size = sc_value_length * sizeof(sc_word);
	ir_tarval *const tv = ALLOCAF(ir_tarval, value, size);
	tv->kind   = k_tarval;
	tv->mode   = mode;
	tv->length = size;
	memcpy(tv->value, value, size);
	assert(get_mode_arithmetic(mode) == irma_twos_complement);
	if (mode_is_signed(mode)) {
		sc_sign_extend((sc_word*)tv->value, get_mode_size_bits(mode));
	} else {
		sc_zero_extend((sc_word*)tv->value, get_mode_size_bits(mode));
	}
	return identify_tarval(tv);
}

static bool is_overflow(const sc_word *value, ir_mode *mode)
{
	assert(get_mode_arithmetic(mode) == irma_twos_complement);
	unsigned bits      = get_mode_size_bits(mode);
	bool     is_signed = mode_is_signed(mode);
	/* some of the uppper bits are not zero? */
	if (sc_get_highest_set_bit(value) >= (int)bits-is_signed) {
		/* all upper bits and sign bit 1 is fine for signed modes,
		 * otherwise it's an overflow */
		return !is_signed || sc_get_highest_clear_bit(value) >= (int)bits-1;
	}

	return false;
}

/** handle overflow */
static ir_tarval *get_int_tarval_overflow(const sc_word *value, ir_mode *mode)
{
	// We can only detect overflows for two complements here.
	assert(get_mode_arithmetic(mode) == irma_twos_complement);
	if (!wrap_on_overflow && is_overflow(value, mode))
		return tarval_bad;
	return get_int_tarval(value, mode);
}

static ir_tarval tarval_bad_obj;
static ir_tarval tarval_unknown_obj;

ir_tarval       *tarval_b_false;
ir_tarval       *tarval_b_true;
ir_tarval *const tarval_bad     = &tarval_bad_obj;
ir_tarval *const tarval_unknown = &tarval_unknown_obj;

/**
 * get the float descriptor for given mode.
 */
static const float_descriptor_t *get_descriptor(const ir_mode *mode)
{
	return &mode->float_desc;
}

ir_tarval *new_integer_tarval_from_str(const char *str, size_t len,
                                       int negative, unsigned char base,
                                       ir_mode *mode)
{
	sc_word *const buffer = ALLOCAN(sc_word, sc_value_length);
	bool ok = sc_val_from_str(negative, base, str, len, buffer);
	if (!ok)
		return tarval_bad;

	return get_int_tarval_overflow(buffer, mode);
}

static ir_tarval *new_tarval_from_str_int(const char *str, size_t len,
                                          ir_mode *mode)
{
	assert(len > 0);

	/* 1 sign character allowed */
	bool negative = false;
	if (str[0] == '-') {
		negative = true;
		++str;
		--len;
	} else if (str[0] == '+') {
		++str;
		--len;
	}

	/* a number starting with '0x' is hexadeciaml,
	 * a number starting with '0' (and at least 1 more char) is octal */
	unsigned base = 10;
	if (len >= 2 && str[0] == '0') {
		if (str[1] == 'x' || str[1] == 'X') {
			str += 2;
			len -= 2;
			base = 16;
		} else if (str[1] == 'b' || str[1] == 'B') {
			str += 2;
			len -= 2;
			base = 2;
		} else {
			++str;
			--len;
			base = 8;
		}
	}
	assert(len > 0);

	sc_word *const buffer = ALLOCAN(sc_word, sc_value_length);
	bool     const ok     = sc_val_from_str(negative, base, str, len, buffer);
	if (!ok)
		return tarval_bad;

	return get_int_tarval_overflow(buffer, mode);
}

ir_tarval *new_tarval_from_str(const char *str, size_t len, ir_mode *mode)
{
	assert(str != NULL);
	assert(len > 0);

	switch (get_mode_sort(mode)) {
	case irms_float_number: {
		fp_value *const buffer = (fp_value*)ALLOCAN(char, fp_value_size);
		fc_val_from_str(str, len, buffer);
		fc_cast(buffer, get_descriptor(mode), buffer);
		return get_fp_tarval(buffer, mode);
	}
	case irms_reference:
	case irms_int_number:
		return new_tarval_from_str_int(str, len, mode);

	case irms_auxiliary:
	case irms_data:
	case irms_internal_boolean:
		break;
	}
	panic("unsupported tarval creation with mode %F", mode);
}

ir_tarval *new_tarval_from_long(long l, ir_mode *mode)
{
	assert(get_mode_arithmetic(mode) == irma_twos_complement);
	sc_word *const buffer = ALLOCAN(sc_word, sc_value_length);
	sc_val_from_long(l, buffer);
	return get_int_tarval(buffer, mode);
}

ir_tarval *new_tarval_nan(ir_mode *mode, int signaling,
                          ir_tarval const *payload)
{
	assert(payload == NULL || get_mode_arithmetic(get_tarval_mode(payload))
	                          == irma_twos_complement);
	sc_word const *sc_payload = payload != NULL ? payload->value : NULL;

	assert(mode_is_float(mode));
	fp_value                 *buffer = (fp_value*)ALLOCAN(char, fp_value_size);
	const float_descriptor_t *desc   = get_descriptor(mode);
	fc_get_nan(desc, buffer, signaling, sc_payload);
	return get_fp_tarval(buffer, mode);
}

ir_tarval *new_tarval_from_bytes(unsigned char const *buf,
                                 ir_mode *mode)
{
	switch (get_mode_arithmetic(mode)) {
	case irma_twos_complement: {
		unsigned bits    = get_mode_size_bits(mode);
		unsigned n_bytes = bits/CHAR_BIT + (bits%CHAR_BIT != 0);
		sc_word *dest    = ALLOCAN(sc_word, sc_value_length);
		sc_val_from_bytes(buf, n_bytes, dest);
		return get_int_tarval(dest, mode);
	}
	case irma_ieee754:
	case irma_x86_extended_float: {
		fp_value *const buffer = (fp_value*)ALLOCAN(char, fp_value_size);
		fc_val_from_bytes(buffer, buf, get_descriptor(mode));
		return get_fp_tarval(buffer, mode);
	}
	case irma_none:
		break;
	}
	panic("tarval from byte requested for non storable mode");
}

void tarval_to_bytes(unsigned char *buffer, ir_tarval const *tv)
{
	switch (get_mode_arithmetic(get_tarval_mode(tv))) {
	case irma_ieee754:
	case irma_x86_extended_float:
		fc_val_to_bytes((const fp_value*)tv->value, buffer);
		return;
	case irma_twos_complement: {
		ir_mode *mode       = get_tarval_mode(tv);
		unsigned bits       = get_mode_size_bits(mode);
		unsigned buffer_len = bits/CHAR_BIT + (bits%CHAR_BIT != 0);
		sc_val_to_bytes((const sc_word*)tv->value, buffer, buffer_len);
		return;
	}
	case irma_none:
		break;
	}
	panic("unexpected arithmetic mode");
}

int tarval_is_long(ir_tarval const *tv)
{
	ir_mode *const mode = get_tarval_mode(tv);
	if (get_mode_arithmetic(mode) != irma_twos_complement)
		return false;
	if (get_mode_size_bytes(mode) <= sizeof(long))
		return true;

	/* the value might be too big to fit in a long */
	size_t long_bits = sizeof(long)*8;
	sc_word *temp = ALLOCAN(sc_word, sc_value_length);
	sc_max_from_bits(long_bits, mode_is_signed(mode), temp);
	if (sc_comp(tv->value, temp) == ir_relation_greater)
		return false;
	if (mode_is_signed(mode)) {
		sc_word *min = ALLOCAN(sc_word, sc_value_length);
		sc_min_from_bits(long_bits, true, min);
		sc_sign_extend(min, long_bits);
		if (sc_comp(tv->value, min) == ir_relation_less)
			return false;
	}
	return true;
}

long get_tarval_long(const ir_tarval* tv)
{
	assert(tarval_is_long(tv));
	return sc_val_to_long(tv->value);
}

bool tarval_is_uint64(ir_tarval const *tv)
{
	ir_mode *const mode = get_tarval_mode(tv);
	if (get_mode_arithmetic(mode) != irma_twos_complement)
		return false;
	if (get_mode_size_bytes(mode) <= sizeof(uint64_t))
		return true;

	/* the value might be too big to fit in a long */
	sc_word *temp = ALLOCAN(sc_word, sc_value_length);
	sc_max_from_bits(sizeof(uint64_t)*8, 0, temp);
	return sc_comp(tv->value, temp) & ir_relation_less_equal;
}

uint64_t get_tarval_uint64(ir_tarval const *tv)
{
	assert(tarval_is_uint64(tv));
	return sc_val_to_uint64(tv->value);
}

ir_tarval *new_tarval_from_long_double(long double d, ir_mode *mode)
{
	assert(mode_is_float(mode));
	fp_value *const buffer = (fp_value*)ALLOCAN(char, fp_value_size);
	fc_val_from_ieee754(d, buffer);
	fc_cast(buffer, get_descriptor(mode), buffer);
	return get_fp_tarval(buffer, mode);
}

ir_tarval *new_tarval_from_double(double d, ir_mode *mode)
{
	return new_tarval_from_long_double(d, mode);
}

int tarval_is_double(ir_tarval const *tv)
{
	return mode_is_float(tv->mode);
}

long double get_tarval_long_double(ir_tarval const *tv)
{
	assert(tarval_is_double(tv));
	return fc_val_to_ieee754((const fp_value*) tv->value);
}

double get_tarval_double(ir_tarval const *tv)
{
	return get_tarval_long_double(tv);
}

ir_mode *(get_tarval_mode)(ir_tarval const *tv)
{
	return get_tarval_mode_(tv);
}

/*
 * Special value query functions ============================================
 *
 * These functions calculate and return a tarval representing the requested
 * value.
 * The functions get_mode_{Max,Min,...} return tarvals retrieved from these
 * functions, but these are stored on initialization of the irmode module and
 * therefore the irmode functions should be preferred to the functions below.
 */

int (tarval_is_constant)(ir_tarval const *tv)
{
	return tarval_is_constant_(tv);
}

ir_tarval *(get_tarval_bad)(void)
{
	return get_tarval_bad_();
}

ir_tarval *(get_tarval_unknown)(void)
{
	return get_tarval_unknown_();
}

ir_tarval *(get_tarval_b_false)(void)
{
	return get_tarval_b_false_();
}

ir_tarval *(get_tarval_b_true)(void)
{
	return get_tarval_b_true_();
}

ir_tarval *get_tarval_small(ir_mode *mode)
{
	assert(mode_is_float(mode));
	fp_value                 *buffer = (fp_value*)ALLOCAN(char, fp_value_size);
	const float_descriptor_t *desc   = get_descriptor(mode);
	fc_get_small(desc, buffer);
	return get_fp_tarval(buffer, mode);
}

ir_tarval *get_tarval_epsilon(ir_mode *mode)
{
	assert(mode_is_float(mode));
	fp_value                 *buffer = (fp_value*)ALLOCAN(char, fp_value_size);
	const float_descriptor_t *desc   = get_descriptor(mode);
	fc_get_epsilon(desc, buffer);
	return get_fp_tarval(buffer, mode);
}

void init_mode_values(ir_mode* mode)
{
	switch (get_mode_sort(mode)) {
	case irms_float_number: {
		const float_descriptor_t *desc = get_descriptor(mode);
		fp_value                 *buf  = alloca(fp_value_size);
		mode->all_one     = tarval_bad;
		fc_get_inf(desc, buf, false);
		mode->infinity    = get_fp_tarval(buf, mode);
		fc_get_max(desc, buf, true); // min = negative maximum
		mode->min         = get_fp_tarval(buf, mode);
		fc_get_max(desc, buf, false);
		mode->max         = get_fp_tarval(buf, mode);
		fc_get_zero(desc, buf, false);
		mode->null        = get_fp_tarval(buf, mode);
		mode->one         = new_tarval_from_double(1.0, mode);
		break;
	}

	case irms_internal_boolean:
		mode->all_one   = tarval_b_true;
		mode->infinity  = tarval_bad;
		mode->min       = tarval_b_false;
		mode->max       = tarval_b_true;
		mode->null      = tarval_b_false;
		mode->one       = tarval_b_true;
		break;

	case irms_reference:
	case irms_int_number: {
		sc_word *buf  = ALLOCAN(sc_word, sc_value_length);
		unsigned bits = get_mode_size_bits(mode);
		bool     sign = mode_is_signed(mode);
		sc_max_from_bits(bits, false, buf);
		mode->all_one   = get_int_tarval(buf, mode);
		mode->infinity  = tarval_bad;
		sc_min_from_bits(bits, sign, buf);
		mode->min       = get_int_tarval(buf, mode);
		sc_max_from_bits(bits, sign, buf);
		mode->max       = get_int_tarval(buf, mode);
		sc_zero(buf);
		mode->null      = get_int_tarval(buf, mode);
		sc_set_bit_at(buf, 0);
		mode->one       = get_int_tarval(buf, mode);
		break;
	}

	case irms_auxiliary:
	case irms_data:
		mode->all_one   = tarval_bad;
		mode->min       = tarval_bad;
		mode->max       = tarval_bad;
		mode->null      = tarval_bad;
		mode->one       = tarval_bad;
		mode->infinity  = tarval_bad;
		break;
	}
}

int tarval_is_negative(ir_tarval const *const a)
{
	switch (get_mode_sort(a->mode)) {
	case irms_int_number:
	case irms_reference:
		if (!mode_is_signed(a->mode)) {
			return 0;
		} else {
			return sc_comp(a->value, get_mode_null(a->mode)->value) == ir_relation_less ? 1 : 0;
		}

	case irms_float_number:
		return fc_is_negative((const fp_value*) a->value);

	case irms_auxiliary:
	case irms_internal_boolean:
	case irms_data:
		break;
	}
	panic("invalid mode sort");
}

int tarval_is_null(ir_tarval const *tv)
{
	return tv == get_tarval_mode(tv)->null && tv != tarval_bad;
}

int tarval_is_one(ir_tarval const *tv)
{
	return tv == get_tarval_mode(tv)->one && tv != tarval_bad;
}

int tarval_is_all_one(ir_tarval const *tv)
{
	return tv == get_tarval_mode(tv)->all_one && tv != tarval_bad;
}

bool tarval_is_minus_null(ir_tarval const *const tv)
{
	assert(mode_is_float(get_tarval_mode(tv)));
	const fp_value *val = (const fp_value*)tv->value;
	return fc_is_negative(val) && fc_is_zero(val);
}

bool tarval_is_minus_one(ir_tarval const *tv)
{
	assert(mode_is_float(get_tarval_mode(tv)));
	const fp_value *val = (const fp_value*)tv->value;
	return fc_is_negative(val) && fc_zero_mantissa(val)
	    && fc_get_exponent(val) == 0 && !fc_is_inf(val) && !fc_is_nan(val);
}

ir_relation tarval_cmp(ir_tarval const *const a, ir_tarval const *const b)
{
	if (a == tarval_unknown || b == tarval_unknown)
		return ir_relation_true;
	if (a == tarval_bad || b == tarval_bad || a->mode != b->mode)
		return ir_relation_false;

	/* Here the two tarvals are unequal and of the same mode */
	switch (get_mode_sort(a->mode)) {
	case irms_float_number:
		/*
		 * BEWARE: we cannot compare a == b here, because
		 * a NaN is always Unordered to any other value, even to itself!
		 */
		return fc_comp((fp_value const*)a->value, (fp_value const*)b->value);

	case irms_reference:
	case irms_int_number:
		if (a == b)
			return ir_relation_equal;
		return sc_comp(a->value, b->value);

	case irms_internal_boolean:
		if (a == b)
			return ir_relation_equal;
		return a == tarval_b_true ? ir_relation_greater : ir_relation_less;

	case irms_auxiliary:
	case irms_data:
		break;
	}
	panic("invalid mode sort");
}

ir_tarval *tarval_convert_to(ir_tarval const *const src,
                             ir_mode *const dst_mode)
{
	if (src->mode == dst_mode)
		return (ir_tarval*)src;

	switch (get_mode_sort(src->mode)) {
	/* cast float to something */
	case irms_float_number:
		switch (get_mode_sort(dst_mode)) {
		case irms_float_number: {
			const float_descriptor_t *desc = get_descriptor(dst_mode);
			fp_value *const buffer = (fp_value*)ALLOCAN(char, fp_value_size);
			fc_cast((const fp_value*)src->value, desc, buffer);
			return get_fp_tarval(buffer, dst_mode);
		}

		case irms_reference:
		case irms_int_number: {
			fp_value *const buffer = (fp_value*)ALLOCAN(char, fp_value_size);
			fc_int((const fp_value*) src->value, buffer);
			sc_word *intval = ALLOCAN(sc_word, sc_value_length);
			flt2int_result_t cres
				= fc_flt2int(buffer, intval, get_mode_size_bits(dst_mode),
				             mode_is_signed(dst_mode));
			switch (cres) {
			case FLT2INT_POSITIVE_OVERFLOW:
				switch (get_mode_float_int_overflow(src->mode)) {
				case ir_overflow_indefinite:
					return get_mode_min(dst_mode);
				case ir_overflow_min_max:
					return get_mode_max(dst_mode);
				}
				break;
			case FLT2INT_NEGATIVE_OVERFLOW:
				return get_mode_min(dst_mode);
			case FLT2INT_BAD:
				return tarval_bad;
			case FLT2INT_OK:
				return get_int_tarval(intval, dst_mode);
			}
		}

		case irms_internal_boolean:
		case irms_auxiliary:
		case irms_data:
			break;
		}
		/* the rest can't be converted */
		return tarval_bad;

	/* cast int/characters to something */
	case irms_int_number:
		switch (get_mode_sort(dst_mode)) {

		case irms_reference:
		case irms_int_number: {
			sc_word *const buffer = ALLOCAN(sc_word, sc_value_length);
			memcpy(buffer, src->value, sc_value_length);
			return get_int_tarval_overflow(buffer, dst_mode);
		}

		case irms_float_number: {
			/* XXX floating point unit does not understand internal integer
			 * representation, convert to string first, then create float from
			 * string */
			char *buffer = ALLOCAN(char, 100);
			/* decimal string representation because hexadecimal output is
			 * interpreted unsigned by fc_val_from_str, so this is a HACK */
			int len = snprintf(buffer, 100, "%s",
				sc_print(src->value, get_mode_size_bits(src->mode), SC_DEC, mode_is_signed(src->mode)));

			fp_value *fpval = (fp_value*)ALLOCAN(char, fp_value_size);
			fc_val_from_str(buffer, len, fpval);
			fc_cast(fpval, get_descriptor(dst_mode), fpval);
			return get_fp_tarval(fpval, dst_mode);
		}
		case irms_auxiliary:
		case irms_data:
		case irms_internal_boolean:
			break;
		}
		break;

	case irms_reference:
		if (get_mode_arithmetic(dst_mode) == irma_twos_complement) {
			sc_word *const buffer = ALLOCAN(sc_word, sc_value_length);
			memcpy(buffer, src->value, sc_value_length);
			unsigned bits = get_mode_size_bits(src->mode);
			if (mode_is_signed(src->mode)) {
				sc_sign_extend(buffer, bits);
			} else {
				sc_zero_extend(buffer, bits);
			}
			return get_int_tarval_overflow(buffer, dst_mode);
		}
		break;

	case irms_auxiliary:
	case irms_data:
	case irms_internal_boolean:
		return tarval_bad;
	}

	return tarval_bad;
}

ir_tarval *tarval_bitcast(ir_tarval const *src, ir_mode *const dst_mode)
{
	ir_mode *const src_mode = get_tarval_mode(src);
	if (src_mode == dst_mode)
		return (ir_tarval*)src;
	unsigned const size = get_mode_size_bits(src_mode);
	assert(get_mode_size_bits(dst_mode) == size);

	unsigned       const buf_len = size/CHAR_BIT + (size%CHAR_BIT != 0);
	unsigned char *const buffer  = ALLOCAN(unsigned char, buf_len);
	tarval_to_bytes(buffer, src);
	return new_tarval_from_bytes(buffer, dst_mode);
}

ir_tarval *tarval_not(ir_tarval const *const a)
{
	ir_mode *const mode = a->mode;
	if (get_mode_sort(mode) == irms_internal_boolean)
		return a == tarval_b_true ? tarval_b_false : tarval_b_true;

	assert(get_mode_arithmetic(mode) == irma_twos_complement);
	sc_word *const buffer = ALLOCAN(sc_word, sc_value_length);
	sc_not(a->value, buffer);
	return get_int_tarval(buffer, mode);
}

ir_tarval *tarval_neg(ir_tarval const *const a)
{
	ir_mode *const mode = a->mode;
	switch (get_mode_sort(mode)) {
	case irms_int_number:
	case irms_reference: {
		sc_word *const buffer = ALLOCAN(sc_word, sc_value_length);
		sc_neg(a->value, buffer);
		return get_int_tarval_overflow(buffer, mode);
	}

	case irms_float_number: {
		fp_value *const buffer = (fp_value*)ALLOCAN(char, fp_value_size);
		fc_neg((const fp_value*)a->value, buffer);
		return get_fp_tarval(buffer, mode);
	}

	case irms_auxiliary:
	case irms_data:
	case irms_internal_boolean:
		break;
	}
	panic("invalid mode sort");
}

ir_tarval *tarval_add(ir_tarval const *a, ir_tarval const *b)
{
	if (mode_is_reference(a->mode) && a->mode != b->mode) {
		b = tarval_convert_to(b, a->mode);
	} else if (mode_is_reference(b->mode) && b->mode != a->mode) {
		a = tarval_convert_to(a, b->mode);
	}

	ir_mode *const mode = a->mode;
	assert(mode == b->mode);

	switch (get_mode_sort(mode)) {
	case irms_reference:
	case irms_int_number: {
		/* modes of a,b are equal, so result has mode of a as this might be the
		 * character */
		sc_word *const buffer = ALLOCAN(sc_word, sc_value_length);
		sc_add(a->value, b->value, buffer);
		return get_int_tarval_overflow(buffer, mode);
	}

	case irms_float_number: {
		fp_value *const buffer = (fp_value*)ALLOCAN(char, fp_value_size);
		fc_add((const fp_value*)a->value, (const fp_value*)b->value, buffer);
		return get_fp_tarval(buffer, mode);
	}

	case irms_auxiliary:
	case irms_data:
	case irms_internal_boolean:
		return tarval_bad;
	}
	panic("invalid mode sort");
}

ir_tarval *tarval_sub(ir_tarval const *a, ir_tarval const *b)
{
	ir_mode *dst_mode;
	if (mode_is_reference(a->mode)) {
		if (mode_is_reference(b->mode)) {
			dst_mode = get_reference_offset_mode(a->mode);
			assert(get_reference_offset_mode(b->mode) == dst_mode);
			a = tarval_convert_to(a, dst_mode);
		} else {
			dst_mode = a->mode;
		}
		b = tarval_convert_to(b, dst_mode);
	} else {
		assert(a->mode == b->mode);
		dst_mode = a->mode;
	}

	switch (get_mode_sort(dst_mode)) {
	case irms_reference:
	case irms_int_number: {
		/* modes of a,b are equal, so result has mode of a as this might be the
		 * character */
		sc_word *const buffer = ALLOCAN(sc_word, sc_value_length);
		sc_sub(a->value, b->value, buffer);
		return get_int_tarval_overflow(buffer, dst_mode);
	}

	case irms_float_number: {
		fp_value *const buffer = (fp_value*)ALLOCAN(char, fp_value_size);
		fc_sub((const fp_value*)a->value, (const fp_value*)b->value, buffer);
		return get_fp_tarval(buffer, dst_mode);
	}

	case irms_auxiliary:
	case irms_data:
	case irms_internal_boolean:
		return tarval_bad;
	}
	panic("invalid mode sort");
}

ir_tarval *tarval_mul(ir_tarval const *const a, ir_tarval const *const b)
{
	ir_mode *const mode = a->mode;
	assert(mode == b->mode);

	switch (get_mode_sort(mode)) {
	case irms_int_number:
	case irms_reference: {
		/* modes of a,b are equal */
		sc_word *const buffer = ALLOCAN(sc_word, sc_value_length);
		sc_mul(a->value, b->value, buffer);
		return get_int_tarval_overflow(buffer, mode);
	}

	case irms_float_number: {
		fp_value *const buffer = (fp_value*)ALLOCAN(char, fp_value_size);
		fc_mul((const fp_value*)a->value, (const fp_value*)b->value, buffer);
		return get_fp_tarval(buffer, mode);
	}

	case irms_auxiliary:
	case irms_data:
	case irms_internal_boolean:
		return tarval_bad;
	}
	panic("invalid mode sort");
}

ir_tarval *tarval_div(ir_tarval const *const a, ir_tarval const *const b)
{
	ir_mode *const mode = a->mode;
	assert(mode == b->mode);

	switch (get_mode_sort(a->mode)) {
	case irms_int_number:
	case irms_reference: {
		/* x/0 error */
		if (b == get_mode_null(mode))
			return tarval_bad;

		sc_word *const buffer = ALLOCAN(sc_word, sc_value_length);
		sc_div(a->value, b->value, buffer);
		return get_int_tarval(buffer, mode);
	}

	case irms_float_number: {
		fp_value *const buffer = (fp_value*)ALLOCAN(char, fp_value_size);
		fc_div((const fp_value*)a->value, (const fp_value*)b->value, buffer);
		return get_fp_tarval(buffer, mode);
	}

	case irms_auxiliary:
	case irms_data:
	case irms_internal_boolean:
		panic("operation not defined on mode");
	}
	panic("invalid mode sort");
}

ir_tarval *tarval_mod(ir_tarval const *const a, ir_tarval const *const b)
{
	ir_mode *const mode = a->mode;
	assert(b->mode == mode);
	assert(get_mode_arithmetic(mode) == irma_twos_complement);

	/* x/0 error */
	if (b == get_mode_null(mode))
		return tarval_bad;
	sc_word *const buffer = ALLOCAN(sc_word, sc_value_length);
	sc_mod(a->value, b->value, buffer);
	return get_int_tarval(buffer, mode);
}

ir_tarval *tarval_divmod(ir_tarval const *const a, ir_tarval const *const b,
                         ir_tarval **const mod)
{
	ir_mode *const mode = a->mode;
	assert(b->mode == mode);
	assert(get_mode_arithmetic(mode) == irma_twos_complement);

	sc_word *const div_res = ALLOCAN(sc_word, sc_value_length);
	sc_word *const mod_res = ALLOCAN(sc_word, sc_value_length);

	/* x/0 error */
	if (b == get_mode_null(mode))
		return tarval_bad;
	sc_divmod(a->value, b->value, div_res, mod_res);
	*mod = get_int_tarval(mod_res, mode);
	return get_int_tarval(div_res, mode);
}

ir_tarval *tarval_abs(ir_tarval const *const a)
{
	if (tarval_is_negative(a))
		return tarval_neg(a);
	return (ir_tarval*)a;
}

ir_tarval *tarval_and(ir_tarval const *const a, ir_tarval const *const b)
{
	ir_mode *const mode = a->mode;
	assert(b->mode == mode);
	if (get_mode_sort(mode) == irms_internal_boolean)
		return a == tarval_b_false ? (ir_tarval*)a : (ir_tarval*)b;

	assert(get_mode_arithmetic(mode) == irma_twos_complement);
	sc_word *const buffer = ALLOCAN(sc_word, sc_value_length);
	sc_and(a->value, b->value, buffer);
	return get_int_tarval(buffer, mode);
}

ir_tarval *tarval_andnot(ir_tarval const *const a, ir_tarval const *const b)
{
	ir_mode *const mode = a->mode;
	assert(b->mode == mode);
	if (get_mode_sort(mode) == irms_internal_boolean)
		return a == tarval_b_true && b == tarval_b_false ? tarval_b_true
		                                                 : tarval_b_false;
	assert(get_mode_arithmetic(mode) == irma_twos_complement);
	sc_word *const buffer = ALLOCAN(sc_word, sc_value_length);
	sc_andnot(a->value, b->value, buffer);
	return get_int_tarval(buffer, mode);
}

ir_tarval *tarval_or(ir_tarval const *const a, ir_tarval const *const b)
{
	ir_mode *const mode = a->mode;
	assert(b->mode == mode);
	if (get_mode_sort(mode) == irms_internal_boolean)
		return a == tarval_b_true ? (ir_tarval*)a : (ir_tarval*)b;

	assert(get_mode_arithmetic(mode) == irma_twos_complement);
	sc_word *const buffer = ALLOCAN(sc_word, sc_value_length);
	sc_or(a->value, b->value, buffer);
	return get_int_tarval(buffer, mode);
}

ir_tarval *tarval_ornot(ir_tarval const *const a, ir_tarval const *const b)
{
	ir_mode *const mode = a->mode;
	assert(b->mode == mode);
	if (get_mode_sort(mode) == irms_internal_boolean)
		return a == tarval_b_true || b == tarval_b_false ? tarval_b_true
		                                                 : tarval_b_false;
	assert(get_mode_arithmetic(mode) == irma_twos_complement);
	sc_word *const buffer = ALLOCAN(sc_word, sc_value_length);
	sc_ornot(a->value, b->value, buffer);
	return get_int_tarval(buffer, mode);
}

ir_tarval *tarval_eor(ir_tarval const *const a, ir_tarval const *const b)
{
	ir_mode *const mode = a->mode;
	assert(b->mode == mode);
	if (get_mode_sort(mode) == irms_internal_boolean)
		return a == b ? tarval_b_false : tarval_b_true;

	assert(get_mode_arithmetic(mode) == irma_twos_complement);
	sc_word *const buffer = ALLOCAN(sc_word, sc_value_length);
	sc_xor(a->value, b->value, buffer);
	return get_int_tarval(buffer, mode);
}

ir_tarval *tarval_shl(ir_tarval const *const a, ir_tarval const *const b)
{
	ir_mode *const a_mode = a->mode;
	assert(get_mode_arithmetic(a_mode) == irma_twos_complement);
	assert(get_mode_arithmetic(b->mode) == irma_twos_complement);

	sc_word *temp_val;
	if (get_mode_modulo_shift(a_mode) != 0) {
		temp_val = ALLOCAN(sc_word, sc_value_length);
		sc_word *const temp2 = ALLOCAN(sc_word, sc_value_length);
		sc_val_from_ulong(get_mode_modulo_shift(a_mode), temp2);
		sc_mod(b->value, temp2, temp_val);
	} else {
		temp_val = (sc_word*)b->value;
	}

	sc_word *const temp = ALLOCAN(sc_word, sc_value_length);
	sc_shl(a->value, temp_val, temp);
	return get_int_tarval(temp, a_mode);
}

ir_tarval *tarval_shl_unsigned(ir_tarval const *const a, unsigned b)
{
	ir_mode *const mode = a->mode;
	assert(get_mode_arithmetic(mode) == irma_twos_complement);

	unsigned const modulo = get_mode_modulo_shift(mode);
	if (modulo != 0)
		b %= modulo;
	assert((unsigned)(long)b==b);

	sc_word *const buffer = ALLOCAN(sc_word, sc_value_length);
	sc_shlI(a->value, (long)b, buffer);
	return get_int_tarval(buffer, mode);
}

ir_tarval *tarval_shr(ir_tarval const *const a, ir_tarval const *const b)
{
	ir_mode *const a_mode = a->mode;
	assert(get_mode_arithmetic(a_mode) == irma_twos_complement);
	assert(get_mode_arithmetic(b->mode) == irma_twos_complement);

	sc_word *temp_val;
	if (get_mode_modulo_shift(a_mode) != 0) {
		temp_val = ALLOCAN(sc_word, sc_value_length);
		sc_word *const temp2 = ALLOCAN(sc_word, sc_value_length);
		sc_val_from_ulong(get_mode_modulo_shift(a_mode), temp2);
		sc_mod(b->value, temp2, temp_val);
	} else {
		temp_val = (sc_word*)b->value;
	}

	sc_word *const temp = ALLOCAN(sc_word, sc_value_length);
	/* workaround for unnecessary internal higher precision */
	memcpy(temp, a->value, sc_value_length);
	sc_zero_extend(temp, get_mode_size_bits(a_mode));
	sc_shr(temp, temp_val, temp);
	return get_int_tarval(temp, a_mode);
}

ir_tarval *tarval_shr_unsigned(ir_tarval const *const a, unsigned b)
{
	ir_mode *const mode = a->mode;
	assert(get_mode_arithmetic(mode) == irma_twos_complement);

	unsigned const modulo = get_mode_modulo_shift(mode);
	if (modulo != 0)
		b %= modulo;
	assert((unsigned)(long)b==b);

	sc_word *const temp = ALLOCAN(sc_word, sc_value_length);
	/* workaround for unnecessary internal higher precision */
	memcpy(temp, a->value, sc_value_length);
	sc_zero_extend(temp, get_mode_size_bits(a->mode));
	sc_shrI(temp, (long)b, temp);
	return get_int_tarval(temp, mode);
}

ir_tarval *tarval_shrs(ir_tarval const *const a, ir_tarval const *const b)
{
	ir_mode *const a_mode = a->mode;
	assert(get_mode_arithmetic(a_mode) == irma_twos_complement);
	assert(get_mode_arithmetic(b->mode) == irma_twos_complement);

	sc_word *temp_val;
	if (get_mode_modulo_shift(a_mode) != 0) {
		temp_val = ALLOCAN(sc_word, sc_value_length);
		sc_word *const temp2 = ALLOCAN(sc_word, sc_value_length);
		sc_val_from_ulong(get_mode_modulo_shift(a_mode), temp2);
		sc_mod(b->value, temp2, temp_val);
	} else {
		temp_val = (sc_word*)b->value;
	}

	sc_word *const temp = ALLOCAN(sc_word, sc_value_length);
	sc_shrs(a->value, temp_val, get_mode_size_bits(a->mode), temp);
	return get_int_tarval(temp, a->mode);
}

ir_tarval *tarval_shrs_unsigned(ir_tarval const *const a, unsigned b)
{
	ir_mode *const mode = a->mode;
	assert(get_mode_arithmetic(mode) == irma_twos_complement);

	unsigned const modulo = get_mode_modulo_shift(mode);
	if (modulo != 0)
		b %= modulo;
	assert((unsigned)(long)b==b);

	sc_word *const temp = ALLOCAN(sc_word, sc_value_length);
	sc_shrsI(a->value, (long)b, get_mode_size_bits(mode), temp);
	return get_int_tarval(temp, mode);
}

int tarval_snprintf(char *buf, size_t len, ir_tarval const *tv)
{
	switch (get_mode_sort(tv->mode)) {
	case irms_reference:
		if (tv == tv->mode->null)
			return snprintf(buf, len, "NULL");
		/* FALLTHROUGH */
	case irms_int_number: {
		unsigned    bits = get_mode_size_bits(tv->mode);
		const char *str  = sc_print(tv->value, bits, SC_HEX, 0);
		return snprintf(buf, len, "0x%s", str);
	}

	case irms_float_number:
		return fc_print((const fp_value*)tv->value, buf, len, FC_DEC);

	case irms_internal_boolean:
		return snprintf(buf, len, "%s",
		                (tv == tarval_b_true) ? "true" : "false");

	default:
		if (tv == tarval_bad)
			return snprintf(buf, len, "<TV_BAD>");
		else if (tv == tarval_unknown)
			return snprintf(buf, len, "<TV_UNKNOWN>");
		else
			return snprintf(buf, len, "<TV_??""?>");
	}
}

static char hexchar(unsigned val)
{
	return "0123456789ABCDEF"[val];
}

static unsigned hexval(char c)
{
	if (is_digit(c))
		return c - '0';
	return (c - 'A')+10;
}

const char *ir_tarval_to_ascii(char *buf, size_t len, ir_tarval const *tv)
{
	ir_mode *const mode = get_tarval_mode(tv);
	switch (get_mode_sort(mode)) {
	case irms_internal_boolean:
	case irms_reference:
	case irms_int_number:
		return sc_print_buf(buf, len, tv->value, get_mode_size_bits(mode),
		                    SC_HEX, 0);
	case irms_float_number: {
		/* fc_print is not specific enough for nans/infs, so we simply dump the
		 * bit representation in hex. */
		unsigned       size  = get_mode_size_bytes(mode);
		unsigned char *bytes = ALLOCAN(unsigned char, size);
		fc_val_to_bytes((const fp_value*)tv->value, bytes);
		for (size_t i = 0; i < size; ++i) {
			unsigned char bits = bytes[i];
			buf[i*2]   = hexchar(bits & 0xf);
			buf[i*2+1] = hexchar(bits >> 4);
		}
		buf[size*2] = '\0';
		return buf;
	}
	case irms_data:
	case irms_auxiliary:
		if (tv == tarval_bad)
			return "bad";
		else if (tv == tarval_unknown)
			return "unknown";
		break;
	}
	panic("invalid tarval");
}

ir_tarval *ir_tarval_from_ascii(const char *buf, ir_mode *mode)
{
	size_t len = strlen(buf);
	switch (get_mode_sort(mode)) {
	case irms_reference:
	case irms_internal_boolean:
	case irms_int_number:
		return new_integer_tarval_from_str(buf, len, false, 16, mode);
	case irms_float_number: {
		unsigned       size = get_mode_size_bytes(mode);
		unsigned char *temp = ALLOCAN(unsigned char, size);
		for (size_t i = 0; i < size; ++i) {
			unsigned char val = hexval(buf[i*2]) | (hexval(buf[i*2+1]) << 4);
			temp[i] = val;
		}
		fp_value *const buffer = (fp_value*)ALLOCAN(char, fp_value_size);
		fc_val_from_bytes(buffer, temp, get_descriptor(mode));
		return get_fp_tarval(buffer, mode);
	}
	case irms_data:
	case irms_auxiliary:
		if (streq(buf, "bad"))
			return tarval_bad;
		else if (streq(buf, "unknown"))
			return tarval_unknown;
		break;
	}
	panic("invalid mode for tarval_from_ascii");
}

unsigned char get_tarval_sub_bits(ir_tarval const *tv, unsigned byte_ofs)
{
	switch (get_mode_arithmetic(tv->mode)) {
	case irma_twos_complement:
		return sc_sub_bits(tv->value, get_mode_size_bits(tv->mode), byte_ofs);
	case irma_ieee754:
	case irma_x86_extended_float:
		return fc_sub_bits((const fp_value*) tv->value, get_mode_size_bits(tv->mode), byte_ofs);
	default:
		panic("arithmetic mode not supported");
	}
}

int get_tarval_popcount(ir_tarval const *tv)
{
	ir_mode *const mode = get_tarval_mode(tv);
	assert(get_mode_arithmetic(mode) == irma_twos_complement);
	return sc_popcount(tv->value, get_mode_size_bits(mode));
}

int get_tarval_lowest_bit(ir_tarval const *tv)
{
	assert(get_mode_arithmetic(tv->mode) == irma_twos_complement);

	int l = get_mode_size_bytes(tv->mode);
	for (int i = 0; i < l; ++i) {
		unsigned char v = get_tarval_sub_bits(tv, (unsigned)i);

		if (v)
			return ntz(v) + i * 8;
	}
	return -1;
}

int get_tarval_highest_bit(ir_tarval const *tv)
{
	assert(get_mode_arithmetic(tv->mode) == irma_twos_complement);

	int l = get_mode_size_bytes(tv->mode);
	for (int i = l - 1; i >= 0; --i) {
		unsigned char v = get_tarval_sub_bits(tv, (unsigned)i);

		if (v)
			return 8*sizeof(unsigned) - nlz(v) + i * 8 - 1;
	}
	return -1;
}

unsigned get_tarval_magnitude(ir_tarval const *tv)
{
	assert(get_mode_arithmetic(tv->mode) == irma_twos_complement);
	unsigned const size = get_mode_size_bits(tv->mode);
	unsigned const neg  = tarval_get_bit(tv, size - 1);
	unsigned const ext  = neg ? (1U << SC_BITS) - 1 : 0;

	unsigned l = get_mode_size_bytes(tv->mode);
	for (unsigned i = l; i-- != 0;) {
		unsigned char const v = get_tarval_sub_bits(tv, i);
		if (v != ext)
			return i * SC_BITS + (32 - nlz(v ^ ext)) + 1;
	}

	return 1;
}

int tarval_zero_mantissa(ir_tarval const *tv)
{
	assert(get_mode_arithmetic(tv->mode) == irma_ieee754
	       || get_mode_arithmetic(tv->mode) == irma_x86_extended_float);
	return fc_zero_mantissa((const fp_value*) tv->value);
}

int tarval_get_exponent(ir_tarval const *tv)
{
	assert(get_mode_arithmetic(tv->mode) == irma_ieee754
	       || get_mode_arithmetic(tv->mode) == irma_x86_extended_float);
	return fc_get_exponent((const fp_value*) tv->value);
}

int tarval_ieee754_can_conv_lossless(ir_tarval const *tv, const ir_mode *mode)
{
	const float_descriptor_t *desc = get_descriptor(mode);
	return fc_can_lossless_conv_to((const fp_value*) tv->value, desc);
}

unsigned tarval_ieee754_get_exact(void)
{
	return fc_is_exact();
}

int tarval_is_nan(ir_tarval const *tv)
{
	if (!mode_is_float(tv->mode))
		return 0;
	return fc_is_nan((const fp_value*) tv->value);
}

int tarval_is_quiet_nan(ir_tarval const *tv)
{
	if (!tarval_is_nan(tv))
		return false;
	return fc_nan_is_quiet((const fp_value*)tv->value);
}

int tarval_is_signaling_nan(ir_tarval const *tv)
{
	if (!tarval_is_nan(tv))
		return false;
	return !fc_nan_is_quiet((const fp_value*)tv->value);
}

int tarval_is_finite(ir_tarval const *tv)
{
	if (mode_is_float(tv->mode))
		return !fc_is_nan((const fp_value*) tv->value)
			&& !fc_is_inf((const fp_value*) tv->value);
	return 1;
}

void tarval_set_wrap_on_overflow(int new_wrap_on_overflow)
{
	wrap_on_overflow = new_wrap_on_overflow;
}

int tarval_get_wrap_on_overflow(void)
{
	return wrap_on_overflow;
}

static ir_tarval *make_b_tarval(unsigned char const val)
{
	ir_tarval *const tv = XMALLOCFZ(ir_tarval, value, sc_value_length);
	tv->kind     = k_tarval;
	tv->length   = sc_value_length;
	tv->value[0] = val;
	/* mode will be set later */
	return tv;
}

void init_tarval_1(void)
{
	/* initialize the sets holding the tarvals with a comparison function and
	 * an initial size, which is the expected number of constants */
	tarvals = new_set(cmp_tv, N_CONSTANTS);
	/* calls init_strcalc() with needed size */
	init_fltcalc(128);

	sc_value_length = sc_get_value_length();
	fp_value_size   = fc_get_value_size();

	/* true/false must be created beforehand without mode due to a cyclic
	 * dependency between tarvals and modes. */
	tarval_b_true  = make_b_tarval(1);
	tarval_b_false = make_b_tarval(0);
}

void init_tarval_2(void)
{
	tarval_bad->kind      = k_tarval;
	tarval_bad->mode      = mode_BAD;

	tarval_unknown->kind  = k_tarval;
	tarval_unknown->mode  = mode_ANY;

	tarval_b_true->mode  = mode_b;
	tarval_b_false->mode = mode_b;
}

void finish_tarval(void)
{
	finish_strcalc();
	del_set(tarvals); tarvals = NULL;
}

bool tarval_in_range(ir_tarval const *const min, ir_tarval const *const val, ir_tarval const *const max)
{
	assert(tarval_cmp(min, max) & ir_relation_less_equal);
	assert(!tarval_is_nan(val));
	/* Shortcuts. */
	if (val == min)
		return true;
	if (min == max)
		return false;
	return tarval_cmp(min, val) & ir_relation_less_equal && tarval_cmp(val, max) & ir_relation_less_equal;
}
