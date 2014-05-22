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
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdlib.h>
#include <strings.h>

#include "bitfiddle.h"
#include "tv_t.h"
#include "set.h"
#include "entity_t.h"
#include "irmode_t.h"
#include "irnode.h"
#include "strcalc.h"
#include "fltcalc.h"
#include "util.h"
#include "xmalloc.h"
#include "firm_common.h"
#include "error.h"

/** Size of hash tables.  Should correspond to average number of distinct
 * constant target values */
#define N_CONSTANTS 2048

/****************************************************************************
 *   local definitions and macros
 ****************************************************************************/
#define INSERT_TARVAL(tv) (set_insert(ir_tarval, tarvals, (tv), offsetof(ir_tarval, value) + (tv)->length, hash_tv((tv))))

/** A set containing all existing tarvals. */
static struct set *tarvals = NULL;

static unsigned sc_value_length;

/** The integer overflow mode. */
static tarval_int_overflow_mode_t int_overflow_mode = TV_OVERFLOW_WRAP;

/** Hash a value. Treat it as a byte array. */
static unsigned hash_val(unsigned char const *value, size_t length)
{
	/* scramble the byte - array */
	unsigned hash = 0;
	for (size_t i = 0; i < length; ++i) {
		hash += (hash << 5) ^ (hash >> 27) ^ value[i];
		hash += (hash << 11) ^ (hash >> 17);
	}
	return hash;
}

/** Hash a tarval. */
static unsigned hash_tv(const ir_tarval *tv)
{
	return (unsigned)((hash_val(tv->value, tv->length) ^ PTR_TO_INT(tv->mode)) + tv->length);
}

static int cmp_tv(const void *p1, const void *p2, size_t n)
{
	const ir_tarval *tv1 = (const ir_tarval*) p1;
	const ir_tarval *tv2 = (const ir_tarval*) p2;
	(void) n;

	assert(tv1->kind == k_tarval);
	assert(tv2->kind == k_tarval);
	if (tv1->mode < tv2->mode)
		return -1;
	if (tv1->mode > tv2->mode)
		return 1;
	if (tv1->length < tv2->length)
		return -1;
	if (tv1->length > tv2->length)
		return 1;
	return memcmp(tv1->value, tv2->value, tv1->length);
}

/** finds tarval with value/mode or creates new tarval */
static ir_tarval *get_tarval(const void *value, size_t length, ir_mode *mode)
{
	ir_tarval *const tv = ALLOCAF(ir_tarval, value, length);
	tv->kind   = k_tarval;
	tv->mode   = mode;
	tv->length = length;
	memcpy(tv->value, value, length);
	if (get_mode_arithmetic(mode) == irma_twos_complement) {
		if (mode_is_signed(mode)) {
			sc_sign_extend((sc_word*)tv->value, get_mode_size_bits(mode));
		} else {
			sc_zero_extend((sc_word*)tv->value, get_mode_size_bits(mode));
		}
	}
	/* if there is such a tarval, it is returned, else tv is copied
	 * into the set */
	return INSERT_TARVAL(tv);
}

/** handle overflow */
static ir_tarval *get_tarval_overflow(const void *value, size_t length,
                                      ir_mode *mode)
{
	switch (get_mode_sort(mode)) {
	case irms_reference: {
		/* addresses always wrap around */
		sc_word *temp = ALLOCAN(sc_word, sc_value_length);
		memcpy(temp, value, sc_value_length);
		unsigned bits = get_mode_size_bits(mode);
		/* the sc_ module expects that all bits are set ... */
		if (mode_is_signed(mode)) {
			sc_sign_extend(temp, bits);
		} else {
			sc_zero_extend(temp, bits);
		}
		return get_tarval(temp, length, mode);
	}

	case irms_int_number:
		if (sc_comp(value, get_mode_max(mode)->value) == ir_relation_greater) {
			switch (tarval_get_integer_overflow_mode()) {
			case TV_OVERFLOW_SATURATE:
				return get_mode_max(mode);
			case TV_OVERFLOW_WRAP: {
				sc_word *temp = ALLOCAN(sc_word, sc_value_length);
				memcpy(temp, value, sc_value_length);
				unsigned bits = get_mode_size_bits(mode);
				/* the sc_ module expects that all bits are set ... */
				if (mode_is_signed(mode)) {
					sc_sign_extend(temp, bits);
				} else {
					sc_zero_extend(temp, bits);
				}
				return get_tarval(temp, length, mode);
			}
			case TV_OVERFLOW_BAD:
				return tarval_bad;
			default:
				return get_tarval(value, length, mode);
			}
		}
		if (sc_comp(value, get_mode_min(mode)->value) == ir_relation_less) {
			switch (tarval_get_integer_overflow_mode()) {
			case TV_OVERFLOW_SATURATE:
				return get_mode_min(mode);
			case TV_OVERFLOW_WRAP: {
				sc_word *temp = ALLOCAN(sc_word, sc_value_length);
				memcpy(temp, value, sc_value_length);
				return get_tarval(temp, length, mode);
			}
			case TV_OVERFLOW_BAD:
				return tarval_bad;
			default:
				return get_tarval(value, length, mode);
			}
		}
		break;

	case irms_float_number:
		break;

	default:
		break;
	}
	return get_tarval(value, length, mode);
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

static ir_tarval *get_tarval_from_fp_value(const fp_value *val, ir_mode *mode)
{
	const float_descriptor_t *desc          = get_descriptor(mode);
	const int                 buffer_length = fc_get_buffer_length();
	fp_value                 *tmp           = alloca(buffer_length);
	memcpy(tmp, val, buffer_length);
	fp_value *casted_val = fc_cast(tmp, desc, NULL);
	return get_tarval(casted_val, buffer_length, mode);
}

ir_tarval *new_integer_tarval_from_str(const char *str, size_t len,
                                       int negative, unsigned char base,
                                       ir_mode *mode)
{
	sc_word *buffer = ALLOCAN(sc_word, sc_value_length);
	bool ok = sc_val_from_str(negative, base, str, len, buffer);
	if (!ok)
		return tarval_bad;

	return get_tarval_overflow(buffer, sc_value_length, mode);
}

static ir_tarval *new_tarval_from_str_int(const char *str, size_t len,
                                          ir_mode *mode)
{
	/* skip leading spaces */
	while (len > 0 && str[0] == ' ') {
		++str;
		--len;
	}
	if (len == 0)
		return tarval_bad;

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
	if (len == 0)
		return tarval_bad;

	sc_word *buffer = ALLOCAN(sc_word, sc_value_length);
	bool     ok     = sc_val_from_str(negative, base, str, len, buffer);
	if (!ok)
		return tarval_bad;

	return get_tarval_overflow(buffer, sc_value_length, mode);
}

ir_tarval *new_tarval_from_str(const char *str, size_t len, ir_mode *mode)
{
	assert(str != NULL);
	assert(len > 0);

	switch (get_mode_sort(mode)) {
	case irms_internal_boolean:
		/* match [tT][rR][uU][eE]|[fF][aA][lL][sS][eE] */
		if (!strcasecmp(str, "true"))
			return tarval_b_true;
		else if (!strcasecmp(str, "false"))
			return tarval_b_false;
		else
			/* XXX This is C semantics */
			return atoi(str) ? tarval_b_true : tarval_b_false;

	case irms_float_number: {
		fp_value *val = fc_val_from_str(str, len, NULL);
		return get_tarval_from_fp_value(val, mode);
	}
	case irms_reference:
		if (!strcasecmp(str, "null"))
			return get_mode_null(mode);
		/* FALLTHROUGH */
	case irms_int_number:
		return new_tarval_from_str_int(str, len, mode);
	default:
		panic("Unsupported tarval creation with mode %F", mode);
	}
}

ir_tarval *new_tarval_from_long(long l, ir_mode *mode)
{
	switch (get_mode_sort(mode))   {
	case irms_internal_boolean:
		/* XXX C semantics ! */
		return l ? tarval_b_true : tarval_b_false ;

	case irms_reference:
		/* same as integer modes */
	case irms_int_number: {
		sc_word *buffer = ALLOCAN(sc_word, sc_value_length);
		sc_val_from_long(l, buffer);
		return get_tarval(buffer, sc_value_length, mode);
	}

	case irms_float_number:
		return new_tarval_from_double((long double)l, mode);

	default:
		panic("unsupported mode sort");
	}
}

ir_tarval *new_tarval_from_bytes(unsigned char const *buf,
                                 ir_mode *mode, int big_endian)
{
	switch (get_mode_arithmetic(mode)) {
	case irma_twos_complement: {
		if (get_mode_size_bytes(mode) == (unsigned)-1)
			return tarval_bad;
		sc_word *dest = ALLOCAN(sc_word, sc_value_length);
		sc_val_from_bytes(buf, get_mode_size_bytes(mode), big_endian, dest);
		return get_tarval(dest, sc_value_length, mode);
	}
	case irma_ieee754:
	case irma_x86_extended_float: {
		if (big_endian) {
			unsigned       size = get_mode_size_bytes(mode);
			unsigned char *temp = ALLOCAN(unsigned char, size);
			for (unsigned i = 0; i < size; ++i) {
				temp[i] = buf[size-i-1];
			}
			buf = temp;
		}
		fc_val_from_bytes(NULL, buf, get_descriptor(mode));
		return get_tarval_from_fp_value(fc_get_buffer(), mode);
	}
	case irma_none:
		break;
	}
	panic("tarval from byte requested for non storable mode");
}

int tarval_is_long(const ir_tarval *tv)
{
	ir_mode *mode = get_tarval_mode(tv);
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

bool tarval_is_uint64(const ir_tarval *tv)
{
	ir_mode *mode = get_tarval_mode(tv);
	if (get_mode_arithmetic(mode) != irma_twos_complement)
		return false;
	if (get_mode_size_bytes(mode) <= sizeof(uint64_t))
		return true;

	/* the value might be too big to fit in a long */
	sc_word *temp = ALLOCAN(sc_word, sc_value_length);
	sc_max_from_bits(sizeof(uint64_t)*8, 0, temp);
	return sc_comp(tv->value, temp) & ir_relation_less_equal;
}

uint64_t get_tarval_uint64(const ir_tarval *tv)
{
	assert(tarval_is_uint64(tv));
	return sc_val_to_uint64(tv->value);
}

ir_tarval *new_tarval_from_long_double(long double d, ir_mode *mode)
{
	assert(mode && (get_mode_sort(mode) == irms_float_number));
	fp_value *val = fc_val_from_ieee754(d, NULL);
	return get_tarval_from_fp_value(val, mode);
}

ir_tarval *new_tarval_from_double(double d, ir_mode *mode)
{
	return new_tarval_from_long_double(d, mode);
}

int tarval_is_double(const ir_tarval *tv)
{
	return get_mode_sort(tv->mode) == irms_float_number;
}

long double get_tarval_long_double(const ir_tarval *tv)
{
	assert(tarval_is_double(tv));
	return fc_val_to_ieee754((const fp_value*) tv->value);
}

double get_tarval_double(const ir_tarval *tv)
{
	return get_tarval_long_double(tv);
}

ir_mode *(get_tarval_mode)(const ir_tarval *tv)
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

int (tarval_is_constant)(const ir_tarval *tv)
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
	if (!mode_is_float(mode))
		panic("mode %+F does not support small value");
	const float_descriptor_t *desc = get_descriptor(mode);
	fc_get_small(desc, NULL);
	return get_tarval(fc_get_buffer(), fc_get_buffer_length(), mode);
}

ir_tarval *get_tarval_epsilon(ir_mode *mode)
{
	if (!mode_is_float(mode))
		panic("mode %+F does not support small value");
	const float_descriptor_t *desc = get_descriptor(mode);
	fc_get_epsilon(desc, NULL);
	return get_tarval(fc_get_buffer(), fc_get_buffer_length(), mode);
}

ir_tarval *get_tarval_minus_inf(ir_mode *mode)
{
	if (get_mode_sort(mode) != irms_float_number)
		panic("mode %F does not support -inf value", mode);
	const float_descriptor_t *desc = get_descriptor(mode);
	fc_get_inf(desc, NULL, true);
	return get_tarval(fc_get_buffer(), fc_get_buffer_length(), mode);
}

void init_mode_values(ir_mode* mode)
{
	switch (get_mode_sort(mode)) {
	case irms_float_number: {
		const float_descriptor_t *desc   = get_descriptor(mode);
		unsigned                  buflen = fc_get_buffer_length();
		fp_value                 *buf    = alloca(buflen);
		mode->all_one     = tarval_bad;
		fc_get_inf(desc, buf, false);
		mode->infinity    = get_tarval(buf, buflen, mode);
		fc_get_qnan(desc, buf);
		mode->nan         = get_tarval(buf, buflen, mode);
		fc_get_max(desc, buf, true); // min = negative maximum
		mode->min         = get_tarval(buf, buflen, mode);
		mode->null        = new_tarval_from_double(0.0, mode);
		mode->one         = new_tarval_from_double(1.0, mode);
		mode->minus_one   = mode_is_signed(mode)
			? new_tarval_from_double(-1.0, mode) : tarval_bad;
		break;
	}

	case irms_internal_boolean:
		mode->all_one   = tarval_b_true;
		mode->infinity  = tarval_bad;
		mode->nan       = tarval_bad;
		mode->min       = tarval_b_false;
		mode->max       = tarval_b_true;
		mode->null      = tarval_b_false;
		mode->one       = tarval_b_true;
		mode->minus_one = tarval_bad;
		break;

	case irms_reference:
	case irms_int_number: {
		sc_word *buf    = ALLOCAN(sc_word, sc_value_length);
		unsigned buflen = sc_value_length * sizeof(buf[0]);
		unsigned bits   = get_mode_size_bits(mode);
		bool     sign   = mode_is_signed(mode);
		sc_max_from_bits(bits, false, buf);
		mode->all_one   = get_tarval(buf, buflen, mode);
		mode->infinity  = tarval_bad;
		mode->nan       = tarval_bad;
		sc_min_from_bits(bits, sign, buf);
		mode->min       = get_tarval(buf, buflen, mode);
		sc_max_from_bits(bits, sign, buf);
		mode->max       = get_tarval(buf, buflen, mode);
		sc_zero(buf);
		mode->null      = get_tarval(buf, buflen, mode);
		sc_set_bit_at(buf, 0);
		mode->one       = get_tarval(buf, buflen, mode);
		if (sign) {
			sc_neg(buf, buf);
			mode->minus_one = get_tarval(buf, buflen, mode);
		} else {
			mode->minus_one = tarval_bad;
		}
		break;
	}

	case irms_auxiliary:
	case irms_data:
		mode->all_one   = tarval_bad;
		mode->min       = tarval_bad;
		mode->max       = tarval_bad;
		mode->null      = tarval_bad;
		mode->one       = tarval_bad;
		mode->minus_one = tarval_bad;
		mode->infinity  = tarval_bad;
		mode->nan       = tarval_bad;
		break;
	}
}

/*
 * Arithmetic operations on tarvals ========================================
 */

int tarval_is_negative(const ir_tarval *a)
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
		panic("mode %F does not support negation value", a->mode);
	}
	panic("invalid mode sort");
}

int tarval_is_null(const ir_tarval *tv)
{
	return tv != tarval_unknown && tv != tarval_bad
	    && tv == get_mode_null(get_tarval_mode(tv));
}

int tarval_is_one(const ir_tarval *tv)
{
	return tv != tarval_unknown && tv != tarval_bad
	    && tv == get_mode_one(get_tarval_mode(tv));
}

int tarval_is_all_one(const ir_tarval *tv)
{
	return tv != tarval_unknown && tv != tarval_bad
	    && tv == get_mode_all_one(get_tarval_mode(tv));
}

int tarval_is_minus_one(const ir_tarval *tv)
{
	return tv != tarval_unknown && tv != tarval_bad
	    && tv == get_mode_minus_one(get_tarval_mode(tv));
}

ir_relation tarval_cmp(const ir_tarval *a, const ir_tarval *b)
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

	default:
		panic("can't compare values of mode %F", a->mode);
	}
}

ir_tarval *tarval_convert_to(ir_tarval *src, ir_mode *dst_mode)
{
	if (src->mode == dst_mode)
		return src;

	switch (get_mode_sort(src->mode)) {
	/* cast float to something */
	case irms_float_number:
		switch (get_mode_sort(dst_mode)) {
		case irms_float_number: {
			const float_descriptor_t *desc = get_descriptor(dst_mode);
			fc_cast((const fp_value*) src->value, desc, NULL);
			return get_tarval(fc_get_buffer(), fc_get_buffer_length(), dst_mode);
		}

		case irms_int_number: {
			fp_value *res    = fc_int((const fp_value*) src->value, NULL);
			sc_word  *buffer = ALLOCAN(sc_word, sc_value_length);
			flt2int_result_t cres
				= fc_flt2int(res, buffer, get_mode_size_bits(dst_mode),
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
				return get_tarval(buffer, sc_value_length, dst_mode);
			}
		}

		default:
			break;
		}
		/* the rest can't be converted */
		return tarval_bad;

	/* cast int/characters to something */
	case irms_int_number:
		switch (get_mode_sort(dst_mode)) {

		case irms_reference:
		case irms_int_number: {
			sc_word *buffer = ALLOCAN(sc_word, sc_value_length);
			memcpy(buffer, src->value, sc_value_length);
			return get_tarval_overflow(buffer, src->length, dst_mode);
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
			buffer[100 - 1] = '\0';

			fp_value *val = fc_val_from_str(buffer, len, NULL);
			return get_tarval_from_fp_value(val, dst_mode);
		}
		case irms_auxiliary:
		case irms_data:
		case irms_internal_boolean:
			break;
		}
		break;

	case irms_reference:
		if (get_mode_sort(dst_mode) == irms_int_number) {
			sc_word *buffer = ALLOCAN(sc_word, sc_value_length);
			memcpy(buffer, src->value, sc_value_length);
			unsigned bits = get_mode_size_bits(src->mode);
			if (mode_is_signed(src->mode)) {
				sc_sign_extend(buffer, bits);
			} else {
				sc_zero_extend(buffer, bits);
			}
			return get_tarval_overflow(buffer, src->length, dst_mode);
		}
		break;

	case irms_auxiliary:
	case irms_data:
	case irms_internal_boolean:
		return tarval_bad;
	}

	return tarval_bad;
}

ir_tarval *tarval_bitcast(ir_tarval *src, ir_mode *dst_mode)
{
	const ir_mode *src_mode = get_tarval_mode(src);
	if (src_mode == dst_mode)
		return src;
	unsigned size = get_mode_size_bits(src_mode);
	assert(get_mode_size_bits(dst_mode) == size);
	assert(size % 8 == 0);

	size_t         buf_len = size / 8;
	unsigned char *buffer  = ALLOCAN(unsigned char, buf_len);
	switch (get_mode_arithmetic(src_mode)) {
	case irma_ieee754:
	case irma_x86_extended_float:
		fc_val_to_bytes((const fp_value*)src->value, buffer);
		break;
	case irma_twos_complement:
		sc_val_to_bytes((const sc_word*)src->value, buffer, buf_len);
		break;
	default:
		panic("unexpected arithmetic mode in tarval_bitcast");
	}

	return new_tarval_from_bytes(buffer, dst_mode, false);
}

ir_tarval *tarval_not(ir_tarval *a)
{
	switch (get_mode_sort(a->mode)) {
	case irms_reference:
	case irms_int_number: {
		sc_word *buffer = ALLOCAN(sc_word, sc_value_length);
		sc_not(a->value, buffer);
		return get_tarval(buffer, a->length, a->mode);
	}

	case irms_internal_boolean:
		if (a == tarval_b_true)
			return tarval_b_false;
		if (a == tarval_b_false)
			return tarval_b_true;
		return tarval_bad;

	case irms_auxiliary:
	case irms_data:
	case irms_float_number:
		panic("bitwise negation is only allowed for integer and boolean");
	}
	panic("invalid mode sort");
}

ir_tarval *tarval_neg(ir_tarval *a)
{
	assert(mode_is_num(a->mode)); /* negation only for numerical values */

	/* note: negation is allowed even for unsigned modes. */

	switch (get_mode_sort(a->mode)) {
	case irms_int_number:
	case irms_reference: {
		sc_word *buffer = ALLOCAN(sc_word, sc_value_length);
		sc_neg(a->value, buffer);
		return get_tarval_overflow(buffer, a->length, a->mode);
	}

	case irms_float_number:
		fc_neg((const fp_value*) a->value, NULL);
		return get_tarval_overflow(fc_get_buffer(), fc_get_buffer_length(), a->mode);

	case irms_auxiliary:
	case irms_data:
	case irms_internal_boolean:
		return tarval_bad;
	}
	panic("invalid mode sort");
}

ir_tarval *tarval_add(ir_tarval *a, ir_tarval *b)
{
	if (mode_is_reference(a->mode) && a->mode != b->mode) {
		b = tarval_convert_to(b, a->mode);
	} else if (mode_is_reference(b->mode) && b->mode != a->mode) {
		a = tarval_convert_to(a, b->mode);
	}

	assert(a->mode == b->mode);

	switch (get_mode_sort(a->mode)) {
	case irms_reference:
	case irms_int_number: {
		/* modes of a,b are equal, so result has mode of a as this might be the character */
		sc_word *buffer = ALLOCAN(sc_word, sc_value_length);
		sc_add(a->value, b->value, buffer);
		return get_tarval_overflow(buffer, a->length, a->mode);
	}

	case irms_float_number:
		fc_add((const fp_value*) a->value, (const fp_value*) b->value, NULL);
		return get_tarval_overflow(fc_get_buffer(), fc_get_buffer_length(), a->mode);

	case irms_auxiliary:
	case irms_data:
	case irms_internal_boolean:
		return tarval_bad;
	}
	panic("invalid mode sort");
}

ir_tarval *tarval_sub(ir_tarval *a, ir_tarval *b, ir_mode *dst_mode)
{
	if (dst_mode != NULL) {
		if (a->mode != dst_mode)
			a = tarval_convert_to(a, dst_mode);
		if (b->mode != dst_mode)
			b = tarval_convert_to(b, dst_mode);
	}
	assert(a->mode == b->mode);

	switch (get_mode_sort(a->mode)) {
	case irms_reference:
	case irms_int_number: {
		/* modes of a,b are equal, so result has mode of a as this might be the character */
		sc_word *buffer = ALLOCAN(sc_word, sc_value_length);
		sc_sub(a->value, b->value, buffer);
		return get_tarval_overflow(buffer, a->length, a->mode);
	}

	case irms_float_number:
		fc_sub((const fp_value*) a->value, (const fp_value*) b->value, NULL);
		return get_tarval_overflow(fc_get_buffer(), fc_get_buffer_length(), a->mode);

	case irms_auxiliary:
	case irms_data:
	case irms_internal_boolean:
		return tarval_bad;
	}
	panic("invalid mode sort");
}

ir_tarval *tarval_mul(ir_tarval *a, ir_tarval *b)
{
	assert(a->mode == b->mode);

	switch (get_mode_sort(a->mode)) {
	case irms_int_number:
	case irms_reference: {
		/* modes of a,b are equal */
		sc_word *buffer = ALLOCAN(sc_word, sc_value_length);
		sc_mul(a->value, b->value, buffer);
		return get_tarval_overflow(buffer, a->length, a->mode);
	}

	case irms_float_number:
		fc_mul((const fp_value*) a->value, (const fp_value*) b->value, NULL);
		return get_tarval_overflow(fc_get_buffer(), fc_get_buffer_length(), a->mode);

	case irms_auxiliary:
	case irms_data:
	case irms_internal_boolean:
		return tarval_bad;
	}
	panic("invalid mode sort");
}

ir_tarval *tarval_div(ir_tarval *a, ir_tarval *b)
{
	ir_mode *mode = a->mode;
	assert(mode == b->mode);

	switch (get_mode_sort(a->mode)) {
	case irms_int_number:
	case irms_reference: {
		/* x/0 error */
		if (b == get_mode_null(mode))
			return tarval_bad;

		sc_word *buffer = ALLOCAN(sc_word, sc_value_length);
		sc_div(a->value, b->value, buffer);
		return get_tarval(buffer, sc_value_length, a->mode);
	}

	case irms_float_number:
		fc_div((const fp_value*) a->value, (const fp_value*) b->value, NULL);
		return get_tarval_overflow(fc_get_buffer(), fc_get_buffer_length(), mode);

	case irms_auxiliary:
	case irms_data:
	case irms_internal_boolean:
		panic("operation not defined on mode");
	}
	panic("invalid mode sort");
}

ir_tarval *tarval_mod(ir_tarval *a, ir_tarval *b)
{
	assert((a->mode == b->mode) && mode_is_int(a->mode));

	switch (get_mode_sort(a->mode)) {
	case irms_int_number:
	case irms_reference: {
		/* x/0 error */
		if (b == get_mode_null(b->mode))
			return tarval_bad;
		/* modes of a,b are equal */
		sc_word *buffer = ALLOCAN(sc_word, sc_value_length);
		sc_mod(a->value, b->value, buffer);
		return get_tarval(buffer, sc_value_length, a->mode);
	}

	case irms_auxiliary:
	case irms_data:
	case irms_internal_boolean:
	case irms_float_number:
		panic("operation not defined on mode");
	}
	panic("invalid mode sort");
}

ir_tarval *tarval_divmod(ir_tarval *a, ir_tarval *b, ir_tarval **mod)
{
	assert((a->mode == b->mode) && mode_is_int(a->mode));

	switch (get_mode_sort(a->mode)) {
	case irms_int_number:
	case irms_reference: {
		size_t   len     = sc_value_length;
		sc_word *div_res = ALLOCAN(sc_word, len);
		sc_word *mod_res = ALLOCAN(sc_word, len);

		/* x/0 error */
		if (b == get_mode_null(b->mode))
			return tarval_bad;
		/* modes of a,b are equal */
		sc_divmod(a->value, b->value, div_res, mod_res);
		*mod = get_tarval(mod_res, len, a->mode);
		return get_tarval(div_res, len, a->mode);
	}

	case irms_auxiliary:
	case irms_data:
	case irms_internal_boolean:
	case irms_float_number:
		panic("operation not defined on mode");
	}
	panic("invalid mode sort");
}

ir_tarval *tarval_abs(ir_tarval *a)
{
	assert(mode_is_num(a->mode));

	switch (get_mode_sort(a->mode)) {
	case irms_int_number:
	case irms_reference:
		if (sc_comp(a->value, get_mode_null(a->mode)->value) == ir_relation_less) {
			sc_word *buffer = ALLOCAN(sc_word, sc_value_length);
			sc_neg(a->value, buffer);
			return get_tarval_overflow(buffer, a->length, a->mode);
		}
		return a;

	case irms_float_number:
		if (fc_is_negative((const fp_value*)a->value)) {
			fc_neg((const fp_value*) a->value, NULL);
			return get_tarval_overflow(fc_get_buffer(), fc_get_buffer_length(), a->mode);
		}
		return a;

	case irms_auxiliary:
	case irms_data:
	case irms_internal_boolean:
		panic("operation not defined on mode");
	}
	panic("invalid mode sort");
}

ir_tarval *tarval_and(ir_tarval *a, ir_tarval *b)
{
	assert(a->mode == b->mode);

	switch (get_mode_sort(a->mode)) {
	case irms_internal_boolean:
		return (a == tarval_b_false) ? a : b;

	case irms_reference:
	case irms_int_number: {
		sc_word *buffer = ALLOCAN(sc_word, sc_value_length);
		sc_and(a->value, b->value, buffer);
		return get_tarval(buffer, sc_value_length, a->mode);
	}

	case irms_auxiliary:
	case irms_data:
	case irms_float_number:
		panic("operation not defined on mode");
	}
	panic("invalid mode sort");
}

ir_tarval *tarval_andnot(ir_tarval *a, ir_tarval *b)
{
	assert(a->mode == b->mode);

	switch (get_mode_sort(a->mode)) {
	case irms_internal_boolean:
		return a == tarval_b_true && b == tarval_b_false ? tarval_b_true : tarval_b_false;

	case irms_reference:
	case irms_int_number: {
		sc_word *buffer = ALLOCAN(sc_word, sc_value_length);
		sc_andnot(a->value, b->value, buffer);
		return get_tarval(buffer, sc_value_length, a->mode);
	}

	case irms_auxiliary:
	case irms_data:
	case irms_float_number:
		panic("operation not defined on mode");
	}
	panic("invalid mode sort");
}

ir_tarval *tarval_or(ir_tarval *a, ir_tarval *b)
{
	assert(a->mode == b->mode);

	switch (get_mode_sort(a->mode)) {
	case irms_internal_boolean:
		return (a == tarval_b_true) ? a : b;

	case irms_reference:
	case irms_int_number: {
		sc_word *buffer = ALLOCAN(sc_word, sc_value_length);
		sc_or(a->value, b->value, buffer);
		return get_tarval(buffer, sc_value_length, a->mode);
	}

	case irms_auxiliary:
	case irms_data:
	case irms_float_number:
		panic("operation not defined on mode");
	}
	panic("invalid mode sort");
}

ir_tarval *tarval_ornot(ir_tarval *a, ir_tarval *b)
{
	assert(a->mode == b->mode);

	switch (get_mode_sort(a->mode)) {
	case irms_internal_boolean:
		return a == tarval_b_true || b == tarval_b_false ? tarval_b_true : tarval_b_false;

	case irms_reference:
	case irms_int_number: {
		sc_word *buffer = ALLOCAN(sc_word, sc_value_length);
		sc_ornot(a->value, b->value, buffer);
		return get_tarval(buffer, sc_value_length, a->mode);
	}

	case irms_auxiliary:
	case irms_data:
	case irms_float_number:
		panic("operation not defined on mode");
	}
	panic("invalid mode sort");
}

ir_tarval *tarval_eor(ir_tarval *a, ir_tarval *b)
{
	assert((a->mode == b->mode));

	switch (get_mode_sort(a->mode)) {
	case irms_internal_boolean:
		return (a == b)? tarval_b_false : tarval_b_true;

	case irms_reference:
	case irms_int_number: {
		sc_word *buffer = ALLOCAN(sc_word, sc_value_length);
		sc_xor(a->value, b->value, buffer);
		return get_tarval(buffer, sc_value_length, a->mode);
	}

	case irms_auxiliary:
	case irms_data:
	case irms_float_number:
		panic("operation not defined on mode");
	}
	panic("invalid mode sort");
}

ir_tarval *tarval_shl(ir_tarval *a, ir_tarval *b)
{
	assert(mode_is_int(a->mode) && mode_is_int(b->mode));

	sc_word *temp_val;
	if (get_mode_modulo_shift(a->mode) != 0) {
		temp_val = ALLOCAN(sc_word, sc_value_length);
		sc_word *temp2 = ALLOCAN(sc_word, sc_value_length);
		sc_val_from_ulong(get_mode_modulo_shift(a->mode), temp2);
		sc_mod(b->value, temp2, temp_val);
	} else {
		temp_val = (sc_word*)b->value;
	}

	sc_word *temp = ALLOCAN(sc_word, sc_value_length);
	sc_shl(a->value, temp_val, temp);
	return get_tarval(temp, sc_value_length, a->mode);
}

ir_tarval *tarval_shl_unsigned(ir_tarval *a, unsigned b)
{
	ir_mode *mode   = a->mode;
	unsigned modulo = get_mode_modulo_shift(mode);
	if (modulo != 0)
		b %= modulo;
	assert((unsigned)(long)b==b);

	sc_word *buffer = ALLOCAN(sc_word, sc_value_length);
	sc_shlI(a->value, (long)b, buffer);
	return get_tarval(buffer, sc_value_length, mode);
}

ir_tarval *tarval_shr(ir_tarval *a, ir_tarval *b)
{
	assert(mode_is_int(a->mode) && mode_is_int(b->mode));

	sc_word *temp_val;
	if (get_mode_modulo_shift(a->mode) != 0) {
		temp_val = ALLOCAN(sc_word, sc_value_length);
		sc_word *temp2 = ALLOCAN(sc_word, sc_value_length);
		sc_val_from_ulong(get_mode_modulo_shift(a->mode), temp2);
		sc_mod(b->value, temp2, temp_val);
	} else {
		temp_val = (sc_word*)b->value;
	}

	sc_word *temp = ALLOCAN(sc_word, sc_value_length);
	/* workaround for unnecessary internal higher precision */
	memcpy(temp, a->value, sc_value_length);
	sc_zero_extend(temp, get_mode_size_bits(a->mode));
	sc_shr(temp, temp_val, temp);
	return get_tarval(temp, sc_value_length, a->mode);
}

ir_tarval *tarval_shr_unsigned(ir_tarval *a, unsigned b)
{
	ir_mode *mode   = a->mode;
	unsigned modulo = get_mode_modulo_shift(mode);
	if (modulo != 0)
		b %= modulo;
	assert((unsigned)(long)b==b);

	sc_word *temp = ALLOCAN(sc_word, sc_value_length);
	/* workaround for unnecessary internal higher precision */
	memcpy(temp, a->value, sc_value_length);
	sc_zero_extend(temp, get_mode_size_bits(a->mode));
	sc_shrI(temp, (long)b, temp);
	return get_tarval(temp, sc_value_length, mode);
}

ir_tarval *tarval_shrs(ir_tarval *a, ir_tarval *b)
{
	assert(mode_is_int(a->mode) && mode_is_int(b->mode));

	sc_word *temp_val;
	if (get_mode_modulo_shift(a->mode) != 0) {
		temp_val = ALLOCAN(sc_word, sc_value_length);
		sc_word *temp2 = ALLOCAN(sc_word, sc_value_length);
		sc_val_from_ulong(get_mode_modulo_shift(a->mode), temp2);
		sc_mod(b->value, temp2, temp_val);
	} else {
		temp_val = (sc_word*)b->value;
	}

	sc_word *temp = ALLOCAN(sc_word, sc_value_length);
	sc_shrs(a->value, temp_val, get_mode_size_bits(a->mode), temp);
	return get_tarval(temp, sc_value_length, a->mode);
}

ir_tarval *tarval_shrs_unsigned(ir_tarval *a, unsigned b)
{
	ir_mode *mode   = a->mode;
	unsigned modulo = get_mode_modulo_shift(mode);
	if (modulo != 0)
		b %= modulo;
	assert((unsigned)(long)b==b);

	sc_word *temp = ALLOCAN(sc_word, sc_value_length);
	sc_shrsI(a->value, (long)b, get_mode_size_bits(mode), temp);
	return get_tarval(temp, sc_value_length, mode);
}

int tarval_snprintf(char *buf, size_t len, ir_tarval *tv)
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
	if (val < 10)
		return val + '0';
	return (val-10) + 'A';
}

static unsigned hexval(char c)
{
	if (c >= '0' && c <= '9')
		return c - '0';
	return (c - 'A')+10;
}

const char *ir_tarval_to_ascii(char *buf, size_t len, ir_tarval *tv)
{
	ir_mode *mode = get_tarval_mode(tv);
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
		fc_val_from_bytes(NULL, temp, get_descriptor(mode));
		return get_tarval_from_fp_value(fc_get_buffer(), mode);
	}
	case irms_data:
	case irms_auxiliary:
		if (strcmp(buf, "bad") == 0)
			return tarval_bad;
		else if (strcmp(buf, "unknown") == 0)
			return tarval_unknown;
		break;
	}
	panic("invalid mode for tarval_from_ascii");
}

char *get_tarval_bitpattern(const ir_tarval *tv)
{
	int   n   = get_mode_size_bits(tv->mode);
	char *res = XMALLOCN(char, n + 1);
	int   pos = 0;
	for (int i = 0, bytes = (n+7)/8; i < bytes; i++) {
		unsigned char byte = get_tarval_sub_bits(tv, i);
		for (int j = 1; j < 256; j <<= 1) {
			if (pos < n)
				res[pos++] = j & byte ? '1' : '0';
		}
	}
	res[n] = '\0';
	return res;
}

unsigned char get_tarval_sub_bits(const ir_tarval *tv, unsigned byte_ofs)
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

int get_tarval_popcount(const ir_tarval *tv)
{
	ir_mode *mode = get_tarval_mode(tv);
	if (!mode_is_int(mode))
		return -1;

	return sc_popcount(tv->value, get_mode_size_bits(mode));
}

int get_tarval_lowest_bit(const ir_tarval *tv)
{
	if (!mode_is_int(tv->mode))
		return -1;

	int l = get_mode_size_bytes(tv->mode);
	for (int i = 0; i < l; ++i) {
		unsigned char v = get_tarval_sub_bits(tv, (unsigned)i);

		if (v)
			return ntz(v) + i * 8;
	}
	return -1;
}

int get_tarval_highest_bit(const ir_tarval *tv)
{
	if (!mode_is_int(tv->mode))
		return -1;

	int l = get_mode_size_bytes(tv->mode);
	for (int i = l - 1; i >= 0; --i) {
		unsigned char v = get_tarval_sub_bits(tv, (unsigned)i);

		if (v)
			return 8*sizeof(unsigned) - nlz(v) + i * 8 - 1;
	}
	return -1;
}

int tarval_zero_mantissa(const ir_tarval *tv)
{
	assert(get_mode_arithmetic(tv->mode) == irma_ieee754
	       || get_mode_arithmetic(tv->mode) == irma_x86_extended_float);
	return fc_zero_mantissa((const fp_value*) tv->value);
}

int tarval_get_exponent(const ir_tarval *tv)
{
	assert(get_mode_arithmetic(tv->mode) == irma_ieee754
	       || get_mode_arithmetic(tv->mode) == irma_x86_extended_float);
	return fc_get_exponent((const fp_value*) tv->value);
}

int tarval_ieee754_can_conv_lossless(const ir_tarval *tv, ir_mode *mode)
{
	const float_descriptor_t *desc = get_descriptor(mode);
	return fc_can_lossless_conv_to((const fp_value*) tv->value, desc);
}

unsigned tarval_ieee754_get_exact(void)
{
	return fc_is_exact();
}

int tarval_is_nan(const ir_tarval *tv)
{
	if (!mode_is_float(tv->mode))
		return 0;
	return fc_is_nan((const fp_value*) tv->value);
}

int tarval_is_plus_inf(const ir_tarval *tv)
{
	if (!mode_is_float(tv->mode))
		return 0;
	return fc_is_inf((const fp_value*) tv->value)
		&& !fc_is_negative((const fp_value*) tv->value);
}

int tarval_is_minus_inf(const ir_tarval *tv)
{
	if (!mode_is_float(tv->mode))
		return 0;
	return fc_is_inf((const fp_value*) tv->value)
		&& fc_is_negative((const fp_value*) tv->value);
}

int tarval_is_finite(const ir_tarval *tv)
{
	if (mode_is_float(tv->mode))
		return !fc_is_nan((const fp_value*) tv->value)
			&& !fc_is_inf((const fp_value*) tv->value);
	return 1;
}

void tarval_set_integer_overflow_mode(tarval_int_overflow_mode_t ov_mode)
{
	int_overflow_mode = ov_mode;
}

tarval_int_overflow_mode_t tarval_get_integer_overflow_mode(void)
{
	return int_overflow_mode;
}

static ir_tarval *make_b_tarval(unsigned char const val)
{
	size_t     const length = 1;
	ir_tarval *const tv     = XMALLOCF(ir_tarval, value, length);
	tv->kind     = k_tarval;
	tv->mode     = NULL;
	tv->length   = length;
	tv->value[0] = val;
	return tv;
}

void init_tarval_1(int support_quad_precision)
{
	/* initialize the sets holding the tarvals with a comparison function and
	 * an initial size, which is the expected number of constants */
	tarvals = new_set(cmp_tv, N_CONSTANTS);
	/* calls init_strcalc() with needed size */
	init_fltcalc(support_quad_precision ? 112 : 64);

	sc_value_length = sc_get_value_length();

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
	finish_fltcalc();
	del_set(tarvals); tarvals = NULL;
}

int (is_tarval)(const void *thing)
{
	return is_tarval_(thing);
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
