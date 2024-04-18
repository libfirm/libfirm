/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Arithmetic operations on arbitrary precision integer numbers.
 * @author  Mathias Heil, Matthias Braun
 */
#include "strcalc.h"

#include "bitfiddle.h"
#include "panic.h"
#include "tv_t.h"
#include "util.h"
#include "xmalloc.h"
#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SC_MASK      ((sc_word)0xFF)
#define SC_RESULT(x) ((x) & SC_MASK)
#define SC_CARRY(x)  ((unsigned)(x) >> SC_BITS)

static char *output_buffer = NULL;  /**< buffer for output */
static unsigned bit_pattern_size;   /**< maximum number of bits */
static unsigned calc_buffer_size;   /**< size of internally stored values */
static unsigned max_value_size;     /**< maximum size of values */

void sc_zero(sc_word *buffer)
{
	memset(buffer, 0, sizeof(buffer[0]) * calc_buffer_size);
}

static sc_word sex_digit(unsigned x)
{
	return (SC_MASK << (x+1)) & SC_MASK;
}

static sc_word max_digit(unsigned x)
{
	return (1u << x) - 1;
}

static sc_word min_digit(unsigned x)
{
	return SC_MASK - max_digit(x);
}

void sc_not(const sc_word *val, sc_word *buffer)
{
	for (unsigned counter = 0; counter<calc_buffer_size; counter++)
		buffer[counter] = val[counter] ^ SC_MASK;
}

void sc_or(const sc_word *val1, const sc_word *val2, sc_word *buffer)
{
	for (unsigned counter = 0; counter<calc_buffer_size; counter++)
		buffer[counter] = val1[counter] | val2[counter];
}

void sc_ornot(const sc_word *val1, const sc_word *val2, sc_word *buffer)
{
	for (unsigned counter = 0; counter < calc_buffer_size; ++counter)
		buffer[counter] = val1[counter] | (SC_MASK ^ val2[counter]);
}

void sc_xor(const sc_word *val1, const sc_word *val2, sc_word *buffer)
{
	for (unsigned counter = 0; counter<calc_buffer_size; counter++)
		buffer[counter] = val1[counter] ^ val2[counter];
}

void sc_and(const sc_word *val1, const sc_word *val2, sc_word *buffer)
{
	for (unsigned counter = 0; counter<calc_buffer_size; counter++)
		buffer[counter] = val1[counter] & val2[counter];
}

void sc_andnot(const sc_word *val1, const sc_word *val2, sc_word *buffer)
{
	for (unsigned counter = 0; counter < calc_buffer_size; ++counter)
		buffer[counter] = val1[counter] & (SC_MASK ^ val2[counter]);
}

void sc_inc(sc_word *buffer)
{
	for (unsigned counter = 0; counter < calc_buffer_size; ++counter) {
		sc_word v = buffer[counter];
		if (v < SC_MASK) {
			buffer[counter] = v+1;
			break;
		}
		buffer[counter] = 0;
	}
}

void sc_neg(const sc_word *val, sc_word *buffer)
{
	sc_not(val, buffer);
	sc_inc(buffer);
}

void sc_add(const sc_word *val1, const sc_word *val2, sc_word *buffer)
{
	sc_word carry = 0;
	for (unsigned counter = 0; counter < calc_buffer_size; ++counter) {
		unsigned const sum = val1[counter] + val2[counter] + carry;
		buffer[counter] = SC_RESULT(sum);
		carry           = SC_CARRY(sum);
	}
}

void sc_sub(const sc_word *val1, const sc_word *val2, sc_word *buffer)
{
	/* intermediate buffer to hold -val2 */
	sc_word *temp_buffer = ALLOCAN(sc_word, calc_buffer_size);

	sc_neg(val2, temp_buffer);
	sc_add(val1, temp_buffer, buffer);
}

void sc_mul(const sc_word *val1, const sc_word *val2, sc_word *buffer)
{
	sc_word *temp_buffer = ALLOCANZ(sc_word, calc_buffer_size);
	sc_word *neg_val1    = ALLOCAN(sc_word, calc_buffer_size);
	sc_word *neg_val2    = ALLOCAN(sc_word, calc_buffer_size);

	/* the multiplication works only for positive values, for negative values *
	 * it is necessary to negate them and adjust the result accordingly       */
	bool sign = false;
	if (sc_is_negative(val1)) {
		sc_neg(val1, neg_val1);
		val1 = neg_val1;
		sign = !sign;
	}
	if (sc_is_negative(val2)) {
		sc_neg(val2, neg_val2);
		val2 = neg_val2;
		sign = !sign;
	}

	for (unsigned c_outer = 0; c_outer < max_value_size; c_outer++) {
		sc_word outer = val2[c_outer];
		if (outer == 0)
			continue;
		unsigned carry = 0; /* container for carries */
		for (unsigned c_inner = 0; c_inner < max_value_size; c_inner++) {
			sc_word inner = val1[c_inner];
			/* do the following calculation:
			 * Add the current carry, the value at position c_outer+c_inner
			 * and the result of the multiplication of val1[c_inner] and
			 * val2[c_outer]. This is the usual pen-and-paper multiplication
			 */

			/* multiplicate the two digits */
			unsigned const mul = inner*outer;
			/* add old value to result of multiplication and the carry */
			unsigned const sum = temp_buffer[c_inner+c_outer] + mul + carry;

			/* all carries together result in new carry. This is always
			 * smaller than the base b:
			 * Both multiplicands, the carry and the value already in the
			 * temp buffer are single digits and their value is therefore
			 * at most equal to (b-1).
			 * This leads to:
			 * (b-1)(b-1)+(b-1)+(b-1) = b*b-1
			 * The tables list all operations rem b, so the carry is at
			 * most
			 * (b*b-1)rem b = -1rem b = b-1
			 */
			temp_buffer[c_inner + c_outer] = SC_RESULT(sum);
			carry                          = SC_CARRY(sum);
		}

		/* A carry may hang over */
		/* c_outer is always smaller than max_value_size! */
		temp_buffer[max_value_size + c_outer] = carry;
	}

	if (sign)
		sc_neg(temp_buffer, buffer);
	else
		memcpy(buffer, temp_buffer, calc_buffer_size);
}

/**
 * Shift the buffer to left and add an sc digit
 */
static void sc_push(sc_word digit, sc_word *buffer)
{
	for (unsigned counter = calc_buffer_size - 1; counter-- > 0; ) {
		buffer[counter+1] = buffer[counter];
	}
	buffer[0] = digit;
}

bool sc_divmod(const sc_word *dividend, const sc_word *divisor,
               sc_word *quot, sc_word *rem)
{
	assert(quot != dividend && quot != divisor);
	assert(rem != dividend && rem != divisor);
	/* clear result buffer */
	sc_zero(quot);
	sc_zero(rem);

	/* division by zero is not allowed */
	assert(!sc_is_zero(divisor, calc_buffer_size*SC_BITS));

	/* if the dividend is zero result is zero (quot is zero) */
	if (sc_is_zero(dividend, calc_buffer_size*SC_BITS))
		return false;

	bool     div_sign = false;
	bool     rem_sign = false;
	sc_word *neg_val1 = ALLOCAN(sc_word, calc_buffer_size);
	if (sc_is_negative(dividend)) {
		sc_neg(dividend, neg_val1);
		div_sign = !div_sign;
		rem_sign = !rem_sign;
		dividend = neg_val1;
	}

	sc_word *neg_val2 = ALLOCAN(sc_word, calc_buffer_size);
	sc_neg(divisor, neg_val2);
	const sc_word *minus_divisor;
	if (sc_is_negative(divisor)) {
		div_sign = !div_sign;
		minus_divisor = divisor;
		divisor = neg_val2;
	} else {
		minus_divisor = neg_val2;
	}

	/* if divisor >= dividend division is easy
	 * (remember these are absolute values) */
	switch (sc_comp(dividend, divisor)) {
	case ir_relation_equal: /* dividend == divisor */
		quot[0] = 1;
		goto end;

	case ir_relation_less: /* dividend < divisor */
		memcpy(rem, dividend, calc_buffer_size);
		goto end;

	default: /* unluckily division is necessary :( */
		break;
	}

	for (unsigned c_dividend = calc_buffer_size; c_dividend-- > 0; ) {
		sc_push(dividend[c_dividend], rem);
		sc_push(0, quot);

		if (sc_comp(rem, divisor) != ir_relation_less) {
			/* remainder >= divisor */
			/* subtract until the remainder becomes negative, this should
			 * be faster than comparing remainder with divisor  */
			sc_add(rem, minus_divisor, rem);

			while (!sc_is_negative(rem)) {
				/* TODO can this generate carry or is masking redundant? */
				quot[0] = SC_RESULT(quot[0] + 1);
				sc_add(rem, minus_divisor, rem);
			}

			/* subtracted one too much */
			sc_add(rem, divisor, rem);
		}
	}
end:
	if (div_sign)
		sc_neg(quot, quot);

	if (rem_sign)
		sc_neg(rem, rem);

	/* sets carry if remainder is non-zero ??? */
	bool carry_flag = !sc_is_zero(rem, calc_buffer_size*SC_BITS);
	return carry_flag;
}

unsigned sc_get_value_length(void)
{
	return calc_buffer_size;
}

void sc_zero_extend(sc_word *buffer, unsigned from_bits)
{
	unsigned bit  = from_bits % SC_BITS;
	unsigned word = from_bits / SC_BITS;
	if (bit > 0) {
		memset(&buffer[word+1], 0, calc_buffer_size-(word+1));
		buffer[word] &= max_digit(bit);
	} else {
		memset(&buffer[word], 0, calc_buffer_size-word);
	}
}

void sc_sign_extend(sc_word *buffer, unsigned from_bits)
{
	assert(from_bits > 0);
	unsigned bits     = from_bits - 1;
	bool     sign_bit = sc_get_bit_at(buffer, bits);
	if (sign_bit) {
		/* sign bit is set, we need sign extension */
		unsigned word = bits / SC_BITS;
		for (unsigned i = word + 1; i < calc_buffer_size; ++i)
			buffer[i] = SC_MASK;
		buffer[word] |= sex_digit(bits % SC_BITS);
	} else {
		sc_zero_extend(buffer, from_bits);
	}
}

/** Ensures that our source character set conforms to ASCII for a-f, A-F, 0-9 */
static inline void check_ascii(void)
{
	assert((('a'-97) | ('b'-98) | ('c'-99) | ('d'-100) | ('e'-101) | ('f'-102)
	       |('A'-65) | ('B'-66) | ('C'-67) | ('D'- 68) | ('E'- 69) | ('F'- 70)
	       |('0'-48) | ('1'-49) | ('2'-50) | ('3'- 51) | ('4'- 52)
	       |('5'-53) | ('6'-54) | ('7'-55) | ('8'- 56) | ('9'- 57)) == 0);
}

bool sc_val_from_str(bool negative, unsigned base, const char *str, size_t len,
                     sc_word *buffer)
{
	assert(str != NULL);
	assert(len > 0);
	check_ascii();

	assert(base > 1 && base <= 16);
	sc_word *sc_base = ALLOCAN(sc_word, calc_buffer_size);
	sc_val_from_ulong(base, sc_base);

	sc_word *val = ALLOCANZ(sc_word, calc_buffer_size);

	sc_zero(buffer);

	/* BEGIN string evaluation, from left to right */
	while (len > 0) {
		char c = *str;
		unsigned v;
		if (is_digit(c))
			v = c - '0';
		else if (c >= 'A' && c <= 'F')
			v = c - 'A' + 10;
		else if (c >= 'a' && c <= 'f')
			v = c - 'a' + 10;
		else
			return false;

		if (v >= base)
			return false;
		val[0] = v;

		/* Radix conversion from base b to base B:
		 *  (UnUn-1...U1U0)b == ((((Un*b + Un-1)*b + ...)*b + U1)*b + U0)B */
		/* multiply current value with base */
		sc_mul(sc_base, buffer, buffer);
		/* add next digit to current value  */
		sc_add(val, buffer, buffer);

		/* get ready for the next letter */
		str++;
		len--;
	}

	if (negative)
		sc_neg(buffer, buffer);

	return true;
}

void sc_val_from_long(long value, sc_word *buffer)
{
	bool sign       = value < 0;
	bool is_minlong = value == LONG_MIN;

	/* use absolute value, special treatment of MIN_LONG to avoid overflow */
	if (sign) {
		if (is_minlong)
			value = -(value+1);
		else
			value = -value;
	}

	sc_zero(buffer);

	sc_word *pos = buffer;
	while ((value != 0) && (pos < buffer + calc_buffer_size)) {
		*pos++ = value & SC_MASK;
		value >>= SC_BITS;
	}

	if (sign) {
		if (is_minlong)
			sc_inc(buffer);

		sc_neg(buffer, buffer);
	}
}

void sc_val_from_ulong(unsigned long value, sc_word *buffer)
{
	sc_word *pos = buffer;

	while (pos < buffer + calc_buffer_size) {
		*pos++ = value & SC_MASK;
		value >>= SC_BITS;
	}
}

long sc_val_to_long(const sc_word *val)
{
	unsigned long l = 0;
	unsigned max_buffer_index = (sizeof(long)*8)/SC_BITS;
	for (unsigned i = max_buffer_index; i-- > 0; ) {
		assert(l <= (ULONG_MAX>>SC_BITS) && "signed shift overflow");
		l = (l << SC_BITS) + val[i];
	}
	return l;
}

uint64_t sc_val_to_uint64(const sc_word *val)
{
	uint64_t res = 0;
	for (unsigned i = calc_buffer_size; i-- > 0; ) {
		res = (res << SC_BITS) + val[i];
	}
	return res;
}

void sc_min_from_bits(unsigned num_bits, bool sign, sc_word *buffer)
{
	if (!sign) {
		sc_zero(buffer);
		return;
	} else {
		sc_word *pos = buffer;

		unsigned bits = num_bits - 1;
		unsigned i    = 0;
		for ( ; i < bits/SC_BITS; i++)
			*pos++ = 0;

		*pos++ = min_digit(bits%SC_BITS);

		for (i++; i < calc_buffer_size; i++)
			*pos++ = SC_MASK;
	}
}

void sc_max_from_bits(unsigned num_bits, bool sign, sc_word *buffer)
{
	sc_word *pos = buffer;

	unsigned bits = num_bits - sign;
	unsigned i    = 0;
	for ( ; i < bits/SC_BITS; i++)
		*pos++ = SC_MASK;

	*pos++ = max_digit(bits%SC_BITS);

	for (i++; i < calc_buffer_size; i++)
		*pos++ = 0;
}

ir_relation sc_comp(const sc_word* const val1, const sc_word* const val2)
{
	/* compare signs first:
	 * the loop below can only compare values of the same sign! */
	bool val1_negative = sc_is_negative(val1);
	bool val2_negative = sc_is_negative(val2);
	if (val1_negative != val2_negative)
		return val1_negative ? ir_relation_less : ir_relation_greater;

	/* loop until two digits differ, the values are equal if there
	 * are no such two digits */
	unsigned counter = calc_buffer_size - 1;
	while (val1[counter] == val2[counter]) {
		if (counter == 0)
			return ir_relation_equal;
		counter--;
	}

	/* the leftmost digit is the most significant, so this returns
	 * the correct result.
	 * This implies the digit enum is ordered */
	return val1[counter] > val2[counter]
	     ? ir_relation_greater : ir_relation_less;
}

int sc_get_highest_set_bit(const sc_word *value)
{
	for (unsigned counter = calc_buffer_size; counter-- > 0; ) {
		sc_word word = value[counter];
		if (word != 0)
			return counter*SC_BITS + (31 - nlz(word));
	}
	return -1;
}

int sc_get_highest_clear_bit(const sc_word *value)
{
	for (unsigned counter = calc_buffer_size; counter-- > 0; ) {
		sc_word word = value[counter] ^ SC_MASK;
		if (word != 0)
			return counter*SC_BITS + (31 - nlz(word));
	}
	return -1;
}

int sc_get_lowest_set_bit(const sc_word *value)
{
	for (unsigned counter = 0; counter < calc_buffer_size;
	     ++counter) {
		sc_word word = value[counter];
		if (word != 0)
			return (counter * SC_BITS) + ntz(word);
	}
	return -1;
}

void sc_set_bit_at(sc_word *value, unsigned pos)
{
	unsigned nibble = pos / SC_BITS;
	value[nibble] |= 1 << (pos % SC_BITS);
}

void sc_clear_bit_at(sc_word *value, unsigned pos)
{
	unsigned nibble = pos / SC_BITS;
	value[nibble] &= ~(1 << (pos % SC_BITS));
}

bool sc_is_zero(const sc_word *value, unsigned bits)
{
	unsigned i;
	for (i = 0; i < bits/SC_BITS; ++i) {
		if (value[i] != 0)
			return false;
	}
	sc_word mask = max_digit(bits%SC_BITS);
	return mask == 0 || (value[i] & mask) == 0;
}

bool sc_is_all_one(const sc_word *value, unsigned bits)
{
	unsigned i;
	for (i = 0; i < bits/SC_BITS; ++i) {
		if (value[i] != SC_MASK)
			return false;
	}
	sc_word mask = max_digit(bits%SC_BITS);
	return mask == 0 || (value[i] & mask) == mask;
}

bool sc_is_negative(const sc_word *value)
{
	return sc_get_bit_at(value, calc_buffer_size*SC_BITS-1);
}

unsigned char sc_sub_bits(const sc_word *value, unsigned len, unsigned byte_ofs)
{
	if (byte_ofs*SC_BITS >= len)
		return 0;

	assert(SC_BITS == CHAR_BIT);
	sc_word val = value[byte_ofs];
	// Mask out if we are at the end
	if (byte_ofs == (len/SC_BITS)-1) {
		unsigned bit = len % SC_BITS;
		if (bit != 0)
			val &= max_digit(bit);
	}
	return val;
}

unsigned sc_popcount(const sc_word *value, unsigned bits)
{
	unsigned res = 0;
	unsigned full_words = bits/SC_BITS;
	for (unsigned i = 0; i < full_words; ++i) {
		res += popcount(value[i]);
	}
	unsigned remaining_bits = bits%SC_BITS;
	if (remaining_bits != 0) {
		sc_word mask = max_digit(remaining_bits);
		res += popcount(value[full_words] & mask);
	}

	return res;
}

void sc_val_from_bytes(unsigned char const *const bytes, size_t n_bytes,
                       sc_word *buffer)
{
	assert(n_bytes*CHAR_BIT <= (size_t)calc_buffer_size*SC_BITS);

	sc_word *p = buffer;
	assert(SC_BITS == CHAR_BIT);
	memcpy(p, bytes, n_bytes);
	memset(p+n_bytes, 0, buffer+calc_buffer_size-p);
}

void sc_val_to_bytes(const sc_word *buffer, unsigned char *const dest,
                     size_t const dest_len)
{
	assert(dest_len*CHAR_BIT <= (size_t)calc_buffer_size*SC_BITS);

	assert(SC_BITS == CHAR_BIT);
	memcpy(dest, buffer, dest_len);
}

void sc_val_from_bits(unsigned char const *const bytes, unsigned from,
                      unsigned to, sc_word *buffer)
{
	assert(from < to);
	assert((to - from) / CHAR_BIT <= calc_buffer_size);
	assert(CHAR_BIT == 8);
	assert(SC_BITS == CHAR_BIT);

	sc_word *p = buffer;

	/* see which is the lowest and highest byte, special case if they are
	 * the same. */
	const unsigned char *const low      = &bytes[from/CHAR_BIT];
	const unsigned char *const high     = &bytes[(to-1)/CHAR_BIT];
	const unsigned             low_bit  = from%CHAR_BIT;
	const unsigned             high_bit = (to-1)%CHAR_BIT + 1;
	if (low == high) {
		uint32_t val
			= ((uint32_t)*low << (32-high_bit)) >> (32-high_bit+low_bit);
		*p++ = val & SC_MASK;
		goto clear_rest;
	}

	/* lowest byte gets applied partially */
	uint32_t val = ((uint32_t)*low) >> low_bit;
	*p     = val & SC_MASK;
	*(p+1) = 0;
	unsigned bit = (CHAR_BIT-low_bit)%SC_BITS;
	p += (CHAR_BIT-low_bit)/SC_BITS;
	/* fully apply bytes in the middle (but note that they may affect up to 2
	 * units of the destination number) */
	for (const unsigned char *mid = low+1; mid < high; ++mid) {
		uint32_t mval = ((uint32_t)*mid) << bit;
		*p++   |= (mval >> 0)       & SC_MASK;
		*p      = (mval >> SC_BITS) & SC_MASK;
	}
	/* partially apply the highest byte */
	uint32_t hval = ((uint32_t)(*high) << (32-high_bit)) >> (32-high_bit-bit);
	*p++ |= (hval >> 0)       & SC_MASK;
	if ((hval >> SC_BITS) != 0)
		*p++ = (hval >> SC_BITS) & SC_MASK;

clear_rest:
	assert(p <= buffer + calc_buffer_size);
	memset(p, 0, buffer+calc_buffer_size - p);
}

const char *sc_print(const sc_word *value, unsigned bits, enum base_t base,
                     bool is_signed)
{
	return sc_print_buf(output_buffer, bit_pattern_size+1, value, bits,
	                    base, is_signed);
}

char *sc_print_buf(char *buf, size_t buf_len, const sc_word *value,
                   unsigned bits, enum base_t base, bool is_signed)
{
	static const char digits[] = "0123456789ABCDEF";


	char *pos = buf + buf_len;
	*(--pos) = '\0';
	assert(pos >= buf);

	unsigned n_full_words   = bits / SC_BITS;
	unsigned remaining_bits = bits % SC_BITS;
	switch (base) {
	case SC_HEX: {
		assert(SC_BITS == 8);
		unsigned counter = 0;
		for ( ; counter < n_full_words; ++counter) {
			sc_word x = value[counter];
			*(--pos) = digits[(x >> 0) & 0xf];
			*(--pos) = digits[(x >> 4) & 0xf];
		}

		/* last nibble must be masked */
		if (remaining_bits != 0) {
			sc_word mask = max_digit(remaining_bits);
			sc_word x    = value[counter++] & mask;
			*(--pos) = digits[(x >> 0) & 0xf];
			*(--pos) = digits[(x >> 4) & 0xf];
			assert(pos >= buf);
		}

		/* now kill zeros */
		assert(pos >= buf);
		for ( ; pos < buf+buf_len-2; ++pos) {
			if (pos[0] != '0')
				break;
		}
		return pos;
	}
	case SC_DEC: {
		sc_word *base_val = ALLOCANZ(sc_word, calc_buffer_size);
		base_val[0] = 10;

		const sc_word *p        = value;
		bool           sign     = false;
		sc_word       *div2_res = ALLOCAN(sc_word, calc_buffer_size);
		if (is_signed) {
			/* check for negative values */
			if (sc_get_bit_at(value, bits-1)) {
				sc_neg(value, div2_res);
				sign = true;
				p = div2_res;
			}
		}

		/* transfer data into oscillating buffers */
		sc_word *div1_res = ALLOCANZ(sc_word, calc_buffer_size);
		unsigned counter = 0;
		for ( ; counter < n_full_words; ++counter)
			div1_res[counter] = p[counter];

		/* last nibble must be masked */
		if (bits % SC_BITS) {
			sc_word mask = max_digit(bits % SC_BITS);
			div1_res[counter] = p[counter] & mask;
			++counter;
		}

		sc_word *m       = div1_res;
		sc_word *n       = div2_res;
		sc_word *rem_res = ALLOCAN(sc_word, calc_buffer_size);
		for (;;) {
			sc_divmod(m, base_val, n, rem_res);
			sc_word *t = m;
			m = n;
			n = t;
			*(--pos) = digits[rem_res[0]];

			sc_word x = 0;
			for (unsigned i = 0; i < calc_buffer_size; ++i)
				x |= m[i];

			if (x == 0)
				break;
		}
		assert(pos >= buf);
		if (sign) {
			*(--pos) = '-';
			assert(pos >= buf);
		}
		return pos;
	}
	}
	panic("invalid base");
}

void init_strcalc(unsigned precision)
{
	if (output_buffer == NULL) {
		/* round up to multiple of SC_BITS */
		assert(is_po2_or_zero(SC_BITS));
		precision = (precision + (SC_BITS-1)) & ~(SC_BITS-1);

		bit_pattern_size = precision;
		calc_buffer_size = precision / (SC_BITS/2);
		max_value_size   = precision / SC_BITS;

		output_buffer = XMALLOCN(char, bit_pattern_size + 1);
	}
}

void finish_strcalc(void)
{
	free(output_buffer);
	output_buffer = NULL;
}

unsigned sc_get_precision(void)
{
	return bit_pattern_size;
}

bool sc_div(const sc_word *value1, const sc_word *value2, sc_word *buffer)
{
	sc_word *unused_res = ALLOCAN(sc_word, calc_buffer_size);
	return sc_divmod(value1, value2, buffer, unused_res);
}

void sc_mod(const sc_word *value1, const sc_word *value2, sc_word *buffer)
{
	sc_word *unused_res = ALLOCAN(sc_word, calc_buffer_size);
	sc_divmod(value1, value2, unused_res, buffer);
}

void sc_shlI(const sc_word *value, unsigned shift_count, sc_word *buffer)
{
	if (shift_count >= calc_buffer_size * SC_BITS) {
		sc_zero(buffer);
		return;
	}

	/* set upper values */
	unsigned const shift_bits  = shift_count % SC_BITS;
	unsigned const shift_words = shift_count / SC_BITS;
	if (shift_bits == 0) {
		for (unsigned counter = calc_buffer_size; counter-- > shift_words; ) {
			buffer[counter] = value[counter - shift_words];
		}
	} else {
		sc_word val = value[calc_buffer_size - shift_words - 1];
		for (unsigned counter = calc_buffer_size; counter-- > shift_words; ) {
			unsigned nextpos = counter - shift_words - 1;
			sc_word  next    = nextpos < calc_buffer_size ? value[nextpos] : 0;
			buffer[counter] = SC_RESULT(val << shift_bits)
			                | SC_RESULT(next >> (SC_BITS - shift_bits));
			val = next;
		}
	}

	/* fill up with zeros */
	memset(buffer, 0, shift_words);
}

void sc_shl(const sc_word *val1, const sc_word *val2, sc_word *buffer)
{
	long shift_count = sc_val_to_long(val2);
	assert(shift_count >= 0);
	sc_shlI(val1, shift_count, buffer);
}

bool sc_shrI(const sc_word *value, unsigned shift_count, sc_word *buffer)
{
	if (shift_count >= calc_buffer_size*SC_BITS) {
		bool carry_flag = !sc_is_zero(value, calc_buffer_size*SC_BITS);
		sc_zero(buffer);
		return carry_flag;
	}

	unsigned shift_words = shift_count / SC_BITS;
	unsigned shift_bits  = shift_count % SC_BITS;

	/* determine carry flag */
	bool carry_flag = false;
	for (unsigned i = 0; i < shift_words; ++i) {
		if (value[i] != 0) {
			carry_flag = true;
			break;
		}
	}

	/* shift to the right */
	if (shift_bits == 0) {
		/* fast path */
		for (unsigned i = 0; i < calc_buffer_size-shift_words; ++i) {
			buffer[i] = value[i+shift_words];
		}
	} else {
		sc_word val = value[shift_words];
		carry_flag |= val & max_digit(shift_bits);
		for (unsigned i = 0; i < calc_buffer_size-shift_words; ++i) {
			unsigned next_pos = i+shift_words+1;
			sc_word  next = next_pos < calc_buffer_size ? value[next_pos] : 0;
			buffer[i] = SC_RESULT(val >> shift_bits)
			          | SC_RESULT(next << (SC_BITS - shift_bits));
			val = next;
		}
	}

	/* fill upper words with zero */
	memset(&buffer[calc_buffer_size-shift_words], 0, shift_words);
	return carry_flag;
}

bool sc_shr(const sc_word *val1, const sc_word *val2, sc_word *buffer)
{
	long shift_count = sc_val_to_long(val2);
	assert(shift_count >= 0);
	return sc_shrI(val1, shift_count, buffer);
}

bool sc_shrsI(const sc_word *value, unsigned shift_count, unsigned bitsize,
              sc_word *buffer)
{
	sc_word sign = sc_get_bit_at(value, bitsize-1) ? SC_MASK : 0;

	/* if shifting far enough the result is either 0 or -1 */
	if (shift_count >= bitsize) {
		bool carry_flag = !sc_is_zero(value, calc_buffer_size*SC_BITS);
		assert(SC_BITS <= CHAR_BIT);
		memset(buffer, sign, calc_buffer_size);
		return carry_flag;
	}

	unsigned shift_words = shift_count / SC_BITS;
	unsigned shift_bits  = shift_count % SC_BITS;

	/* determine carry flag */
	bool carry_flag = false;
	for (unsigned i = 0; i < shift_words; ++i) {
		if (value[i] != 0) {
			carry_flag = true;
			break;
		}
	}

	/* TODO: bitsize % SC_BITS != 0 not implemented yet */
	assert(bitsize % SC_BITS == 0);
	unsigned limit = bitsize / SC_BITS;

	/* shift to the right */
	if (shift_bits == 0) {
		/* fast path */
		for (unsigned i = 0; i < limit-shift_words; ++i) {
			buffer[i] = value[i+shift_words];
		}
	} else {
		sc_word val = value[shift_words];
		carry_flag |= val & max_digit(shift_bits);
		for (unsigned i = 0; i < limit-shift_words; ++i) {
			unsigned next_pos = i+shift_words+1;
			sc_word next = next_pos<limit ? value[next_pos] : sign;
			buffer[i] = SC_RESULT(val >> shift_bits)
			          | SC_RESULT(next << (SC_BITS - shift_bits));
			val = next;
		}
	}

	/* fill upper words with extended sign */
	assert(SC_BITS <= CHAR_BIT);
	memset(&buffer[limit-shift_words], sign,
	       calc_buffer_size-(limit-shift_words));
	return carry_flag;
}

bool sc_shrs(const sc_word *val1, const sc_word *val2, unsigned bitsize,
             sc_word *buffer)
{
	long shift_count = sc_val_to_long(val2);
	assert(shift_count >= 0);
	return sc_shrsI(val1, shift_count, bitsize, buffer);
}
