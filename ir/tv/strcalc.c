/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Provides basic mathematical operations on values represented as strings.
 * @date     2003
 * @author   Mathias Heil
 */
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <limits.h>

#include "strcalc.h"
#include "xmalloc.h"
#include "error.h"

/*
 * local definitions and macros
 */
#define SC_BITS      4
#define SC_RESULT(x) ((x) & ((1U << SC_BITS) - 1U))
#define SC_CARRY(x)  ((unsigned)(x) >> SC_BITS)

#define CLEAR_BUFFER(b) assert(b); memset(b, SC_0, calc_buffer_size)
#define SHIFT(count) (SC_1 << (count))
#define _val(a) ((a)-SC_0)
#define _digit(a) ((a)+SC_0)
#define _bitisset(digit, pos) (((digit) & SHIFT(pos)) != SC_0)

/*
 * private variables
 */
static char *calc_buffer = NULL;    /* buffer holding all results */
static char *output_buffer = NULL;  /* buffer for output */
static int bit_pattern_size;        /* maximum number of bits */
static int calc_buffer_size;        /* size of internally stored values */
static int max_value_size;          /* maximum size of values */

static int carry_flag;              /**< some computation set the carry_flag:
                                         - right shift if bits were lost due to shifting
                                         - division if there was a remainder
                                         However, the meaning of carry is machine dependent
                                         and often defined in other ways! */

static const char sex_digit[4] = { SC_E, SC_C, SC_8, SC_0 };
static const char zex_digit[4] = { SC_1, SC_3, SC_7, SC_F };
static const char max_digit[4] = { SC_0, SC_1, SC_3, SC_7 };
static const char min_digit[4] = { SC_F, SC_E, SC_C, SC_8 };

static char const shrs_table[16][4][2] = {
                       { {SC_0, SC_0}, {SC_0, SC_0}, {SC_0, SC_0}, {SC_0, SC_0} },
                       { {SC_1, SC_0}, {SC_0, SC_8}, {SC_0, SC_4}, {SC_0, SC_2} },
                       { {SC_2, SC_0}, {SC_1, SC_0}, {SC_0, SC_8}, {SC_0, SC_4} },
                       { {SC_3, SC_0}, {SC_1, SC_8}, {SC_0, SC_C}, {SC_0, SC_6} },
                       { {SC_4, SC_0}, {SC_2, SC_0}, {SC_1, SC_0}, {SC_0, SC_8} },
                       { {SC_5, SC_0}, {SC_2, SC_8}, {SC_1, SC_4}, {SC_0, SC_A} },
                       { {SC_6, SC_0}, {SC_3, SC_0}, {SC_1, SC_8}, {SC_0, SC_C} },
                       { {SC_7, SC_0}, {SC_3, SC_8}, {SC_1, SC_C}, {SC_0, SC_E} },
                       { {SC_8, SC_0}, {SC_4, SC_0}, {SC_2, SC_0}, {SC_1, SC_0} },
                       { {SC_9, SC_0}, {SC_4, SC_8}, {SC_2, SC_4}, {SC_1, SC_2} },
                       { {SC_A, SC_0}, {SC_5, SC_0}, {SC_2, SC_8}, {SC_1, SC_4} },
                       { {SC_B, SC_0}, {SC_5, SC_8}, {SC_2, SC_C}, {SC_1, SC_6} },
                       { {SC_C, SC_0}, {SC_6, SC_0}, {SC_3, SC_0}, {SC_1, SC_8} },
                       { {SC_D, SC_0}, {SC_6, SC_8}, {SC_3, SC_4}, {SC_1, SC_A} },
                       { {SC_E, SC_0}, {SC_7, SC_0}, {SC_3, SC_8}, {SC_1, SC_C} },
                       { {SC_F, SC_0}, {SC_7, SC_8}, {SC_3, SC_C}, {SC_1, SC_E} }
                                   };

/** converting a digit to a binary string */
static char const *const binary_table[] = {
	"0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111",
	"1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111"
};

/*****************************************************************************
 * private functions
 *****************************************************************************/

/**
 * implements the bitwise NOT operation
 */
static void do_bitnot(const char *val, char *buffer)
{
	for (int counter = 0; counter<calc_buffer_size; counter++)
		buffer[counter] = val[counter] ^ SC_F;
}

/**
 * implements the bitwise OR operation
 */
static void do_bitor(const char *val1, const char *val2, char *buffer)
{
	for (int counter = 0; counter<calc_buffer_size; counter++)
		buffer[counter] = val1[counter] | val2[counter];
}

/**
 * implements the bitwise eXclusive OR operation
 */
static void do_bitxor(const char *val1, const char *val2, char *buffer)
{
	for (int counter = 0; counter<calc_buffer_size; counter++)
		buffer[counter] = val1[counter] ^ val2[counter];
}

/**
 * implements the bitwise AND operation
 */
static void do_bitand(const char *val1, const char *val2, char *buffer)
{
	for (int counter = 0; counter<calc_buffer_size; counter++)
		buffer[counter] = val1[counter] & val2[counter];
}

/**
 * implements the bitwise AND not operation
 */
static void do_bitandnot(const char *val1, const char *val2, char *buffer)
{
	for (int counter = 0; counter < calc_buffer_size; ++counter)
		buffer[counter] = val1[counter] & (SC_F ^ val2[counter]);
}

/**
 * returns the sign bit.
 *
 * @todo This implementation is wrong, as it returns the highest bit of the buffer
 *       NOT the highest bit depending on the real mode
 */
static int do_sign(const char *val)
{
	return (val[calc_buffer_size-1] <= SC_7) ? (1) : (-1);
}

/**
 * returns non-zero if bit at position pos is set
 */
static int do_bit(const char *val, int pos)
{
	int bit    = pos & 3;
	int nibble = pos >> 2;

	return _bitisset(val[nibble], bit);
}

/**
 * Implements a fast ADD + 1
 */
static void do_inc(const char *val, char *buffer)
{
	int counter = 0;

	while (counter++ < calc_buffer_size) {
		if (*val == SC_F) {
			*buffer++ = SC_0;
			val++;
		} else {
			/* No carry here, *val != SC_F */
			*buffer = *val + SC_1;
			return;
		}
	}
	/* here a carry could be lost, this is intended because this should
	 * happen only when a value changes sign. */
}

/**
 * Implements a unary MINUS
 */
static void do_negate(const char *val, char *buffer)
{
	do_bitnot(val, buffer);
	do_inc(buffer, buffer);
}

/**
 * Implements a binary ADD
 *
 * @todo The implementation of carry is wrong, as it is the
 *       calc_buffer_size carry, not the mode depending
 */
static void do_add(const char *val1, const char *val2, char *buffer)
{
	unsigned carry = SC_0;
	for (int counter = 0; counter < calc_buffer_size; ++counter) {
		unsigned const sum = val1[counter] + val2[counter] + carry;
		buffer[counter] = SC_RESULT(sum);
		carry           = SC_CARRY(sum);
	}
	carry_flag = carry != SC_0;
}

/**
 * Implements a binary SUB
 */
static void do_sub(const char *val1, const char *val2, char *buffer)
{
	char *temp_buffer = (char*) alloca(calc_buffer_size); /* intermediate buffer to hold -val2 */

	do_negate(val2, temp_buffer);
	do_add(val1, temp_buffer, buffer);
}

/**
 * Implements a binary MUL
 */
static void do_mul(const char *val1, const char *val2, char *buffer)
{
	char *temp_buffer = (char*) alloca(calc_buffer_size);
	char *neg_val1    = (char*) alloca(calc_buffer_size);
	char *neg_val2    = (char*) alloca(calc_buffer_size);

	/* init result buffer to zeros */
	memset(temp_buffer, SC_0, calc_buffer_size);

	/* the multiplication works only for positive values, for negative values *
	 * it is necessary to negate them and adjust the result accordingly       */
	char sign = 0;
	if (do_sign(val1) == -1) {
		do_negate(val1, neg_val1);
		val1 = neg_val1;
		sign ^= 1;
	}
	if (do_sign(val2) == -1) {
		do_negate(val2, neg_val2);
		val2 = neg_val2;
		sign ^= 1;
	}

	for (int c_outer = 0; c_outer < max_value_size; c_outer++) {
		if (val2[c_outer] != SC_0) {
			unsigned carry = SC_0; /* container for carries */
			for (int c_inner = 0; c_inner < max_value_size; c_inner++) {
				/* do the following calculation:
				 * Add the current carry, the value at position c_outer+c_inner
				 * and the result of the multiplication of val1[c_inner] and
				 * val2[c_outer]. This is the usual pen-and-paper multiplication
				 */

				/* multiplicate the two digits */
				unsigned const mul = val1[c_inner] * val2[c_outer];
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
	}

	if (sign)
		do_negate(temp_buffer, buffer);
	else
		memcpy(buffer, temp_buffer, calc_buffer_size);
}

/**
 * Shift the buffer to left and add a 4 bit digit
 */
static void do_push(const char digit, char *buffer)
{
	for (int counter = calc_buffer_size - 2; counter >= 0; counter--) {
		buffer[counter+1] = buffer[counter];
	}
	buffer[0] = digit;
}

/**
 * Implements truncating integer division and remainder.
 *
 * Note: This is MOST slow
 */
static void do_divmod(const char *rDividend, const char *divisor, char *quot,
                      char *rem)
{
	/* clear result buffer */
	memset(quot, SC_0, calc_buffer_size);
	memset(rem, SC_0, calc_buffer_size);

	/* if the divisor is zero this won't work (quot is zero) */
	assert(sc_comp(divisor, quot) != ir_relation_equal && "division by zero!");

	/* if the dividend is zero result is zero (quot is zero) */
	const char *dividend = rDividend;
	if (sc_comp(dividend, quot) == ir_relation_equal)
		return;

	char  div_sign = 0;
	char  rem_sign = 0;
	char *neg_val1 = (char*) alloca(calc_buffer_size);
	if (do_sign(dividend) == -1) {
		do_negate(dividend, neg_val1);
		div_sign ^= 1;
		rem_sign ^= 1;
		dividend = neg_val1;
	}

	char *neg_val2 = (char*) alloca(calc_buffer_size);
	do_negate(divisor, neg_val2);
	const char *minus_divisor;
	if (do_sign(divisor) == -1) {
		div_sign ^= 1;
		minus_divisor = divisor;
		divisor = neg_val2;
	} else {
		minus_divisor = neg_val2;
	}

	/* if divisor >= dividend division is easy
	 * (remember these are absolute values) */
	switch (sc_comp(dividend, divisor)) {
	case ir_relation_equal: /* dividend == divisor */
		quot[0] = SC_1;
		goto end;

	case ir_relation_less: /* dividend < divisor */
		memcpy(rem, dividend, calc_buffer_size);
		goto end;

	default: /* unluckily division is necessary :( */
		break;
	}

	for (int c_dividend = calc_buffer_size - 1; c_dividend >= 0; c_dividend--) {
		do_push(dividend[c_dividend], rem);
		do_push(SC_0, quot);

		if (sc_comp(rem, divisor) != ir_relation_less) {
			/* remainder >= divisor */
			/* subtract until the remainder becomes negative, this should
			 * be faster than comparing remainder with divisor  */
			do_add(rem, minus_divisor, rem);

			while (do_sign(rem) == 1) {
				quot[0] = SC_RESULT(quot[0] + SC_1); /* TODO can this generate carry or is masking redundant? */
				do_add(rem, minus_divisor, rem);
			}

			/* subtracted one too much */
			do_add(rem, divisor, rem);
		}
	}
end:
	/* sets carry if remainder is non-zero ??? */
	carry_flag = !sc_is_zero(rem);

	if (div_sign)
		do_negate(quot, quot);

	if (rem_sign)
		do_negate(rem, rem);
}

/**
 * Implements a Shift Left, which can either preserve the sign bit
 * or not.
 *
 * @todo Assertions seems to be wrong
 */
static void do_shl(const char *val1, char *buffer, long shift_cnt, int bitsize,
                   unsigned is_signed)
{
	assert((shift_cnt >= 0) || (0 && "negative leftshift"));
	assert(((do_sign(val1) != -1) || is_signed) || (0 && "unsigned mode and negative value"));
	assert(((!_bitisset(val1[(bitsize-1)/4], (bitsize-1)%4)) || !is_signed || (do_sign(val1) == -1)) || (0 && "value is positive, should be negative"));
	assert(((_bitisset(val1[(bitsize-1)/4], (bitsize-1)%4)) || !is_signed || (do_sign(val1) == 1)) || (0 && "value is negative, should be positive"));

	/* if shifting far enough the result is zero */
	if (shift_cnt >= bitsize) {
		memset(buffer, SC_0, calc_buffer_size);
		return;
	}

	unsigned const shift = shift_cnt % SC_BITS;
	shift_cnt = shift_cnt / 4;

	/* shift the single digits some bytes (offset) and some bits (table)
	 * to the left */
	unsigned carry = SC_0;
	for (int counter = 0; counter < bitsize/4 - shift_cnt; counter++) {
		unsigned const shl = val1[counter] << shift | carry;
		buffer[counter + shift_cnt] = SC_RESULT(shl);
		carry = SC_CARRY(shl);
	}
	int counter   = bitsize/4 - shift_cnt;
	int bitoffset = 0;
	if (bitsize%4 > 0) {
		unsigned const shl = val1[counter] << shift | carry;
		buffer[counter + shift_cnt] = SC_RESULT(shl);
		bitoffset = counter;
	} else {
		bitoffset = counter - 1;
	}

	/* fill with zeroes */
	for (int counter = 0; counter < shift_cnt; counter++)
		buffer[counter] = SC_0;

	/* if the mode was signed, change sign when the mode's msb is now 1 */
	shift_cnt = bitoffset + shift_cnt;
	bitoffset = (bitsize-1) % 4;
	if (is_signed && _bitisset(buffer[shift_cnt], bitoffset)) {
		/* this sets the upper bits of the leftmost digit */
		buffer[shift_cnt] |= min_digit[bitoffset];
		for (int counter = shift_cnt+1; counter < calc_buffer_size; counter++) {
			buffer[counter] = SC_F;
		}
	} else if (is_signed && !_bitisset(buffer[shift_cnt], bitoffset)) {
		/* this clears the upper bits of the leftmost digit */
		buffer[shift_cnt] &= max_digit[bitoffset];
		for (int counter = shift_cnt+1; counter < calc_buffer_size; counter++) {
			buffer[counter] = SC_0;
		}
	}
}

/**
 * Implements a Shift Right, which can either preserve the sign bit
 * or not.
 *
 * @param bitsize   bitsize of the value to be shifted
 *
 * @todo Assertions seems to be wrong
 */
static void do_shr(const char *val1, char *buffer, long shift_cnt, int bitsize,
                   unsigned is_signed, int signed_shift)
{
	assert((shift_cnt >= 0) || (0 && "negative rightshift"));
	assert(((!_bitisset(val1[(bitsize-1)/4], (bitsize-1)%4)) || !is_signed || (do_sign(val1) == -1)) || (0 && "value is positive, should be negative"));
	assert(((_bitisset(val1[(bitsize-1)/4], (bitsize-1)%4)) || !is_signed || (do_sign(val1) == 1)) || (0 && "value is negative, should be positive"));

	char sign = signed_shift && do_bit(val1, bitsize - 1) ? SC_F : SC_0;

	/* if shifting far enough the result is either 0 or -1 */
	if (shift_cnt >= bitsize) {
		if (!sc_is_zero(val1)) {
			carry_flag = 1;
		}
		memset(buffer, sign, calc_buffer_size);
		return;
	}

	int shift_mod = shift_cnt &  3;
	int shift_nib = shift_cnt >> 2;

	/* check if any bits are lost, and set carry_flag if so */
	for (int counter = 0; counter < shift_nib; ++counter) {
		if (val1[counter] != 0) {
			carry_flag = 1;
			break;
		}
	}
	if ((_val(val1[shift_nib]) & ((1<<shift_mod)-1)) != 0)
		carry_flag = 1;

	/* shift digits to the right with offset, carry and all */
	buffer[0] = shrs_table[_val(val1[shift_nib])][shift_mod][0];
	int counter;
	for (counter = 1; counter < ((bitsize + 3) >> 2) - shift_nib; counter++) {
		const char *shrs = shrs_table[_val(val1[counter + shift_nib])][shift_mod];
		buffer[counter]      = shrs[0];
		buffer[counter - 1] |= shrs[1];
	}

	/* the last digit is special in regard of signed/unsigned shift */
	int bitoffset = bitsize & 3;
	char msd = sign;  /* most significant digit */

	/* remove sign bits if mode was signed and this is an unsigned shift */
	if (!signed_shift && is_signed) {
		msd &= max_digit[bitoffset];
	}

	const char *shrs = shrs_table[_val(msd)][shift_mod];

	/* signed shift and signed mode and negative value means all bits to the left are set */
	if (signed_shift && sign == SC_F) {
		buffer[counter] = shrs[0] | min_digit[bitoffset];
	} else {
		buffer[counter] = shrs[0];
	}

	if (counter > 0)
		buffer[counter - 1] |= shrs[1];

	/* fill with SC_F or SC_0 depending on sign */
	for (counter++; counter < calc_buffer_size; counter++) {
		buffer[counter] = sign;
	}
}

/*****************************************************************************
 * public functions, declared in strcalc.h
 *****************************************************************************/
const void *sc_get_buffer(void)
{
	return (void*)calc_buffer;
}

int sc_get_buffer_length(void)
{
	return calc_buffer_size;
}

void sign_extend(void *buffer, ir_mode *mode)
{
	char *calc_buffer = (char*)buffer;
	int bits          = get_mode_size_bits(mode) - 1;
	int nibble        = bits >> 2;

	if (mode_is_signed(mode)) {
		int max = max_digit[bits & 3];
		if (calc_buffer[nibble] > max) {
			/* sign bit is set, we need sign expansion */

			for (int i = nibble + 1; i < calc_buffer_size; ++i)
				calc_buffer[i] = SC_F;
			calc_buffer[nibble] |= sex_digit[bits & 3];
		} else {
			/* set all bits to zero */
			for (int i = nibble + 1; i < calc_buffer_size; ++i)
				calc_buffer[i] = SC_0;
			calc_buffer[nibble] &= zex_digit[bits & 3];
		}
	} else {
		/* do zero extension */
		for (int i = nibble + 1; i < calc_buffer_size; ++i)
			calc_buffer[i] = SC_0;
		calc_buffer[nibble] &= zex_digit[bits & 3];
	}
}

/* ensure that our source character set conforms to ASCII for a-f, A-F, 0-9 */
static inline void check_ascii(void)
{
	assert((('a'-97) | ('b'-98) | ('c'-99) | ('d'-100) | ('e'-101) | ('f'-102)
	       |('A'-65) | ('B'-66) | ('C'-67) | ('D'- 68) | ('E'- 69) | ('F'- 70)
	       |('0'-48) | ('1'-49) | ('2'-50) | ('3'- 51) | ('4'- 52)
	       |('5'-53) | ('6'-54) | ('7'-55) | ('8'- 56) | ('9'- 57)) == 0);
}

int sc_val_from_str(char sign, unsigned base, const char *str, size_t len,
                    void *buffer)
{
	assert(sign == -1 || sign == 1);
	assert(str != NULL);
	assert(len > 0);
	check_ascii();

	assert(base > 1 && base <= 16);
	char *sc_base = (char*) alloca(calc_buffer_size);
	sc_val_from_ulong(base, sc_base);

	char *val = (char*) alloca(calc_buffer_size);
	if (buffer == NULL)
		buffer = calc_buffer;

	CLEAR_BUFFER(buffer);
	CLEAR_BUFFER(val);

	/* BEGIN string evaluation, from left to right */
	while (len > 0) {
		char c = *str;
		unsigned v;
		if (c >= '0' && c <= '9')
			v = c - '0';
		else if (c >= 'A' && c <= 'F')
			v = c - 'A' + 10;
		else if (c >= 'a' && c <= 'f')
			v = c - 'a' + 10;
		else
			return 0;

		if (v >= base)
			return 0;
		val[0] = v;

		/* Radix conversion from base b to base B:
		 *  (UnUn-1...U1U0)b == ((((Un*b + Un-1)*b + ...)*b + U1)*b + U0)B */
		/* multiply current value with base */
		do_mul(sc_base, (const char*) buffer, (char*) buffer);
		/* add next digit to current value  */
		do_add(val, (const char*) buffer, (char*) buffer);

		/* get ready for the next letter */
		str++;
		len--;
	}

	if (sign < 0)
		do_negate((const char*) buffer, (char*) buffer);

	return 1;
}

void sc_val_from_long(long value, void *buffer)
{
	if (buffer == NULL) buffer = calc_buffer;
	char *pos = (char*) buffer;

	char sign       = (value < 0);
	char is_minlong = value == LONG_MIN;

	/* use absolute value, special treatment of MIN_LONG to avoid overflow */
	if (sign) {
		if (is_minlong)
			value = -(value+1);
		else
			value = -value;
	}

	CLEAR_BUFFER(buffer);

	while ((value != 0) && (pos < (char*)buffer + calc_buffer_size)) {
		*pos++ = _digit(value & 0xf);
		value >>= 4;
	}

	if (sign) {
		if (is_minlong)
			do_inc((const char*) buffer, (char*) buffer);

		do_negate((const char*) buffer, (char*) buffer);
	}
}

void sc_val_from_ulong(unsigned long value, void *buffer)
{
	if (buffer == NULL) buffer = calc_buffer;
	unsigned char *pos = (unsigned char*) buffer;

	while (pos < (unsigned char *)buffer + calc_buffer_size) {
		*pos++ = (unsigned char)_digit(value & 0xf);
		value >>= 4;
	}
}

long sc_val_to_long(const void *val)
{
	long l = 0;
	for (int i = calc_buffer_size - 1; i >= 0; i--) {
		l = (l << 4) + _val(((char *)val)[i]);
	}
	return l;
}

uint64_t sc_val_to_uint64(const void *val)
{
	uint64_t res = 0;
	for (int i = calc_buffer_size - 1; i >= 0; i--) {
		res = (res << 4) + _val(((char*)val)[i]);
	}
	return res;
}

void sc_min_from_bits(unsigned int num_bits, unsigned int sign, void *buffer)
{
	if (buffer == NULL) buffer = calc_buffer;
	CLEAR_BUFFER(buffer);

	if (!sign) return;  /* unsigned means minimum is 0(zero) */

	char *pos = (char*) buffer;

	int bits = num_bits - 1;
	int i;
	for (i = 0; i < bits/4; i++)
		*pos++ = SC_0;

	*pos++ = min_digit[bits%4];

	for (i++; i <= calc_buffer_size - 1; i++)
		*pos++ = SC_F;
}

void sc_max_from_bits(unsigned int num_bits, unsigned int sign, void *buffer)
{
	char* pos;
	int i, bits;

	if (buffer == NULL) buffer = calc_buffer;
	CLEAR_BUFFER(buffer);
	pos = (char*) buffer;

	bits = num_bits - sign;
	for (i = 0; i < bits/4; i++)
		*pos++ = SC_F;

	*pos++ = max_digit[bits%4];

	for (i++; i <= calc_buffer_size - 1; i++)
		*pos++ = SC_0;
}

void sc_truncate(unsigned int num_bits, void *buffer)
{
	char *cbuffer = (char*) buffer;
	char *pos = cbuffer + (num_bits / 4);
	char *end = cbuffer + calc_buffer_size;

	assert(pos < end);

	switch (num_bits % 4) {
	case 0: /* nothing to do */ break;
	case 1: *pos++ &= SC_1; break;
	case 2: *pos++ &= SC_3; break;
	case 3: *pos++ &= SC_7; break;
	}

	for ( ; pos < end; ++pos)
		*pos = SC_0;
}

ir_relation sc_comp(void const* const value1, void const* const value2)
{
	int counter = calc_buffer_size - 1;
	const char *val1 = (const char *)value1;
	const char *val2 = (const char *)value2;

	/* compare signs first:
	 * the loop below can only compare values of the same sign! */
	if (do_sign(val1) != do_sign(val2))
		return do_sign(val1) == 1 ? ir_relation_greater : ir_relation_less;

	/* loop until two digits differ, the values are equal if there
	 * are no such two digits */
	while (val1[counter] == val2[counter]) {
		counter--;
		if (counter < 0) return ir_relation_equal;
	}

	/* the leftmost digit is the most significant, so this returns
	 * the correct result.
	 * This implies the digit enum is ordered */
	return val1[counter] > val2[counter] ? ir_relation_greater : ir_relation_less;
}

int sc_get_highest_set_bit(const void *value)
{
	const char *val  = (const char*)value;
	int         high = calc_buffer_size * 4 - 1;
	for (int counter = calc_buffer_size-1; counter >= 0; counter--) {
		if (val[counter] == SC_0)
			high -= 4;
		else {
			if (val[counter] > SC_7) return high;
			else if (val[counter] > SC_3) return high - 1;
			else if (val[counter] > SC_1) return high - 2;
			else return high - 3;
		}
	}
	return high;
}

int sc_get_lowest_set_bit(const void *value)
{
	const char *val = (const char*)value;
	int         low = 0;
	for (int counter = 0; counter < calc_buffer_size; counter++) {
		switch (val[counter]) {
		case SC_1:
		case SC_3:
		case SC_5:
		case SC_7:
		case SC_9:
		case SC_B:
		case SC_D:
		case SC_F:
			return low;
		case SC_2:
		case SC_6:
		case SC_A:
		case SC_E:
			return low + 1;
		case SC_4:
		case SC_C:
			return low + 2;
		case SC_8:
			return low + 3;
		default:
			low += 4;
		}
	}
	return -1;
}

int sc_get_bit_at(const void *value, unsigned pos)
{
	const char *val    = (const char*) value;
	unsigned    nibble = pos >> 2;

	return (val[nibble] & SHIFT(pos & 3)) != SC_0;
}

void sc_set_bit_at(void *value, unsigned pos)
{
	char    *val    = (char*) value;
	unsigned nibble = pos >> 2;

	val[nibble] |= SHIFT(pos & 3);
}

int sc_is_zero(const void *value)
{
	const char* val = (const char *)value;
	for (int counter = 0; counter < calc_buffer_size; ++counter) {
		if (val[counter] != SC_0)
			return 0;
	}
	return 1;
}

int sc_is_negative(const void *value)
{
	return do_sign((const char*) value) == -1;
}

int sc_had_carry(void)
{
	return carry_flag;
}

unsigned char sc_sub_bits(const void *value, int len, unsigned byte_ofs)
{
	/* the current scheme uses one byte to store a nibble */
	int nibble_ofs = 2 * byte_ofs;
	if (4 * nibble_ofs >= len)
		return 0;

	const char   *val = (const char *)value;
	unsigned char res = _val(val[nibble_ofs]);
	if (len > 4 * (nibble_ofs + 1))
		res |= _val(val[nibble_ofs + 1]) << 4;

	/* kick bits outsize */
	if (len - 8 * byte_ofs < 8) {
		res &= (1 << (len - 8 * byte_ofs)) - 1;
	}
	return res;
}

void sc_val_from_bytes(unsigned char const *const bytes, size_t n_bytes,
                       bool big_endian, void *buffer)
{
	assert(n_bytes <= (size_t)calc_buffer_size);

	if (buffer == NULL)
		buffer = calc_buffer;
	char *p = (char*)buffer;
	if (big_endian) {
		for (unsigned char const *bp = bytes+n_bytes; bp >= bytes; --bp) {
			unsigned char v = *bp;
			*p++ =  v     & 0xf;
			*p++ = (v>>4) & 0xf;
		}
	} else {
		for (unsigned char const *bp = bytes, *bp_end = bytes + n_bytes;
		     bp < bp_end; ++bp) {
			unsigned char v = *bp;
			*p++ =  v     & 0xf;
			*p++ = (v>>4) & 0xf;
		}
	}
	for (char *p_end = (char*)buffer+calc_buffer_size; p < p_end; ++p)
		*p = 0;
}

/*
 * convert to a string
 * FIXME: Doesn't check buffer bounds
 */
const char *sc_print(const void *value, unsigned bits, enum base_t base, int signed_mode)
{
	static const char big_digits[]   = "0123456789ABCDEF";
	static const char small_digits[] = "0123456789abcdef";

	const char *val    = (const char *)value;
	const char *digits = small_digits;

	char *base_val = (char*) alloca(calc_buffer_size);
	char *div1_res = (char*) alloca(calc_buffer_size);
	char *div2_res = (char*) alloca(calc_buffer_size);
	char *rem_res  = (char*) alloca(calc_buffer_size);

	char *pos = output_buffer + bit_pattern_size;
	*(--pos) = '\0';

	/* special case */
	if (bits == 0) {
		bits = bit_pattern_size;
	}
	int nibbles = bits >> 2;
	int counter;
	switch (base) {
	case SC_HEX:
		digits = big_digits;
	case SC_hex:
		for (counter = 0; counter < nibbles; ++counter) {
			*(--pos) = digits[_val(val[counter])];
		}

		/* last nibble must be masked */
		if (bits & 3) {
			int  mask = zex_digit[(bits & 3) - 1];
			char x    = val[counter++] & mask;
			*(--pos) = digits[_val(x)];
		}

		/* now kill zeros */
		for (; counter > 1; --counter, ++pos) {
			if (pos[0] != '0')
				break;
		}
		break;

	case SC_BIN:
		for (counter = 0; counter < nibbles; ++counter) {
			pos -= 4;
			const char *p = binary_table[_val(val[counter])];
			pos[0] = p[0];
			pos[1] = p[1];
			pos[2] = p[2];
			pos[3] = p[3];
		}

		/* last nibble must be masked */
		if (bits & 3) {
			int  mask = zex_digit[(bits & 3) - 1];
			char x    = val[counter++] & mask;

			pos -= 4;
			const char *p = binary_table[_val(x)];
			pos[0] = p[0];
			pos[1] = p[1];
			pos[2] = p[2];
			pos[3] = p[3];
		}

		/* now kill zeros */
		for (counter <<= 2; counter > 1; --counter, ++pos)
			if (pos[0] != '0')
				break;
			break;

	case SC_DEC:
	case SC_OCT:
		memset(base_val, SC_0, calc_buffer_size);
		base_val[0] = base == SC_DEC ? SC_A : SC_8;

		const char *p    = val;
		int         sign = 0;
		if (signed_mode && base == SC_DEC) {
			/* check for negative values */
			if (do_bit(val, bits - 1)) {
				do_negate(val, div2_res);
				sign = 1;
				p = div2_res;
			}
		}

		/* transfer data into oscillating buffers */
		memset(div1_res, SC_0, calc_buffer_size);
		for (counter = 0; counter < nibbles; ++counter)
			div1_res[counter] = p[counter];

		/* last nibble must be masked */
		if (bits & 3) {
			int mask = zex_digit[(bits & 3) - 1];
			div1_res[counter] = p[counter] & mask;
			++counter;
		}

		char *m = div1_res;
		char *n = div2_res;
		for (;;) {
			do_divmod(m, base_val, n, rem_res);
			char *t = m;
			m = n;
			n = t;
			*(--pos) = digits[_val(rem_res[0])];

			char x = 0;
			for (int i = 0; i < calc_buffer_size; ++i)
				x |= _val(m[i]);

			if (x == 0)
				break;
		}
		if (sign)
			*(--pos) = '-';
		break;

	default:
		panic("Unsupported base %d", base);
	}
	return pos;
}

void init_strcalc(int precision)
{
	if (calc_buffer == NULL) {
		if (precision <= 0) precision = SC_DEFAULT_PRECISION;

		/* round up to multiple of 4 */
		precision = (precision + 3) & ~3;

		bit_pattern_size = (precision);
		calc_buffer_size = (precision / 2);
		max_value_size   = (precision / 4);

		calc_buffer   = XMALLOCN(char, calc_buffer_size + 1);
		output_buffer = XMALLOCN(char, bit_pattern_size + 1);
	}
}


void finish_strcalc(void)
{
	free(calc_buffer);   calc_buffer   = NULL;
	free(output_buffer); output_buffer = NULL;
}

int sc_get_precision(void)
{
	return bit_pattern_size;
}


void sc_add(const void *value1, const void *value2, void *buffer)
{
	CLEAR_BUFFER(calc_buffer);
	carry_flag = 0;

	do_add((const char*) value1, (const char*) value2, (char*) calc_buffer);

	if ((buffer != NULL) && (buffer != calc_buffer)) {
		memcpy(buffer, calc_buffer, calc_buffer_size);
	}
}

void sc_sub(const void *value1, const void *value2, void *buffer)
{
	CLEAR_BUFFER(calc_buffer);
	carry_flag = 0;

	do_sub((const char*) value1, (const char*) value2, calc_buffer);

	if ((buffer != NULL) && (buffer != calc_buffer)) {
		memcpy(buffer, calc_buffer, calc_buffer_size);
	}
}

void sc_neg(const void *value1, void *buffer)
{
	carry_flag = 0;

	do_negate((const char*) value1, calc_buffer);

	if ((buffer != NULL) && (buffer != calc_buffer)) {
		memcpy(buffer, calc_buffer, calc_buffer_size);
	}
}

void sc_and(const void *value1, const void *value2, void *buffer)
{
	CLEAR_BUFFER(calc_buffer);
	carry_flag = 0;

	do_bitand((const char*) value1, (const char*) value2, calc_buffer);

	if ((buffer != NULL) && (buffer != calc_buffer)) {
		memcpy(buffer, calc_buffer, calc_buffer_size);
	}
}

void sc_andnot(const void *value1, const void *value2, void *buffer)
{
	CLEAR_BUFFER(calc_buffer);
	carry_flag = 0;

	do_bitandnot((const char*) value1, (const char*) value2, calc_buffer);

	if (buffer != NULL && buffer != calc_buffer) {
		memcpy(buffer, calc_buffer, calc_buffer_size);
	}
}

void sc_or(const void *value1, const void *value2, void *buffer)
{
	CLEAR_BUFFER(calc_buffer);
	carry_flag = 0;

	do_bitor((const char*) value1, (const char*) value2, calc_buffer);

	if ((buffer != NULL) && (buffer != calc_buffer)) {
		memcpy(buffer, calc_buffer, calc_buffer_size);
	}
}

void sc_xor(const void *value1, const void *value2, void *buffer)
{
	CLEAR_BUFFER(calc_buffer);
	carry_flag = 0;

	do_bitxor((const char*) value1, (const char*) value2, calc_buffer);

	if ((buffer != NULL) && (buffer != calc_buffer)) {
		memcpy(buffer, calc_buffer, calc_buffer_size);
	}
}

void sc_not(const void *value1, void *buffer)
{
	CLEAR_BUFFER(calc_buffer);
	carry_flag = 0;

	do_bitnot((const char*) value1, calc_buffer);

	if ((buffer != NULL) && (buffer != calc_buffer)) {
		memcpy(buffer, calc_buffer, calc_buffer_size);
	}
}

void sc_mul(const void *value1, const void *value2, void *buffer)
{
	CLEAR_BUFFER(calc_buffer);
	carry_flag = 0;

	do_mul((const char*) value1, (const char*) value2, calc_buffer);

	if ((buffer != NULL) && (buffer != calc_buffer)) {
		memcpy(buffer, calc_buffer, calc_buffer_size);
	}
}

void sc_div(const void *value1, const void *value2, void *buffer)
{
	/* temp buffer holding unused result of divmod */
	char *unused_res = (char*) alloca(calc_buffer_size);

	CLEAR_BUFFER(calc_buffer);
	carry_flag = 0;

	do_divmod((const char*) value1, (const char*) value2, calc_buffer, unused_res);

	if ((buffer != NULL) && (buffer != calc_buffer)) {
		memcpy(buffer, calc_buffer, calc_buffer_size);
	}
}

void sc_mod(const void *value1, const void *value2, void *buffer)
{
	/* temp buffer holding unused result of divmod */
	char *unused_res = (char*) alloca(calc_buffer_size);

	CLEAR_BUFFER(calc_buffer);
	carry_flag = 0;

	do_divmod((const char*) value1, (const char*) value2, unused_res, calc_buffer);

	if ((buffer != NULL) && (buffer != calc_buffer)) {
		memcpy(buffer, calc_buffer, calc_buffer_size);
	}
}

void sc_divmod(const void *value1, const void *value2, void *div_buffer, void *mod_buffer)
{
	CLEAR_BUFFER(calc_buffer);
	carry_flag = 0;

	do_divmod((const char*) value1, (const char*) value2, (char*) div_buffer, (char*) mod_buffer);
}


void sc_shlI(const void *val1, long shift_cnt, int bitsize, int sign, void *buffer)
{
	carry_flag = 0;

	do_shl((const char*) val1, calc_buffer, shift_cnt, bitsize, sign);

	if ((buffer != NULL) && (buffer != calc_buffer)) {
		memmove(buffer, calc_buffer, calc_buffer_size);
	}
}

void sc_shl(const void *val1, const void *val2, int bitsize, int sign, void *buffer)
{
	long offset = sc_val_to_long(val2);

	sc_shlI(val1, offset, bitsize, sign, buffer);
}

void sc_shrI(const void *val1, long shift_cnt, int bitsize, int sign, void *buffer)
{
	carry_flag = 0;

	do_shr((const char*) val1, calc_buffer, shift_cnt, bitsize, sign, 0);

	if ((buffer != NULL) && (buffer != calc_buffer)) {
		memmove(buffer, calc_buffer, calc_buffer_size);
	}
}

void sc_shr(const void *val1, const void *val2, int bitsize, int sign, void *buffer)
{
	long shift_cnt = sc_val_to_long(val2);

	sc_shrI(val1, shift_cnt, bitsize, sign, buffer);
}

void sc_shrsI(const void *val1, long shift_cnt, int bitsize, int sign, void *buffer)
{
	carry_flag = 0;

	do_shr((const char*) val1, calc_buffer, shift_cnt, bitsize, sign, 1);

	if ((buffer != NULL) && (buffer != calc_buffer)) {
		memmove(buffer, calc_buffer, calc_buffer_size);
	}
}

void sc_shrs(const void *val1, const void *val2, int bitsize, int sign, void *buffer)
{
	long offset = sc_val_to_long(val2);

	carry_flag = 0;

	do_shr((const char*) val1, calc_buffer, offset, bitsize, sign, 1);

	if ((buffer != NULL) && (buffer != calc_buffer)) {
		memmove(buffer, calc_buffer, calc_buffer_size);
	}
}

void sc_zero(void *buffer)
{
	if (buffer == NULL)
		buffer = calc_buffer;
	CLEAR_BUFFER(buffer);
	carry_flag = 0;
}
