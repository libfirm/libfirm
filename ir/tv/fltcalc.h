/*
 * Project:     libFIRM
 * File name:   ir/tv/fltcalc.h
 * Purpose:
 * Author:
 * Modified by:
 * Created:     2003
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef _FLTCALC_H_
#define _FLTCALC_H_

#include "config.h"

#ifdef HAVE_LONG_DOUBLE
/* XXX Set this via autoconf */
#define HAVE_EXPLICIT_ONE
typedef long double LLDBL;
#else
typedef double LLDBL;
#endif

typedef enum {
  FC_add,   /**< addition */
  FC_sub,   /**< subtraction */
  FC_mul,   /**< multiplication */
  FC_div,   /**< divide */
  FC_neg,   /**< negate */
  FC_int,   /**< truncate to integer */
  FC_rnd   /**< round to integer */
} fc_op_t;

enum {
  FC_DEC,
  FC_HEX,
  FC_BIN,
  FC_PACKED
};

/* rounding modes */
typedef enum {
  FC_TONEAREST,
  FC_TOPOSITIVE,
  FC_TONEGATIVE,
  FC_TOZERO
} fc_rounding_mode_t;

#define FC_DEFAULT_PRECISION 64

#define FC_DECLARE1(code) char* fc_##code(const void *a, void *result)
#define FC_DECLARE2(code) char* fc_##code(const void *a, const void *b, void *result)

/*@{*/
/** internal buffer access
 * All functions that accept NULL as return buffer put their result into an
 * internal buffer.
 * @return fc_get_buffer() returns the pointer to the buffer, fc_get_buffer_length()
 * returns the size of this buffer
 */
const void *fc_get_buffer(void);
const int fc_get_buffer_length(void);
/*}@*/

char* fc_val_from_str(const char *str, unsigned int len, char exp_size, char mant_size, char *result);

/** get the representation of a floating point value
 * This function tries to builds a representation having the same value as the
 * float number passed.
 * If the wished precision is less than the precicion of LLDBL the value built
 * will be rounded. Therefore only an approximation of the passed float can be
 * expected in this case.
 *
 * @param l The floating point number to build a representation for
 * @param exp_size The number of bits of the new exponent
 * @param mant_size The number of bits of the new mantissa
 * @param result A buffer to hold the value built. If this is NULL, the internal
 *               accumulator buffer is used. Note that the buffer must be big
 *               enough to hold the value. Use fc_get_buffer_length() to find out
 *               the size needed
 * @return The result pointer passed to the function. If this was NULL this returns
 *               a pointer to the internal accumulator buffer
 */
char* fc_val_from_float(LLDBL l, char exp_size, char mant_size, char *result);

/** retrieve the float value of an internal value
 * This function casts the internal value to LLDBL and returns a LLDBL with
 * that value.
 * This implies that values of higher precision than LLDBL are subject to
 * rounding, so the returned value might not the same than the actually
 * represented value.
 *
 * @param val The representation of a float value
 * @return a float value approximating the represented value
 */
LLDBL fc_val_to_float(const void *val);

/** cast a value to another precision
 * This function changes the precision of a float representation.
 * If the new precision is less than the original precision the returned
 * value might not be the same as the original value.
 *
 * @param val The value to be casted
 * @param exp_size The number of bits of the new exponent
 * @param mant_size The number of bits of the new mantissa
 * @param result A buffer to hold the value built. If this is NULL, the internal
 *               accumulator buffer is used. Note that the buffer must be big
 *               enough to hold the value. Use fc_get_buffer_length() to find out
 *               the size needed
 * @return The result pointer passed to the function. If this was NULL this returns
 *               a pointer to the internal accumulator buffer
 */
char* fc_cast(const void *val, char exp_size, char mant_size, char *result);

/*@{*/
/** build a special float value
 * This function builds a representation for a special float value, as indicated by the
 * function's suffix.
 *
 * @param exponent_size The number of bits of exponent of the float type the value
 *               is created for
 * @param mantissa_size The number of bits of mantissa of the float type the value
 *               is created for
 * @param result A buffer to hold the value built. If this is NULL, the internal
 *               accumulator buffer is used. Note that the buffer must be big
 *               enough to hold the value. Use fc_get_buffer_length() to find out
 *               the size needed
 * @return The result pointer passed to the function. If this was NULL this returns
 *               a pointer to the internal accumulator buffer
 */
char* fc_get_min(unsigned int exponent_size, unsigned int mantissa_size, char* result);
char* fc_get_max(unsigned int exponent_size, unsigned int mantissa_size, char* result);
char* fc_get_snan(unsigned int exponent_size, unsigned int mantissa_size, char* result);
char* fc_get_qnan(unsigned int exponent_size, unsigned int mantissa_size, char* result);
char* fc_get_plusinf(unsigned int exponent_size, unsigned int mantissa_size, char* result);
char* fc_get_minusinf(unsigned int exponent_size, unsigned int mantissa_size, char* result);
/*}@*/

int fc_is_zero(const void *a);
int fc_is_negative(const void *a);
int fc_is_inf(const void *a);
int fc_is_nan(const void *a);
int fc_is_subnormal(const void *a);

FC_DECLARE2(add);
FC_DECLARE2(sub);
FC_DECLARE2(mul);
FC_DECLARE2(div);
FC_DECLARE1(neg);
FC_DECLARE1(int);
FC_DECLARE1(rnd);

char *fc_print(const void *a, char *buf, int buflen, unsigned base);

/** Compare two values
 * This function compares two values
 *
 * @param a Value No. 1
 * @param b Value No. 2
 * @result The returned value will be one of
 *          -1  if a < b
 *           0  if a == b
 *           1  if a > b
 *           2  if either value is NaN
 */
int fc_comp(const void *a, const void *b);

/** Set new rounding mode
 * This function sets the rounding mode to one of the following, returning
 * the previously set rounding mode.
 * FC_TONEAREST (default):
 *    Any unrepresentable value is rounded to the nearest representable
 *    value. If it lies in the middle the value with the least significant
 *    bit of zero is chosen.
 *    Values too big to represent will round to +-infinity.
 * FC_TONEGATIVE
 *    Any unrepresentable value is rounded towards negative infinity.
 *    Positive values too big to represent will round to the biggest
 *    representable value, negative values too small to represent will
 *    round to -infinity.
 * FC_TOPOSITIVE
 *    Any unrepresentable value is rounded towards positive infinity
 *    Negative values too small to represent will round to the biggest
 *    representable value, positive values too big to represent will
 *    round to +infinity.
 * FC_TOZERO
 *    Any unrepresentable value is rounded towards zero, effectively
 *    chopping off any bits beyond the mantissa size.
 *    Values too big to represent will round to the biggest/smallest
 *    representable value.
 *
 * These modes correspond to the modes required by the ieee standard.
 *
 * @param mode The new rounding mode. Any value other than the four
 *        defined values will have no effect.
 * @return The previous rounding mode.
 *
 * @see fc_get_rounding_mode()
 * @see IEEE754, IEEE854 Floating Point Standard
 */
fc_rounding_mode_t fc_set_rounding_mode(fc_rounding_mode_t mode);

/** Get the rounding mode
 * This function retrieves the currently used rounding mode
 *
 * @return The current rounding mode
 * @see fc_set_rounding_mode()
 */
fc_rounding_mode_t fc_get_rounding_mode(void);

/** Get bit representation of a value
 * This function allows to read a value in encoded form, bytewise.
 * The value will be packed corresponding to the way used by the ieee
 * encoding formats, i.e.
 *        One bit   sign
 *   exp_size bits  exponent + bias
 *  mant_size bits  mantissa, without leading 1
 *
 * As in ieee, an exponent of 0 indicates a denormalized number, which
 * implies a most significant bit of zero instead of one; an exponent
 * of all ones (2**exp_size - 1) encodes infinity if the mantissa is
 * all zeroes, else Not A Number.
 *
 * @param val A pointer to the value. If NULL is passed a copy of the
 *        most recent value passed to this function is used, saving the
 *        packing step. This behaviour may be changed in the future.
 * @param num_bit The maximum number of bits to return. Any bit beyond
 *        num_bit will be returned as zero.
 * @param byte_ofs The byte index to read, 0 is the least significant
 *        byte.
 * @return 8 bits of encoded data
 */
unsigned char fc_sub_bits(const void *val, unsigned num_bit, unsigned byte_ofs);

void init_fltcalc(int precision);
void finish_fltcalc (void);

#endif /* _FLTCALC_H_ */
