/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    tarval floating point calculations
 * @date     2003
 * @author   Mathias Heil
 */
#ifndef FIRM_TV_FLTCALC_H
#define FIRM_TV_FLTCALC_H

#include <stdlib.h>
#include "irmode_t.h"
#include "strcalc.h"

typedef enum {
	FC_DEC,
	FC_HEX,
} fc_base_t;

/** IEEE-754 Rounding modes. */
typedef enum {
	FC_TONEAREST,   /**< if unsure, to the nearest even */
	FC_TOPOSITIVE,  /**< to +oo */
	FC_TONEGATIVE,  /**< to -oo */
	FC_TOZERO       /**< to 0 */
} fc_rounding_mode_t;

/**
 * possible float states
 */
typedef enum {
	FC_NORMAL,       /**< normal representation, implicit 1 */
	FC_ZERO,         /**< +/-0 */
	FC_SUBNORMAL,    /**< denormals, implicit 0 */
	FC_INF,          /**< +/-oo */
	FC_NAN,          /**< Not A Number */
} value_class_t;

struct fp_value;
typedef struct fp_value fp_value;

/** Returns the size in bytes of an fp_value */
unsigned fc_get_value_size(void);

void fc_val_from_str(const char *str, size_t len, fp_value *result);

/** get the representation of a floating point value
 * This function tries to builds a representation having the same value as the
 * long double floating point number passed.
 *
 * @param l       The floating point number to build a representation for
 * @param result  A buffer to hold the value built.
 */
void fc_val_from_ieee754(long double l, fp_value *result);

/**
 * get fp_value from a floatingpoint an ieee754 float number encoded in little
 * endian format in @p buffer.
 */
void fc_val_from_bytes(fp_value *result, const unsigned char *buffer,
                       const float_descriptor_t *desc);

/** retrieve the float value of an internal value
 * This function casts the internal value to long double and returns a
 * long double with that value.
 * This implies that values of higher precision than long double are subject to
 * rounding, so the returned value might not the same than the actually
 * represented value.
 *
 * @param val  The representation of a float value
 *
 * @return a float value approximating the represented value
 */
long double fc_val_to_ieee754(const fp_value *val);

/** cast a value to another precision
 * This function changes the precision of a float representation.
 * If the new precision is less than the original precision the returned
 * value might not be the same as the original value.
 *
 * @param val     The value to be casted
 * @param desc    The floating point descriptor
 * @param result  A buffer to hold the value built.
 */
void fc_cast(const fp_value *val, const float_descriptor_t *desc,
             fp_value *result);

/*@{*/
/** build a special float value
 * This function builds a representation for a special float value, as indicated by the
 * function's suffix.
 *
 * @param desc    The floating point descriptor
 * @param result  A buffer to hold the value built.
 */
void fc_get_max(const float_descriptor_t *desc, fp_value *result, bool sign);
void fc_get_nan(const float_descriptor_t *desc, fp_value *result,
                bool signaling, const sc_word *payload);
void fc_get_inf(const float_descriptor_t *desc, fp_value *result, bool sign);
void fc_get_small(const float_descriptor_t *desc, fp_value *result);
void fc_get_epsilon(const float_descriptor_t *desc, fp_value *result);
void fc_get_zero(const float_descriptor_t *desc, fp_value *result, bool sign);
/*@}*/

bool fc_is_zero(const fp_value *a);
bool fc_is_negative(const fp_value *a);
bool fc_is_inf(const fp_value *a);
bool fc_is_nan(const fp_value *a);
bool fc_nan_is_quiet(const fp_value *a);
bool fc_is_subnormal(const fp_value *a);

void fc_add(const fp_value *a, const fp_value *b, fp_value *result);
void fc_sub(const fp_value *a, const fp_value *b, fp_value *result);
void fc_mul(const fp_value *a, const fp_value *b, fp_value *result);
void fc_div(const fp_value *a, const fp_value *b, fp_value *result);
void fc_neg(const fp_value *a, fp_value *result);
void fc_int(const fp_value *a, fp_value *result);

int fc_print(const fp_value *a, char *buf, size_t buflen, fc_base_t base);

/** Compare two values
 * This function compares two values
 *
 * @param a Value No. 1
 * @param b Value No. 2
 * @result The relation between a and b; either less, equal, greater or
 *         unordered.
 */
ir_relation fc_comp(const fp_value *a, const fp_value *b);

typedef enum flt2int_result_t {
	FLT2INT_OK,
	FLT2INT_POSITIVE_OVERFLOW,
	FLT2INT_NEGATIVE_OVERFLOW,
	FLT2INT_BAD
} flt2int_result_t;

/**
 * Converts an floating point value into an integer value.
 */
flt2int_result_t fc_flt2int(const fp_value *a, sc_word *result,
                            unsigned result_bits, bool result_signed);

/**
 * Returns non-zero if the mantissa is zero, i.e. 1.0Exxx
 */
bool fc_zero_mantissa(const fp_value *value);

/** Returns the exponent of a value. */
int fc_get_exponent(const fp_value *value);

/**
 * Return non-zero if a given value can be converted lossless into another
 * precision.
 */
bool fc_can_lossless_conv_to(const fp_value *value,
                             const float_descriptor_t *desc);

/** Set new rounding mode
 * This function sets the rounding mode to one of the following, returning
 * the previously set rounding mode.
 * FC_TONEAREST (default):
 *    Any unrepresentable value is rounded to the nearest representable
 *    value. If it lies in the middle the value with the least significant
 *    bit of zero is chosen (the even one).
 *    Values too big to represent will round to +/-infinity.
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
 * These modes correspond to the modes required by the IEEE-754 standard.
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
 * This function allows to read a value in encoded form, byte wise.
 * The value will be packed corresponding to the way used by the IEEE
 * encoding formats, i.e.
 *        One bit   sign
 *   exp_size bits  exponent + bias
 *  mant_size bits  mantissa, without leading 1
 *
 * As in IEEE, an exponent of 0 indicates a denormalized number, which
 * implies a most significant bit of zero instead of one; an exponent
 * of all ones (2**exp_size - 1) encodes infinity if the mantissa is
 * all zeros, else Not A Number.
 *
 * @param val A pointer to the value. If NULL is passed a copy of the
 *        most recent value passed to this function is used, saving the
 *        packing step. This behavior may be changed in the future.
 * @param num_bit The maximum number of bits to return. Any bit beyond
 *        num_bit will be returned as zero.
 * @param byte_ofs The byte index to read, 0 is the least significant
 *        byte.
 * @return 8 bits of encoded data
 */
unsigned char fc_sub_bits(const fp_value *val, unsigned num_bit,
                          unsigned byte_ofs);

/**
 * Write (little endian) byte representation of the floatingpoint number to
 * buffer buf.
 */
void fc_val_to_bytes(const fp_value *val, unsigned char *buf);

/**
 * Returns non-zero if the result of the last operation was exact.
 */
bool fc_is_exact(void);

void init_fltcalc(unsigned precision);

#endif
