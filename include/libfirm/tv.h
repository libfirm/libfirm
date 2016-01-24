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
 * @brief    target machine values.
 */
#ifndef FIRM_TV_TV_H
#define FIRM_TV_TV_H

#include <stddef.h>
#include "firm_types.h"

#include "begin.h"

/** @defgroup ir_tarval  Target Machine Values
 *
 * Tarvals only represent values of mode_sort:
 *    - int_number,
 *    - float_number,
 *    - boolean,
 *    - reference,
 *    - character
 *
 *   In case of references the module accepts an entity to represent the
 *   value. Furthermore, computations and conversions of these values can
 *   be performed.
 *
 * @sa
 *    Techreport 1999-14
 *    irmode.h for the modes definitions
 *
 * @{
 */

/**
 * Constructor function for new tarvals.
 *
 * @param str   The string representing the target value
 * @param len   The length of the string
 * @param mode  The mode requested for the result tarval
 *
 * This function creates a new tarval representing the value represented
 * by a CString, aka char array. If a tarval representing this value already
 * exists, this tarval is returned instead of a new one. So tarvals are
 * directly comparable since their representation is unique.
 *
 * This function accepts the following strings:
 *
 * if mode is int_number:
 *  - [+-]?0[xX][0-9a-fA-F]+ (hexadecimal representation)
 *  - [+-]?0[0-7]*           (octal representation)
 *  - [+-]?0[bB][01]+        (binary representation)
 *  - [+-]?[1-9][0-9]*       (decimal representation)
 *
 * if mode is float_number:
 *  - [+-]?(decimal int) (. (decimal int))? ([eE][+-]?(decimal int))?
 *
 * if mode is boolean: true, True, TRUE ... False... 0, 1,
 *
 * if mode is reference: "null" and the same as for int_number
 *
 * Leading and/or trailing spaces are ignored
 *
 * @return
 *   A tarval of proper type representing the requested value is returned.
 *   Tarvals are unique, so for any value/mode pair at most one tarval will
 *   exist, which will be returned upon further requests with an identical
 *   value/mode pair.
 *
 * @note
 *   Behaviour is undefined if the number in @p str is malformed. Debug builds
 *   should fail assertions in these cases.
 *
 * @sa
 *   irmode.h for predefined modes
 *   new_tarval_from_long()
 *   new_tarval_from_double()
 */
FIRM_API ir_tarval *new_tarval_from_str(const char *str, size_t len,
                                        ir_mode *mode);

/**
 * Construct a new tarval from a given string.
 *
 * @param str      The string representing the target value
 * @param len      The length of the string
 * @param negative != 0 if number should be negative
 * @param base     number system base.
 *                 binary(2), octal(8), decimal(10) and hexadecimal(16) numbers
 *                 are supported.
 * @param mode     The mode requested for the result tarval
 *
 * @return
 *   A tarval with the given mode. If overflow settings are set to
 *   TV_OVERFLOW_BAD then a tarval_bad is returned if the number can't be
 *   represented in the given mode.
 *   Returns bad if the number couldn't successfully be parsed.
 */
FIRM_API ir_tarval *new_integer_tarval_from_str(const char *str, size_t len,
                                                int negative,
                                                unsigned char base,
                                                ir_mode *mode);

/**
 * Constructor function for new tarvals
 *
 * @param l     The long representing the value
 * @param mode  The mode requested for the result tarval must be a
 *              twos_complement mode.
 *
 * This function creates a new tarval representing the value represented
 * by a long integer. If a tarval representing this value already exists,
 * this tarval is returned instead of a new one. So tarvals are directly
 * comparable since their representation is unique.
 *
 * @return
 *   A tarval of proper type representing the requested value is returned.
 *   Tarvals are unique, so for any value/mode pair at most one tarval will
 *   exist, which will be returned upon further requests with an identical
 *   value/mode pair.
 *
 * @note
 *   If the long is not representable in the given mode an assertion is
 *   thrown in assert build.
 *
 * @sa
 *   irmode.h for predefined modes
 *   new_tarval_from_str()
 *   new_tarval_from_double()
 *
 */
FIRM_API ir_tarval *new_tarval_from_long(long l, ir_mode *mode);

/**
 * Construct a new tarval from a sequence of bytes. The bytes are interpreted
 * in a "little endian" fashion which means less significant bytes come first.
 *
 * @param buf  pointer to a buffer holding at least
 *             get_ir_mode_size_bytes(mode) bytes.
 * @param mode mode for the resulting tarval
 * @return A newly created (or cached) tarval representing the value.
 */
FIRM_API ir_tarval *new_tarval_from_bytes(unsigned char const *buf,
                                          ir_mode *mode);

/**
 * Construct a new floating point quiet NaN value.
 * @param mode       floating point mode for the resulting value
 * @param signaling  if != 0 produces a signaling NaN else a quiet one.
 * @param payload    if != NULL puts the integer tarval into the mantissa.
 */
FIRM_API ir_tarval *new_tarval_nan(ir_mode *mode, int signaling,
                                   ir_tarval const *payload);

/**
 * Write tarval to a sequence of bytes. The value is written in a
 * "little endian" fashion which means the less significant bytes come first.
 */
FIRM_API void tarval_to_bytes(unsigned char *buffer, ir_tarval const *tv);

/**
 * Returns value as long if possible.
 */
FIRM_API long get_tarval_long(ir_tarval const *tv);

/**
 * This validates if get_tarval_long() will return something sensible.
 * This is the case if the value is a two_complement (integer/reference) mode
 * and converting it to a mode equivalent to "long" would not result in
 * information loss. So ULONGMAX in an unsigned mode is fine, ULONG_MAX in a
 * signed mode not.
 *
 * @param tv    the tarval
 */
FIRM_API int tarval_is_long(ir_tarval const *tv);

/**
 * Constructor function for new tarvals.
 *
 * @param d     The (long) double representing the value
 * @param mode  The mode requested for the result tarval
 *
 * This function creates a new tarval representing the value represented
 * by a (long) double. If a tarval representing this value already exists,
 * this tarval is returned instead of a new one. So tarvals are directly
 * comparable since their representation is unique.
 * Only modes of sort float_number can be constructed this way.
 *
 * @return
 *   A tarval of proper type representing the requested value is returned.
 *   Tarvals are unique, so for any value/mode pair at most one tarval will
 *   exist, which will be returned upon further requests with an identical
 *   value/mode pair.
 *
 * @note
 *   If the (long) double is not representable in the given mode an assertion
 *   is thrown. This will happen for any mode not of sort float_number.
 *
 * @sa
 *   irmode.h for predefined values
 *   new_tarval_from_str()
 *   new_tarval_from_long()
 */
FIRM_API ir_tarval *new_tarval_from_double(double d, ir_mode *mode);

/**
 * same as new_tarval_from_double(), but with a long double argument
 */
FIRM_API ir_tarval *new_tarval_from_long_double(long double d, ir_mode *mode);

/**
 * This returns a double with the value represented value, or
 * gibberish, depending on the size of double and the size of the
 * stored value.
 * This will overflow silently, so use only if you know what
 * you are doing! (better check with tarval_is_long...)
 *
 * @param tv    the tarval
 */
FIRM_API double get_tarval_double(ir_tarval const *tv);

/**
 * same as get_tarval_double but returns a long double value
 */
FIRM_API long double get_tarval_long_double(ir_tarval const *tv);

/**
 * This validates if tarval_to_double() will return a satisfying
 * result. I.e. if tv is an float_number and between min, max
 * of double
 *
 * @param tv    the tarval
 */
FIRM_API int tarval_is_double(ir_tarval const *tv);

/**
 * Returns the mode of the tarval.
 *
 * @param tv    the tarval
 */
FIRM_API ir_mode *get_tarval_mode(ir_tarval const *tv);

/**
 * Returns 1 if tv is negative
 *
 * @param tv    the tarval
 */
FIRM_API int tarval_is_negative(ir_tarval const *tv);

/**
 * Returns 1 if tv is null
 *
 * @param tv    the tarval
 */
FIRM_API int tarval_is_null(ir_tarval const *tv);

/**
 * Returns 1 if tv is the "one"
 *
 * @param tv    the tarval
 */
FIRM_API int tarval_is_one(ir_tarval const *tv);

/**
 * returns non-zero if all bits in the tarval are set.
 * This means the value is -1 for signed modes with irma_twos_complement.
 */
FIRM_API int tarval_is_all_one(ir_tarval const *tv);

/**
 * Returns non-zero if the tarval is a constant (i.e. NOT
 * a reserved tarval like bad, undef, reachable etc.)
 */
FIRM_API int tarval_is_constant(ir_tarval const *tv);

/** The 'bad' tarval. Representing "no value", do not confuse this with
 * tarval_unknown. */
FIRM_API ir_tarval *const tarval_bad;
/** Returns the 'bad' tarval. */
FIRM_API ir_tarval *get_tarval_bad(void);

/** The 'unknown' tarval. Representing an unknown (but legal) value, do
 * not confuse this with tarval_bad. */
FIRM_API ir_tarval *const tarval_unknown;
/** Returns the 'unknown' tarval. */
FIRM_API ir_tarval *get_tarval_unknown(void);

/** The mode_b tarval 'false'. */
FIRM_API ir_tarval *tarval_b_false;
/** Returns the mode_b tarval 'false'. */
FIRM_API ir_tarval *get_tarval_b_false(void);

/** The mode_b tarval 'true'. */
FIRM_API ir_tarval *tarval_b_true;
/** Returns the mode_b tarval 'true'. */
FIRM_API ir_tarval *get_tarval_b_true(void);

/**
 * Sets whether values should wrap on overflow or return the bad value.
 */
FIRM_API void tarval_set_wrap_on_overflow(int wrap_on_overflow);

/**
 * Returns 0 if operations return bad on overflow, != 0 if they wrap around.
 */
FIRM_API int tarval_get_wrap_on_overflow(void);

/**
 * Compares two tarvals
 *
 * Compare a with b and return their relation.
 * This is either ir_rel_unordered, ir_rel_less, ir_rel_greater, ir_rel_equal
 * or ir_rel_false if a or b are symbolic pointers which can not be compared at
 * all.
 *
 * @param a   the first tarval to be compared
 * @param b   the second tarval to be compared
 */
FIRM_API ir_relation tarval_cmp(ir_tarval const *a, ir_tarval const *b);

/**
 * Converts a tarval to another mode.
 *
 * Convert tarval 'src' to mode 'mode', this will succeed if and only if mode
 * 'mode' is wider than the mode of src, as defined in the firm documentation
 * and as returned by the function mode_is_smaller defined in irmode.h.
 *
 * @param src    The tarval to convert
 * @param mode   Tho mode to convert to
 *
 * @return
 *   If a tarval of mode 'mode' with the result of the conversion of the 'src'
 *   tarvals value already exists, it will be returned, else a new tarval is
 *   constructed and returned
 *
 * @note
 *    Illegal conversions will trigger a panic
 *
 * @sa
 *    FIRM documentation for conversion rules
 *    mode_is_smaller defined in irmode.h
 */
FIRM_API ir_tarval *tarval_convert_to(ir_tarval const *src, ir_mode *mode);

/**
 * Converts a tarval to another mode by reinterpreting the contained
 * bits. This is only allowed if the source and destination mode have the
 * same number of bits.
 */
FIRM_API ir_tarval *tarval_bitcast(ir_tarval const *src, ir_mode *mode);

/*
 * These function implement basic computations representable as opcodes
 * in FIRM nodes.
 *
 * PARAMETERS
 *    tarval_neg:
 *    traval_abs:
 *      a - the tarval to operate on
 *
 *    all others:
 *      a - the first operand tarval
 *      b - the second operand tarval
 *
 * RESULT
 *    If necessary a new tarval is constructed for the resulting value,
 *   or the one already carrying the computation result is retrieved and
 *   returned as result.
 *
 * NOTES
 *   The order the arguments are given in is important, imagine postfix
 *   notation.
 *   Illegal operations will trigger an assertion.
 *   The sort member of the struct mode defines which operations are valid
 */

/**
 * Bitwise Negation of a tarval.
 *
 * @param a  the first tarval
 *
 * @return ~a or tarval_bad
 */
FIRM_API ir_tarval *tarval_not(ir_tarval const *a);

/**
 * Arithmetic Negation of a tarval.
 *
 * @param a  the first tarval
 *
 * @return -a or tarval_bad
 */
FIRM_API ir_tarval *tarval_neg(ir_tarval const *a);

/**
 * Addition of two tarvals.
 *
 * @param a  the first tarval
 * @param b  the second tarval
 *
 * @return a + b or tarval_bad
 */
FIRM_API ir_tarval *tarval_add(ir_tarval const *a, ir_tarval const *b);

/**
 * Subtraction from a tarval.
 *
 * @param a         the first tarval
 * @param b         the second tarval
 *
 * @return a - b or tarval_bad
 */
FIRM_API ir_tarval *tarval_sub(ir_tarval const *a, ir_tarval const *b);

/**
 * Multiplication of tarvals.
 *
 * @param a  the first tarval
 * @param b  the second tarval
 *
 * @return a * b or tarval_bad
 */
FIRM_API ir_tarval *tarval_mul(ir_tarval const *a, ir_tarval const *b);

/**
 * Integer division of two tarvals.
 *
 * @param a  the first tarval
 * @param b  the second tarval
 *
 * @return a / b or tarval_bad
 */
FIRM_API ir_tarval *tarval_div(ir_tarval const *a, ir_tarval const *b);

/**
 * Remainder of integer division.
 *
 * @param a  the first tarval
 * @param b  the second tarval
 *
 * @return a % b or tarval_bad
 */
FIRM_API ir_tarval *tarval_mod(ir_tarval const *a, ir_tarval const *b);

/**
 * Integer division AND remainder.
 *
 * @param a        the first tarval
 * @param b        the second tarval
 * @param mod_res  after return, contains the remainder result, a % b or tarval_bad
 *
 * @return a / b or tarval_bad
 */
FIRM_API ir_tarval *tarval_divmod(ir_tarval const *a, ir_tarval const *b,
                                  ir_tarval **mod_res);

/**
 * Absolute value of a tarval.
 *
 * @param a  the first tarval
 *
 * @return |a| or tarval_bad
 */
FIRM_API ir_tarval *tarval_abs(ir_tarval const *a);

/**
 * Bitwise and of two integer tarvals.
 *
 * @param a  the first tarval
 * @param b  the second tarval
 *
 * @return a & b or tarval_bad
 */
FIRM_API ir_tarval *tarval_and(ir_tarval const *a, ir_tarval const *b);

/**
 * Bitwise and not of two integer tarvals.
 *
 * @param a  the first tarval
 * @param b  the second tarval
 *
 * @return a & ~b or tarval_bad
 */
FIRM_API ir_tarval *tarval_andnot(ir_tarval const *a, ir_tarval const *b);

/**
 * Bitwise or of two integer tarvals.
 *
 * @param a  the first tarval
 * @param b  the second tarval
 *
 * @return a | b or tarval_bad
 */
FIRM_API ir_tarval *tarval_or(ir_tarval const *a, ir_tarval const *b);

/**
 * Bitwise or not of two integer tarvals.
 *
 * @param a  the first tarval
 * @param b  the second tarval
 *
 * @return a | ~b or tarval_bad
 */
FIRM_API ir_tarval *tarval_ornot(ir_tarval const *a, ir_tarval const *b);

/**
 * Bitwise exclusive or of two integer tarvals.
 *
 * @param a  the first tarval
 * @param b  the second tarval
 *
 * @return a ^ b or tarval_bad
 */
FIRM_API ir_tarval *tarval_eor(ir_tarval const *a, ir_tarval const *b);

/**
 * Logical Left shift.
 *
 * @param a  the first tarval
 * @param b  the second tarval
 *
 * @return a << b or tarval_bad
 */
FIRM_API ir_tarval *tarval_shl(ir_tarval const *a, ir_tarval const *b);

/**
 * logical left shift (variant with unsigned argument).
 * @see tarval_shl()
 */
FIRM_API ir_tarval *tarval_shl_unsigned(ir_tarval const *a, unsigned b);

/**
 * Unsigned (logical) right shift.
 *
 * @param a  the first tarval
 * @param b  the second tarval
 *
 * @return a >>u b or tarval_bad
 */
FIRM_API ir_tarval *tarval_shr(ir_tarval const *a, ir_tarval const *b);

/**
 * unsigned (logical) right shift (variant with unsigned argument).
 * @see tarval_shr()
 */
FIRM_API ir_tarval *tarval_shr_unsigned(ir_tarval const *a, unsigned b);

/**
 * Signed (arithmetic) right shift.
 *
 * @param a  the first tarval
 * @param b  the second tarval
 *
 * @return a >>s b or tarval_bad
 */
FIRM_API ir_tarval *tarval_shrs(ir_tarval const *a, ir_tarval const *b);

/**
 * signed (arithmetic) right shift (variant with unsigned argument).
 * @see tarval_shrs()
 */
FIRM_API ir_tarval *tarval_shrs_unsigned(ir_tarval const *a, unsigned b);

/**
 * Returns the bitpattern of the bytes_ofs byte.
 *
 * This function succeeds even if the mode of the tarval uses fewer bits
 * than requested, in that case the bitpattern is filled with zero bits.
 *
 * To query a 32bit value the following code can be used:
 *
 * val0 = tarval_sub_bits(tv, 0);  <- lowest bits
 * val1 = tarval_sub_bits(tv, 1);
 * val2 = tarval_sub_bits(tv, 2);
 * val3 = tarval_sub_bits(tv, 3);  <- highest bits
 *
 * Because this is the bit representation of the target machine, only the
 * following operations are legal on the result:
 *
 * - concatenation (endian dependence MUST be handled by the CALLER)
 * - bitwise logical operations to select/mask bits
 *
 * @param tv        the tarval
 * @param byte_ofs  the byte offset from lower to higher
 *
 * @note
 *   The result of this function is undefined if the mode is neither integer
 *   nor float.
 */
FIRM_API unsigned char get_tarval_sub_bits(ir_tarval const *tv, unsigned byte_ofs);

/**
 * Returns the number of set bits in a given (integer) tarval.
 *
 * @param tv    the tarval
 *
 * @return number of set bits or -1 on error
 */
FIRM_API int get_tarval_popcount(ir_tarval const *tv);

/**
 * Returns the number of the lowest set bit in a given (integer) tarval.
 *
 * @param tv    the tarval
 *
 * @return number of lowest set bit or -1 on error
 */
FIRM_API int get_tarval_lowest_bit(ir_tarval const *tv);

/**
 * Return the number of the highest set bit in a given (integer) tarval.
 *
 * @param tv    the tarval
 *
 * @return number of highest set bit or -1 on error
 */
FIRM_API int get_tarval_highest_bit(ir_tarval const *tv);

/**
 * Returns non-zero if the mantissa of a floating point tarval is zero
 * (i.e. 1.0Exxx)
 *
 * @param tv    the tarval
 */
FIRM_API int tarval_zero_mantissa(ir_tarval const *tv);

/**
 * Returns the exponent of a floating point IEEE-754
 * tarval.
 *
 * @param tv    the tarval
 */
FIRM_API int tarval_get_exponent(ir_tarval const *tv);

/**
 * Check if the tarval can be converted to the given mode without
 * precision loss.
 *
 * @param tv    the tarval
 * @param mode  the mode to convert to
 */
FIRM_API int tarval_ieee754_can_conv_lossless(ir_tarval const *tv, const ir_mode *mode);

/**
 * Returns non-zero if the result of the last IEEE-754 operation was exact.
 */
FIRM_API unsigned tarval_ieee754_get_exact(void);

/**
 * Check if @p tv is a floating point NaN.
 *
 * @param tv    the tarval
 */
FIRM_API int tarval_is_nan(ir_tarval const *tv);

/**
 * Check if @p tv is a floating point quiet NaN.
 *
 * @param tv    the tarval
 */
FIRM_API int tarval_is_quiet_nan(ir_tarval const *tv);

/**
 * Check if @p tv is a floating point signaling NaN.
 *
 * @param tv    the tarval
 */
FIRM_API int tarval_is_signaling_nan(ir_tarval const *tv);

/**
 * Check if the tarval represents a finite value, ie neither NaN nor inf.
 *
 * @param tv    the tarval
 */
FIRM_API int tarval_is_finite(ir_tarval const *tv);

/** @} */

#include "end.h"

#endif
