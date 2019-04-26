/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Data modes of operations.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Mathias Heil,
 *          Michael Beck
 */
#ifndef FIRM_IR_IRMODE_H
#define FIRM_IR_IRMODE_H

#include <stddef.h>
#include "firm_types.h"

#include "begin.h"

/**
 * @defgroup ir_mode Value Modes
 *  This module specifies the modes that type the firm nodes.  It defines
 *  a data structure that describes a mode and implements constructors and
 *  access routines to this data structure. Further it defines a set of
 *  predefined modes.
 *
 *  SEE ALSO:
 *    UKA tech report 1999-44 for more information about modes.
 * @{
 */

/**
 * These values represent the different arithmetic operations possible with a
 * mode.
 */
typedef enum ir_mode_arithmetic {
	irma_none = 1,            /**< For modes for which no representation is
	                               specified. */
	irma_twos_complement = 2, /**< Values of the mode are represented as two's
	                               complement. Only legal for modes of sort
	                               int_number and reference. */
	irma_ieee754 = 256,       /**< Values of the mode are represented according
	                               to ieee754 floating point standard.  Only
	                               legal for modes of sort float_number. */
	irma_x86_extended_float,  /**< x86 extended floating point values */
	irma_last = irma_x86_extended_float,
} ir_mode_arithmetic;

/**
 * Creates a new mode.
 *
 * @param name          the name of the mode to be created
 * @param bit_size      number of bits this mode allocate
 * @param sign          non-zero if this is a signed mode
 * @param modulo_shift  Is ignored for modes other than integer.
 *
 * This function constructs a new mode given by the parameters.
 * If the parameters match an already defined mode, this mode is returned
 * (including the default modes).
 * Arithmetic of int modes is irma_twos_complement.
 *
 * @return
 *   The new mode or NULL on error.
 */
FIRM_API ir_mode *new_int_mode(const char *name,
                               unsigned bit_size, int sign,
                               unsigned modulo_shift);

/**
 * Create a new reference mode.
 * Reference modes are always unsigned.
 * Arithmetic of reference modes is irma_twos_complement.
 */
FIRM_API ir_mode *new_reference_mode(const char *name,
                                     unsigned bit_size,
                                     unsigned modulo_shift);

/**
 * Create a new ieee754 float mode.
 *
 * float-modes are always signed and have no modulo shift.
 * @param name          the name of the mode to be created
 * @param arithmetic    arithmetic/representation of the mode
 * @param exponent_size size of exponent in bits
 * @param mantissa_size size of mantissa in bits (including explicit one in
 *                      irma_x86_extended_float)
 * @param int_conv_overflow Semantic on float to integer conversion overflow.
 */
FIRM_API ir_mode *new_float_mode(const char *name,
                                 ir_mode_arithmetic arithmetic,
                                 unsigned exponent_size,
                                 unsigned mantissa_size,
                                 float_int_conversion_overflow_style_t
                                     int_conv_overflow);

/**
 * Creates a new mode for data values which are not used to perform arithmetic.
 * Arithmetic will be set to irma_none.
 */
FIRM_API ir_mode *new_non_arithmetic_mode(const char *name, unsigned bit_size);

/** Returns the ident* of the mode */
FIRM_API ident *get_mode_ident(const ir_mode *mode);

/** Returns the null-terminated name of this mode. */
FIRM_API const char *get_mode_name(const ir_mode *mode);

/** Returns the size of values of the mode in bits. */
FIRM_API unsigned get_mode_size_bits(const ir_mode *mode);

/** Returns the size of values of the mode in bytes.
 *  If the size is not dividable by 8 returns -1. */
FIRM_API unsigned get_mode_size_bytes(const ir_mode *mode);

/** Returns the arithmetic of a mode */
FIRM_API ir_mode_arithmetic get_mode_arithmetic(const ir_mode *mode);

/** Returns the modulo shift attribute.
 *
 *  Attribute modulo shift specifies for modes of kind irms_int_number
 *  whether shift applies modulo to value of bits to shift.  Zero for
 *  modes that are not integer.
 */
FIRM_API unsigned int get_mode_modulo_shift(const ir_mode *mode);

/**
 * Returns the smallest representable value of a given mode.
 *
 * For modes of the sort float_number this is the most negative value
 * bigger than -infinite.
 */
FIRM_API ir_tarval *get_mode_min(const ir_mode *mode);

/**
 * Returns the biggest representable value of a given mode.
 *
 * For modes of the sort float_number this is the largest value lower
 * than infinite.
 */
FIRM_API ir_tarval *get_mode_max(const ir_mode *mode);

/**
 * Returns the value Zero represented in this mode.
 *
 * Zero is the additive neutral element and as such
 * is defined only for modes allowing addition, i.e.
 * op_pin_state_floats and ints, and references (NULL-Pointer)
 * else returns tarval_bad.
 */
FIRM_API ir_tarval *get_mode_null(const ir_mode *mode);

/**
 * Returns the value One, represented in this mode.
 *
 * One, being the multiplicative neutral element,
 * is defined only for modes allowing multiplication,
 * i.e. ints and floats.
 */
FIRM_API ir_tarval *get_mode_one(const ir_mode *mode);

/**
 * Returns the value where all bits are One, represented in this mode.
 *
 * All One is defined only for modes integer, reference and boolean modes
 * This represents the value -1 for signed modes with irma_twos_complement.
 */
FIRM_API ir_tarval *get_mode_all_one(const ir_mode *mode);

/**
 * Returns a positive infinite value of a mode.
 *
 * This is only valid for float_numbers, other modes will result in tarval_bad.
 * There are typically multiple possible representations of infinity, don't
 * compare with this value but use tarval_is_plus_inf() instead.
 */
FIRM_API ir_tarval *get_mode_infinite(const ir_mode *mode);

FIRM_API ir_mode *mode_M; /**< memory */

FIRM_API ir_mode *mode_F;   /**< ieee754 binary32 float (single precision) */
FIRM_API ir_mode *mode_D;   /**< ieee754 binary64 float (double precision) */
FIRM_API ir_mode *mode_Bs;  /**< int8 */
FIRM_API ir_mode *mode_Bu;  /**< uint8 */
FIRM_API ir_mode *mode_Hs;  /**< int16 */
FIRM_API ir_mode *mode_Hu;  /**< uint16 */
FIRM_API ir_mode *mode_Is;  /**< int32 */
FIRM_API ir_mode *mode_Iu;  /**< uint32 */
FIRM_API ir_mode *mode_Ls;  /**< int64 */
FIRM_API ir_mode *mode_Lu;  /**< uint64 */
FIRM_API ir_mode *mode_P;   /**< pointer */

/**
 * This mode represents (parts of) the processor status flag queried in
 * conditional jumps or predicated code.
 *
 * Do not confuse this with boolean variables found in some languages.
 * mode_b values are used as the inputs to conditional jumps or Mux nodes.
 * As is the case with most hardware flags registers you cannot simply
 * load/store them to memory or convert them to integer/float values with a
 * single operation. You have to use an if-like construct to produce integer
 * numbers based on a mode_b value.
 */
FIRM_API ir_mode *mode_b;

FIRM_API ir_mode *mode_X;  /**< execution */
FIRM_API ir_mode *mode_BB; /**< block */

FIRM_API ir_mode *mode_T;  /**< tuple (none) */
FIRM_API ir_mode *mode_ANY;/**< undefined mode */
FIRM_API ir_mode *mode_BAD;/**< bad mode */

/** Returns float mode. @copydoc mode_F */
FIRM_API ir_mode *get_modeF(void);
/** Returns double mode. @copydoc mode_D */
FIRM_API ir_mode *get_modeD(void);
/** Returns byte signed mode. @copydoc mode_Bs */
FIRM_API ir_mode *get_modeBs(void);
/** Returns byte unsigned mode. @copydoc mode_Bu */
FIRM_API ir_mode *get_modeBu(void);
/** Returns halfword signed mode. @copydoc mode_Hs */
FIRM_API ir_mode *get_modeHs(void);
/** Returns halfword unsigned mode. @copydoc mode_Hu */
FIRM_API ir_mode *get_modeHu(void);
/** Returns integer signed mode. @copydoc mode_Is */
FIRM_API ir_mode *get_modeIs(void);
/** Returns integer unsigned mode. @copydoc mode_Iu */
FIRM_API ir_mode *get_modeIu(void);
/** Returns long signed mode. @copydoc mode_Ls */
FIRM_API ir_mode *get_modeLs(void);
/** Returns long unsigned mode. @copydoc mode_Lu */
FIRM_API ir_mode *get_modeLu(void);
/** Returns pointer mode. @copydoc mode_P */
FIRM_API ir_mode *get_modeP(void);
/** Returns internal boolean mode. @copydoc mode_b */
FIRM_API ir_mode *get_modeb(void);
/** Returns control-flow mode. @copydoc mode_X */
FIRM_API ir_mode *get_modeX(void);
/** Returns Basic-Block mode. @copydoc mode_BB */
FIRM_API ir_mode *get_modeBB(void);
/** Returns memory mode. @copydoc mode_M */
FIRM_API ir_mode *get_modeM(void);
/** Returns tuple mode. @copydoc mode_T */
FIRM_API ir_mode *get_modeT(void);
/** Returns ANY mode. @copydoc mode_ANY */
FIRM_API ir_mode *get_modeANY(void);
/** Returns BAD mode. @copydoc mode_BAD */
FIRM_API ir_mode *get_modeBAD(void);

/** Sets the machine specific pointer mode. */
FIRM_API void set_modeP(ir_mode *p);

/** Returns 1 if @p mode is signed, 0 otherwise */
FIRM_API int mode_is_signed(const ir_mode *mode);

/** Returns 1 if @p mode is for floating point numbers, 0 otherwise */
FIRM_API int mode_is_float(const ir_mode *mode);

/** Returns 1 if @p mode is for integer numbers, 0 otherwise */
FIRM_API int mode_is_int(const ir_mode *mode);

/** Returns 1 if @p mode is for references/pointers, 0 otherwise */
FIRM_API int mode_is_reference(const ir_mode *mode);

/**
 * Returns 1 if @p mode is for numeric values, 0 otherwise.
 *
 * A numeric mode supports the Add, Sub, Mul, Div and Minus operations.
 */
FIRM_API int mode_is_num(const ir_mode *mode);

/**
 * Returns 1 if @p mode is for data values, 0 otherwise.
 *
 * A data value is made from a fixed size of bits, you can build a tarval for
 * such values.
 */
FIRM_API int mode_is_data(const ir_mode *mode);

/**
 * Returns true if a value of mode @p sm can be converted to mode @p lm without
 * loss.
 *
 * That is the interpretation of the numbers does not changes, so a signed
 * integer mode is never smaller than an unsigned integer mode since the
 * unsigned mode can't represent negative numbers in a way that they are
 * interpreted as negative numbers.
 *
 * @see values_in_mode()
 */
FIRM_API int smaller_mode(const ir_mode *sm, const ir_mode *lm);

/**
 * Returns true if no information is lost when converting a value of mode @p sm
 * into mode @p lm (and back to mode @p sm).
 *
 * So the interpretation of the values may change in the intermediate mode @p lm
 * (for example when converting negative signed integer numbers into unsigned
 * integers) but after a conversion back they are exactly the same value.
 *
 * @see smaller_mode()
 */
FIRM_API int values_in_mode(const ir_mode *sm, const ir_mode *lm);

/**
 * Returns a matching unsigned mode for a given integer signed mode.
 * Returns NULL if no matching mode exists.
 */
FIRM_API ir_mode *find_unsigned_mode(const ir_mode *mode);

/**
 * Returns a matching signed mode for a given integer unsigned mode.
 * Returns NULL if no matching mode exists.
 */
FIRM_API ir_mode *find_signed_mode(const ir_mode *mode);

/**
 * Returns an integer mode with 2*n bits for a given integer mode with n bits.
 * Returns NULL if no matching mode exists.
 */
FIRM_API ir_mode *find_double_bits_int_mode(const ir_mode *mode);

/**
 * Returns non-zero if the given mode has negative zeros, i.e. +0 and -0 exist.
 * Note that for comparisons +0 and -0 are considered equal, the sign only
 * shows in expressions like 1/x which results in +inf/-inf.
 */
FIRM_API int mode_has_signed_zero(const ir_mode *mode);

/**
 * Returns non-zero if the given mode might overflow on unary Minus.
 */
FIRM_API int mode_overflow_on_unary_Minus(const ir_mode *mode);

/**
 * Returns non-zero if the mode has a reversed wrap-around
 * logic, especially (a + x) - x == a.
 * This is normally true for integer modes, not for floating
 * point modes.
 */
FIRM_API int mode_wrap_around(const ir_mode *mode);

/**
 * Returns the integer equivalent mode for an reference mode. This is typically
 * used to add/subtract offsets from reference values.
 */
FIRM_API ir_mode *get_reference_offset_mode(const ir_mode *mode);

/**
 * Sets the (signed) integer equivalent mode for an reference mode.
 */
FIRM_API void set_reference_offset_mode(ir_mode *ref_mode, ir_mode *int_mode);

/**
 * Returns size of bits used for to encode the mantissa (for float modes).
 * This includes the leading one for modes with irma_x86_extended_float.
 */
FIRM_API unsigned get_mode_mantissa_size(const ir_mode *mode);

/**
 * Returns size of exponent in bits (for float modes)
 */
FIRM_API unsigned get_mode_exponent_size(const ir_mode *mode);

/**
 * Returns semantic on float to integer conversion overflow.
 */
FIRM_API float_int_conversion_overflow_style_t get_mode_float_int_overflow(
		const ir_mode *mode);

/**
 * Returns non-zero if the cast from mode src to mode dst is a
 * reinterpret cast (i.e. only the bit pattern is reinterpreted,
 * no conversion is done)
 */
FIRM_API int is_reinterpret_cast(const ir_mode *src, const ir_mode *dst);

/**
 * Returns the primitive type matching the given mode
 */
FIRM_API ir_type *get_type_for_mode(const ir_mode *mode);

/** Returns number of known modes. */
FIRM_API size_t ir_get_n_modes(void);

/** Returns known mode number @p num. */
FIRM_API ir_mode *ir_get_mode(size_t num);

/** @} */

#include "end.h"

#endif
