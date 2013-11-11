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

#include "firm_types.h"
#include "begin.h"

#include <stddef.h>

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
	                               specified. These are modes of sort auxiliary,
	                               internal_boolean and character. */
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
 * @param arithmetic    arithmetic operations possible with a mode
 * @param bit_size      number of bits this mode allocate
 * @param sign          non-zero if this is a signed mode
 * @param modulo_shift  Is ignored for modes other than integer.
 *
 * This function constructs a new mode given by the parameters.
 * If the parameters match an already defined mode, this mode is returned
 * (including the default modes).
 *
 * @return
 *   The new mode or NULL on error.
 */
FIRM_API ir_mode *new_int_mode(const char *name,
                               ir_mode_arithmetic arithmetic,
                               unsigned bit_size, int sign,
                               unsigned modulo_shift);

/**
 * Create a new reference mode.
 *
 * Reference modes are always unsigned.
 */
FIRM_API ir_mode *new_reference_mode(const char *name,
                                     ir_mode_arithmetic arithmetic,
                                     unsigned bit_size,
                                     unsigned modulo_shift);

/**
 * Create a new ieee754 float mode.
 *
 * float-modes are always signed and have no modulo shift.
 * @param name          the name of the mode to be created
 * @param arithmetic    arithmetic/representation of the mode
 * @param exponent_size size of exponent in bits
 * @param mantissa_size size of mantissa in bits (number of bits after the
 *                      leading one).
 */
FIRM_API ir_mode *new_float_mode(const char *name,
                                 ir_mode_arithmetic arithmetic,
                                 unsigned exponent_size,
                                 unsigned mantissa_size);

/**
 * Checks whether a pointer points to a mode.
 *
 * @param thing     an arbitrary pointer
 *
 * @return
 *     true if the thing is a mode, else false
 */
FIRM_API int is_mode(const void *thing);

/** Returns the ident* of the mode */
FIRM_API ident *get_mode_ident(const ir_mode *mode);

/** Returns the null-terminated name of this mode. */
FIRM_API const char *get_mode_name(const ir_mode *mode);

/** Returns the size of values of the mode in bits. */
FIRM_API unsigned get_mode_size_bits(const ir_mode *mode);

/** Returns the size of values of the mode in bytes.
 *  If the size is not dividable by 8 returns -1. */
FIRM_API unsigned get_mode_size_bytes(const ir_mode *mode);

/** Returns the signedness of a mode.
 *
 * Returns the signedness of a mode: 1 if mode is signed. */
FIRM_API int get_mode_sign(const ir_mode *mode);

/** Returns the arithmetic of a mode */
FIRM_API ir_mode_arithmetic get_mode_arithmetic(const ir_mode *mode);

/** Returns the modulo shift attribute.
 *
 *  Attribute modulo shift specifies for modes of kind irms_int_number
 *  whether shift applies modulo to value of bits to shift.  Zero for
 *  modes that are not integer.
 */
FIRM_API unsigned int get_mode_modulo_shift(const ir_mode *mode);

/** Returns the stored intermediate information. */
FIRM_API void *get_mode_link(const ir_mode *mode);

/** Stores new intermediate information. */
FIRM_API void set_mode_link(ir_mode *mode, void *l);

/**
 * Returns the smallest representable value of a given mode.
 *
 * For modes of the sort float_number this is the most negative value
 * bigger than -infinite.
 */
FIRM_API ir_tarval *get_mode_min(ir_mode *mode);

/**
 * Returns the biggest representable value of a given mode.
 *
 * For modes of the sort float_number this is the largest value lower
 * than infinite.
 */
FIRM_API ir_tarval *get_mode_max(ir_mode *mode);

/**
 * Returns the value Zero represented in this mode.
 *
 * Zero is the additive neutral element and as such
 * is defined only for modes allowing addition, i.e.
 * op_pin_state_floats and ints, and references (NULL-Pointer)
 * else returns tarval_bad.
 */
FIRM_API ir_tarval *get_mode_null(ir_mode *mode);

/**
 * Returns the value One, represented in this mode.
 *
 * One, being the multiplicative neutral element,
 * is defined only for modes allowing multiplication,
 * i.e. ints and floats.
 */
FIRM_API ir_tarval *get_mode_one(ir_mode *mode);

/**
 * Returns the value Minus One, represented in this mode.
 *
 * Minus One is defined only for modes allowing
 * multiplication with signed values, i.e. signed ints and floats.
 */
FIRM_API ir_tarval *get_mode_minus_one(ir_mode *mode);

/**
 * Returns the value where all bits are One, represented in this mode.
 *
 * All One is defined only for modes integer, reference and boolean modes
 */
FIRM_API ir_tarval *get_mode_all_one(ir_mode *mode);

/**
 * Returns the positive infinite value of a mode.
 *
 * This is only valid for float_numbers, other modes
 * will result in tarval_bad.
 */
FIRM_API ir_tarval *get_mode_infinite(ir_mode *mode);

/**
 * Returns the NAN value of a given mode.
 *
 * This is only valid for float_numbers, other modes
 * will result in tarval_bad.
 */
FIRM_API ir_tarval *get_mode_NAN(ir_mode *mode);

FIRM_API ir_mode *mode_M; /**< memory */

FIRM_API ir_mode *mode_F;   /**< ieee754 binary32 float (single precision) */
FIRM_API ir_mode *mode_D;   /**< ieee754 binary64 float (double precision) */
FIRM_API ir_mode *mode_Q;   /**< ieee754 binary128 float (quadruple precision)*/
FIRM_API ir_mode *mode_Bs;  /**< int8 */
FIRM_API ir_mode *mode_Bu;  /**< uint8 */
FIRM_API ir_mode *mode_Hs;  /**< int16 */
FIRM_API ir_mode *mode_Hu;  /**< uint16 */
FIRM_API ir_mode *mode_Is;  /**< int32 */
FIRM_API ir_mode *mode_Iu;  /**< uint32 */
FIRM_API ir_mode *mode_Ls;  /**< int64 */
FIRM_API ir_mode *mode_Lu;  /**< uint64 */
FIRM_API ir_mode *mode_LLs; /**< int128 */
FIRM_API ir_mode *mode_LLu; /**< uint128 */

FIRM_API ir_mode *mode_P;   /**< pointer */
FIRM_API ir_mode *mode_P_code; /**< A pointer mode that is set by the client of libfirm.  This mode
                                  represents the pointer size of the target machine code addresses. Is initialized
                                  to mode_P. */
FIRM_API ir_mode *mode_P_data; /**< A pointer mode that is set by the client of libfirm.  This mode
                                  represents the pointer size of the target machine data addresses. Is initialized
                                  to mode_P. */

/**
 * This mode represents (parts of) the processor status flag queried in
 * conditional jumps or predicated code.
 *
 * Do not confuse this with boolean variables found in some languages. You
 * cannot perform any operations like And, Or, Not, Phi, etc. on mode_b
 * values (although some of these constructs can be legalized by lower_mode_b().
 */
FIRM_API ir_mode *mode_b;

FIRM_API ir_mode *mode_X;  /**< execution */
FIRM_API ir_mode *mode_BB; /**< block */

FIRM_API ir_mode *mode_T;  /**< tuple (none) */
FIRM_API ir_mode *mode_ANY;/**< undefined mode */
FIRM_API ir_mode *mode_BAD;/**< bad mode */

/** Returns float mode */
FIRM_API ir_mode *get_modeF(void);
/** Returns double mode */
FIRM_API ir_mode *get_modeD(void);
/** Returns quadruple prevision mode */
FIRM_API ir_mode *get_modeQ(void);
/** Returns byte signed mode */
FIRM_API ir_mode *get_modeBs(void);
/** Returns byte unsigned mode */
FIRM_API ir_mode *get_modeBu(void);
/** Returns halfword signed mode */
FIRM_API ir_mode *get_modeHs(void);
/** Returns halfword unsigned mode */
FIRM_API ir_mode *get_modeHu(void);
/** Returns integer signed mode */
FIRM_API ir_mode *get_modeIs(void);
/** Returns integer unsigned mode */
FIRM_API ir_mode *get_modeIu(void);
/** Returns long signed mode */
FIRM_API ir_mode *get_modeLs(void);
/** Returns long unsigned mode */
FIRM_API ir_mode *get_modeLu(void);
/** Returns long long signed mode */
FIRM_API ir_mode *get_modeLLs(void);
/** Returns long long unsigned mode */
FIRM_API ir_mode *get_modeLLu(void);
/** Returns pointer mode */
FIRM_API ir_mode *get_modeP(void);
/** Returns internal boolean mode */
FIRM_API ir_mode *get_modeb(void);
/** Returns control-flow mode */
FIRM_API ir_mode *get_modeX(void);
/** Returns Basic-Block mode */
FIRM_API ir_mode *get_modeBB(void);
/** Returns memory mode */
FIRM_API ir_mode *get_modeM(void);
/** Returns tuple mode */
FIRM_API ir_mode *get_modeT(void);
/** Returns ANY mode */
FIRM_API ir_mode *get_modeANY(void);
/** Returns BAD mode */
FIRM_API ir_mode *get_modeBAD(void);

/** Returns the machine specific pointer mode for code addresses. */
FIRM_API ir_mode *get_modeP_code(void);

/** Returns the machine specific pointer mode for data addresses. */
FIRM_API ir_mode *get_modeP_data(void);

/**
 * Sets the machine specific pointer mode for code addresses.
 * If not set, the predefined mode mode_P will be used.
 */
FIRM_API void set_modeP_code(ir_mode *p);

/**
 * Sets the machine specific pointer mode for data addresses.
 * If not set, the predefined mode mode_P will be used.
 */
FIRM_API void set_modeP_data(ir_mode *p);

/** Returns 1 if @p mode is signed, 0 otherwise */
FIRM_API int mode_is_signed (const ir_mode *mode);
/** Returns 1 if @p mode is for floating point numbers, 0 otherwise */
FIRM_API int mode_is_float (const ir_mode *mode);
/** Returns 1 if @p mode is for integer numbers, 0 otherwise */
FIRM_API int mode_is_int (const ir_mode *mode);
/** Returns 1 if @p mode is for references/pointers, 0 otherwise */
FIRM_API int mode_is_reference (const ir_mode *mode);
/** Returns 1 if @p mode is for numeric values, 0 otherwise */
FIRM_API int mode_is_num (const ir_mode *mode);
/** Returns 1 if @p mode is for data values, 0 otherwise */
FIRM_API int mode_is_data (const ir_mode *mode);

/**
 * Returns true if a value of mode @p sm can be converted to mode @p lm without
 * loss.
 *
 * That is the interpretation of the numbers does not changes, so you a signed
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
 * So the interpretation of the values may change in the intermediate mode @p sm
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
 * Returns the signed integer equivalent mode for an reference mode.
 */
FIRM_API ir_mode *get_reference_mode_signed_eq(ir_mode *mode);

/**
 * Sets the signed integer equivalent mode for an reference mode.
 */
FIRM_API void set_reference_mode_signed_eq(ir_mode *ref_mode, ir_mode *int_mode);

/**
 * Returns the unsigned integer equivalent mode for an reference mode.
 */
FIRM_API ir_mode *get_reference_mode_unsigned_eq(ir_mode *mode);

/**
 * Sets the unsigned integer equivalent mode for an reference mode.
 */
FIRM_API void set_reference_mode_unsigned_eq(ir_mode *ref_mode, ir_mode *int_mode);

/**
 * Returns size of mantissa in bits (for float modes).
 * Note: This is the number of bits used after the leading one. So the actual
 * accuracy of the significand is get_mode_mantissa_size()+1. The number of bits
 * used in the encoding depends on whether the floating point mode has an implicit
 * (ieee754) or explicit (x86_extended) encoding of the leading one.
 */
FIRM_API unsigned get_mode_mantissa_size(const ir_mode *mode);

/**
 * Returns size of exponent in bits (for float modes)
 */
FIRM_API unsigned get_mode_exponent_size(const ir_mode *mode);

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
