/*
 * Project:     libFIRM
 * File name:   ir/tv/tv.h
 * Purpose:     Representation of and static computations on target machine
 *              values.
 * Author:      Mathias Heil
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file tv.h
 *
 * Declarations for Target Values.
 */

#ifndef _TV_H_
#define _TV_H_

# include "irmode.h"
# include "entity.h"
# include "irnode.h"    /* for pnc_number enum */


/****h* libfirm/tv
 *
 * NAME
 *    tv -- TargetValue, short tarval.
 *   Internal representation for machine values.
 *
 * AUTHORS
 *    Matthias Heil
 *
 * DESCRIPTION
 *    Tarvals represent target machine values.  They are typed by modes.
 *   Tarvals only represent values of mode_sort:
 *     int_number,
 *     float_number,
 *     boolean,
 *     reference,
 *     character
 *
 *   In case of references the module accepts an entity to represent the
 *   value.
 *    Furthermore, computations and conversions of these values can
 *   be performed.
 *
 * USES
 *    This module is closely related to the irmode module, as the modes
 *   defined there are thoroughly used throughout the whole module.
 *    Also, the comparison functions rely on the definition of comparison
 *   values in the irnode module.
 *
 * HISTORY
 *    The original tv module originated in the fiasco compiler written ...
 *    This is the new version, described in the tech report 1999-14 by ...
 *
 * SEE ALSO
 *    Techreport 1999-14
 *    irmode.h for the modes definitions
 *    irnode.h for the pnc_numbers table
 *
 *    tarval_init1 and tarval_init2 for initialization of the
 *   module
 *
 ******/

#ifndef _TARVAL_TYPEDEF_
#define _TARVAL_TYPEDEF_
  typedef struct tarval tarval;
#endif

/* ************************ Constructors for tarvals ************************ */

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
 *  - 0(x|X)[0-9a-fA-F]+ (hexadecimal representation)
 *  - 0[0-7]*            (octal representation)
 *  - (+|-)?[1-9][0-9]*  (decimal representation)
 *
 * if mode if float_number:
 *  - (+|-)?(decimal int) (. (decimal int))? ((e|E)(+|-)?(decimal int))?
 *
 * if mode is boolean: true, True, TRUE ... False... 0, 1,
 *
 * if mode is reference: hexadecimal of decimal number as int
 *
 * if mode is character: hex or dec
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
 *   If the string is not representable in the given mode an assertion is
 *   thrown in assert build.
 *
 * @sa
 *   irmode.h for predefined modes
 *   new_tarval_from_long()
 *   new_tarval_from_double()
 */
tarval *new_tarval_from_str(const char *str, size_t len, ir_mode *mode);

/**
 * Constructor function for new tarvals
 *
 * @param l     The long representing the value
 * @param mode  The mode requested for the result tarval
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
tarval *new_tarval_from_long(long l, ir_mode *mode);

/**
 * This returns a long int with the value represented value, or
 * gibberish, depending on the size of long int and the size of the
 * stored value. It works for e.g. 1 as mode_Ls, but might not work for
 * get_mode_max(mode_Ls).
 * This will overflow silently, so use only if you know what
 * you are doing! (better check with tarval_is_long()...)
 */
long tarval_to_long(tarval *tv);

/**
 * This validates if tarval_to_long() will return a satisfying
 * result. I.e. if tv is an int_number and between min, max
 * of long int (signed!)
 */
int tarval_is_long(tarval *tv);

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
tarval *new_tarval_from_double(long double d, ir_mode *mode);

/**
 * This returns a double with the value represented value, or
 * gibberish, depending on the size of double and the size of the
 * stored value.
 * This will overflow silently, so use only if you know what
 * you are doing! (better check with tarval_is_long...)
 */
long double tarval_to_double(tarval *tv);

/**
 * This validates if tarval_to_double() will return a satisfying
 * result. I.e. if tv is an float_number and between min, max
 * of double
 */
int tarval_is_double(tarval *tv);

/**
 * Construct a tarval that represents the address of the entity.
 *
 * The address must be constant, the entity must have as owner the global type.
 */
tarval *new_tarval_from_entity (entity *ent, ir_mode *mode);

/**
 * Returns the associated entity of a tarval.
 */
entity *tarval_to_entity(tarval *tv);

/**
 * Returns non-zero if a the given tarval represents an entity.
 */
int tarval_is_entity(tarval *tv);

/** ********** Access routines for tarval fields ********** **/

/*
 * NAME
 *   get_tarval_mode
 *   get_tarval_ ...
 *
 * SYNOPSIS
 *   ir_mode *get_tarval_mode(tarval *tv)
 *   ...
 *
 * DESCRIPTION
 *    These are access function for tarval struct members. It is encouraged
 *   to use them instead of direct access to the struct fields.
 *
 * PARAMETERS
 *   tv - The tarval to access fields of
 *
 * RESULT
 *   get_tv_mode: The mode of the tarval
 *
 * SEE ALSO
 *   the struct tarval
 */

/** Returns the mode of the tarval. */
ir_mode *get_tarval_mode (tarval *tv);

/* Testing properties of the represented values */

/**
 * Returns 1 if tv is negative
 *
 * @param a	the tarval
 */
int tarval_is_negative(tarval *a);

/**
 * Returns 1 if tv is null
 *
 * @param a	the tarval
 */
int tarval_is_null(tarval *a);

/** The 'bad' tarval. */
extern tarval *tarval_bad;
/** Returns the 'bad tarval. */
tarval *get_tarval_bad(void);

/** The 'undefined' tarval. */
extern tarval *tarval_undefined;
/** Returns the 'undefined' tarval. */
tarval *get_tarval_undefined(void);

/** The mode_b tarval 'false'. */
extern tarval *tarval_b_false;
/** Returns the mode_b tarval 'false'. */
tarval *get_tarval_b_false(void);

/** The mode_b tarval 'true'. */
extern tarval *tarval_b_true;
/** Returns the mode_b tarval 'true'. */
tarval *get_tarval_b_true(void);

/** The 'void' pointer tarval. */
extern tarval *tarval_P_void;
/** Returns the 'void' pointer tarval. */
tarval *get_tarval_P_void(void);

/* These functions calculate and return a tarval representing the requested
 * value.
 * The functions get_mode_{Max,Min,...} return tarvals retrieved from these
 * functions, but these are stored on initialization of the irmode module and
 * therefore the irmode functions should be prefered to the functions below. */

/** Returns the maximum value of a given mode. */
tarval *get_tarval_max(ir_mode *mode);

/** Returns the minimum value of a given mode. */
tarval *get_tarval_min(ir_mode *mode);

/** Returns the 0 value (additive neutral) of a given mode. */
tarval *get_tarval_null(ir_mode *mode);

/** Returns the 1 value (multiplicative neutral) of a given mode. */
tarval *get_tarval_one(ir_mode *mode);

/** Return quite nan for float_number modes. */
tarval *get_tarval_nan(ir_mode *mode);

/** Return +inf for float_number modes. */
tarval *get_tarval_inf(ir_mode *mode);

/* ******************** Arithmethic operations on tarvals ******************** */

/**
 * Compares two tarvals
 *
 * Compare a with b and return a pnc_number describing the relation
 * between a and b.  This is either Uo, Lt, Eq, Gt, or False if a or b
 * are symbolic pointers which can not be compared at all.
 *
 * @param a   A tarval to be compared
 * @param b   A tarval to be compared
 *
 * @return
 *   The pnc_number best describing the relation between a and b is returned.
 *   This means the mode with the least bits set is returned, e.g. if the
 *   tarvals are equal the pnc_number 'Eq' is returned, not 'Ge' which
 *   indicates 'greater or equal'
 *
 * @sa
 *    irnode.h for the definition of pnc_numbers
 */
pnc_number tarval_cmp(tarval *a, tarval *b);

/**
 * Converts a tarval to another mode.
 *
 * Convert tarval 'src' to mode 'mode', this will suceed if and only if mode
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
 *    Illegal conversations will trigger an assertion
 *
 * @sa
 *    FIRM documentation for conversion rules
 *    mode_is_smaller defined in irmode.h
 */
tarval *tarval_convert_to(tarval *src, ir_mode *m);

/*
 * These function implement basic computations representable as opcodes
 * in FIRM nodes.
 *
 * PARAMETERS
 *    tarval_neg:
 *    traval_abs:
 *      a - the tarval to operate on
 *
 *    all oters:
 *      a - the first operand tarval
 *      b - the second operand tarval
 *
 * RESULT
 *    If neccessary a new tarval is constructed for the resulting value,
 *   or the one already carrying the computation result is retrieved and
 *   returned as result.
 *
 * NOTES
 *   The order the arguments are given in is important, imagine postfix
 *   notation.
 *   Illegal operations will trigger an assertion.
 *   The sort member of the struct mode defines which operations are valid
 */

/** Negation of a tarval. */
tarval *tarval_neg(tarval *a);

/** Addition of two tarvals. */
tarval *tarval_add(tarval *a, tarval *b);

/** Subtraction from a tarval. */
tarval *tarval_sub(tarval *a, tarval *b);

/** Multiplication of tarvals. */
tarval *tarval_mul(tarval *a, tarval *b);

/** 'Exact' division. */
tarval *tarval_quo(tarval *a, tarval *b);

/** Integer division. */
tarval *tarval_div(tarval *a, tarval *b);

/** Remainder of integer division. */
tarval *tarval_mod(tarval *a, tarval *b);

/** Absolute value. */
tarval *tarval_abs(tarval *a);

/** Bitwise and. */
tarval *tarval_and(tarval *a, tarval *b);

/** Bitwise or. */
tarval *tarval_or(tarval *a, tarval *b);

/** Bitwise exclusive or. */
tarval *tarval_eor(tarval *a, tarval *b);

/** Left shift. */
tarval *tarval_shl(tarval *a, tarval *b);

/** Unsigned (logical) right shift. */
tarval *tarval_shr(tarval *a, tarval *b);

/** Signed (arithmetic) right shift. */
tarval *tarval_shrs(tarval *a, tarval *b);

/** Rotation. */
tarval *tarval_rot(tarval *a, tarval *b);

/* *********** Output of tarvals *********** */

/**
 * The output mode for tarval values.
 *
 * Some modes allow more that one representation, for instance integers
 * can be represented hex or decimal. Of course it would be enough to have
 * one and let every backend convert it into the 'right' one.
 * However, we can do this in the tarval much simplier...
 */
typedef enum {
  TVO_NATIVE,			/**< the default output mode, depends on the mode */
  TVO_HEX,			/**< use hex representation, always possible */
  TVO_DECIMAL,			/**< use decimal representation */
  TVO_OCTAL,			/**< use octal representation */
  TVO_BINARY,			/**< use binary representation */
  TVO_FLOAT,			/**< use floating point representation (i.e 1.342e-2)*/
  TVO_HEXFLOAT                  /**< use hexadecimal floating point representation (i.e 0x1.ea32p-12)*/
} tv_output_mode;

/**
 * This structure contains helper information to format the output
 * of a tarval of an mode.
 */
typedef struct tarval_mode_info {
    tv_output_mode mode_output;	        /**< if != TVO_NATIVE select a special mode */
    const char *mode_prefix;		/**< if set, this prefix will be printed
					     before a value of this mode */
    const char *mode_suffix;		/**< if set, this suffix will be printed
					     after a value of this mode */
} tarval_mode_info;

/**
 * Specify the output options of one mode.
 *
 * This functions stores the modinfo, so DO NOT DESTROY it.
 *
 * @param mode		a ir_mode that should be associated
 * @param modeinfo	the output format info
 *
 * @return zero on success.
 */
int tarval_set_mode_output_option(ir_mode *mode, const tarval_mode_info *modeinfo);

/**
 * Returns the output options of one mode.
 *
 * This functions returns the modinfo of a given mode.
 *
 * @param mode		a ir_mode that should be associated
 *
 * @return the output option
 */
const tarval_mode_info *tarval_get_mode_output_option(ir_mode *mode);

/**
 * Returns Bit representation of a tarval value, as string of '0' and '1'
 *
 * @param tv   The tarval
 *
 * This function returns a printable bit representation of any value
 * stored as tarval. This representation is a null terminated C string.
 *
 * @return
 *   As usual in C a pointer to a char is returned. The length of the
 *   returned string if fixed, just read as many chars as the mode defines
 *   as size.
 *
 * @note
 *   The string is allocated using malloc() and is free()ed on the next call
 *   of this function.
 *   The string consists of the ascii characters '0' and '1' and is
 *   null terminated
 *
 * @sa
 *    irmode.h for the definition of the ir_mode struct
 *    the size member of aforementioned struct
 */
char *tarval_bitpattern(tarval *tv);

/**
 * Returns the bitpattern of the bytes_ofs byte.
 *
 * This function succeeds even if the mode of the tarval uses lesser bits
 * than requested, in that case the bitpattern is filled with zero bits.
 *
 * To query a 32bit value the following code can be used:
 *
 * val0 = tarval_sub_bits(tv, 0);
 * val1 = tarval_sub_bits(tv, 1);
 * val2 = tarval_sub_bits(tv, 2);
 * val3 = tarval_sub_bits(tv, 3);
 *
 * Because this is the bit representation of the target machine, only the following
 * operations are legal on the result:
 *
 * - concatenation (endian dependance MUST be handled by the CALLER)
 * - bitwise logical operations to select/mask bits
 *
 * @param tv		the tarval
 * @param byte_ofs	the byte offset
 *
 * @note
 *   The result of this funcion is undefined if the mode is neither integer nor float.
 */
unsigned char tarval_sub_bits(tarval *tv, unsigned byte_ofs);

/**
 * Identifying some tarvals ???
 *
 * @return
 *   - 0 for additive neutral,
 *   - +1 for multiplicative neutral,
 *   - -1 for bitwise-and neutral
 *   - 2 else
 *
 * @deprecated
 *   This function is deprecated and its use strongly discouraged.
 *   Implemented for completeness.
 */
long tarval_classify(tarval *tv);

/**
 * Initialization of the tarval module.
 *
 * Call before init_mode().
 */
void init_tarval_1(void);

/**
 * Initialization of the tarval module.
 *
 * Call after init_mode().
 */
void init_tarval_2(void);

/**
 * Output of tarvals to a buffer.
 */
int tarval_snprintf(char *buf, size_t buflen, tarval *tv);

/**
 * Output of tarvals to stdio.
 */
int tarval_printf(tarval *tv);

#endif  /* _TV_H_ */
