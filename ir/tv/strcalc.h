/*
 * Project:     libFIRM
 * File name:   ir/tv/strcalc.h
 * Purpose:     Provides basic mathematical operations on values represented as strings.
 * Author:      Mathias Heil
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file strcalc.h
 *
 * The module uses a string to represent values, and provides operations
 * to perform calculations with these values.
 * Results are stored in an internal buffer, so you have to make a copy
 * of them if you need to store the result.
 *
 */

#ifndef _STRCALC_H_
#define _STRCALC_H_

#ifdef STRCALC_DEBUG_ALL             /* switch on all debug options */
#  ifndef STRCALC_DEBUG
#    define STRCALC_DEBUG            /* switch on debug output */
#  endif
#  ifndef STRCALC_DEBUG_PRINTCOMP    /* print arguments and result of each computation */
#    define STRCALC_DEBUG_PRINTCOMP
#  endif
#  ifndef STRCALC_DEBUG_FULLPRINT
#    define STRCALC_DEBUG_FULLPRINT  /* print full length of values (e.g. 128 bit instead of 64 bit using default init) */
#  endif
#  ifndef STRCALC_DEBUG_GROUPPRINT
#    define STRCALC_DEBUG_GROUPPRINT /* print spaces after each 8 bits */
#  endif
#endif

/*
 * constants, typedefs, enums
 */

#define SC_DEFAULT_PRECISION 64

enum {
  SC_0 = 0,
  SC_1,
  SC_2,
  SC_3,
  SC_4,
  SC_5,
  SC_6,
  SC_7,
  SC_8,
  SC_9,
  SC_A,
  SC_B,
  SC_C,
  SC_D,
  SC_E,
  SC_F
};

/**
 * Possible operations on integer values.
 */
typedef enum {
  SC_ADD = 0,       /**< Addition */
  SC_SUB,       /**< Substraction */
  SC_NEG,       /**< Unary Minus */
  SC_MUL,       /**< Multiplication */
  SC_DIV,       /**< Integer Division (with rounding toward zero ?) */
  SC_MOD,       /**< Devision Remainder */
  SC_SHL,       /**< Left Shift */
  SC_SHR,       /**< Logical (unsigned) Right Shift */
  SC_SHRS,      /**< Arithmetic (signed) Right Shift */
  SC_ROT,       /**< Rotation (both directions) */
  SC_AND,       /**< Bitwise And */
  SC_OR,        /**< Bitwise Or */
  SC_NOT,       /**< Bitwise Not */
  SC_XOR        /**< Bitwise Exclusive Or */
} sc_op_t;

/**
 * The output mode for ntger values.
 */
enum base_t {
  SC_hex,   /**< hexadecimal output with small letters */
  SC_HEX,   /**< hexadecimal output with BIG letters */
  SC_DEC,   /**< decimal output */
  SC_OCT,   /**< octal output */
  SC_BIN    /**< binary output */
};

/*
 * definitions and macros
 */
#define sc_add(a, b, c) sc_calc((a), (b), SC_ADD, (c))
#define sc_sub(a, b, c) sc_calc((a), (b), SC_SUB, (c))
#define sc_neg(a, c) sc_calc((a), NULL, SC_NEG, (c))
#define sc_and(a, b, c) sc_calc((a), (b), SC_AND, (c))
#define sc_or(a, b, c) sc_calc((a), (b), SC_OR, (c))
#define sc_xor(a, b, c) sc_calc((a), (b), SC_XOR, (c))
#define sc_not(a, c) sc_calc((a), NULL, SC_NOT, (c))
#define sc_mul(a, b, c) sc_calc((a), (b), SC_MUL, (c))
#define sc_div(a, b, c) sc_calc((a), (b), SC_DIV, (c))
#define sc_mod(a, b, c) sc_calc((a), (b), SC_MOD, (c))
#define sc_shl(a, b, c, d, e) sc_bitcalc((a), (b), (c), (d), SC_SHL, (e))
#define sc_shr(a, b, c, d, e) sc_bitcalc((a), (b), (c), (d), SC_SHR, (e))
#define sc_shrs(a, b, c, d, e) sc_bitcalc((a), (b), (c), (d), SC_SHRS, (e))
#define sc_rot(a, b, c, d, e) sc_bitcalc((a), (b), (c), (d), SC_ROT, (e))

/*
 * function declarations
 */
const void *sc_get_buffer(void);
const int sc_get_buffer_length(void);

void sc_val_from_str(const char *str, unsigned int len, void *buffer);
void sc_val_from_long(long l, void *buffer);
void sc_val_from_ulong(unsigned long l, void *buffer);
long sc_val_to_long(const void *val);
void sc_min_from_bits(unsigned int num_bits, unsigned int sign, void *buffer);
void sc_max_from_bits(unsigned int num_bits, unsigned int sign, void *buffer);

void sc_calc(const void *val1, const void *val2, unsigned op, void *buffer);
void sc_bitcalc(const void *val1, const void *val2, int radius, int sign, unsigned op, void *buffer);
int  sc_comp(const void *val1, const void *val2);

int sc_get_highest_set_bit(const void *value);
int sc_get_lowest_set_bit(const void *value);
int sc_is_zero(const void *value);
int sc_is_negative(const void *value);
int sc_had_carry(void);
unsigned char sc_sub_bits(const void *value, int len, unsigned byte_ofs);

/**
 * Converts a tarval into a string.
 *
 * @param val1      the value pointer
 * @param bits      number of valid bits in this value
 * @param base      output base
 */
const char *sc_print(const void *val1, unsigned bits, enum base_t base);

/** Init strcalc module
 * Sets up internal data structures and constants
 * After the first call subsequent calls have no effect
 *
 * @param precision_in_bytes Specifies internal precision to be used
 *   for calculations. The reason for being multiples of 8 eludes me
 */
void init_strcalc(int precision_in_bytes);
void finish_strcalc(void);
int sc_get_precision(void);

#endif /* _STRCALC_H_ */
