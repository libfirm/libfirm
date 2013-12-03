/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Provides basic mathematical operations on values represented as
 *           strings.
 * @date     2003
 * @author   Mathias Heil
 * @brief
 *
 * Results are stored in an internal buffer, so you have to make a copy
 * of them if you need to store the result.
 */
#ifndef FIRM_TV_STRCALC_H
#define FIRM_TV_STRCALC_H

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include "firm_types.h"

typedef unsigned char sc_word;

/**
 * The output mode for integer values.
 */
enum base_t {
  SC_hex,   /**< hexadecimal output with small letters */
  SC_HEX,   /**< hexadecimal output with BIG letters */
  SC_DEC,   /**< decimal output */
  SC_OCT,   /**< octal output */
  SC_BIN    /**< binary output */
};

/**
 * buffer = value1 + value2
 */
void sc_add(const sc_word *value1, const sc_word *value2, sc_word *buffer);

/**
 * buffer = value1 - value2
 */
void sc_sub(const sc_word *value1, const sc_word *value2, sc_word *buffer);

/**
 * buffer = -value
 */
void sc_neg(const sc_word *value, sc_word *buffer);

/**
 * buffer = value1 & value2
 */
void sc_and(const sc_word *value1, const sc_word *value2, sc_word *buffer);

/**
 * buffer = value1 & ~value2
 */
void sc_andnot(const sc_word *value1, const sc_word *value2, sc_word *buffer);

/**
 * buffer = value1 | value2
 */
void sc_or(const sc_word *value1, const sc_word *value2, sc_word *buffer);

/**
 * buffer = value1 ^ value2
 */
void sc_xor(const sc_word *value1, const sc_word *value2, sc_word *buffer);

/**
 * buffer = ~value
 */
void sc_not(const sc_word *value, sc_word *buffer);

/**
 * buffer = value1 * value2
 */
void sc_mul(const sc_word *value1, const sc_word *value2, sc_word *buffer);

/**
 * buffer = value1 / value2
 * @returns carry flag
 */
bool sc_div(const sc_word *value1, const sc_word *value2, sc_word *buffer);

/**
 * buffer = value1 % value2
 */
void sc_mod(const sc_word *value1, const sc_word *value2, sc_word *buffer);

/**
 * div_buffer = value1 / value2
 * mod_buffer = value1 % value2
 */
void sc_divmod(const sc_word *value1, const sc_word *value2,
               sc_word *div_buffer, sc_word *mod_buffer);

/**
 * buffer = value1 << offset
 */
void sc_shlI(const sc_word *val1, long shift_cnt, int bitsize, bool sign,
             sc_word *buffer);

/**
 * buffer = value1 << value2
 */
void sc_shl(const sc_word *value1, const sc_word *value2, int bitsize,
            bool sign, sc_word *buffer);

/**
 * buffer = value1 >>u offset
 * @returns carry flag
 */
bool sc_shrI(const sc_word *val1, long shift_cnt, int bitsize, bool sign,
             sc_word *buffer);

/**
 * buffer = value1 >>u value2
 * @returns carry flag
 */
bool sc_shr(const sc_word *value1, const sc_word *value2, int bitsize,
            bool sign, sc_word *buffer);

/**
 * buffer = value1 >>s offset
 * @returns carry flag
 */
bool sc_shrsI(const sc_word *val1, long shift_cnt, int bitsize, bool sign,
              sc_word *buffer);

/**
 * buffer = value1 >>s value2
 * @returns carry flag
 */
bool sc_shrs(const sc_word *value1, const sc_word *value2, int bitsize,
             bool sign, sc_word *buffer);

/**
 * buffer = 0
 */
void sc_zero(sc_word *buffer);

/*
 * function declarations
 */
const sc_word *sc_get_buffer(void);
int sc_get_buffer_length(void);

void sign_extend(sc_word *buffer, unsigned from_bits, bool is_signed);

/**
 * create an value form a string representation
 * @return true if ok, false in case of parse error
 */
bool sc_val_from_str(bool negative, unsigned base, const char *str,
                     size_t len, sc_word *buffer);

/** create a value from a long */
void sc_val_from_long(long l, sc_word *buffer);

/** create a value form an unsigned long */
void sc_val_from_ulong(unsigned long l, sc_word *buffer);

/**
 * Construct a strcalc value form a sequence of bytes in two complement big
 * or little endian format.
 * @param bytes       pointer to array of bytes
 * @param n_bytes     number of bytes in the sequence
 * @param big_endian  interpret bytes as big_endian format if true, else
 *                    little endian
 * @param buffer      destination buffer (calc_buffer if used if NULL)
 */
void sc_val_from_bytes(unsigned char const *bytes, size_t n_bytes,
                       bool big_endian, sc_word *buffer);

/**
 * Construct a strcalc value from a sequence of bytes in a little endian
 * formatted buffer. @p from specifies the first byte, @p to the last byte
 * (excluding).
 */
void sc_val_from_bits(unsigned char const *const bytes, unsigned from,
                      unsigned to, sc_word *buffer);

/** converts a value to a long */
long sc_val_to_long(const sc_word *val);
uint64_t sc_val_to_uint64(const sc_word *val);
void sc_min_from_bits(unsigned int num_bits, bool sign, sc_word *buffer);
void sc_max_from_bits(unsigned int num_bits, bool sign, sc_word *buffer);

/** truncates a value to lowest @p num_bits bits */
void sc_truncate(unsigned num_bits, sc_word *buffer);

/**
 * Compares val1 and val2
 */
ir_relation sc_comp(const sc_word *val1, const sc_word *val2);

int sc_get_highest_set_bit(const sc_word *value);
int sc_get_lowest_set_bit(const sc_word *value);
bool sc_is_negative(const sc_word *value);
bool sc_is_zero(const sc_word *value, unsigned num_bits);
bool sc_is_all_one(const sc_word *value, unsigned num_bits);

/**
 * Return the bits of a tarval at a given byte-offset.
 *
 * @param value     the value
 * @param len       number of valid bits in the value
 * @param byte_ofs  the byte offset
 */
unsigned char sc_sub_bits(const sc_word *value, int len, unsigned byte_ofs);

/**
 * Converts a tarval into a string.
 *
 * @param val1        the value pointer
 * @param bits        number of valid bits in this value
 * @param base        output base
 * @param signed_mode print it signed (only decimal mode supported
 */
const char *sc_print(const sc_word *val1, unsigned bits, enum base_t base,
                     bool signed_mode);

/**
 * Write value into string. The buffer is filled from the end, use the return
 * value to get the real start position of the string!
 * If the buffer is too small for the value, the behavior is undefined!
 */
char *sc_print_buf(char *buf, size_t buf_len, const sc_word *val, unsigned bits,
                   enum base_t base, bool is_signed);

/** Initialize the strcalc module.
 * Sets up internal data structures and constants
 * After the first call subsequent calls have no effect
 */
void init_strcalc(int precision);
void finish_strcalc(void);
int sc_get_precision(void);

/** Return the bit at a given position. */
bool sc_get_bit_at(const sc_word *value, unsigned pos);

/** Set the bit at the specified position. */
void sc_set_bit_at(sc_word *value, unsigned pos);
void sc_clear_bit_at(sc_word *value, unsigned pos);

unsigned sc_popcount(const sc_word *value, unsigned bits);

#endif
