/*
 * Project:     libFIRM
 * File name:   ir/tv/strcalc.c
 * Purpose:
 * Author:      Mathias Heil
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


#ifdef HAVE_CONFIG_H
# include "config.h"
#endif


#include "strcalc.h"

#include <stdlib.h>
#ifdef HAVE_ALLOCA_H
# include <alloca.h>
#endif
#ifdef HAVE_MALLOC_H
# include <malloc.h>
#endif
#include <assert.h>   /* assertions */
#include <string.h>   /* memset/memcmp */
#include <stdio.h>    /* output for error messages */
#include <limits.h>   /* definition of LONG_MIN, used in sc_get_val_from_long */

/*
 * local definitions and macros
 */
#define CLEAR_BUFFER(b) assert(b); memset(b, SC_0, CALC_BUFFER_SIZE)
#define _val(a) ((a)-SC_0)
#define _digit(a) ((a)+SC_0)
#define _bitisset(digit, pos) (and_table[_val(digit)][_val(shift_table[pos])] != SC_0)

#define fail_char(a, b, c, d) _fail_char((a), (b), (c), (d), __FILE__,  __LINE__)

/* shortcut output for debugging */
#  define sc_print_hex(a) sc_print((a), 0, SC_HEX)
#  define sc_print_dec(a) sc_print((a), 0, SC_DEC)
#  define sc_print_oct(a) sc_print((a), 0, SC_OCT)
#  define sc_print_bin(a) sc_print((a), 0, SC_BIN)

#ifdef STRCALC_DEBUG_PRINTCOMP
#  define DEBUGPRINTF_COMPUTATION(x) printf x
#else
#  define DEBUGPRINTF_COMPUTATION(x) ((void)0)
#endif
#ifdef STRCALC_DEBUG
#  define DEBUGPRINTF(x) printf x
#else
#  define DEBUGPRINTF(x) ((void)0)
#endif


/*
 * private variables
 */
static char *calc_buffer = NULL;    /* buffer holding all results */
static char *output_buffer = NULL;  /* buffer for output */
static int BIT_PATTERN_SIZE;        /* maximum number of bits */
static int CALC_BUFFER_SIZE;        /* size of internally stored values */
static int MAX_VALUE_SIZE;          /* maximum size of values */

static int carry_flag;              /**< some computation set the carry_flag:
                                         - rightshift if bits were lost due to shifting
                                         - division if there was a remainder
                                         However, the meaning of carry is machine dependant
                                         and often devined in other ways! */

static const char max_digit[4] = { SC_0, SC_1, SC_3, SC_7 };
static const char min_digit[4] = { SC_F, SC_E, SC_C, SC_8 };

static const char not_table[16] = { SC_F, SC_E, SC_D, SC_C, SC_B, SC_A, SC_9, SC_8,
                              SC_7, SC_6, SC_5, SC_4, SC_3, SC_2, SC_1, SC_0 };

static const char shift_table[4] = { SC_1, SC_2, SC_4, SC_8 };

static const char and_table[16][16] = {
                            { SC_0, SC_0, SC_0, SC_0, SC_0, SC_0, SC_0, SC_0,
                              SC_0, SC_0, SC_0, SC_0, SC_0, SC_0, SC_0, SC_0 },

                            { SC_0, SC_1, SC_0, SC_1, SC_0, SC_1, SC_0, SC_1,
                              SC_0, SC_1, SC_0, SC_1, SC_0, SC_1, SC_0, SC_1 },

                            { SC_0, SC_0, SC_2, SC_2, SC_0, SC_0, SC_2, SC_2,
                              SC_0, SC_0, SC_2, SC_2, SC_0, SC_0, SC_2, SC_2 },

                            { SC_0, SC_1, SC_2, SC_3, SC_0, SC_1, SC_2, SC_3,
                              SC_0, SC_1, SC_2, SC_3, SC_0, SC_1, SC_2, SC_3 },

                            { SC_0, SC_0, SC_0, SC_0, SC_4, SC_4, SC_4, SC_4,
                              SC_0, SC_0, SC_0, SC_0, SC_4, SC_4, SC_4, SC_4 },

                            { SC_0, SC_1, SC_0, SC_1, SC_4, SC_5, SC_4, SC_5,
                              SC_0, SC_1, SC_0, SC_1, SC_4, SC_5, SC_4, SC_5 },

                            { SC_0, SC_0, SC_2, SC_2, SC_4, SC_4, SC_6, SC_6,
                              SC_0, SC_0, SC_2, SC_2, SC_4, SC_4, SC_6, SC_6 },

                            { SC_0, SC_1, SC_2, SC_3, SC_4, SC_5, SC_6, SC_7,
                              SC_0, SC_1, SC_2, SC_3, SC_4, SC_5, SC_6, SC_7 },

                            { SC_0, SC_0, SC_0, SC_0, SC_0, SC_0, SC_0, SC_0,
                              SC_8, SC_8, SC_8, SC_8, SC_8, SC_8, SC_8, SC_8 },

                            { SC_0, SC_1, SC_0, SC_1, SC_0, SC_1, SC_0, SC_1,
                              SC_8, SC_9, SC_8, SC_9, SC_8, SC_9, SC_8, SC_9 },

                            { SC_0, SC_0, SC_2, SC_2, SC_0, SC_0, SC_2, SC_2,
                              SC_8, SC_8, SC_A, SC_A, SC_8, SC_8, SC_A, SC_A },

                            { SC_0, SC_1, SC_2, SC_3, SC_0, SC_1, SC_2, SC_3,
                              SC_8, SC_9, SC_A, SC_B, SC_8, SC_9, SC_A, SC_B },

                            { SC_0, SC_0, SC_0, SC_0, SC_4, SC_4, SC_4, SC_4,
                              SC_8, SC_8, SC_8, SC_8, SC_C, SC_C, SC_C, SC_C },

                            { SC_0, SC_1, SC_0, SC_1, SC_4, SC_5, SC_4, SC_5,
                              SC_8, SC_9, SC_8, SC_9, SC_C, SC_D, SC_C, SC_D },

                            { SC_0, SC_0, SC_2, SC_2, SC_4, SC_4, SC_6, SC_6,
                              SC_8, SC_8, SC_A, SC_A, SC_C, SC_C, SC_E, SC_E },

                            { SC_0, SC_1, SC_2, SC_3, SC_4, SC_5, SC_6, SC_7,
                              SC_8, SC_9, SC_A, SC_B, SC_C, SC_D, SC_E, SC_F } };

static const char or_table[16][16] = {
                            { SC_0, SC_1, SC_2, SC_3, SC_4, SC_5, SC_6, SC_7,
                              SC_8, SC_9, SC_A, SC_B, SC_C, SC_D, SC_E, SC_F },

                            { SC_1, SC_1, SC_3, SC_3, SC_5, SC_5, SC_7, SC_7,
                              SC_9, SC_9, SC_B, SC_B, SC_D, SC_D, SC_F, SC_F },

                            { SC_2, SC_3, SC_2, SC_3, SC_6, SC_7, SC_6, SC_7,
                              SC_A, SC_B, SC_A, SC_B, SC_E, SC_F, SC_E, SC_F },

                            { SC_3, SC_3, SC_3, SC_3, SC_7, SC_7, SC_7, SC_7,
                              SC_B, SC_B, SC_B, SC_B, SC_F, SC_F, SC_F, SC_F },

                            { SC_4, SC_5, SC_6, SC_7, SC_4, SC_5, SC_6, SC_7,
                              SC_C, SC_D, SC_E, SC_F, SC_C, SC_D, SC_E, SC_F },

                            { SC_5, SC_5, SC_7, SC_7, SC_5, SC_5, SC_7, SC_7,
                              SC_D, SC_D, SC_F, SC_F, SC_D, SC_D, SC_F, SC_F },

                            { SC_6, SC_7, SC_6, SC_7, SC_6, SC_7, SC_6, SC_7,
                              SC_E, SC_F, SC_E, SC_F, SC_E, SC_F, SC_E, SC_F },

                            { SC_7, SC_7, SC_7, SC_7, SC_7, SC_7, SC_7, SC_7,
                              SC_F, SC_F, SC_F, SC_F, SC_F, SC_F, SC_F, SC_F },

                            { SC_8, SC_9, SC_A, SC_B, SC_C, SC_D, SC_E, SC_F,
                              SC_8, SC_9, SC_A, SC_B, SC_C, SC_D, SC_E, SC_F },

                            { SC_9, SC_9, SC_B, SC_B, SC_D, SC_D, SC_F, SC_F,
                              SC_9, SC_9, SC_B, SC_B, SC_D, SC_D, SC_F, SC_F },

                            { SC_A, SC_B, SC_A, SC_B, SC_E, SC_F, SC_E, SC_F,
                              SC_A, SC_B, SC_A, SC_B, SC_E, SC_F, SC_E, SC_F },

                            { SC_B, SC_B, SC_B, SC_B, SC_F, SC_F, SC_F, SC_F,
                              SC_B, SC_B, SC_B, SC_B, SC_F, SC_F, SC_F, SC_F },

                            { SC_C, SC_D, SC_E, SC_F, SC_C, SC_D, SC_E, SC_F,
                              SC_C, SC_D, SC_E, SC_F, SC_C, SC_D, SC_E, SC_F },

                            { SC_D, SC_D, SC_F, SC_F, SC_D, SC_D, SC_F, SC_F,
                              SC_D, SC_D, SC_F, SC_F, SC_D, SC_D, SC_F, SC_F },

                            { SC_E, SC_F, SC_E, SC_F, SC_E, SC_F, SC_E, SC_F,
                              SC_E, SC_F, SC_E, SC_F, SC_E, SC_F, SC_E, SC_F },

                            { SC_F, SC_F, SC_F, SC_F, SC_F, SC_F, SC_F, SC_F,
                              SC_F, SC_F, SC_F, SC_F, SC_F, SC_F, SC_F, SC_F } };

static char const xor_table[16][16] = {
                             { SC_0, SC_1, SC_2, SC_3, SC_4, SC_5, SC_6, SC_7,
                               SC_8, SC_9, SC_A, SC_B, SC_C, SC_D, SC_E, SC_F },

                             { SC_1, SC_0, SC_3, SC_2, SC_5, SC_4, SC_7, SC_6,
                               SC_9, SC_8, SC_B, SC_A, SC_D, SC_C, SC_F, SC_E },

                             { SC_2, SC_3, SC_0, SC_1, SC_6, SC_7, SC_4, SC_5,
                               SC_A, SC_B, SC_8, SC_9, SC_E, SC_F, SC_C, SC_D },

                             { SC_3, SC_2, SC_1, SC_0, SC_7, SC_6, SC_5, SC_4,
                               SC_B, SC_A, SC_9, SC_8, SC_F, SC_E, SC_D, SC_C },

                             { SC_4, SC_5, SC_6, SC_7, SC_0, SC_1, SC_2, SC_3,
                               SC_C, SC_D, SC_E, SC_F, SC_8, SC_9, SC_A, SC_B },

                             { SC_5, SC_4, SC_7, SC_6, SC_1, SC_0, SC_3, SC_2,
                               SC_D, SC_C, SC_F, SC_E, SC_9, SC_8, SC_B, SC_A },

                             { SC_6, SC_7, SC_4, SC_5, SC_2, SC_3, SC_0, SC_1,
                               SC_E, SC_F, SC_C, SC_D, SC_A, SC_B, SC_8, SC_9 },

                             { SC_7, SC_6, SC_5, SC_4, SC_3, SC_2, SC_1, SC_0,
                               SC_F, SC_E, SC_D, SC_C, SC_B, SC_A, SC_9, SC_8 },

                             { SC_8, SC_9, SC_A, SC_B, SC_C, SC_D, SC_E, SC_F,
                               SC_0, SC_1, SC_2, SC_3, SC_4, SC_5, SC_6, SC_7 },

                             { SC_9, SC_8, SC_B, SC_A, SC_D, SC_C, SC_F, SC_E,
                               SC_1, SC_0, SC_3, SC_2, SC_5, SC_4, SC_7, SC_6 },

                             { SC_A, SC_B, SC_8, SC_9, SC_E, SC_F, SC_C, SC_D,
                               SC_2, SC_3, SC_0, SC_1, SC_6, SC_7, SC_4, SC_5 },

                             { SC_B, SC_A, SC_9, SC_8, SC_F, SC_E, SC_D, SC_C,
                               SC_3, SC_2, SC_1, SC_0, SC_7, SC_6, SC_5, SC_4 },

                             { SC_C, SC_D, SC_E, SC_F, SC_8, SC_9, SC_A, SC_B,
                               SC_4, SC_5, SC_6, SC_7, SC_0, SC_1, SC_2, SC_3 },

                             { SC_D, SC_C, SC_F, SC_E, SC_9, SC_8, SC_B, SC_A,
                               SC_5, SC_4, SC_7, SC_6, SC_1, SC_0, SC_3, SC_2 },

                             { SC_E, SC_F, SC_C, SC_D, SC_A, SC_B, SC_8, SC_9,
                               SC_6, SC_7, SC_4, SC_5, SC_2, SC_3, SC_0, SC_1 },

                             { SC_F, SC_E, SC_D, SC_C, SC_B, SC_A, SC_9, SC_8,
                               SC_7, SC_6, SC_5, SC_4, SC_3, SC_2, SC_1, SC_0 }
                                };

static char const add_table[16][16][2] = {
                       { {SC_0, SC_0}, {SC_1, SC_0}, {SC_2, SC_0}, {SC_3, SC_0},
                         {SC_4, SC_0}, {SC_5, SC_0}, {SC_6, SC_0}, {SC_7, SC_0},
                         {SC_8, SC_0}, {SC_9, SC_0}, {SC_A, SC_0}, {SC_B, SC_0},
                         {SC_C, SC_0}, {SC_D, SC_0}, {SC_E, SC_0}, {SC_F, SC_0} },

                       { {SC_1, SC_0}, {SC_2, SC_0}, {SC_3, SC_0}, {SC_4, SC_0},
                         {SC_5, SC_0}, {SC_6, SC_0}, {SC_7, SC_0}, {SC_8, SC_0},
                         {SC_9, SC_0}, {SC_A, SC_0}, {SC_B, SC_0}, {SC_C, SC_0},
                         {SC_D, SC_0}, {SC_E, SC_0}, {SC_F, SC_0}, {SC_0, SC_1} },

                       { {SC_2, SC_0}, {SC_3, SC_0}, {SC_4, SC_0}, {SC_5, SC_0},
                         {SC_6, SC_0}, {SC_7, SC_0}, {SC_8, SC_0}, {SC_9, SC_0},
                         {SC_A, SC_0}, {SC_B, SC_0}, {SC_C, SC_0}, {SC_D, SC_0},
                         {SC_E, SC_0}, {SC_F, SC_0}, {SC_0, SC_1}, {SC_1, SC_1} },

                       { {SC_3, SC_0}, {SC_4, SC_0}, {SC_5, SC_0}, {SC_6, SC_0},
                         {SC_7, SC_0}, {SC_8, SC_0}, {SC_9, SC_0}, {SC_A, SC_0},
                         {SC_B, SC_0}, {SC_C, SC_0}, {SC_D, SC_0}, {SC_E, SC_0},
                         {SC_F, SC_0}, {SC_0, SC_1}, {SC_1, SC_1}, {SC_2, SC_1} },

                       { {SC_4, SC_0}, {SC_5, SC_0}, {SC_6, SC_0}, {SC_7, SC_0},
                         {SC_8, SC_0}, {SC_9, SC_0}, {SC_A, SC_0}, {SC_B, SC_0},
                         {SC_C, SC_0}, {SC_D, SC_0}, {SC_E, SC_0}, {SC_F, SC_0},
                         {SC_0, SC_1}, {SC_1, SC_1}, {SC_2, SC_1}, {SC_3, SC_1} },

                       { {SC_5, SC_0}, {SC_6, SC_0}, {SC_7, SC_0}, {SC_8, SC_0},
                         {SC_9, SC_0}, {SC_A, SC_0}, {SC_B, SC_0}, {SC_C, SC_0},
                         {SC_D, SC_0}, {SC_E, SC_0}, {SC_F, SC_0}, {SC_0, SC_1},
                         {SC_1, SC_1}, {SC_2, SC_1}, {SC_3, SC_1}, {SC_4, SC_1} },

                       { {SC_6, SC_0}, {SC_7, SC_0}, {SC_8, SC_0}, {SC_9, SC_0},
                         {SC_A, SC_0}, {SC_B, SC_0}, {SC_C, SC_0}, {SC_D, SC_0},
                         {SC_E, SC_0}, {SC_F, SC_0}, {SC_0, SC_1}, {SC_1, SC_1},
                         {SC_2, SC_1}, {SC_3, SC_1}, {SC_4, SC_1}, {SC_5, SC_1} },

                       { {SC_7, SC_0}, {SC_8, SC_0}, {SC_9, SC_0}, {SC_A, SC_0},
                         {SC_B, SC_0}, {SC_C, SC_0}, {SC_D, SC_0}, {SC_E, SC_0},
                         {SC_F, SC_0}, {SC_0, SC_1}, {SC_1, SC_1}, {SC_2, SC_1},
                         {SC_3, SC_1}, {SC_4, SC_1}, {SC_5, SC_1}, {SC_6, SC_1} },

                       { {SC_8, SC_0}, {SC_9, SC_0}, {SC_A, SC_0}, {SC_B, SC_0},
                         {SC_C, SC_0}, {SC_D, SC_0}, {SC_E, SC_0}, {SC_F, SC_0},
                         {SC_0, SC_1}, {SC_1, SC_1}, {SC_2, SC_1}, {SC_3, SC_1},
                         {SC_4, SC_1}, {SC_5, SC_1}, {SC_6, SC_1}, {SC_7, SC_1} },

                       { {SC_9, SC_0}, {SC_A, SC_0}, {SC_B, SC_0}, {SC_C, SC_0},
                         {SC_D, SC_0}, {SC_E, SC_0}, {SC_F, SC_0}, {SC_0, SC_1},
                         {SC_1, SC_1}, {SC_2, SC_1}, {SC_3, SC_1}, {SC_4, SC_1},
                         {SC_5, SC_1}, {SC_6, SC_1}, {SC_7, SC_1}, {SC_8, SC_1} },

                       { {SC_A, SC_0}, {SC_B, SC_0}, {SC_C, SC_0}, {SC_D, SC_0},
                         {SC_E, SC_0}, {SC_F, SC_0}, {SC_0, SC_1}, {SC_1, SC_1},
                         {SC_2, SC_1}, {SC_3, SC_1}, {SC_4, SC_1}, {SC_5, SC_1},
                         {SC_6, SC_1}, {SC_7, SC_1}, {SC_8, SC_1}, {SC_9, SC_1} },

                       { {SC_B, SC_0}, {SC_C, SC_0}, {SC_D, SC_0}, {SC_E, SC_0},
                         {SC_F, SC_0}, {SC_0, SC_1}, {SC_1, SC_1}, {SC_2, SC_1},
                         {SC_3, SC_1}, {SC_4, SC_1}, {SC_5, SC_1}, {SC_6, SC_1},
                         {SC_7, SC_1}, {SC_8, SC_1}, {SC_9, SC_1}, {SC_A, SC_1} },

                       { {SC_C, SC_0}, {SC_D, SC_0}, {SC_E, SC_0}, {SC_F, SC_0},
                         {SC_0, SC_1}, {SC_1, SC_1}, {SC_2, SC_1}, {SC_3, SC_1},
                         {SC_4, SC_1}, {SC_5, SC_1}, {SC_6, SC_1}, {SC_7, SC_1},
                         {SC_8, SC_1}, {SC_9, SC_1}, {SC_A, SC_1}, {SC_B, SC_1} },

                       { {SC_D, SC_0}, {SC_E, SC_0}, {SC_F, SC_0}, {SC_0, SC_1},
                         {SC_1, SC_1}, {SC_2, SC_1}, {SC_3, SC_1}, {SC_4, SC_1},
                         {SC_5, SC_1}, {SC_6, SC_1}, {SC_7, SC_1}, {SC_8, SC_1},
                         {SC_9, SC_1}, {SC_A, SC_1}, {SC_B, SC_1}, {SC_C, SC_1} },

                       { {SC_E, SC_0}, {SC_F, SC_0}, {SC_0, SC_1}, {SC_1, SC_1},
                         {SC_2, SC_1}, {SC_3, SC_1}, {SC_4, SC_1}, {SC_5, SC_1},
                         {SC_6, SC_1}, {SC_7, SC_1}, {SC_8, SC_1}, {SC_9, SC_1},
                         {SC_A, SC_1}, {SC_B, SC_1}, {SC_C, SC_1}, {SC_D, SC_1} },

                       { {SC_F, SC_0}, {SC_0, SC_1}, {SC_1, SC_1}, {SC_2, SC_1},
                         {SC_3, SC_1}, {SC_4, SC_1}, {SC_5, SC_1}, {SC_6, SC_1},
                         {SC_7, SC_1}, {SC_8, SC_1}, {SC_9, SC_1}, {SC_A, SC_1},
                         {SC_B, SC_1}, {SC_C, SC_1}, {SC_D, SC_1}, {SC_E, SC_1} }
                             };

static char const mul_table[16][16][2] = {
                       { {SC_0, SC_0}, {SC_0, SC_0}, {SC_0, SC_0}, {SC_0, SC_0},
                         {SC_0, SC_0}, {SC_0, SC_0}, {SC_0, SC_0}, {SC_0, SC_0},
                         {SC_0, SC_0}, {SC_0, SC_0}, {SC_0, SC_0}, {SC_0, SC_0},
                         {SC_0, SC_0}, {SC_0, SC_0}, {SC_0, SC_0}, {SC_0, SC_0} },

                       { {SC_0, SC_0}, {SC_1, SC_0}, {SC_2, SC_0}, {SC_3, SC_0},
                         {SC_4, SC_0}, {SC_5, SC_0}, {SC_6, SC_0}, {SC_7, SC_0},
                         {SC_8, SC_0}, {SC_9, SC_0}, {SC_A, SC_0}, {SC_B, SC_0},
                         {SC_C, SC_0}, {SC_D, SC_0}, {SC_E, SC_0}, {SC_F, SC_0} },

                       { {SC_0, SC_0}, {SC_2, SC_0}, {SC_4, SC_0}, {SC_6, SC_0},
                         {SC_8, SC_0}, {SC_A, SC_0}, {SC_C, SC_0}, {SC_E, SC_0},
                         {SC_0, SC_1}, {SC_2, SC_1}, {SC_4, SC_1}, {SC_6, SC_1},
                         {SC_8, SC_1}, {SC_A, SC_1}, {SC_C, SC_1}, {SC_E, SC_1} },

                       { {SC_0, SC_0}, {SC_3, SC_0}, {SC_6, SC_0}, {SC_9, SC_0},
                         {SC_C, SC_0}, {SC_F, SC_0}, {SC_2, SC_1}, {SC_5, SC_1},
                         {SC_8, SC_1}, {SC_B, SC_1}, {SC_E, SC_1}, {SC_1, SC_2},
                         {SC_4, SC_2}, {SC_7, SC_2}, {SC_A, SC_2}, {SC_D, SC_2} },

                       { {SC_0, SC_0}, {SC_4, SC_0}, {SC_8, SC_0}, {SC_C, SC_0},
                         {SC_0, SC_1}, {SC_4, SC_1}, {SC_8, SC_1}, {SC_C, SC_1},
                         {SC_0, SC_2}, {SC_4, SC_2}, {SC_8, SC_2}, {SC_C, SC_2},
                         {SC_0, SC_3}, {SC_4, SC_3}, {SC_8, SC_3}, {SC_C, SC_3} },

                       { {SC_0, SC_0}, {SC_5, SC_0}, {SC_A, SC_0}, {SC_F, SC_0},
                         {SC_4, SC_1}, {SC_9, SC_1}, {SC_E, SC_1}, {SC_3, SC_2},
                         {SC_8, SC_2}, {SC_D, SC_2}, {SC_2, SC_3}, {SC_7, SC_3},
                         {SC_C, SC_3}, {SC_1, SC_4}, {SC_6, SC_4}, {SC_B, SC_4} },

                       { {SC_0, SC_0}, {SC_6, SC_0}, {SC_C, SC_0}, {SC_2, SC_1},
                         {SC_8, SC_1}, {SC_E, SC_1}, {SC_4, SC_2}, {SC_A, SC_2},
                         {SC_0, SC_3}, {SC_6, SC_3}, {SC_C, SC_3}, {SC_2, SC_4},
                         {SC_8, SC_4}, {SC_E, SC_4}, {SC_4, SC_5}, {SC_A, SC_5} },

                       { {SC_0, SC_0}, {SC_7, SC_0}, {SC_E, SC_0}, {SC_5, SC_1},
                         {SC_C, SC_1}, {SC_3, SC_2}, {SC_A, SC_2}, {SC_1, SC_3},
                         {SC_8, SC_3}, {SC_F, SC_3}, {SC_6, SC_4}, {SC_D, SC_4},
                         {SC_4, SC_5}, {SC_B, SC_5}, {SC_2, SC_6}, {SC_9, SC_6} },

                       { {SC_0, SC_0}, {SC_8, SC_0}, {SC_0, SC_1}, {SC_8, SC_1},
                         {SC_0, SC_2}, {SC_8, SC_2}, {SC_0, SC_3}, {SC_8, SC_3},
                         {SC_0, SC_4}, {SC_8, SC_4}, {SC_0, SC_5}, {SC_8, SC_5},
                         {SC_0, SC_6}, {SC_8, SC_6}, {SC_0, SC_7}, {SC_8, SC_7} },

                       { {SC_0, SC_0}, {SC_9, SC_0}, {SC_2, SC_1}, {SC_B, SC_1},
                         {SC_4, SC_2}, {SC_D, SC_2}, {SC_6, SC_3}, {SC_F, SC_3},
                         {SC_8, SC_4}, {SC_1, SC_5}, {SC_A, SC_5}, {SC_3, SC_6},
                         {SC_C, SC_6}, {SC_5, SC_7}, {SC_E, SC_7}, {SC_7, SC_8} },

                       { {SC_0, SC_0}, {SC_A, SC_0}, {SC_4, SC_1}, {SC_E, SC_1},
                         {SC_8, SC_2}, {SC_2, SC_3}, {SC_C, SC_3}, {SC_6, SC_4},
                         {SC_0, SC_5}, {SC_A, SC_5}, {SC_4, SC_6}, {SC_E, SC_6},
                         {SC_8, SC_7}, {SC_2, SC_8}, {SC_C, SC_8}, {SC_6, SC_9} },

                       { {SC_0, SC_0}, {SC_B, SC_0}, {SC_6, SC_1}, {SC_1, SC_2},
                         {SC_C, SC_2}, {SC_7, SC_3}, {SC_2, SC_4}, {SC_D, SC_4},
                         {SC_8, SC_5}, {SC_3, SC_6}, {SC_E, SC_6}, {SC_9, SC_7},
                         {SC_4, SC_8}, {SC_F, SC_8}, {SC_A, SC_9}, {SC_5, SC_A} },

                       { {SC_0, SC_0}, {SC_C, SC_0}, {SC_8, SC_1}, {SC_4, SC_2},
                         {SC_0, SC_3}, {SC_C, SC_3}, {SC_8, SC_4}, {SC_4, SC_5},
                         {SC_0, SC_6}, {SC_C, SC_6}, {SC_8, SC_7}, {SC_4, SC_8},
                         {SC_0, SC_9}, {SC_C, SC_9}, {SC_8, SC_A}, {SC_4, SC_B} },

                       { {SC_0, SC_0}, {SC_D, SC_0}, {SC_A, SC_1}, {SC_7, SC_2},
                         {SC_4, SC_3}, {SC_1, SC_4}, {SC_E, SC_4}, {SC_B, SC_5},
                         {SC_8, SC_6}, {SC_5, SC_7}, {SC_2, SC_8}, {SC_F, SC_8},
                         {SC_C, SC_9}, {SC_9, SC_A}, {SC_6, SC_B}, {SC_3, SC_C} },

                       { {SC_0, SC_0}, {SC_E, SC_0}, {SC_C, SC_1}, {SC_A, SC_2},
                         {SC_8, SC_3}, {SC_6, SC_4}, {SC_4, SC_5}, {SC_2, SC_6},
                         {SC_0, SC_7}, {SC_E, SC_7}, {SC_C, SC_8}, {SC_A, SC_9},
                         {SC_8, SC_A}, {SC_6, SC_B}, {SC_4, SC_C}, {SC_2, SC_D} },

                       { {SC_0, SC_0}, {SC_F, SC_0}, {SC_E, SC_1}, {SC_D, SC_2},
                         {SC_C, SC_3}, {SC_B, SC_4}, {SC_A, SC_5}, {SC_9, SC_6},
                         {SC_8, SC_7}, {SC_7, SC_8}, {SC_6, SC_9}, {SC_5, SC_A},
                         {SC_4, SC_B}, {SC_3, SC_C}, {SC_2, SC_D}, {SC_1, SC_E} }
                             };

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

/* for converting to binary string */
static const char *binary_table[16] = {
  "0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111",
  "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111"
};

/*****************************************************************************
 * private functions
 *****************************************************************************/
static void _fail_char(const char *str, size_t len, const char fchar, int pos,
                       const char *file, int line)
{
  printf("ERROR:\n");
  printf("Unexpected character '%c' in %s:%d\n", fchar, file, line);
  while (len-- && *str) printf("%c", *str++); printf("\n");
  while (--pos) printf(" "); printf("^\n");
  exit(-1);
}

/**
 * implements the bitwise NOT operation
 */
static void _bitnot(const char *val, char *buffer)
{
  int counter;

  for (counter = 0; counter<CALC_BUFFER_SIZE; counter++)
    buffer[counter] = not_table[_val(val[counter])];
}

/**
 * implements the bitwise OR operation
 */
static void _bitor(const char *val1, const char *val2, char *buffer)
{
  int counter;

  for (counter = 0; counter<CALC_BUFFER_SIZE; counter++)
    buffer[counter] = or_table[_val(val1[counter])][_val(val2[counter])];
}

/**
 * implements the bitwise eXclusive OR operation
 */
static void _bitxor(const char *val1, const char *val2, char *buffer)
{
  int counter;

  for (counter = 0; counter<CALC_BUFFER_SIZE; counter++)
    buffer[counter] = xor_table[_val(val1[counter])][_val(val2[counter])];
}

/**
 * implements the bitwise AND operation
 */
static void _bitand(const char *val1, const char *val2, char *buffer)
{
  int counter;

  for (counter = 0; counter<CALC_BUFFER_SIZE; counter++)
    buffer[counter] = and_table[_val(val1[counter])][_val(val2[counter])];
}

/**
 * returns the sign bit.
 *
 * @todo This implementation is wrong, as it returns the highest bit of the buffer
 *       NOT the highest bit depending on the real mode
 */
static int _sign(const char *val)
{
  return (val[CALC_BUFFER_SIZE-1] <= SC_7) ? (1) : (-1);
}

/**
 * returns non-zero if bit at position pos is set
 */
static int _bit(const char *val, int pos)
{
  int bit    = pos & 3;
  int nibble = pos >> 2;

  return _bitisset(val[nibble], bit);
}

/**
 * Implements a fast ADD + 1
 */
static void _inc(const char *val, char *buffer)
{
  int counter = 0;

  while (counter++ < CALC_BUFFER_SIZE)
  {
    if (*val == SC_F)
    {
      *buffer++ = SC_0;
      val++;
    }
    else
    {
      /* No carry here, *val != SC_F */
      *buffer = add_table[_val(*val)][SC_1][0];
      return;
    }
  }
  /* here a carry could be lost, this is intended because this should
   * happen only when a value changes sign. */
}

/**
 * Implements a unary MINUS
 */
static void _negate(const char *val, char *buffer)
{
  _bitnot(val, buffer);
  _inc(buffer, buffer);
}

/**
 * Implements a binary ADD
 *
 * @todo The implementation of carry is wrong, as it is the
 *       CALC_BUFFER_SIZE carry, not the mode depending
 */
static void _add(const char *val1, const char *val2, char *buffer)
{
  int counter;
  const char *add1, *add2;
  char carry = SC_0;

  for (counter = 0; counter < CALC_BUFFER_SIZE; counter++)
  {
    add1 = add_table[_val(val1[counter])][_val(val2[counter])];
    add2 = add_table[_val(add1[0])][_val(carry)];
    /* carry might be zero */
    buffer[counter] = add2[0];
    carry = add_table[_val(add1[1])][_val(add2[1])][0];
  }
  carry_flag = carry != SC_0;
}

/**
 * Implements a binary SUB
 */
static void _sub(const char *val1, const char *val2, char *buffer)
{
  char temp_buffer[CALC_BUFFER_SIZE];  /* intermediate buffer to hold -val2 */

  _negate(val2, temp_buffer);
  _add(val1, temp_buffer, buffer);
}

/**
 * Implements a binary MUL
 */
static void _mul(const char *val1, const char *val2, char *buffer)
{
  char* temp_buffer; /* result buffer */
  char* neg_val1;    /* abs of val1 */
  char* neg_val2;    /* abs of val2 */

  const char *mul, *add1, *add2;      /* intermediate result containers */
  char carry = SC_0;                  /* container for carries */
  char sign = 0;                      /* marks result sign */
  int c_inner, c_outer;               /* loop counters */

  temp_buffer = alloca(CALC_BUFFER_SIZE);
  neg_val1 = alloca(CALC_BUFFER_SIZE);
  neg_val2 = alloca(CALC_BUFFER_SIZE);

  /* init result buffer to zeroes */
  memset(temp_buffer, SC_0, CALC_BUFFER_SIZE);

  /* the multiplication works only for positive values, for negative values *
   * it is necessary to negate them and adjust the result accordingly       */
  if (_sign(val1) == -1) {
    _negate(val1, neg_val1);
    val1 = neg_val1;
    sign ^= 1;
  }
  if (_sign(val2) == -1) {
    _negate(val2, neg_val2);
    val2 = neg_val2;
    sign ^= 1;
  }

  for (c_outer = 0; c_outer < MAX_VALUE_SIZE; c_outer++)
  {
    if (val2[c_outer] != SC_0)
    {
      for (c_inner = 0; c_inner < MAX_VALUE_SIZE; c_inner++)
      {
        /* do the following calculation:                                    *
         * Add the current carry, the value at position c_outer+c_inner     *
         * and the result of the multiplication of val1[c_inner] and        *
         * val2[c_outer]. This is the usual pen-and-paper multiplication.   */

        /* multiplicate the two digits */
        mul = mul_table[_val(val1[c_inner])][_val(val2[c_outer])];
        /* add old value to result of multiplication */
        add1 = add_table[_val(temp_buffer[c_inner + c_outer])][_val(mul[0])];
        /* add carry to the sum */
        add2 = add_table[_val(add1[0])][_val(carry)];

        /* all carries together result in new carry. This is always smaller *
         * than the base b:                                                 *
         * Both multiplicands, the carry and the value already in the temp  *
         * buffer are single digits and their value is therefore at most    *
         * equal to (b-1).                                                  *
         * This leads to:                                                   *
         * (b-1)(b-1)+(b-1)+(b-1) = b*b-1                                   *
         * The tables list all operations rem b, so the carry is at most    *
         * (b*b-1)rem b = -1rem b = b-1                                     */
        carry = add_table[_val(mul[1])][_val(add1[1])][0];
        carry = add_table[_val(carry)][_val(add2[1])][0];

        temp_buffer[c_inner + c_outer] = add2[0];
      }

      /* A carry may hang over */
      /* c_outer is always smaller than MAX_VALUE_SIZE! */
      temp_buffer[MAX_VALUE_SIZE + c_outer] = carry;
      carry = SC_0;
    }
  }

  if (sign)
    _negate(temp_buffer, buffer);
  else
    memcpy(buffer, temp_buffer, CALC_BUFFER_SIZE);
}

/**
 * Shift the buffer to left and add a 4 bit digit
 */
static void _push(const char digit, char *buffer)
{
  int counter;

  for (counter = CALC_BUFFER_SIZE - 2; counter >= 0; counter--)
  {
    buffer[counter+1] = buffer[counter];
  }
  buffer[0] = digit;
}

/**
 * Implements truncating integer division and remainder.
 *
 * Note: This is MOST slow
 */
static void _divmod(const char *rDividend, const char *divisor, char *quot, char *rem)
{
  const char *dividend = rDividend;
  const char *minus_divisor;
  char *neg_val1;
  char *neg_val2;

  char div_sign = 0;     /* remember division result sign */
  char rem_sign = 0;     /* remember remainder esult sign */

  int c_dividend;      /* loop counters */

  neg_val1 = alloca(CALC_BUFFER_SIZE);
  neg_val2 = alloca(CALC_BUFFER_SIZE);

  /* clear result buffer */
  memset(quot, SC_0, CALC_BUFFER_SIZE);
  memset(rem, SC_0, CALC_BUFFER_SIZE);

  /* if the divisor is zero this won't work (quot is zero) */
  if (sc_comp(divisor, quot) == 0) assert(0 && "division by zero!");

  /* if the dividend is zero result is zero (quot is zero)*/
  if (sc_comp(dividend, quot) == 0)
    return;

  if (_sign(dividend) == -1) {
    _negate(dividend, neg_val1);
    div_sign ^= 1;
    rem_sign ^= 1;
    dividend = neg_val1;
  }

  _negate(divisor, neg_val2);
  if (_sign(divisor) == -1) {
    div_sign ^= 1;
    minus_divisor = divisor;
    divisor = neg_val2;
  }
  else
    minus_divisor = neg_val2;

  /* if divisor >= dividend division is easy
   * (remember these are absolute values) */
  switch (sc_comp(dividend, divisor))
  {
    case 0: /* dividend == divisor */
      quot[0] = SC_1;
      return;

    case -1: /* dividend < divisor */
      memcpy(rem, rDividend, CALC_BUFFER_SIZE);
      return;

    default: /* unluckily division is necessary :( */
      break;
  }

  for (c_dividend = CALC_BUFFER_SIZE - 1; c_dividend >= 0; c_dividend--)
  {
    _push(dividend[c_dividend], rem);
    _push(SC_0, quot);

    if (sc_comp(rem, divisor) != -1)    /* remainder >= divisor */
    {
      /* subtract until the remainder becomes negative, this should
       * be faster than comparing remainder with divisor  */
      _add(rem, minus_divisor, rem);

      while (_sign(rem) == 1)
      {
        quot[0] = add_table[_val(quot[0])][SC_1][0];
        _add(rem, minus_divisor, rem);
      }

      /* subtracted one too much */
      _add(rem, divisor, rem);
    }
  }

  /* sets carry if remainder is non-zero ??? */
  carry_flag = !sc_is_zero(rem);

  if (div_sign)
    _negate(quot, quot);

  if (rem_sign)
    _negate(rem, rem);
}

/**
 * Implements a Shift Left, which can either preserve the sign bit
 * or not.
 *
 * @todo Assertions seems to be wrong
 */
static void _shl(const char *val1, char *buffer, long offset, int radius, unsigned is_signed)
{
  const char *shl;
  char shift;
  char carry = SC_0;

  int counter;
  int bitoffset = 0;

  assert((offset >= 0) || (0 && "negative leftshift"));
  assert(((_sign(val1) != -1) || is_signed) || (0 && "unsigned mode and negative value"));
  assert(((!_bitisset(val1[(radius-1)/4], (radius-1)%4)) || !is_signed || (_sign(val1) == -1)) || (0 && "value is positive, should be negative"));
  assert(((_bitisset(val1[(radius-1)/4], (radius-1)%4)) || !is_signed || (_sign(val1) == 1)) || (0 && "value is negative, should be positive"));

  /* if shifting far enough the result is zero */
  if (offset >= radius)
  {
    memset(buffer, SC_0, CALC_BUFFER_SIZE);
    return;
  }

  shift = shift_table[_val(offset%4)];      /* this is 2 ** (offset % 4) */
  offset = offset / 4;

  /* shift the single digits some bytes (offset) and some bits (table)
   * to the left */
  for (counter = 0; counter < radius/4 - offset; counter++)
  {
    shl = mul_table[_val(val1[counter])][_val(shift)];
    buffer[counter + offset] = or_table[_val(shl[0])][_val(carry)];
    carry = shl[1];
  }
  if (radius%4 > 0)
  {
    shl = mul_table[_val(val1[counter])][_val(shift)];
    buffer[counter + offset] = or_table[_val(shl[0])][_val(carry)];
    bitoffset = counter;
  } else {
    bitoffset = counter - 1;
  }

  /* fill with zeroes */
  for (counter = 0; counter < offset; counter++) buffer[counter] = SC_0;

  /* if the mode was signed, change sign when the mode's msb is now 1 */
  offset = bitoffset + offset;
  bitoffset = (radius-1) % 4;
  if (is_signed && _bitisset(buffer[offset], bitoffset))
  {
    /* this sets the upper bits of the leftmost digit */
    buffer[offset] = or_table[_val(buffer[offset])][_val(min_digit[bitoffset])];
    for (counter = offset+1; counter < CALC_BUFFER_SIZE; counter++)
    {
      buffer[counter] = SC_F;
    }
  }
  else if (is_signed && !_bitisset(buffer[offset], bitoffset))
  {
    /* this unsets the upper bits of the leftmost digit */
    buffer[offset] = and_table[_val(buffer[offset])][_val(max_digit[bitoffset])];
    for (counter = offset+1; counter < CALC_BUFFER_SIZE; counter++)
    {
      buffer[counter] = SC_0;
    }
  }
}

/**
 * Implements a Shift Right, which can either preserve the sign bit
 * or not.
 *
 * @todo Assertions seems to be wrong
 */
static void _shr(const char *val1, char *buffer, long offset, int radius, unsigned is_signed, int signed_shift)
{
  const char *shrs;
  char sign;
  char msd;

  int shift;

  int counter;
  int bitoffset = 0;

  assert((offset >= 0) || (0 && "negative rightshift"));
  assert(((_sign(val1) != -1) || is_signed) || (0 && "unsigned mode and negative value"));
  assert(((!_bitisset(val1[(radius-1)/4], (radius-1)%4)) || !is_signed || (_sign(val1) == -1)) || (0 && "value is positive, should be negative"));
  assert(((_bitisset(val1[(radius-1)/4], (radius-1)%4)) || !is_signed || (_sign(val1) == 1)) || (0 && "value is negative, should be positive"));

  sign = ((signed_shift) && (_sign(val1) == -1))?(SC_F):(SC_0);

  /* if shifting far enough the result is either 0 or -1 */
  if (offset >= radius)
  {
    if (!sc_is_zero(val1)) {
      carry_flag = 1;
    }
    memset(buffer, sign, CALC_BUFFER_SIZE);
    return;
  }

  shift = offset % 4;
  offset = offset / 4;

  /* check if any bits are lost, and set carry_flag if so */
  for (counter = 0; counter < offset; counter++)
  {
    if (val1[counter] != 0)
    {
      carry_flag = 1;
      break;
    }
  }
  if ((_val(val1[counter]) & ((1<<shift)-1)) != 0)
  {
    carry_flag = 1;
  }
  /* shift digits to the right with offset, carry and all */
  counter = 0;
  if (radius/4 - offset > 0) {
    buffer[counter] = shrs_table[_val(val1[offset])][shift][0];
    counter = 1;
  }
  for (; counter < radius/4 - offset; counter++)
  {
    shrs = shrs_table[_val(val1[counter + offset])][shift];
    buffer[counter] = shrs[0];
    buffer[counter-1] = or_table[_val(buffer[counter-1])][_val(shrs[1])];
  }

  /* the last digit is special in regard of signed/unsigned shift */
  bitoffset = radius%4;
  msd = (radius/4<CALC_BUFFER_SIZE)?(val1[radius/4]):(sign);  /* most significant digit */

  /* remove sign bits if mode was signed and this is an unsigned shift */
  if (!signed_shift && is_signed) {
    msd = and_table[_val(msd)][_val(max_digit[bitoffset])];
  }

  shrs = shrs_table[_val(msd)][shift];

  /* signed shift and signed mode and negative value means all bits to the left are set */
  if (signed_shift && is_signed && (_sign(val1) == -1)) {
    buffer[counter] = or_table[_val(shrs[0])][_val(min_digit[bitoffset])];
  } else {
    buffer[counter] = shrs[0];
  }

  if (counter > 0) buffer[counter - 1] = or_table[_val(buffer[counter-1])][_val(shrs[1])];

  /* fill with SC_F or SC_0 depending on sign */
  for (counter++; counter < CALC_BUFFER_SIZE; counter++)
  {
    buffer[counter] = sign;
  }
}

/**
 * Implements a Rotate Right.
 * positive: low-order -> high order, negative other direction
 */
static void _rot(const char *val1, char *buffer, long offset, int radius, unsigned is_signed)
{
  char *temp1, *temp2;
  temp1 = alloca(CALC_BUFFER_SIZE);
  temp2 = alloca(CALC_BUFFER_SIZE);

  offset = offset % radius;

  /* rotation by multiples of the typelength is identity */
  if (offset == 0) {
    memmove(buffer, val1, CALC_BUFFER_SIZE);
    return;
  }

  _shl(val1, temp1, offset, radius, is_signed);
  _shr(val1, temp2, radius - offset, radius, is_signed, 0);
  _bitor(temp1, temp2, buffer);
  carry_flag = 0; /* set by shr, but due to rot this is false */
}

/*****************************************************************************
 * public functions, declared in strcalc.h
 *****************************************************************************/
const void *sc_get_buffer(void)
{
  return (void*)calc_buffer;
}

const int sc_get_buffer_length(void)
{
  return CALC_BUFFER_SIZE;
}

/* XXX doesn't check for overflows */
void sc_val_from_str(const char *str, unsigned int len, void *buffer)
{
  const char *orig_str = str;
  unsigned int orig_len = len;

  char sign = 0;
  char *base, *val;

  base = alloca(CALC_BUFFER_SIZE);
  val = alloca(CALC_BUFFER_SIZE);

  /* verify valid pointers (not null) */
  assert(str);
  /* a string no characters long is an error */
  assert(len);

  if (buffer == NULL) buffer = calc_buffer;

  CLEAR_BUFFER(buffer);
  memset(base, SC_0, CALC_BUFFER_SIZE);
  memset(val, SC_0, CALC_BUFFER_SIZE);

  /* strip leading spaces */
  while ((len > 0) && (*str == ' ')) { len--; str++; }

  /* if the first two characters are 0x or 0X -> hex
   * if the first is a 0 -> oct
   * else dec, strip leading -/+ and remember sign
   *
   * only a + or - sign is no number resulting in an error */
  if (len >= 2)
    switch (str[0])
    {
      case '0':
        if (str[1] == 'x' || str[1] == 'X') /* hex */
        {
          str += 2;
          len -= 2;
          base[1] = SC_1; base[0] = SC_0;
        }
        else /* oct */
        {
          str += 1;
          len -= 1;
          base[1] = SC_0; base[0] = SC_8;
        }
        break;

      case '+':
        {
          str += 1;
          len -= 1;
          base[1] = SC_0; base[0] = SC_A;
        }
        break;

      case '-':
        {
          str += 1;
          len -= 1;
          sign = 1;
          base[1] = SC_0; base[0] = SC_A;
        }
        break;

      default: /* dec, else would have begun with 0x or 0 */
        base[1] = SC_0; base[0] = SC_A;
    }

  else /* dec, else would have begun with 0x or 0 */
  {
    base[1] = SC_0; base[0] = SC_A;
  }

  /* BEGIN string evaluation, from left to right */
  while (len > 0)
  {
    switch (*str)
    {
      case 'f':
      case 'e':
      case 'd':
      case 'c':
      case 'b':
      case 'a':
        if (base[0] > SC_9 || base[1] > SC_0) /* (base > 10) */
        {
          val[0] = _digit((*str)-'a'+10);
        }
        else fail_char(orig_str, orig_len, *str, str-orig_str+1);
        break;

      case 'F':
      case 'E':
      case 'D':
      case 'C':
      case 'B':
      case 'A':
        if (base[0] > SC_9 || base[1] > SC_0) /* (base > 10) */
        {
          val[0] = _digit((*str)-'A'+10);
        }
        else fail_char(orig_str, orig_len, *str, str-orig_str+1);
        break;

      case '9':
      case '8':
        if (base[0] > SC_7 || base[1] > SC_0) /* (base > 8) */
        {
          val[0] = _digit((*str)-'0');
        }
        else fail_char(orig_str, orig_len, *str, str-orig_str+1);
        break;

      case '7':
      case '6':
      case '5':
      case '4':
      case '3':
      case '2':
      case '1':
      case '0':
        {
          val[0] = _digit((*str)-'0');
        }
        break;

      default:
        fail_char(orig_str, orig_len, *str, str-orig_str+1);
    } /* switch(*str) */

    /* Radix conversion from base b to base B:
     *  (UnUn-1...U1U0)b == ((((Un*b + Un-1)*b + ...)*b + U1)*b + U0)B */
    _mul(base, calc_buffer, calc_buffer); /* multiply current value with base */
    _add(val, calc_buffer, calc_buffer);  /* add next digit to current value  */

    /* get ready for the next letter */
    str++;
    len--;

  } /* while (len > 0 ) */

  if (sign)
  {
    _negate(calc_buffer, calc_buffer);
  }
}

void sc_val_from_long(long value, void *buffer)
{
  char *pos;
  char sign, is_minlong;

  if (buffer == NULL) buffer = calc_buffer;
  pos = buffer;

  sign = (value < 0);
  is_minlong = value == LONG_MIN;

  /* use absolute value, special treatment of MIN_LONG */
  if (sign) {
    if (is_minlong)
      value = -(value+1);
    else
      value = -value;
  }

  CLEAR_BUFFER(buffer);

  while ((value != 0) && (pos < (char*)buffer + CALC_BUFFER_SIZE))
  {
    *pos++ = _digit(value & 0xf);
    value >>= 4;
  }


  if (sign) {
    if (is_minlong)
      _inc(buffer, buffer);

    _negate(buffer, buffer);
  }
}

void sc_val_from_ulong(unsigned long value, void *buffer)
{
  char *pos;

  if (buffer == NULL) buffer = calc_buffer;
  pos = buffer;

  while (pos < (char*)buffer + CALC_BUFFER_SIZE)
  {
    *pos++ = _digit(value & 0xf);
    value >>= 4;
  }
}

long sc_val_to_long(const void *val)
{
  int i;
  long l = 0;

  for (i = CALC_BUFFER_SIZE - 1; i >= 0; i--)
  {
    l = (l << 4) + _val(((char *)val)[i]);
  }
  return l;
}

void sc_min_from_bits(unsigned int num_bits, unsigned int sign, void *buffer)
{
  char* pos;
  int i, bits;

  if (buffer == NULL) buffer = calc_buffer;
  CLEAR_BUFFER(buffer);

  if (!sign) return;  /* unsigned means minimum is 0(zero) */

  pos = buffer;

  bits = num_bits - 1;
  for (i = 0; i < bits/4; i++)
    *pos++ = SC_0;

  *pos++ = min_digit[bits%4];

  for (i++; i <= CALC_BUFFER_SIZE - 1; i++)
    *pos++ = SC_F;
}

void sc_max_from_bits(unsigned int num_bits, unsigned int sign, void *buffer)
{
  char* pos;
  int i, bits;

  if (buffer == NULL) buffer = calc_buffer;
  CLEAR_BUFFER(buffer);
  pos = buffer;

  bits = num_bits - sign;
  for (i = 0; i < bits/4; i++)
    *pos++ = SC_F;

  *pos++ = max_digit[bits%4];

  for (i++; i <= CALC_BUFFER_SIZE - 1; i++)
    *pos++ = SC_0;
}

void sc_calc(const void* value1, const void* value2, unsigned op, void *buffer)
{
  char unused_res[CALC_BUFFER_SIZE]; /* temp buffer holding unused result of divmod */

  const char *val1 = (const char *)value1;
  const char *val2 = (const char *)value2;

  CLEAR_BUFFER(calc_buffer);
  carry_flag = 0;

  DEBUGPRINTF_COMPUTATION(("%s ", sc_print_hex(value1)));

  switch (op)
  {
    case SC_NEG:
      _negate(val1, calc_buffer);
      DEBUGPRINTF_COMPUTATION(("negated: %s\n", sc_print_hex(calc_buffer)));
      break;
    case SC_OR:
      DEBUGPRINTF_COMPUTATION(("| "));
      _bitor(val1, val2, calc_buffer);
      break;
    case SC_AND:
      DEBUGPRINTF_COMPUTATION(("& "));
      _bitand(val1, val2, calc_buffer);
      break;
    case SC_XOR:
      DEBUGPRINTF_COMPUTATION(("^ "));
      _bitxor(val1, val2, calc_buffer);
      break;
    case SC_NOT:
      _bitnot(val1, calc_buffer);
      DEBUGPRINTF_COMPUTATION(("bit-negated: %s\n", sc_print_hex(calc_buffer)));
      break;
    case SC_ADD:
      DEBUGPRINTF_COMPUTATION(("+ "));
      _add(val1, val2, calc_buffer);
      break;
    case SC_SUB:
      DEBUGPRINTF_COMPUTATION(("- "));
      _sub(val1, val2, calc_buffer);
      break;
    case SC_MUL:
      DEBUGPRINTF_COMPUTATION(("* "));
      _mul(val1, val2, calc_buffer);
      break;
    case SC_DIV:
      DEBUGPRINTF_COMPUTATION(("/ "));
      _divmod(val1, val2, calc_buffer, unused_res);
      break;
    case SC_MOD:
      DEBUGPRINTF_COMPUTATION(("%% "));
      _divmod(val1, val2, unused_res, calc_buffer);
      break;
    default:
      assert(0);
  }
  DEBUGPRINTF_COMPUTATION(("%s -> ", sc_print_hex(value2)));
  DEBUGPRINTF_COMPUTATION(("%s\n", sc_print_hex(calc_buffer)));

  if ((buffer != NULL) && (buffer != calc_buffer))
  {
    memcpy(buffer, calc_buffer, CALC_BUFFER_SIZE);
  }
}

void sc_bitcalc(const void* value1, const void* value2, int radius, int sign, unsigned op, void* buffer)
{
  const char *val1 = (const char *)value1;
  const char *val2 = (const char *)value2;
  long offset;

  carry_flag = 0;
  offset = sc_val_to_long(val2);

  DEBUGPRINTF_COMPUTATION(("%s ", sc_print_hex(value1)));
  switch (op)
  {
    case SC_SHL:
      DEBUGPRINTF_COMPUTATION(("<< %ld ", offset));
      _shl(val1, calc_buffer, offset, radius, sign);
      break;
    case SC_SHR:
      DEBUGPRINTF_COMPUTATION((">> %ld ", offset));
      _shr(val1, calc_buffer, offset, radius, sign, 0);
      break;
    case SC_SHRS:
      DEBUGPRINTF_COMPUTATION((">>> %ld ", offset));
      _shr(val1, calc_buffer, offset, radius, sign, 1);
      break;
    case SC_ROT:
      DEBUGPRINTF_COMPUTATION(("<<>> %ld ", offset));
      _rot(val1, calc_buffer, offset, radius, sign);
      break;
    default:
      assert(0);
  }
  DEBUGPRINTF_COMPUTATION(("-> %s\n", sc_print_hex(calc_buffer)));

  if ((buffer != NULL) && (buffer != calc_buffer))
  {
    memmove(buffer, calc_buffer, CALC_BUFFER_SIZE);
  }
}

int sc_comp(const void* value1, const void* value2)
{
  int counter = CALC_BUFFER_SIZE - 1;
  const char *val1 = (const char *)value1;
  const char *val2 = (const char *)value2;

  /* compare signs first:
   * the loop below can only compare values of the same sign! */
  if (_sign(val1) != _sign(val2)) return (_sign(val1) == 1)?(1):(-1);

  /* loop until two digits differ, the values are equal if there
   * are no such two digits */
  while (val1[counter] == val2[counter])
  {
    counter--;
    if (counter < 0) return 0;
  }

  /* the leftmost digit is the most significant, so this returns
   * the correct result.
   * This implies the digit enum is ordered */
  return (val1[counter] > val2[counter]) ? (1) : (-1);
}

int sc_get_highest_set_bit(const void *value)
{
  const char *val = (const char*)value;
  int high, counter;

  high = CALC_BUFFER_SIZE * 4 - 1;

  for (counter = CALC_BUFFER_SIZE-1; counter >= 0; counter--) {
    if (val[counter] == SC_0) high -= 4;
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
  int low, counter;
  char sign;

  sign = (_sign(val)==1)?(SC_0):(SC_F);
  low = 0;

  for (counter = 0; counter < CALC_BUFFER_SIZE; counter++) {
    if (val[counter] == SC_0) low += 4;
    else {
      if (val[counter] < SC_2) return low;
      else if (val[counter] < SC_4) return low + 1;
      else if (val[counter] < SC_8) return low + 2;
      else return low + 3;
    }
  }
  return -1;
}

int sc_is_zero(const void *value)
{
  const char* val = (const char *)value;
  int counter;

  for (counter = 0; counter < CALC_BUFFER_SIZE; counter++) {
    if (val[counter] != SC_0) return 0;
  }
  return 1;
}

int sc_is_negative(const void *value)
{
  return _sign(value) == -1;
}

int sc_had_carry(void)
{
  return carry_flag;
}

unsigned char sc_sub_bits(const void *value, int len, unsigned byte_ofs)
{
  const char *val     = (const char *)value;
  unsigned nibble_ofs = 2 * byte_ofs;
  unsigned char res;

  /* the current scheme uses one byte to store a nibble */
  if (nibble_ofs >= len)
    return 0;

  res = _val(val[nibble_ofs]);
  if (len > nibble_ofs + 1)
    res |= _val(val[nibble_ofs + 1]) << 4;

  return res;
}

/*
 * convert to a string
 * XXX Doesn't check buffer bounds
 */
const char *sc_print(const void *value, unsigned bits, enum base_t base)
{
  static const char big_digits[]   = "0123456789ABCDEF";
  static const char small_digits[] = "0123456789abcdef";

  char *base_val, *div1_res, *div2_res, *rem_res;
  int counter, nibbles, i, sign;
  char x;

  const char *val = (const char *)value;
  const char *p;
  char *m, *n, *t;
  char *pos;
  const char *digits = small_digits;

  base_val = alloca(CALC_BUFFER_SIZE);
  div1_res = alloca(CALC_BUFFER_SIZE);
  div2_res = alloca(CALC_BUFFER_SIZE);
  rem_res = alloca(CALC_BUFFER_SIZE);

  pos = output_buffer + BIT_PATTERN_SIZE;
  *(--pos) = '\0';

  /* special case */
  if (bits == 0) {
    bits = BIT_PATTERN_SIZE;
#ifdef STRCALC_DEBUG_FULLPRINT
    bits <<= 1;
#endif
  }
  nibbles = bits >> 2;
  switch (base) {

  case SC_HEX:
    digits = big_digits;
  case SC_hex:
    for (counter = 0; counter < nibbles; ++counter) {
      *(--pos) = digits[_val(val[counter])];
#ifdef STRCALC_DEBUG_GROUPPRINT
      if ((counter+1)%8 == 0)
        *(--pos) = ' ';
#endif
    }

    /* last nibble must be masked */
    if (bits & 3) {
      x = and_table[_val(val[++counter])][bits & 3];
      *(--pos) = digits[_val(x)];
    }

    /* now kill zeros */
    for (; counter > 1; --counter, ++pos) {
#ifdef STRCALC_DEBUG_GROUPPRINT
      if (pos[0] == ' ') ++pos;
#endif
      if (pos[0] != '0')
	break;
    }
    break;

  case SC_BIN:
    for (counter = 0; counter < nibbles; ++counter) {
      pos -= 4;
      p = binary_table[_val(val[counter])];
      pos[0] = p[0];
      pos[1] = p[1];
      pos[2] = p[2];
      pos[3] = p[3];
    }

    /* last nibble must be masked */
    if (bits & 3) {
      x = and_table[_val(val[++counter])][bits & 3];

      pos -= 4;
      p = binary_table[_val(x)];
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
    memset(base_val, SC_0, CALC_BUFFER_SIZE);
    base_val[0] = base == SC_DEC ? SC_A : SC_8;

    p    = val;
    sign = 0;
    if (base == SC_DEC) {
      /* check for negative values */
      if (_bit(val, bits - 1)) {
	_negate(val, div2_res);
	sign = 1;
	p = div2_res;
      }
    }

    /* transfer data into oscilating buffers */
    memset(div1_res, SC_0, CALC_BUFFER_SIZE);
    for (counter = 0; counter < nibbles; ++counter)
      div1_res[counter] = p[counter];

     /* last nibble must be masked */
    if (bits & 3) {
      ++counter;

      div1_res[counter] = and_table[_val(p[counter])][bits & 3];
    }

    m = div1_res;
    n = div2_res;
    for (;;) {
      _divmod(m, base_val, n, rem_res);
      t = m;
      m = n;
      n = t;
      *(--pos) = digits[_val(rem_res[0])];

      x = 0;
      for (i = 0; i < sizeof(div1_res); ++i)
     	x |= _val(m[i]);

      if (x == 0)
	break;
    }
    if (sign)
       *(--pos) = '-';
    break;

  default:
    printf("%i\n", base);
    assert(0);
    return NULL;
}
  return pos;
}

void init_strcalc(int precision)
{
  if (calc_buffer == NULL) {
    if (precision <= 0) precision = SC_DEFAULT_PRECISION;

    /* round up to multiple of 4 */
    precision = (precision + 3) & ~3;

    BIT_PATTERN_SIZE = (precision);
    CALC_BUFFER_SIZE = (precision / 2);
    MAX_VALUE_SIZE   = (precision / 4);

    calc_buffer   = malloc(CALC_BUFFER_SIZE+1 * sizeof(char));
    output_buffer = malloc(BIT_PATTERN_SIZE+1 * sizeof(char));

    if (calc_buffer == NULL || output_buffer == NULL) {
      assert(0 && "malloc failed");
      exit(-1);
    }

    DEBUGPRINTF(("init strcalc: \n\tPRECISION: %d\n\tCALC_BUFFER_SIZE = %d\n\tMAX_VALUE_SIZE = %d\n\tbuffer pointer: %p\n", precision, CALC_BUFFER_SIZE, MAX_VALUE_SIZE, calc_buffer));
  }
}


void finish_strcalc() {
  free(calc_buffer);   calc_buffer   = NULL;
  free(output_buffer); output_buffer = NULL;
}

int sc_get_precision(void)
{
  return BIT_PATTERN_SIZE;
}
