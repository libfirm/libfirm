/****i* strcalc/implementation
 *
 * AUTHORS
 *    Matthias Heil
 *
 * NOTES
 ******/

#include <assert.h>   /* assertions */
#include <string.h>   /* memset/memcmp */

#include "strcalc.h"

#include <stdio.h>    /* output for error messages */
#include <stdlib.h>

/*****************************************************************************
 * local definitions and macros
 *****************************************************************************/
#define BIT_PATTERN_SIZE (8 * BIGGEST_INTEGER_SIZE_IN_BYTES)
#define CALC_BUFFER_SIZE (4 * BIGGEST_INTEGER_SIZE_IN_BYTES)
#define MAX_VALUE_SIZE   (2 * BIGGEST_INTEGER_SIZE_IN_BYTES)

#define CLEAR_CALC_BUFFER() assert(calc_buffer); memset(calc_buffer, SC_0, CALC_BUFFER_SIZE)
#define _val(a) ((a)-SC_0)
#define _digit(a) ((a)+SC_0)
#define _bitisset(digit, pos) (and_table[_val(digit)][_val(shift_table[pos])] != SC_0)

#define fail_char(a, b, c, d) _fail_char((a), (b), (c), (d), __FILE__,  __LINE__)

#if 0
#  define DEBUGPRINTF(x) printf x
#else
#  define DEBUGPRINTF(x) ((void)0)
#endif

/*****************************************************************************
 * private variables
 *****************************************************************************/

static char calc_buffer[CALC_BUFFER_SIZE];    /* buffer holding all results */

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
                              SC_8, SC_9, SC_8, SC_9, SC_D, SC_E, SC_D, SC_E },

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

static void _bitnot(const char *val, char *buffer)
{
  int counter;

  for (counter = 0; counter<CALC_BUFFER_SIZE; counter++)
    buffer[counter] = not_table[_val(val[counter])];
}

static void _bitor(const char *val1, const char *val2, char *buffer)
{
  int counter;

  for (counter = 0; counter<CALC_BUFFER_SIZE; counter++)
    buffer[counter] = or_table[_val(val1[counter])][_val(val2[counter])];
}

static void _bitxor(const char *val1, const char *val2, char *buffer)
{
  int counter;

  for (counter = 0; counter<CALC_BUFFER_SIZE; counter++)
    buffer[counter] = xor_table[_val(val1[counter])][_val(val2[counter])];
}

static void _bitand(const char *val1, const char *val2, char *buffer)
{
  int counter;

  for (counter = 0; counter<CALC_BUFFER_SIZE; counter++)
    buffer[counter] = and_table[_val(val1[counter])][_val(val2[counter])];
}

static int _sign(const char *val)
{
  return (val[CALC_BUFFER_SIZE-1] < SC_7) ? (1) : (-1);
}

static void _inc(char *val, char *buffer)
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
  /* here a carry could be lost, this is intended because this will only
   * happen when a value changes sign. */
}

static void _negate(const char *val, char *buffer)
{
  _bitnot(val, buffer);
  _inc(buffer, buffer);
}

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
  /* loose last carry, which will occur only when changing sign */
}

static void _mul(const char *val1, const char *val2, char *buffer)
{
  char temp_buffer[CALC_BUFFER_SIZE]; /* result buffer */
  char neg_val1[CALC_BUFFER_SIZE];    /* abs of val1 */
  char neg_val2[CALC_BUFFER_SIZE];    /* abs of val2 */

  const char *mul, *add1, *add2;      /* intermediate result containers */
  char carry = SC_0;                  /* container for carries */
  char sign = 0;                      /* marks result sign */
  int c_inner, c_outer;               /* loop counters */

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
    }
  }

  if (sign)
    _negate(temp_buffer, buffer);
  else
    memcpy(buffer, temp_buffer, CALC_BUFFER_SIZE);
}

static void _sub(const char *val1, const char *val2, char *buffer)
{
  char temp_buffer[CALC_BUFFER_SIZE];  /* intermediate buffer to hold -val2 */

  _negate(val2, temp_buffer);
  _add(val1, temp_buffer, buffer);
}

static void _push(const char digit, char *buffer)
{
  int counter;

  for (counter = CALC_BUFFER_SIZE - 2; counter >= 0; counter--)
  {
    buffer[counter+1] = buffer[counter];
  }
  buffer[0] = digit;
}

/* XXX: This is MOST slow */
static void _divmod(const char *dividend, const char *divisor, char *quot, char *rem)
{
  const char *minus_divisor;
  char neg_val1[CALC_BUFFER_SIZE];
  char neg_val2[CALC_BUFFER_SIZE];

  char sign = 0;     /* remember result sign */

  int c_dividend;      /* loop counters */

  /* clear result buffer */
  memset(quot, SC_0, CALC_BUFFER_SIZE);
  memset(rem, SC_0, CALC_BUFFER_SIZE);

  /* if the dividend is zero result is zero (quot is zero)*/
  if (sc_comp(dividend, quot) == 0) return;
  /* if the divisor is zero this won't work (quot is zero) */
  if (sc_comp(divisor, quot) == 0) assert(0 && "quotision by zero!");

  if (_sign(dividend) == -1)
  {
    _negate(dividend, neg_val1);
    sign ^= 1;
    dividend = neg_val1;
  }

  _negate(divisor, neg_val2);
  if (_sign(divisor) == -1)
  {
    sign ^= 1;
    minus_divisor = divisor;
    divisor = neg_val2;
  }
  else
  {
    minus_divisor = neg_val2;
  }

  /* if divisor >= dividend quotision is easy
   * (remember these are absolute values) */
  switch (sc_comp(dividend, divisor))
  {
    case 0: /* dividend == divisor */
      quot[0] = SC_1;
      return;

    case -1: /* dividend < divisor */
      memcpy(rem, dividend, CALC_BUFFER_SIZE);
      return;

    default: /* unluckily quotision is necessary :( */
      break;
  }

  for (c_dividend = MAX_VALUE_SIZE - 1; c_dividend >= 0; c_dividend--)
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

  if (sign)
  {
    _negate(quot, quot);
    _negate(rem, rem);
  }
}

static void _shl(const char *val1, const char *val2, char *buffer, unsigned radius, unsigned is_signed)
{
  const char *shl;
  char shift;
  char carry = SC_0;

  int counter;
  int offset = 0;
  int bitoffset = 0;

  assert((_sign(val2) != -1) || (0 && "negative leftshift"));
  assert(((_sign(val1) != -1) || is_signed) || (0 && "unsigned mode and negative value"));
  assert(((!_bitisset(val1[(radius-1)/4], (radius-1)%4)) || !is_signed || (_sign(val1) == -1)) || (0 && "value is positive, should be negative"));
  assert(((_bitisset(val1[(radius-1)/4], (radius-1)%4)) || !is_signed || (_sign(val1) == 1)) || (0 && "value is negative, should be positive"));

  /* the whole value must be moved left the number of bytes represented
   * by the value in quot, with bytes to the right set to zero */
  /*XXX This might result in trouble */
  for (counter = MAX_VALUE_SIZE - 1; counter >= 0; counter--)
  {
    offset = (offset << 4) | (_val(val2[counter]));
  }

  shift = shift_table[_val(offset%4)];      /* this is 2 ** (val2 % 4) */

  /* if shifting far enough the result is zero */
  if (offset >= radius)
  {
    memset(buffer, SC_0, CALC_BUFFER_SIZE);
    return;
  }
  offset = offset / 4;

  /* shift the single digits some bytes (offset) and some bits (table)
   * to the left */
  for (counter = 0; counter < CALC_BUFFER_SIZE - offset; counter++)
  {
    shl = mul_table[_val(val1[counter])][_val(shift)];
    buffer[counter + offset] = or_table[_val(shl[0])][_val(carry)];
    carry = shl[1];
  }

  /* fill with zeroes */
  for (counter = 0; counter < offset; counter++) buffer[counter] = 0;
  /* if the mode was signed, change sign when the mode's msb is now 1 */
  offset = (radius - 1) / 4;
  bitoffset = (radius - 1) % 4;
  if (is_signed && _bitisset(buffer[offset], bitoffset) && (_sign(buffer) == 1))
  {
    /* this sets the upper bits of the leftmost digit */
    buffer[offset] = or_table[_val(buffer[offset])][_val(min_digit[bitoffset])];
    for (counter = offset+1; counter < CALC_BUFFER_SIZE; counter++)
    {
      buffer[counter] = SC_F;
    }
  }
  else if (is_signed && !_bitisset(buffer[offset], bitoffset) && (_sign(buffer) == -1))
  {
    /* this unsets the upper bits of the leftmost digit */
    buffer[offset] = and_table[_val(buffer[offset])][_val(max_digit[bitoffset])];
    /* zero the upper part of the value */
    for (counter = offset+1; counter < CALC_BUFFER_SIZE; counter++)
    {
      buffer[counter] = SC_0;
    }
  }
}

static void _shr(const char *val1, const char *val2, char *buffer, unsigned radius, unsigned is_signed, int signed_shift)
{
  const char *shrs;
  char sign;
  char msd;

  int shift;

  int counter;
  int offset = 0;
  int bitoffset = 0;

  assert((_sign(val2) != -1) || (0 && "negative rightshift"));
  assert(((_sign(val1) != -1) || is_signed) || (0 && "unsigned mode and negative value"));
  assert(((!_bitisset(val1[(radius-1)/4], (radius-1)%4)) || !is_signed || (_sign(val1) == -1)) || (0 && "value is positive, should be negative"));
  assert(((_bitisset(val1[(radius-1)/4], (radius-1)%4)) || !is_signed || (_sign(val1) == 1)) || (0 && "value is negative, should be positive"));

  /*XXX get the value of val2, this might result in trouble *
   * (but who wants shifts THAT far anyway)                 */
  for (counter = MAX_VALUE_SIZE - 1; counter >= 0; counter--)
  {
    offset = (offset << 4) | (_val(val2[counter]));
  }

  shift = offset % 4;     /* this is val2 % 4 */

  sign = ((signed_shift) && (_sign(val1) == -1))?(SC_F):(SC_0);
  /* if shifting far enough the result is either 0 or -1 */
  if (offset >= radius)
  {
    memset(buffer, sign, CALC_BUFFER_SIZE);
    return;
  }
  offset = offset / 4;

  buffer[0] = shrs_table[_val(val1[offset])][shift][0];
  /* shift digits to the right with offset, carry and all */
  for (counter = 1; counter < radius/4; counter++)
  {
    shrs = shrs_table[_val(val1[counter + offset])][shift];
    buffer[counter] = shrs[0];
    buffer[counter-1] = or_table[_val(buffer[counter-1])][_val(shrs[1])];
  }

  /* the last digit is special in regard of signed/unsigned shift */
  /* counter = radius/4 (after for loop) */
  bitoffset = radius%4;
  msd = val1[counter];  /* most significant digit */

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
  buffer[counter - 1] = or_table[_val(buffer[counter-1])][_val(shrs[1])];

  /* fill with SC_F or SC_0 depending on sign */
  for (counter++; counter < CALC_BUFFER_SIZE; counter++)
  {
    buffer[counter] = sign;
  }
}

/* positive: low-order -> high order, negative other direction */
static void _rot(const char *val1, const char *val2, char *buffer, unsigned radius, unsigned is_signed)
{
  char temp_buffer[CALC_BUFFER_SIZE];

  const char *shl;
  char carry = SC_0;

  int counter, old_counter;
  int shift;
  int offset = 0;
  int bitoffset;

  /*XXX get the value of val2, this might result in trouble *
   * (but who wants shifts THAT far anyway)                 */
  for (counter = MAX_VALUE_SIZE - 1; counter >= 0; counter--)
  {
    offset = (offset << 4) | (_val(val2[counter]));
  }
  /* rotation by multiples of the typelength is identity */
  offset = offset % radius;
  if (offset == 0) {
    memmove(buffer, val1, CALC_BUFFER_SIZE);
    return;
  }
  /* rotation to the right is the same as rotation to the left
   * when done by the right amount */
  if (offset < 0) offset = radius + offset;

  shift = _val(shift_table[offset % 4]);
  offset = offset / 4;

  DEBUGPRINTF(("offset: %d, shift: %d\n", offset, shift));
  for (counter = 0; counter < radius/4 - offset; counter++)
  {
    shl = mul_table[_val(val1[counter])][_val(shift)];
    temp_buffer[counter + offset] = or_table[_val(shl[0])][_val(carry)];
    carry = shl[1];
    DEBUGPRINTF(("%d(%x): %s\n", counter, shl[0], sc_print_hex(temp_buffer)));
  }
  old_counter = counter;
  for (; counter < radius/4; counter++)
  {
    shl = mul_table[_val(val1[counter])][_val(shift)];
    temp_buffer[counter - old_counter] = or_table[_val(shl[0])][_val(carry)];
    carry = shl[1];
    DEBUGPRINTF(("%d(%x)> %s\n", counter, shl[0], sc_print_hex(temp_buffer)));
  }
  temp_buffer[counter - old_counter] = or_table[_val(temp_buffer[counter-old_counter])][_val(carry)];

  offset = (radius-1)/4;
  bitoffset - (radius-1)%4;
  /* fill the rest of the buffer depending on msb and mode signedness*/
  if (is_signed && _bitisset(temp_buffer[offset], bitoffset))
  {

  }
  else
  {

  }

  memcpy(buffer, temp_buffer, CALC_BUFFER_SIZE);
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

void sc_val_from_str(const char *str, unsigned int len)
{
  const char *orig_str = str;
  unsigned int orig_len = len;

  char sign = 0;
  char base[CALC_BUFFER_SIZE];
  char val[CALC_BUFFER_SIZE];

  /* verify valid pointers (not null) */
  assert(str);
  /* a string no characters long is an error */
  assert(len);

  CLEAR_CALC_BUFFER();
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

  /* begin string evaluation, from left to right */
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

void sc_val_from_long(long value)
{
  char *pos;
  int sign;

  pos = calc_buffer;
  sign = (value < 0);

  /* FIXME MININT won't work */
  if (sign) value = -value;

  CLEAR_CALC_BUFFER();

  while ((value != 0) && (pos < calc_buffer + CALC_BUFFER_SIZE))
  {
    *pos++ = _digit(value % 16);
    value /= 16;
  }

  if (sign) _negate(calc_buffer, calc_buffer);
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

void sc_min_from_bits(unsigned int num_bits, unsigned int sign)
{
  char* pos;
  int i, bits;

  CLEAR_CALC_BUFFER();
  if (!sign) return;  /* unsigned means minimum is 0(zero) */

  pos = calc_buffer;

  bits = num_bits - 1;
  for (i = 0; i < bits/4; i++)
    *pos++ = SC_0;

  *pos++ = min_digit[bits%4];

  for (i++; i <= CALC_BUFFER_SIZE - 1; i++)
    *pos++ = SC_F;
}

void sc_max_from_bits(unsigned int num_bits, unsigned int sign)
{
  char* pos;
  int i, bits;

  CLEAR_CALC_BUFFER();
  pos = calc_buffer;

  bits = num_bits - sign;
  for (i = 0; i < bits/4; i++)
    *pos++ = SC_F;

  *pos++ = max_digit[bits%4];

  for (i++; i <= CALC_BUFFER_SIZE - 1; i++)
    *pos++ = SC_0;
}

void sc_calc(const void* value1, const void* value2, unsigned op)
{
  char unused_res[CALC_BUFFER_SIZE]; /* temp buffer holding unused result of divmod */

  const char *val1 = (const char *)value1;
  const char *val2 = (const char *)value2;
  CLEAR_CALC_BUFFER();

  DEBUGPRINTF(("%s ", sc_print(value1, SC_HEX)));

  switch (op)
  {
    case SC_NEG:
      _negate(val1, calc_buffer);
      DEBUGPRINTF(("negated: %s\n", sc_print_hex(calc_buffer)));
      return;
    case SC_OR:
      DEBUGPRINTF(("| "));
      _bitor(val1, val2, calc_buffer);
      break;
    case SC_AND:
      DEBUGPRINTF(("& "));
      _bitand(val1, val2, calc_buffer);
      break;
    case SC_XOR:
      DEBUGPRINTF(("^ "));
      _bitxor(val1, val2, calc_buffer);
      break;
    case SC_NOT:
      _bitnot(val1, calc_buffer);
      DEBUGPRINTF(("bit-negated: %s\n", sc_print_hex(calc_buffer)));
      return;
    case SC_ADD:
      DEBUGPRINTF(("+ "));
      _add(val1, val2, calc_buffer);
      break;
    case SC_SUB:
      DEBUGPRINTF(("- "));
      _sub(val1, val2, calc_buffer);
      break;
    case SC_MUL:
      DEBUGPRINTF(("* "));
      _mul(val1, val2, calc_buffer);
      break;
    case SC_DIV:
      DEBUGPRINTF(("/ "));
      _divmod(val1, val2, calc_buffer, unused_res);
      break;
    case SC_MOD:
      DEBUGPRINTF(("%% "));
      _divmod(val1, val2, unused_res, calc_buffer);
      break;
    default:
      assert(0);
  }
  DEBUGPRINTF(("%s -> ", sc_print_hex(value2)));
  DEBUGPRINTF(("%s\n", sc_print_hex(calc_buffer)));
}

void sc_bitcalc(const void* value1, const void* value2, unsigned radius, unsigned sign, unsigned op)
{
  const char *val1 = (const char *)value1;
  const char *val2 = (const char *)value2;
  CLEAR_CALC_BUFFER();

  DEBUGPRINTF(("%s ", sc_print_hex(value1)));
  switch (op)
  {
    case SC_SHL:
      DEBUGPRINTF(("<< "));
      _shl(val1, val2, calc_buffer, radius, sign);
      break;
    case SC_SHR:
      DEBUGPRINTF((">> "));
      _shr(val1, val2, calc_buffer, radius, sign, 0);
      break;
    case SC_SHRS:
      DEBUGPRINTF((">>> "));
      _shr(val1, val2, calc_buffer, radius, sign, 1);
      break;
    case SC_ROT:
      DEBUGPRINTF(("<<>> "));
      _rot(val1, val2, calc_buffer, radius, sign);
      break;
    default:
      assert(0);
  }
  DEBUGPRINTF(("%s -> ", sc_print_hex(value2)));
  DEBUGPRINTF(("%s\n", sc_print_hex(calc_buffer)));
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

const char *sc_print(const void *value, unsigned base)
{
  int counter;

  const char *val = (const char *)value;
  char *pos;
  static char *buf = NULL;

  if (buf != NULL) free(buf);
  buf = malloc(BIT_PATTERN_SIZE);

  pos = buf + BIT_PATTERN_SIZE - 1;
  *pos = '\0';

  switch (base)
  {
    case SC_HEX:
      for (counter = 0; counter < MAX_VALUE_SIZE; counter++)
      {
        if (val[counter] < SC_A)
          *(--pos) = val[counter] + '0';
        else
          *(--pos) = val[counter] + 'a' - 10;
      }
      break;

    default:
      assert(0);
  }
  return pos;
}
