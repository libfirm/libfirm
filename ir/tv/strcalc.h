  /****h* tools/strcalc
 *
 * NAME
 *   strcalc -- calculations using strings
 *   Provides basic mathematical operations on values represented as strings
 *
 * AUTHORS
 *   Matthias Heil
 *
 * DESCRIPTION
 *    The module uses a string to represent values, and provides operations
 *   to perform calculations with these values.
 *    Results are stored in an internal buffer, so you have to make a copy
 *   of them if you need to store the result.
 *
 ******/

#ifndef _STRCALC_H_
#define _STRCALC_H_

#define BIGGEST_INTEGER_SIZE_IN_BYTES 8
#define SCDEBUG

/*****************************************************************************
 * typedefs, enums and structs
 *****************************************************************************/
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
  SC_F,
};

enum {
  SC_ADD = 0,
  SC_SUB,
  SC_NEG,
  SC_MUL,
  SC_DIV,
  SC_MOD,
  SC_SHL,
  SC_SHR,
  SC_SHRS,
  SC_ROT,
  SC_AND,
  SC_OR,
  SC_NOT,
  SC_XOR,
};

enum {
  SC_HEX,
  SC_DEC,
  SC_OKT,
  SC_BIN,
};

/*****************************************************************************
 * definitions and macros
 *****************************************************************************/
#define sc_add(a, b) sc_calc((a), (b), SC_ADD)
#define sc_sub(a, b) sc_calc((a), (b), SC_SUB)
#define sc_neg(a) sc_calc((a), NULL, SC_NEG)
#define sc_and(a, b) sc_calc((a), (b), SC_AND)
#define sc_or(a, b) sc_calc((a), (b), SC_OR)
#define sc_xor(a, b) sc_calc((a), (b), SC_XOR)
#define sc_not(a) sc_calc((a), NULL, SC_NOT)
#define sc_mul(a, b) sc_calc((a), (b), SC_MUL)
#define sc_div(a, b) sc_calc((a), (b), SC_DIV)
#define sc_mod(a, b) sc_calc((a), (b), SC_MOD)
#define sc_shl(a, b, c, d) sc_bitcalc((a), (b), (c), (d), SC_SHL)
#define sc_shr(a, b, c, d) sc_bitcalc((a), (b), (c), (d), SC_SHR)
#define sc_shrs(a, b, c, d) sc_bitcalc((a), (b), (c), (d), SC_SHRS)
#define sc_rot(a, b, c, d) sc_bitcalc((a), (b), (c), (d), SC_ROT)

#define sc_print_hex(a) sc_print((a), SC_HEX)
#define sc_print_dec(a) sc_print((a), SC_DEC)
#define sc_print_okt(a) sc_print((a), SC_OKT)
#define sc_print_bin(a) sc_print((a), SC_BIN)
/*****************************************************************************
 * function declarations
 *****************************************************************************/
const void *sc_get_buffer(void);
const int sc_get_buffer_length(void);

void sc_val_from_str(const char *str, unsigned int len);
void sc_val_from_long(long l);
long sc_val_to_long(const void *val);
void sc_min_from_bits(unsigned int num_bits, unsigned int sign);
void sc_max_from_bits(unsigned int num_bits, unsigned int sign);

void sc_calc(const void *val1, const void *val2, unsigned op);
void sc_bitcalc(const void *val1, const void *val2, unsigned radius, unsigned sign, unsigned op);
int  sc_comp(const void *val1, const void *val2);

char* sc_print(const void *val1, unsigned base);

#endif /* _STRCALC_H_ */
