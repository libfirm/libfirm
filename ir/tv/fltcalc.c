/*
 * Project:     libFIRM
 * File name:   ir/tv/fltcalc.c
 * Purpose:
 * Author:
 * Modified by:
 * Created:     2003
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


#ifdef HAVE_CONFIG_H
# include "config.h"
#endif


#include "fltcalc.h"
#include "strcalc.h"

#include <math.h>    /* need isnan() and isinf() (will be changed)*/
/* undef some reused constants defined by math.h */
#ifdef NAN
#  undef NAN
#endif

#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#ifdef HAVE_ALLOCA_H
# include <alloca.h>
#endif
#ifdef HAVE_MALLOC_H
# include <malloc.h>
#endif

typedef uint32_t UINT32;

#ifdef HAVE_LONG_DOUBLE
#ifdef WORDS_BIGENDIAN
typedef union {
  struct {
    UINT32 high;
    UINT32 mid;
    UINT32 low;
  } val;
  volatile long double d;
} value_t;
#else
typedef union {
  struct {
    UINT32 low;
    UINT32 mid;
    UINT32 high;
  } val;
  volatile long double d;
} value_t;
#endif
#else
#ifdef WORDS_BIGENDIAN
typedef union {
  struct {
    UINT32 high;
    UINT32 low;
  } val;
  volatile double d;
} value_t;
#else
typedef union {
  struct {
    UINT32 low;
    UINT32 high;
  } val;
  volatile double d;
} value_t;
#endif
#endif


/********
 * globals
 ********/
typedef enum {
  NORMAL,
  ZERO,
  SUBNORMAL,
  INF,
  NAN,
} value_class_t;

typedef struct {
  char  exponent_size;
  char  mantissa_size;
  value_class_t  class;
} descriptor_t;

#define CLEAR_BUFFER(buffer) memset(buffer, 0, CALC_BUFFER_SIZE)

/* because variable sized structs are impossible, the internal
 * value is represented as a pseudo-struct char array, addressed
 * by macros
 * struct {
 *   char sign;             //  0 for positive, 1 for negative
 *   char exp[VALUE_SIZE];
 *   char mant[VALUE_SIZE];
 *   descriptor_t desc;
 * };
 */
#define _sign(a) (((char*)a)[SIGN_POS])
#define _exp(a) (&((char*)a)[EXPONENT_POS])
#define _mant(a) (&((char*)a)[MANTISSA_POS])
#define _desc(a) (*(descriptor_t *)&((char*)a)[DESCRIPTOR_POS])

#define _save_result(x) memcpy((x), sc_get_buffer(), VALUE_SIZE)
#define _shift_right(x, y, b) sc_shr((x), (y), VALUE_SIZE*4, 0, (b))
#define _shift_left(x, y, b) sc_shl((x), (y), VALUE_SIZE*4, 0, (b))

#define FC_DEFINE1(code) char* fc_##code(const void *a, void *result)                          \
                   {                                                                           \
                     return _calc((const char*)a, NULL, FC_##code, (char*)result);             \
                   }

#define FC_DEFINE2(code) char* fc_##code(const void *a, const void *b, void *result)           \
                   {                                                                           \
                     return _calc((const char*)a, (const char*)b, FC_##code, (char*)result);   \
                   }

#define FUNC_PTR(code) fc_##code

#if FLTCALC_DEBUG
#  define DEBUGPRINTF(x) printf x
#else
#  define DEBUGPRINTF(x) ((void)0)
#endif

#if FLTCALC_TRACE_CALC
#  define TRACEPRINTF(x) printf x
#else
#  define TRACEPRINTF(x) ((void)0)
#endif

static char *calc_buffer = NULL;

static fc_rounding_mode_t ROUNDING_MODE;

static int CALC_BUFFER_SIZE;
static int VALUE_SIZE;
static int SIGN_POS;
static int EXPONENT_POS;
static int MANTISSA_POS;
static int DESCRIPTOR_POS;

static int max_precision;
/********
 * private functions
 ********/
#if 0
static void _fail_char(const char *str, unsigned int len, int pos)
{
  if (*(str+pos))
    printf("ERROR: Unexpected character '%c'\n", *(str + pos));
  else
    printf("ERROR: Unexpected end of string\n");
  while (len-- && *str) printf("%c", *str++); printf("\n");
  while (pos--) printf(" "); printf("^\n");
  /* the front end has to to check constant strings */
  exit(-1);
}
#endif

/* pack machine-like */
static char* _pack(const char *int_float, char *packed)
{
  char *shift_val;
  char *temp;
  char *val_buffer;

  temp = alloca(VALUE_SIZE);
  shift_val = alloca(VALUE_SIZE);

  switch (_desc(int_float).class) {
    case NAN:
      val_buffer = alloca(CALC_BUFFER_SIZE);
      fc_get_qnan(_desc(int_float).exponent_size, _desc(int_float).mantissa_size, val_buffer);
      int_float = val_buffer;
      break;

    case INF:
      val_buffer = alloca(CALC_BUFFER_SIZE);
      fc_get_plusinf(_desc(int_float).exponent_size, _desc(int_float).mantissa_size, val_buffer);
      _sign(val_buffer) = _sign(int_float);
      int_float = val_buffer;
      break;

    default:
      break;
  }
  /* pack sign */
  sc_val_from_ulong(_sign(int_float), temp);

  sc_val_from_ulong(_desc(int_float).exponent_size + _desc(int_float).mantissa_size, NULL);
  _shift_left(temp, sc_get_buffer(), packed);

  /* extract exponent */
  sc_val_from_ulong(_desc(int_float).mantissa_size, shift_val);

  _shift_left(_exp(int_float), shift_val, temp);

  sc_or(temp, packed, packed);

  /* extract mantissa */
  /* remove 2 rounding bits */
  sc_val_from_ulong(2, shift_val);
  _shift_right(_mant(int_float), shift_val, temp);

  /* remove leading 1 (or 0 if denormalized) */
  sc_max_from_bits(_desc(int_float).mantissa_size, 0, shift_val); /* all mantissa bits are 1's */
  sc_and(temp, shift_val, temp);

  /* save result */
  sc_or(temp, packed, packed);

  return packed;
}

char* _normalize(const char *in_val, char *out_val, int sticky)
{
  int hsb;
  char lsb, guard, round, round_dir = 0;
  char *temp;

  temp = alloca(VALUE_SIZE);

  /* +2: save two rounding bits at the end */
  hsb = 2 + _desc(in_val).mantissa_size - sc_get_highest_set_bit(_mant(in_val)) - 1;

  if (in_val != out_val)
  {
    _sign(out_val) = _sign(in_val);
    memcpy(&_desc(out_val), &_desc(in_val), sizeof(descriptor_t));
  }

  _desc(out_val).class = NORMAL;

  /* mantissa all zeroes, so zero exponent (because of explicit one)*/
  if (hsb == 2 + _desc(in_val).mantissa_size)
  {
    sc_val_from_ulong(0, _exp(out_val));
    hsb = -1;
  }

  /* shift the first 1 into the left of the radix point (i.e. hsb == -1) */
  if (hsb < -1)
  {
    /* shift right */
    sc_val_from_ulong(-hsb-1, temp);

    _shift_right(_mant(in_val), temp, _mant(out_val));

    /* remember if some bits were shifted away */
    if (!sticky) sticky = sc_had_carry();

    sc_add(_exp(in_val), temp, _exp(out_val));
  }
  else if (hsb > -1)
  {
    /* shift left */
    sc_val_from_ulong(hsb+1, temp);

    _shift_left(_mant(in_val), temp, _mant(out_val));

    sc_sub(_exp(in_val), temp, _exp(out_val));
  }

  /* check for exponent underflow */
  if (sc_is_negative(_exp(out_val)) || sc_is_zero(_exp(out_val))) {
    DEBUGPRINTF(("Exponent underflow!\n"));
    /* exponent underflow */
    /* shift the mantissa right to have a zero exponent */
    sc_val_from_ulong(1, temp);
    sc_sub(temp, _exp(out_val), NULL);

    _shift_right(_mant(out_val), sc_get_buffer(), _mant(out_val));
    if (!sticky) sticky = sc_had_carry();
    /* denormalized means exponent of zero */
    sc_val_from_ulong(0, _exp(out_val));

    _desc(out_val).class = SUBNORMAL;
  }

  /* perform rounding by adding a value that clears the guard bit and the round bit
   * and either causes a carry to round up or not */
  /* get the last 3 bits of the value */
  lsb = sc_sub_bits(_mant(out_val), _desc(out_val).mantissa_size + 2, 0) & 0x7;
  guard = (lsb&0x2)>>1;
  round = lsb&0x1;

  switch (ROUNDING_MODE)
  {
    case FC_TONEAREST:
      /* round to nearest representable value, if in doubt choose the version
       * with lsb == 0 */
      round_dir = guard && (sticky || round || lsb>>2);
      break;
    case FC_TOPOSITIVE:
      /* if positive: round to one if the exact value is bigger, else to zero */
      round_dir = (!_sign(out_val) && (guard || round || sticky));
      break;
    case FC_TONEGATIVE:
      /* if negative: round to one if the exact value is bigger, else to zero */
      round_dir = (_sign(out_val) && (guard || round || sticky));
      break;
    case FC_TOZERO:
      /* always round to 0 (chopping mode) */
      round_dir = 0;
      break;
  }
  DEBUGPRINTF(("Rounding (s%d, l%d, g%d, r%d, s%d) %s\n", _sign(out_val), lsb>>2, guard, round, sticky, (round_dir)?"up":"down"));

  if (round_dir == 1)
  {
    guard = (round^guard)<<1;
    lsb = !(round || guard)<<2 | guard | round;
  }
  else
  {
    lsb = -((guard<<1) | round);
  }

  /* add the rounded value */
  if (lsb != 0) {
    sc_val_from_long(lsb, temp);
    sc_add(_mant(out_val), temp, _mant(out_val));
  }

  /* could have rounded down to zero */
  if (sc_is_zero(_mant(out_val)) && (_desc(out_val).class == SUBNORMAL))
    _desc(out_val).class = ZERO;

  /* check for rounding overflow */
  hsb = 2 + _desc(out_val).mantissa_size - sc_get_highest_set_bit(_mant(out_val)) - 1;
  if ((_desc(out_val).class != SUBNORMAL) && (hsb < -1))
  {
    sc_val_from_ulong(1, temp);
    _shift_right(_mant(out_val), temp, _mant(out_val));

    sc_add(_exp(out_val), temp, _exp(out_val));
  }
  else if ((_desc(out_val).class == SUBNORMAL) && (hsb == -1))
  {
    /* overflow caused the matissa to be normal again,
     * so adapt the exponent accordingly */
    sc_val_from_ulong(1, temp);
    sc_add(_exp(out_val), temp, _exp(out_val));

    _desc(out_val).class = NORMAL;
  }
  /* no further rounding is needed, because rounding overflow means
   * the carry of the original rounding was propagated all the way
   * up to the bit left of the radix point. This implies the bits
   * to the right are all zeros (rounding is +1) */

  /* check for exponent overflow */
  sc_val_from_ulong((1 << _desc(out_val).exponent_size) - 1, temp);
  if (sc_comp(_exp(out_val), temp) != -1) {
    DEBUGPRINTF(("Exponent overflow!\n"));
    /* exponent overflow, reaction depends on rounding method:
     *
     * mode        | sign of value |  result
     *--------------------------------------------------------------
     * TO_NEAREST  |      +        |   +inf
     *             |      -        |   -inf
     *--------------------------------------------------------------
     * TO_POSITIVE |      +        |   +inf
     *             |      -        |   smallest representable value
     *--------------------------------------------------------------
     * TO_NEAGTIVE |      +        |   largest representable value
     *             |      -        |   -inf
     *--------------------------------------------------------------
     * TO_ZERO     |      +        |   largest representable value
     *             |      -        |   smallest representable value
     *--------------------------------------------------------------*/
    if (_sign(out_val) == 0)
    {
      /* value is positive */
      switch (ROUNDING_MODE) {
        case FC_TONEAREST:
        case FC_TOPOSITIVE:
          _desc(out_val).class = INF;
          break;

        case FC_TONEGATIVE:
        case FC_TOZERO:
          fc_get_max(_desc(out_val).exponent_size, _desc(out_val).mantissa_size, out_val);
      }
    } else {
      /* value is negative */
      switch (ROUNDING_MODE) {
        case FC_TONEAREST:
        case FC_TONEGATIVE:
          _desc(out_val).class = INF;
          break;

        case FC_TOPOSITIVE:
        case FC_TOZERO:
          fc_get_min(_desc(out_val).exponent_size, _desc(out_val).mantissa_size, out_val);
      }
    }
  }

  return out_val;
}

/*
 * calculate a + b, where a is the value with the bigger exponent
 */
static char* _add(const char* a, const char* b, char* result)
{
  char *temp;
  char *exp_diff;

  char sign;
  char sticky;

  if (_desc(a).class == NAN) {
    if (a != result) memcpy(result, a, CALC_BUFFER_SIZE);
    return result;
  }
  if (_desc(b).class == NAN) {
    if (b != result) memcpy(result, b, CALC_BUFFER_SIZE);
    return result;
  }

  /* make sure result has a descriptor */
  if (result != a && result != b)
    memcpy(&_desc(result), &_desc(a), sizeof(descriptor_t));

  /* determine if this is an addition or subtraction */
  sign = _sign(a) ^ _sign(b);

  /* produce nan on inf - inf */
  if (sign && (_desc(a).class == INF) && (_desc(b).class == INF))
    return fc_get_qnan(_desc(a).exponent_size, _desc(b).mantissa_size, result);

  temp = alloca(VALUE_SIZE);
  exp_diff = alloca(VALUE_SIZE);

  /* get exponent difference */
  sc_sub(_exp(a), _exp(b), exp_diff);

  /* initially set sign to be the sign of a, special treatment of subtraction
   * when exponents are equal is required though.
   * Also special care about the sign is needed when the mantissas are equal
   * (+/- 0 ?) */
  if (sign && sc_val_to_long(exp_diff, VALUE_SIZE >> 2, 1) == 0) {
    switch (sc_comp(_mant(a), _mant(b))) {
      case 1:  /* a > b */
        if (_sign(a)) _sign(result) = 1;  /* abs(a) is bigger and a is negative */
        else _sign(result) = 0;
        break;
      case 0:  /* a == b */
        if (ROUNDING_MODE == FC_TONEGATIVE)
          _sign(result) = 1;
        else
          _sign(result) = 0;
        break;
      case -1: /* a < b */
        if (_sign(b)) _sign(result) = 1; /* abs(b) is bigger and b is negative */
        else _sign(result) = 0;
        break;
      default:
        /* can't be reached */
        break;
    }
  } else {
    _sign(result) = _sign(a);
  }

  /* sign has been taken care of, check for special cases */
  if (_desc(a).class == ZERO) {
    if (b != result) memcpy(result+SIGN_POS+1, b+SIGN_POS+1, CALC_BUFFER_SIZE-SIGN_POS-1);
    return result;
  }
  if (_desc(b).class == ZERO) {
    if (a != result) memcpy(result+SIGN_POS+1, a+SIGN_POS+1, CALC_BUFFER_SIZE-SIGN_POS-1);
    return result;
  }

  if (_desc(a).class == INF) {
    if (a != result) memcpy(result+SIGN_POS+1, a+SIGN_POS+1, CALC_BUFFER_SIZE-SIGN_POS-1);
    return result;
  }
  if (_desc(b).class == INF) {
    if (b != result) memcpy(result+SIGN_POS+1, b+SIGN_POS+1, CALC_BUFFER_SIZE-SIGN_POS-1);
    return result;
  }

  /* shift the smaller value to the right to align the radix point */
  /* subnormals have their radix point shifted to the right,
   * take care of this first */
  if ((_desc(b).class == SUBNORMAL) && (_desc(a).class != SUBNORMAL))
  {
    sc_val_from_ulong(1, temp);
    sc_sub(exp_diff, temp, exp_diff);
  }

  _shift_right(_mant(b), exp_diff, temp);
  sticky = sc_had_carry();

  if (sticky && sign)
  {
    /* if subtracting a little more than the represented value or adding a little
     * more than the represented value to a negative value this, in addition to the
     * still set sticky bit, takes account of the 'little more' */
    char *temp1 = alloca(CALC_BUFFER_SIZE);
    sc_val_from_ulong(1, temp1);
    sc_add(temp, temp1, temp);
  }

  if (sign) {
    if (sc_comp(_mant(a), temp) == -1)
      sc_sub(temp, _mant(a), _mant(result));
    else
      sc_sub(_mant(a), temp, _mant(result));
  } else {
    sc_add(_mant(a), temp, _mant(result));
  }

  /* _normalize expects a 'normal' radix point, adding two subnormals
   * results in a subnormal radix point -> shifting before normalizing */
  if ((_desc(a).class == SUBNORMAL) && (_desc(b).class == SUBNORMAL))
  {
    sc_val_from_ulong(1, NULL);
    _shift_left(_mant(result), sc_get_buffer(), _mant(result));
  }

  /* resulting exponent is the bigger one */
  memmove(_exp(result), _exp(a), VALUE_SIZE);

  return _normalize(result, result, sticky);
}

static char* _mul(const char* a, const char* b, char* result)
{
  char *temp;

  if (_desc(a).class == NAN) {
    if (a != result) memcpy(result, a, CALC_BUFFER_SIZE);
    return result;
  }
  if (_desc(b).class == NAN) {
    if (b != result) memcpy(result, b, CALC_BUFFER_SIZE);
    return result;
  }

  temp = alloca(VALUE_SIZE);

  if (result != a && result != b)
    memcpy(&_desc(result), &_desc(a), sizeof(descriptor_t));

  _sign(result) = _sign(a) ^ _sign(b);

  /* produce nan on 0 * inf */
  if (_desc(a).class == ZERO) {
    if (_desc(b).class == INF)
      fc_get_qnan(_desc(a).exponent_size, _desc(a).mantissa_size, result);
    else
      if (a != result) memcpy(result+SIGN_POS+1, a+SIGN_POS+1, CALC_BUFFER_SIZE-1);
    return result;
  }
  if (_desc(b).class == ZERO) {
    if (_desc(a).class == INF)
      fc_get_qnan(_desc(a).exponent_size, _desc(a).mantissa_size, result);
    else
      if (b != result) memcpy(result+SIGN_POS+1, b+SIGN_POS+1, CALC_BUFFER_SIZE-1);
    return result;
  }

  if (_desc(a).class == INF) {
    if (a != result) memcpy(result+SIGN_POS+1, a+SIGN_POS+1, CALC_BUFFER_SIZE-1);
    return result;
  }
  if (_desc(b).class == INF) {
    if (b != result) memcpy(result+SIGN_POS+1, b+SIGN_POS+1, CALC_BUFFER_SIZE-1);
    return result;
  }

  /* exp = exp(a) + exp(b) - excess */
  sc_add(_exp(a), _exp(b), _exp(result));

  sc_val_from_ulong((1<<_desc(a).exponent_size)/2-1, temp);
  sc_sub(_exp(result), temp, _exp(result));

  /* mixed normal, subnormal values introduce an error of 1, correct it */
  if ((_desc(a).class == SUBNORMAL) ^ (_desc(b).class == SUBNORMAL))
  {
    sc_val_from_ulong(1, temp);
    sc_add(_exp(result), temp, _exp(result));
  }

  sc_mul(_mant(a), _mant(b), _mant(result));

  /* realign result: after a multiplication the digits right of the radix
   * point are the sum of the factors' digits after the radix point. As all
   * values are normalized they both have the same amount of these digits,
   * which has to be restored by proper shifting
   * +2 because of the two rounding bits */
  sc_val_from_ulong(2 + _desc(result).mantissa_size, temp);

  _shift_right(_mant(result), temp, _mant(result));

  return _normalize(result, result, sc_had_carry());
}

static char* _div(const char* a, const char* b, char* result)
{
  char *temp, *dividend;

  if (_desc(a).class == NAN) {
    if (a != result) memcpy(result, a, CALC_BUFFER_SIZE);
    return result;
  }
  if (_desc(b).class == NAN) {
    if (b != result) memcpy(result, b, CALC_BUFFER_SIZE);
    return result;
  }

  temp = alloca(VALUE_SIZE);
  dividend = alloca(VALUE_SIZE);

  if (result != a && result != b)
    memcpy(&_desc(result), &_desc(a), sizeof(descriptor_t));

  _sign(result) = _sign(a) ^ _sign(b);

  /* produce nan on 0/0 and inf/inf */
  if (_desc(a).class == ZERO) {
    if (_desc(b).class == ZERO)
      /* 0/0 -> nan */
      fc_get_qnan(_desc(a).exponent_size, _desc(a).mantissa_size, result);
    else
      /* 0/x -> a */
      if (a != result) memcpy(result+SIGN_POS+1, a+SIGN_POS+1, CALC_BUFFER_SIZE-1);
    return result;
  }

  if (_desc(b).class == INF) {
    if (_desc(a).class == INF)
      /* inf/inf -> nan */
      fc_get_qnan(_desc(a).exponent_size, _desc(a).mantissa_size, result);
    else {
      /* x/inf -> 0 */
      sc_val_from_ulong(0, NULL);
      _save_result(_exp(result));
      _save_result(_mant(result));
      _desc(result).class = ZERO;
    }
    return result;
  }

  if (_desc(a).class == INF) {
    /* inf/x -> inf */
    if (a != result) memcpy(result+SIGN_POS+1, a+SIGN_POS+1, CALC_BUFFER_SIZE-1);
    return result;
  }
  if (_desc(b).class == ZERO) {
    /* division by zero */
    if (_sign(result))
      fc_get_minusinf(_desc(a).exponent_size, _desc(a).mantissa_size, result);
    else
      fc_get_plusinf(_desc(a).exponent_size, _desc(a).mantissa_size, result);
    return result;
  }

  /* exp = exp(a) - exp(b) + excess - 1*/
  sc_sub(_exp(a), _exp(b), _exp(result));
  sc_val_from_ulong((1 << _desc(a).exponent_size)/2-2, temp);
  sc_add(_exp(result), temp, _exp(result));

  /* mixed normal, subnormal values introduce an error of 1, correct it */
  if ((_desc(a).class == SUBNORMAL) ^ (_desc(b).class == SUBNORMAL))
  {
    sc_val_from_ulong(1, temp);
    sc_add(_exp(result), temp, _exp(result));
  }

  /* mant(res) = mant(a) / 1/2mant(b) */
  /* to gain more bits of precision in the result the dividend could be
   * shifted left, as this operation does not loose bits. This would not
   * fit into the integer precision, but due to the rounding bits (which
   * are always zero because the values are all normalized) the divisor
   * can be shifted right instead to achieve the same result */
  sc_val_from_ulong(2 + _desc(result).mantissa_size, temp);

  _shift_left(_mant(a), temp, dividend);

  {
    char *divisor = alloca(CALC_BUFFER_SIZE);
    sc_val_from_ulong(1, divisor);
    _shift_right(_mant(b), divisor, divisor);
    sc_div(dividend, divisor, _mant(result));
  }

  return _normalize(result, result, sc_had_carry());
}

void _power_of_ten(int exp, descriptor_t *desc, char *result)
{
  char *build;
  char *temp;

  /* positive sign */
  _sign(result) = 0;

  /* set new descriptor (else result is supposed to already have one) */
  if (desc != NULL)
    memcpy(&_desc(result), desc, sizeof(descriptor_t));

  build = alloca(VALUE_SIZE);
  temp = alloca(VALUE_SIZE);

  sc_val_from_ulong((1 << _desc(result).exponent_size)/2-1, _exp(result));

  if (exp > 0)
  {
    /* temp is value of ten now */
    sc_val_from_ulong(10, NULL);
    _save_result(temp);

    for (exp--; exp > 0; exp--) {
      _save_result(build);
      sc_mul(build, temp, NULL);
    }
    _save_result(build);

    /* temp is amount of leftshift needed to put the value left of the radix point */
    sc_val_from_ulong(_desc(result).mantissa_size + 2, temp);

    _shift_left(build, temp, _mant(result));

    _normalize(result, result, 0);
  }
}

static char* _trunc(const char *a, char *result)
{
  /* when exponent == 0 all bits left of the radix point
   * are the integral part of the value. For 15bit exp_size
   * this would require a leftshift of max. 16383 bits which
   * is too much.
   * But it is enough to ensure that no bit right of the radix
   * point remains set. This restricts the interesting
   * exponents to the interval [0, mant_size-1].
   * Outside this interval the truncated value is either 0 or
   * it is are already truncated */

  int exp_bias, exp_val;
  char *temp;

  temp = alloca(VALUE_SIZE);

  if (a != result)
    memcpy(&_desc(result), &_desc(a), sizeof(descriptor_t));

  exp_bias = (1<<_desc(a).exponent_size)/2-1;
  exp_val = sc_val_to_long(_exp(a), VALUE_SIZE >> 2, 1) - exp_bias;

  if (exp_val < 0) {
    sc_val_from_ulong(0, NULL);
    _save_result(_exp(result));
    _save_result(_mant(result));
    _desc(result).class = ZERO;

    return result;
  }

  if (exp_val > _desc(a).mantissa_size) {
    if (a != result)
      memcpy(result, a, CALC_BUFFER_SIZE);

    return result;
  }

  /* set up a proper mask to delete all bits right of the
   * radix point if the mantissa had been shifted until exp == 0 */
  sc_max_from_bits(1 + exp_val, 0, temp);
  sc_val_from_long(_desc(a).mantissa_size - exp_val + 2, NULL);
  _shift_left(temp, sc_get_buffer(), temp);

  /* and the mask and return the result */
  sc_and(_mant(a), temp, _mant(result));

  if (a != result) memcpy(_exp(result), _exp(a), VALUE_SIZE);

  return result;
}

/*
 * This does value sanity checking(or should do it), sets up any prerequisites,
 * calls the proper internal functions, clears up and returns
 * the result */
char* _calc(const char *a, const char *b, int opcode, char *result)
{
  char *temp;
#ifdef FLTCALC_TRACE_CALC
  char *buffer;

  buffer = alloca(100);
#endif

  if (result == NULL) result = calc_buffer;

  TRACEPRINTF(("%s ", fc_print(a, buffer, 100, FC_PACKED)));
  switch (opcode)
  {
    case FC_add:
      /* make the value with the bigger exponent the first one */
      TRACEPRINTF(("+ %s ", fc_print(b, buffer, 100, FC_PACKED)));
      if (sc_comp(_exp(a), _exp(b)) == -1)
        _add(b, a, result);
      else
        _add(a, b, result);
      break;
    case FC_sub:
      TRACEPRINTF(("- %s ", fc_print(b, buffer, 100, FC_PACKED)));
      temp = alloca(CALC_BUFFER_SIZE);
      memcpy(temp, b, CALC_BUFFER_SIZE);
      _sign(temp) = !_sign(b);
      if (sc_comp(_exp(a), _exp(temp)) == -1)
        _add(temp, a, result);
      else
        _add(a, temp, result);
      break;
    case FC_mul:
      TRACEPRINTF(("* %s ", fc_print(b, buffer, 100, FC_PACKED)));
      _mul(a, b, result);
      break;
    case FC_div:
      TRACEPRINTF(("/ %s ", fc_print(b, buffer, 100, FC_PACKED)));
      _div(a, b, result);
      break;
    case FC_neg:
      TRACEPRINTF(("negated "));
      if (a != result) memcpy(result, a, CALC_BUFFER_SIZE);
      _sign(result) = !_sign(a);
      break;
    case FC_int:
      _trunc(a, result);
      break;
    case FC_rnd:
      break;
  }

  TRACEPRINTF(("= %s\n", fc_print(result, buffer, 100, FC_PACKED)));
  return result;
}

/********
 * functions defined in fltcalc.h
 ********/
const void *fc_get_buffer(void)
{
  return calc_buffer;
}

const int fc_get_buffer_length(void)
{
  return CALC_BUFFER_SIZE;
}

char* fc_val_from_str(const char *str, unsigned int len, char exp_size, char mant_size, char *result)
{
#if 0
  enum {
    START,
    LEFT_OF_DOT,
    RIGHT_OF_DOT,
    EXP_START,
    EXPONENT,
    END
  };

  char exp_sign;
  int exp_int, hsb, state;

  const char *old_str;

  int pos;
  char *mant_str, *exp_val, *power_val;

  if (result == NULL) result = calc_buffer;

  exp_val = alloca(VALUE_SIZE);
  power_val = alloca(CALC_BUFFER_SIZE);
  mant_str = alloca((len)?(len):(strlen(str)));

  _desc(result).exponent_size = exp_size;
  _desc(result).mantissa_size = mant_size;
  _desc(result).class = NORMAL;

  old_str = str;
  pos = 0;
  exp_int = 0;
  state = START;

  while (len == 0 || str-old_str < len)
  {
    switch (state) {
      case START:
        switch (*str) {
          case '+':
            _sign(result) = 0;
            state = LEFT_OF_DOT;
            str++;
            break;

          case '-':
            _sign(result) = 1;
            state = LEFT_OF_DOT;
            str++;
            break;

          case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
            _sign(result) = 0;
            state = LEFT_OF_DOT;
            break;

          case '.':
            _sign(result) = 0;
            state = RIGHT_OF_DOT;
            str++;
            break;

          case 'n':
          case 'N':
          case 'i':
          case 'I':
            break;

          default:
            _fail_char(old_str, len, str - old_str);
        }
        break;

      case LEFT_OF_DOT:
        switch (*str) {
          case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
            mant_str[pos++] = *(str++);
            break;

          case '.':
            state = RIGHT_OF_DOT;
            str++;
            break;

          case 'e':
          case 'E':
            state = EXP_START;
            str++;
            break;

          case '\0':
            mant_str[pos] = '\0';
            goto done;

          default:
            _fail_char(old_str, len, str - old_str);
        }
        break;

      case RIGHT_OF_DOT:
        switch (*str) {
          case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
            mant_str[pos++] = *(str++);
            exp_int++;
            break;

          case 'e':
          case 'E':
            state = EXP_START;
            str++;
            break;

          case '\0':
            mant_str[pos] = '\0';
            goto done;

          default:
            _fail_char(old_str, len, str - old_str);
        }
        break;

      case EXP_START:
        switch (*str) {
          case '-':
            exp_sign = 1;
            /* fall through */
          case '+':
            if (*(str-1) != 'e' && *(str-1) != 'E') _fail_char(old_str, len, str - old_str);
            str++;
            break;

          case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
            mant_str[pos] = '\0';
            pos = 1;
            str++;
            state = EXPONENT;
            break;

          default:
            _fail_char(old_str, len, str - old_str);
        }
        break;

      case EXPONENT:
        switch (*str) {
          case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
            pos++;
            str++;
            break;

          case '\0': goto done;

          default:
            _fail_char(old_str, len, str - old_str);
        }
    }
  } /*  switch(state) */

done:
  sc_val_from_str(mant_str, strlen(mant_str), _mant(result));

  /* shift to put value left of radix point */
  sc_val_from_ulong(mant_size + 2, exp_val);

  _shift_left(_mant(result), exp_val, _mant(result));

  sc_val_from_ulong((1 << exp_size)/2-1, _exp(result));

  _normalize(result, result, 0);

  if (state == EXPONENT) {
    exp_int -= atoi(str-pos);
  }

  _power_of_ten(exp_int, &_desc(result), power_val);

  _div(result, power_val, result);

  return result;
#else

  /* XXX excuse of an implementation to make things work */
  LLDBL val;
#ifdef HAVE_LONG_DOUBLE
  val = strtold(str, NULL);
#else
  val = strtod(str, NULL);
#endif

  DEBUGPRINTF(("val_from_str(%s)\n", str));
  return fc_val_from_float(val, exp_size, mant_size, result);
#endif
}

char* fc_val_from_float(LLDBL l, char exp_size, char mant_size, char* result)
{
  char *temp;
  int bias_res, bias_val, mant_val;
  value_t srcval;
  UINT32 sign, exponent, mantissa0, mantissa1;

  srcval.d = l;
  bias_res = ((1<<exp_size)/2-1);

#ifdef HAVE_LONG_DOUBLE
  mant_val  = 64;
  bias_val  = 0x3fff;
  sign      = (srcval.val.high & 0x00008000) != 0;
  exponent  = (srcval.val.high & 0x00007FFF) ;
  mantissa0 = srcval.val.mid;
  mantissa1 = srcval.val.low;
#else /* no long double */
  mant_val  = 52;
  bias_val  = 0x3ff;
  sign      = (srcval.val.high & 0x80000000) != 0;
  exponent  = (srcval.val.high & 0x7FF00000) >> 20;
  mantissa0 = srcval.val.high & 0x000FFFFF;
  mantissa1 = srcval.val.low;
#endif

#ifdef HAVE_LONG_DOUBLE
  TRACEPRINTF(("val_from_float(%.8X%.8X%.8X)\n", ((int*)&l)[2], ((int*)&l)[1], ((int*)&l)[0]));/* srcval.val.high, srcval.val.mid, srcval.val.low)); */
  DEBUGPRINTF(("(%d-%.4X-%.8X%.8X)\n", sign, exponent, mantissa0, mantissa1));
#else
  TRACEPRINTF(("val_from_float(%.8X%.8X)\n", srcval.val.high, srcval.val.low));
  DEBUGPRINTF(("(%d-%.3X-%.5X%.8X)\n", sign, exponent, mantissa0, mantissa1));
#endif

  if (result == NULL) result = calc_buffer;
  temp = alloca(VALUE_SIZE);

  _desc(result).exponent_size = exp_size;
  _desc(result).mantissa_size = mant_size;

  /* extract sign */
  _sign(result) = sign;

  /* sign and flag suffice to identify nan or inf, no exponent/mantissa
   * encoding is needed. the function can return immediately in these cases */
  if (isnan(l)) {
    _desc(result).class = NAN;
    TRACEPRINTF(("val_from_float resulted in NAN\n"));
    return result;
  }
  else if (isinf(l)) {
    _desc(result).class = INF;
    TRACEPRINTF(("val_from_float resulted in %sINF\n", (_sign(result)==1)?"-":""));
    return result;
  }

  /* build exponent, because input and output exponent and mantissa sizes may differ
   * this looks more complicated than it is: unbiased input exponent + output bias,
   * minus the mantissa difference which is added again later when the output float
   * becomes normalized */
#ifdef HAVE_EXPLICIT_ONE
  sc_val_from_long((exponent-bias_val+bias_res)-(mant_val-mant_size-1), _exp(result));
#else
  sc_val_from_long((exponent-bias_val+bias_res)-(mant_val-mant_size), _exp(result));
#endif

  /* build mantissa representation */
#ifndef HAVE_EXPLICIT_ONE
  if (exponent != 0)
  {
    /* insert the hidden bit */
    sc_val_from_ulong(1, temp);
    sc_val_from_ulong(mant_val + 2, NULL);
    _shift_left(temp, sc_get_buffer(), NULL);
  }
  else
#endif
  {
    sc_val_from_ulong(0, NULL);
  }

  _save_result(_mant(result));

  /* bits from the upper word */
  sc_val_from_ulong(mantissa0, temp);
  sc_val_from_ulong(34, NULL);
  _shift_left(temp, sc_get_buffer(), temp);
  sc_or(_mant(result), temp, _mant(result));

  /* bits from the lower word */
  sc_val_from_ulong(mantissa1, temp);
  sc_val_from_ulong(2, NULL);
  _shift_left(temp, sc_get_buffer(), temp);
  sc_or(_mant(result), temp, _mant(result));

  /* _normalize expects the radix point to be normal, so shift mantissa of subnormal
   * origin one to the left */
  if (exponent == 0)
  {
    sc_val_from_ulong(1, NULL);
    _shift_left(_mant(result), sc_get_buffer(), _mant(result));
  }

  _normalize(result, result, 0);

  TRACEPRINTF(("val_from_float results in %s\n", fc_print(result, temp, CALC_BUFFER_SIZE, FC_PACKED)));

  return result;
}

LLDBL fc_val_to_float(const void *val)
{
  const char *value;
  char *temp = NULL;

  int byte_offset;

  UINT32 sign;
  UINT32 exponent;
  UINT32 mantissa0;
  UINT32 mantissa1;

  value_t buildval;

#ifdef HAVE_LONG_DOUBLE
  char result_exponent = 15;
  char result_mantissa = 64;
#else
  char result_exponent = 11;
  char result_mantissa = 52;
#endif

  temp = alloca(CALC_BUFFER_SIZE);
#ifdef HAVE_EXPLICIT_ONE
  value = fc_cast(val, result_exponent, result_mantissa-1, temp);
#else
  value = fc_cast(val, result_exponent, result_mantissa, temp);
#endif

  sign = _sign(value);

  /* @@@ long double exponent is 15bit, so the use of sc_val_to_long should not
   * lead to wrong results */
  exponent = sc_val_to_long(_exp(value), VALUE_SIZE >> 2, 1) ;

  sc_val_from_ulong(2, NULL);
  _shift_right(_mant(value), sc_get_buffer(), _mant(value));

  mantissa0 = 0;
  mantissa1 = 0;

  for (byte_offset = 0; byte_offset < 4; byte_offset++)
    mantissa1 |= sc_sub_bits(_mant(value), result_mantissa, byte_offset) << (byte_offset<<3);

  for (; (byte_offset<<3) < result_mantissa; byte_offset++)
    mantissa0 |= sc_sub_bits(_mant(value), result_mantissa, byte_offset) << ((byte_offset-4)<<3);

#ifndef HAVE_LONG_DOUBLE
  mantissa0 &= 0x000FFFFF;  /* get rid of garbage */
#endif

#ifdef HAVE_LONG_DOUBLE
  buildval.val.high = sign << 15;
  buildval.val.high |= exponent;
  buildval.val.mid = mantissa0;
  buildval.val.low = mantissa1;
#else /* no long double */
  buildval.val.high = sign << 31;
  buildval.val.high |= exponent << 20;
  buildval.val.high |= mantissa0;
  buildval.val.low = mantissa1;
#endif

  TRACEPRINTF(("val_to_float: %d-%x-%x%x\n", sign, exponent, mantissa0, mantissa1));
  return buildval.d;
}

char* fc_cast(const void *val, char exp_size, char mant_size, char *result)
{
  const char *value = (const char*) val;
  char *temp;
  int exp_offset, val_bias, res_bias;

  if (result == NULL) result = calc_buffer;
  temp = alloca(VALUE_SIZE);

  if (_desc(value).exponent_size == exp_size && _desc(value).mantissa_size == mant_size)
  {
    if (value != result) memcpy(result, value, CALC_BUFFER_SIZE);
    return result;
  }

  /* set the descriptor of the new value */
  _desc(result).exponent_size = exp_size;
  _desc(result).mantissa_size = mant_size;
  _desc(result).class = _desc(value).class;

  _sign(result) = _sign(value);

  /* when the mantissa sizes differ normalizing has to shift to align it.
   * this would change the exponent, which is unwanted. So calculate this
   * offset and add it */
  val_bias = (1<<_desc(value).exponent_size)/2-1;
  res_bias = (1<<exp_size)/2-1;

  exp_offset = (res_bias - val_bias) - (_desc(value).mantissa_size - mant_size);
  sc_val_from_long(exp_offset, temp);
  sc_add(_exp(value), temp, _exp(result));

  /* _normalize expects normalized radix point */
  if (_desc(val).class == SUBNORMAL) {
    sc_val_from_ulong(1, NULL);
    _shift_left(_mant(val), sc_get_buffer(), _mant(result));
  } else if (value != result) {
    memcpy(_mant(result), _mant(value), VALUE_SIZE);
  } else {
    memmove(_mant(result), _mant(value), VALUE_SIZE);
  }

  _normalize(result, result, 0);
  TRACEPRINTF(("Cast results in %s\n", fc_print(result, temp, VALUE_SIZE, FC_PACKED)));
  return result;
}

char* fc_get_max(unsigned int exponent_size, unsigned int mantissa_size, char* result)
{
  if (result == NULL) result = calc_buffer;

  _desc(result).exponent_size = exponent_size;
  _desc(result).mantissa_size = mantissa_size;
  _desc(result).class = NORMAL;

  _sign(result) = 0;

  sc_val_from_ulong((1<<exponent_size) - 2, _exp(result));

  sc_max_from_bits(mantissa_size + 1, 0, _mant(result));
  sc_val_from_ulong(2, NULL);
  _shift_left(_mant(result), sc_get_buffer(), _mant(result));

  return result;
}

char* fc_get_min(unsigned int exponent_size, unsigned int mantissa_size, char *result)
{
  if (result == NULL) result = calc_buffer;

  fc_get_max(exponent_size, mantissa_size, result);
  _sign(result) = 1;

  return result;
}

char* fc_get_snan(unsigned int exponent_size, unsigned int mantissa_size, char *result)
{
  if (result == NULL) result = calc_buffer;

  _desc(result).exponent_size = exponent_size;
  _desc(result).mantissa_size = mantissa_size;
  _desc(result).class = NAN;

  _sign(result) = 0;

  sc_val_from_ulong((1<<exponent_size)-1, _exp(result));

  /* signalling nan has non-zero mantissa with msb not set */
  sc_val_from_ulong(1, _mant(result));

  return result;
}

char* fc_get_qnan(unsigned int exponent_size, unsigned int mantissa_size, char *result)
{
  if (result == NULL) result = calc_buffer;

  _desc(result).exponent_size = exponent_size;
  _desc(result).mantissa_size = mantissa_size;
  _desc(result).class = NAN;

  _sign(result) = 0;

  sc_val_from_ulong((1<<exponent_size)-1, _exp(result));

  /* quiet nan has the msb of the mantissa set, so shift one there */
  sc_val_from_ulong(1, _mant(result));
  /* mantissa_size >+< 1 because of two extra rounding bits */
  sc_val_from_ulong(mantissa_size + 1, NULL);
  _shift_left(_mant(result), sc_get_buffer(), _mant(result));

  return result;
}

char* fc_get_plusinf(unsigned int exponent_size, unsigned int mantissa_size, char *result)
{
  if (result == NULL) result = calc_buffer;

  _desc(result).exponent_size = exponent_size;
  _desc(result).mantissa_size = mantissa_size;
  _desc(result).class = NORMAL;

  _sign(result) = 0;

  sc_val_from_ulong((1<<exponent_size)-1, _exp(result));

  sc_val_from_ulong(0, _mant(result));

  return result;
}

char* fc_get_minusinf(unsigned int exponent_size, unsigned int mantissa_size, char *result)
{
  if (result == NULL) result = calc_buffer;

  fc_get_plusinf(exponent_size, mantissa_size, result);
  _sign(result) = 1;

  return result;
}

int fc_comp(const void *a, const void *b)
{
  const char *val_a = (const char*)a;
  const char *val_b = (const char*)b;

  /* unordered */
  if (_desc(val_a).class == NAN || _desc(val_b).class == NAN) return 2;
  /* zero is equal independent of sign */
  if ((_desc(val_a).class == ZERO) && (_desc(val_b).class == ZERO)) return 0;
  /* different signs make compare easy */
  if (_sign(val_a) != _sign(val_b)) return (_sign(val_a)==0)?(1):(-1);
  /* both infinity means equality */
  if ((_desc(val_a).class == INF) && (_desc(val_b).class == INF)) return 0;
  /* infinity is bigger than the rest */
  if (_desc(val_a).class == INF) return _sign(val_a)?(-1):(1);
  if (_desc(val_b).class == INF) return _sign(val_b)?(1):(-1);

  switch (sc_comp(_exp(val_a), _exp(val_b))) {
    case -1:
      return -1;
    case  1:
      return  1;
    case  0:
      return sc_comp(_mant(val_a), _mant(val_b));
    default:
      return 2;
  }
}

int fc_is_zero(const void *a)
{
  return _desc((const char*)a).class == ZERO;
}

int fc_is_negative(const void *a)
{
  return _sign((const char*)a);
}

int fc_is_inf(const void *a)
{
  return _desc(a).class == INF;
}

int fc_is_nan(const void *a)
{
  return _desc(a).class == NAN;
}

int fc_is_subnormal(const void *a)
{
  return _desc(a).class == SUBNORMAL;
}

char *fc_print(const void *a, char *buf, int buflen, unsigned base)
{
  const char *val;
  char *mul_1;

  val = (const char*)a;

  mul_1 = alloca(CALC_BUFFER_SIZE);

  switch (base) {
    case FC_DEC:
      switch (_desc(val).class) {
        case INF:
          if (buflen >= 8+_sign(val)) sprintf(buf, "%sINFINITY", _sign(val)?"-":"");
          else snprintf(buf, buflen, "%sINF", _sign(val)?"-":NULL);
          break;
        case NAN:
          snprintf(buf, buflen, "NAN");
          break;
        case ZERO:
          snprintf(buf, buflen, "0.0");
          break;
        default:
          /* XXX to be implemented */
#ifdef HAVE_LONG_DOUBLE
          /* XXX 30 is arbitrary */
          snprintf(buf, buflen, "%.30LE", fc_val_to_float(val));
#else
          snprintf(buf, buflen, "%.18E", fc_val_to_float(val));
#endif
      }
      break;

    case FC_HEX:
      switch (_desc(val).class) {
        case INF:
          if (buflen >= 8+_sign(val)) sprintf(buf, "%sINFINITY", _sign(val)?"-":"");
          else snprintf(buf, buflen, "%sINF", _sign(val)?"-":NULL);
          break;
        case NAN:
          snprintf(buf, buflen, "NAN");
          break;
        case ZERO:
          snprintf(buf, buflen, "0.0");
          break;
        default:
#ifdef HAVE_LONG_DOUBLE
          snprintf(buf, buflen, "%LA", fc_val_to_float(val));
#else
          snprintf(buf, buflen, "%A", fc_val_to_float(val));
#endif
      }
      break;

    case FC_PACKED:
    default:
      snprintf(buf, buflen, "%s", sc_print(_pack(val, mul_1), VALUE_SIZE*4, SC_HEX));
      break;
  }
  return buf;
}

unsigned char fc_sub_bits(const void *value, unsigned num_bits, unsigned byte_ofs)
{
  /* this is used to cache the packed version of the value */
  static char *pack = NULL;

  if (pack == NULL) pack = malloc(VALUE_SIZE);

  if (value != NULL)
    _pack((const char*)value, pack);

  return sc_sub_bits(pack, num_bits, byte_ofs);
}

fc_rounding_mode_t fc_set_rounding_mode(fc_rounding_mode_t mode)
{
  if (mode == FC_TONEAREST || mode == FC_TOPOSITIVE || mode == FC_TONEGATIVE || mode == FC_TOZERO)
      ROUNDING_MODE = mode;

  return ROUNDING_MODE;
}

fc_rounding_mode_t fc_get_rounding_mode(void)
{
  return ROUNDING_MODE;
}

void init_fltcalc(int precision)
{
  if (calc_buffer == NULL) {
    /* does nothing if already init */
    if (precision == 0) precision = FC_DEFAULT_PRECISION;

    init_strcalc(precision + 4);

    /* needs additionally two bits to round, a bit as explicit 1., and one for
     * addition overflow */
    max_precision = sc_get_precision() - 4;
    if (max_precision < precision)
      printf("WARING: not enough precision available, using %d\n", max_precision);

    ROUNDING_MODE = FC_TONEAREST;
    VALUE_SIZE = sc_get_buffer_length();
    SIGN_POS = 0;
    EXPONENT_POS = SIGN_POS + sizeof(char);
    MANTISSA_POS = EXPONENT_POS + VALUE_SIZE;
    DESCRIPTOR_POS = MANTISSA_POS + VALUE_SIZE;
    CALC_BUFFER_SIZE = DESCRIPTOR_POS + sizeof(descriptor_t);

    calc_buffer = malloc(CALC_BUFFER_SIZE);
    DEBUGPRINTF(("init fltcalc:\n\tVALUE_SIZE = %d\n\tSIGN_POS = %d\n\tEXPONENT_POS = %d\n\tMANTISSA_POS = %d\n\tDESCRIPTOR_POS = %d\n\tCALC_BUFFER_SIZE = %d\n\tcalc_buffer = %p\n\n", VALUE_SIZE, SIGN_POS, EXPONENT_POS, MANTISSA_POS, DESCRIPTOR_POS, CALC_BUFFER_SIZE, calc_buffer));
#ifdef HAVE_LONG_DOUBLE
    DEBUGPRINTF(("\tUsing long double (1-15-64) interface\n"));
#else
    DEBUGPRINTF(("\tUsing double (1-11-52) interface\n"));
#endif
#ifdef WORDS_BIGENDIAN
    DEBUGPRINTF(("\tWord order is big endian\n\n"));
#else
    DEBUGPRINTF(("\tWord order is little endian\n\n"));
#endif
  }
}

void finish_fltcalc (void) {
  free(calc_buffer); calc_buffer = NULL;
}

/* definition of interface functions */
FC_DEFINE2(add)
FC_DEFINE2(sub)
FC_DEFINE2(mul)
FC_DEFINE2(div)
FC_DEFINE1(neg)
FC_DEFINE1(int)
FC_DEFINE1(rnd)
