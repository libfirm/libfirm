/* fltcalc.c
 * Authors: Matthias Heil
 */

#include "fltcalc.h"
#include "ieee754.h"
#include <string.h>
#include <float.h>
#include <stdlib.h>
#include <stdio.h>

/********
 * globals
 ********/
static long double value;

#define CAST_IN(val) (*((long double *)((val))))
#define CAST_OUT(val) ((void *)&(val))

#define CLEAR_BUFFER() memset((char*)&value, 0, sizeof(long double))
/********
 * private functions
 ********/

/********
 * functions defined in fltcalc.h
 ********/
const void *fc_get_buffer(void)
{
  return CAST_OUT(value);
}

const int fc_get_buffer_length(void)
{
  return sizeof(long double);
}

void fc_val_from_str(const char *str, unsigned int len)
{
  CLEAR_BUFFER();
  value = strtold(str, NULL);
}

void fc_val_from_float(long double l)
{
  CLEAR_BUFFER();
  value = l;
}

long double fc_val_to_float(const void *val)
{
  return CAST_IN(val);
}

void fc_get_min(unsigned int num_bits)
{
  CLEAR_BUFFER();
  switch (num_bits)
  {
    case 32:
      value = FLT_MIN;
      break;
    case 64:
      value = DBL_MIN;
      break;
    case 80:
    default:
      value = LDBL_MIN;
      break;
  }
}

void fc_get_max(unsigned int num_bits)
{
  CLEAR_BUFFER();
  switch (num_bits)
  {
    case 32:
      value = FLT_MAX;
      break;
    case 64:
      value = DBL_MAX;
      break;
    case 80:
    default:
      value = LDBL_MAX;
      break;
  }
}

void fc_get_nan(void)
{
  /* nan: all exponent bit set, non-zero mantissa. not signalling wheni
   * msb of mantissa is set (easily found using this struct */
  union ieee854_long_double ld;

  CLEAR_BUFFER();
  ld.ieee_nan.negative = 0;
  ld.ieee_nan.exponent = 0x7FFF;
  ld.ieee_nan.quiet_nan = 1;
  ld.ieee_nan.mantissa0 = 42;

  value = ld.d;
}

void fc_get_inf(void)
{
  /* +-inf: all exponent bit set, sign is easy, one is strange XXX */
  union ieee854_long_double ld;

  CLEAR_BUFFER();
  ld.ieee_nan.negative = 0;
  ld.ieee_nan.exponent = 0x7FFF;
  ld.ieee_nan.quiet_nan = 0;
  ld.ieee_nan.one = 1;
  ld.ieee_nan.mantissa0 = 0;
  ld.ieee_nan.mantissa1 = 0;

  value = ld.d;
}

void fc_calc(const void *a, const void *b, int opcode)
{
  CLEAR_BUFFER();
  switch (opcode)
  {
    case FC_ADD:
      value = CAST_IN(a) + CAST_IN(b);
      break;
    case FC_SUB:
      value = CAST_IN(a) - CAST_IN(b);
      break;
    case FC_MUL:
      value = CAST_IN(a) * CAST_IN(b);
      break;
    case FC_DIV:
      value = CAST_IN(a) / CAST_IN(b);
      break;
    case FC_NEG:
      value = -CAST_IN(a);
  }
}

int fc_comp(const void *a, const void *b)
{
  if (CAST_IN(a) == CAST_IN(b)) return 0;
  else return (CAST_IN(a) > CAST_IN(b))?(1):(-1);
}

char *fc_print_dec(const void *a, char *buf, int buflen)
{
  snprintf(buf, buflen, "%1.30Lg", CAST_IN(a));
  return buf;
}
