/* fltcalc.c
 * Authors: Matthias Heil
 */

/*
 * TODO:
 *
 * This code uses the C-type LLDBL to respesent floating
 * point values. This is bad because:
 *
 * 1.) It depends on IEEE arithmetic on the compilation engine (hardly a problem)
 * 2.) The existance on the type and its bits size (may be identical to double or even float)
 * 3.) Arithmetic operations will be done with "higher order" precision, which might be wrong
 *
 * Replace this code ASAP.
 */
#include "fltcalc.h"
#include <string.h>
#include <float.h>
#include <stdlib.h>
#include <stdio.h>

#ifdef USE_LONG_DOUBLE
/* have long double type */

/* only defined in C99 mode */
extern long double strtold(const char *str, char **end);

#define strtoLLD	strtold

#else
/* don't have long double type */

extern double strtod(const char *str, char **end);

#define strtoLLD	strtod

#endif

/********
 * globals
 ********/
static LLDBL value;

#define CAST_IN(val) (*((LLDBL *)((val))))
#define CAST_OUT(val) ((void *)&(val))

#define CLEAR_BUFFER() memset((char*)&value, 0, sizeof(LLDBL))
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
  return sizeof(LLDBL);
}

void fc_val_from_str(const char *str, unsigned int len)
{
  CLEAR_BUFFER();
  value = strtoLLD(str, NULL);
}

void fc_val_from_float(LLDBL l)
{
  CLEAR_BUFFER();
  value = l;
}

LLDBL fc_val_to_float(const void *val)
{
  return CAST_IN(val);
}

void fc_get_min(unsigned int num_bits)
{
  CLEAR_BUFFER();

  /*
   * Beware: FLT_MIN is the "Minimum normalised float",
   * not the smallest number in arithmetic sense
   */
  switch (num_bits) {
    case 32:
      value = -FLT_MAX;
      break;
    case 64:
      value = -DBL_MAX;
      break;
    case 80:
    default:
      value = -LDBL_MAX;
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
  value = strtoLLD("nan", NULL);

}

void fc_get_inf(void)
{
  value = strtoLLD("inf", NULL);
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
      break;
  }
}

int fc_comp(const void *a, const void *b)
{

  if (CAST_IN(a) == CAST_IN(b)) {
    return 0;
  }
  else if (CAST_IN(a) > CAST_IN(b)) {
    return 1;
  }
  else {
    return -1;
  }
}

char *fc_print_dec(const void *a, char *buf, int buflen)
{
#ifdef USE_LONG_DOUBLE
  snprintf(buf, buflen, "%1.30Lg", CAST_IN(a));
#else
  snprintf(buf, buflen, "%1.30g", CAST_IN(a));
#endif
  return buf;
}

unsigned char fc_sub_bits(const void *value, unsigned num_bits, unsigned byte_ofs)
{
  LLDBL val = CAST_IN(value);
  float f;
  double d;

  unsigned char *p;
  unsigned len;

  switch (num_bits) {
  case 32:
    f = (float)val;
    p = (unsigned char *)&f;
    len = 4;
    break;

  case 64:
    d = (double)val;
    p = (unsigned char *)&d;
    len = 8;
    break;

  case 80:
    p = (unsigned char *)&val;
    len = 10;
    break;

  default:
    return 0;
  }

  if (byte_ofs > len)
    return 0;
  return p[byte_ofs];
}
