#include "ieee754.h"
#include "config.h"

static int __isnan(double d)
{
  union ieee754_double f;

  f.d = d;
  if (f.ieee.exponent == 0x7FF &&
    (f.ieee.mantissa0 != 0 ||
     f.ieee.mantissa1 != 0))
    return 1;

  return 0;
}

static int __isnanl(long double d)
{
  union ieee854_long_double f;

  f.d = d;
  if (f.ieee_nan.exponent == 0x7FFF &&
    (f.ieee_nan.quiet_nan ||
    f.ieee_nan.mantissa1 ||
    f.ieee_nan.mantissa0))
    return 1;
  return 0;
}

static int __isinf(double d)
{
  union ieee754_double f;

  f.d = d;
  if (f.ieee.exponent == 0x7FF &&
    f.ieee.mantissa0 == 0 &&
    f.ieee.mantissa1 == 0)
    return 1;

  return 0;
}

static int __isinfl(long double d)
{
  union ieee854_long_double f;

  f.d = d;
  if (f.ieee.exponent == 0x7FFF &&
      f.ieee_nan.mantissa1 == 0x80000000 &&
      f.ieee_nan.mantissa0 == 0)
    return 1;

  return 0;
}

#ifdef HAVE_LONG_DOUBLE
int isnan(long double d)
{
  return __isnanl(d);
}

int isinf(long double d)
{
  return __isinfl(d);
}

#else
int isnan(double d)
{
  return __isnan(d);
}

int isinf(double d)
{
  return __isinf(d);
}
#endif /* HAVE_LONG_DOUBLE */
