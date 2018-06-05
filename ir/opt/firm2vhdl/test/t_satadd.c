#include "testcase.h"
#include <limits.h>

/* Add saturated.  From libfirm's bitfiddle.h */
uint32_t test_atom(uint8_t control, uint32_t input0, uint32_t input1)
{
  int x,y;
  x = input0;
  y = input1;

  int sum      = x + y;
  /*
    An overflow occurs, if the sign of the both summands is equal
    and the one of the sum is different from the summand's one.
    The sign bit is 1, if an overflow occurred, 0 otherwise.
    int overflow = ~(x ^ y) & (sum ^ x);
  */
  int overflow = (x ^ sum) & (y ^ sum);

  /*
    The infinity to use.
    Make a mask of the sign bit of x and y (they are the same if an
    overflow occurred).
    INT_MIN == ~INT_MAX, so if the sign was negative, INT_MAX becomes
    INT_MIN.
  */
  int inf = (x >> (sizeof(x) * 8 - 1)) ^ INT_MAX;

  return overflow < 0 ? inf : sum;
}

pattern patterns[] = {
  {42, INT_MAX, 1},
  {42, INT_MAX, INT_MAX},
  {42, INT_MAX, INT_MIN},
  {42, INT_MIN, -1},
  {42, INT_MIN, INT_MIN},
  {42, -1, -1},
  {42, -1, -1},
  {42, 0xffffffff, 1},
  {42, 0x8fffffff, 0x80000001},
  {42, 0x8fffffff, 0x80000000},
};

int npatterns = sizeof(patterns)/sizeof(patterns[0]);
