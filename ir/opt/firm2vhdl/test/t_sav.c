/* -- SAV operation takes 2 register input and outputs 1 */
/* -- register it calculates the absolute value of 4 16-bit */
/* -- values and then sum all these 4 and outputs in 1 register */

#include "testcase.h"

uint32_t test_atom(uint8_t control, uint32_t input0, uint32_t input1)
{
  int16_t inp0, inp1, inp2, inp3,
    abs0, abs1, abs2, abs3;
#define ABS(x) ( x<0 ? -x : x )

  inp0 = input0;
  inp1 = input0 >> 16;
  inp2 = input1;
  inp3 = input1 >> 16;

  abs0 = ABS(inp0);
  abs1 = ABS(inp1);
  abs2 = ABS(inp2);
  abs3 = ABS(inp3);

  return abs0 + abs1 + abs2 + abs3;
}

pattern patterns[] = {
  {0, 0x00010001, 0x00010001},
  {0, 0x80018001, 0x80018001},
  {0, 0xffffffff, 0xffffffff},
  {0, 0x80428666, 0x8007800b},
  {0, 0x052e32e8, 0x25ac30fb},
  {0, 0x33383541, 0x4ddea695},
  {0, 0xfa24dbf4, 0x2febf20c},
  {0, 0xd3f7c22d, 0x749cf9fb},
  {0, 0x9e5ffd9b, 0xac017df4},
  {0, 0xa6737487, 0x31b0ea8f},
  {0, 0xfe57219f, 0x73907090},
  {0, 0xcd862c0f, 0x7f75ba21},
  {0, 0x6dc02da1, 0x6dfd68ac},
  {0, 0xba76d265, 0xbc9286cd},
  {0, 0x301cd9fd, 0x195edcea},
  {0, 0x1382acda, 0x003c432a},
  {0, 0x98ce8009, 0x6838fe72},
  {0, 0x531da800, 0x9af1a229},
  {0, 0xa8d3a10e, 0x0022fba5},
  {0, 0xf86bd58b, 0x80b3c425},
  {0, 0x79d2f273, 0xbb873b07},
  {0, 0xd0ffd1ce, 0x777ba571},
  {0, 0xa8c2572d, 0x4b0e2842},
  {0, 0xfea0dcf6, 0xb0c65d83},
  {0, 0x97d10d00, 0xb01571d6},
  {0, 0xa93c028f, 0x4c86529f},
  {0, 0x367be445, 0xad1e1e2b},
  {0, 0x1ff18eef, 0x6c7901aa},
};

int npatterns = sizeof(patterns)/sizeof(patterns[0]);
