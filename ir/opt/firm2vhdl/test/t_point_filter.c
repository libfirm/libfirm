#include "testcase.h"

uint32_t test_atom(uint8_t control, uint32_t input0, uint32_t input1)
{
  /* -- Coefficients */
  uint8_t c0, c1, c2, c3, c4, c5, c6, c7;
  /* -- Pixels */
  uint8_t p0, p1, p2, p3, p4, p5, p6, p7;
  int16_t temp;

  control = control & 3;

  if (0 == control) {
    c0 = 1;
    c1 = -5;
    c2 = 20;
    c3 = 20;
    c4 = -5;
    c5 = 1;
    c6 = 0;
    c7 = 0;
  } else if (1 == control) {
    c0 = 0;
    c1 = 1;
    c2 = -5;
    c3 = 20;
    c4 = 20;
    c5 = -5;
    c6 = 1;
    c7 = 0;
  } else {
    c0 = 0;
    c1 = 0;
    c2 = 1;
    c3 = -5;
    c4 = 20;
    c5 = 20;
    c6 = -5;
    c7 = 1;
  }

  p0 = input0 >> 24;
  p1 = input0 >> 16;
  p2 = input0 >> 8;
  p3 = input0;
  p4 = input1 >> 24;
  p5 = input1 >> 16;
  p6 = input1 >> 8;
  p7 = input1;

  temp = (c0*p0)+(c1*p1)+(c2*p2)+(c3*p3)+(c4*p4)+(c5*p5)+(c6*p6)+(c7*p7);

  return temp;
}

pattern patterns[] = {
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

  {2, 0x052e32e8, 0x25ac30fb},
  {2, 0x33383541, 0x4ddea695},
  {2, 0xfa24dbf4, 0x2febf20c},
  {2, 0xd3f7c22d, 0x749cf9fb},
  {2, 0x9e5ffd9b, 0xac017df4},
  {2, 0xa6737487, 0x31b0ea8f},
  {2, 0xfe57219f, 0x73907090},
  {2, 0xcd862c0f, 0x7f75ba21},
  {2, 0x6dc02da1, 0x6dfd68ac},
  {2, 0xba76d265, 0xbc9286cd},
  {2, 0x301cd9fd, 0x195edcea},
  {2, 0x1382acda, 0x003c432a},
  {2, 0x98ce8009, 0x6838fe72},
  {2, 0x531da800, 0x9af1a229},
  {2, 0xa8d3a10e, 0x0022fba5},
  {2, 0xf86bd58b, 0x80b3c425},
  {2, 0x79d2f273, 0xbb873b07},
  {2, 0xd0ffd1ce, 0x777ba571},
  {2, 0xa8c2572d, 0x4b0e2842},
  {2, 0xfea0dcf6, 0xb0c65d83},
  {2, 0x97d10d00, 0xb01571d6},
  {2, 0xa93c028f, 0x4c86529f},
  {2, 0x367be445, 0xad1e1e2b},
  {2, 0x1ff18eef, 0x6c7901aa},

  {1, 0x052e32e8, 0x25ac30fb},
  {1, 0x33383541, 0x4ddea695},
  {1, 0xfa24dbf4, 0x2febf20c},
  {1, 0xd3f7c22d, 0x749cf9fb},
  {1, 0x9e5ffd9b, 0xac017df4},
  {1, 0xa6737487, 0x31b0ea8f},
  {1, 0xfe57219f, 0x73907090},
  {1, 0xcd862c0f, 0x7f75ba21},
  {1, 0x6dc02da1, 0x6dfd68ac},
  {1, 0xba76d265, 0xbc9286cd},
  {1, 0x301cd9fd, 0x195edcea},
  {1, 0x1382acda, 0x003c432a},
  {1, 0x98ce8009, 0x6838fe72},
  {1, 0x531da800, 0x9af1a229},
  {1, 0xa8d3a10e, 0x0022fba5},
  {1, 0xf86bd58b, 0x80b3c425},
  {1, 0x79d2f273, 0xbb873b07},
  {1, 0xd0ffd1ce, 0x777ba571},
  {1, 0xa8c2572d, 0x4b0e2842},
  {1, 0xfea0dcf6, 0xb0c65d83},
  {1, 0x97d10d00, 0xb01571d6},
  {1, 0xa93c028f, 0x4c86529f},
  {1, 0x367be445, 0xad1e1e2b},
  {1, 0x1ff18eef, 0x6c7901aa},
};

int npatterns = sizeof(patterns)/sizeof(patterns[0]);
