#include "testcase.h"

uint32_t test_atom(uint8_t control, uint32_t x, uint32_t input1)
{
  x -= ((x >> 1) & 0x55555555);
  x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
  x = (x + (x >> 4)) & 0x0f0f0f0f;
  x += x >> 8;
  x += x >> 16;
  return x & 0x3f;
}

pattern patterns[] = {
  {0, 0, 0},
  {0, 1, 0},
  {0, 3, 0},
  {0, 7, 0},
  {0, 0x55, 0},
  {0, 0xaa, 0},
  {0, 0x12345678, 0},
  {0, 0xffffffff, 0},
  {0, 0xdead9bfc, 0},
};

int npatterns = sizeof(patterns)/sizeof(patterns[0]);
