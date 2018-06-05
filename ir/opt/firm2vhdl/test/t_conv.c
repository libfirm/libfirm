#include "testcase.h"

uint32_t test_atom(uint8_t control, uint32_t input0, uint32_t input1)
{
  int16_t ls16, hs16;
  int8_t s8;

  s8 = control;

  hs16 = s8;
  ls16 = control;

  return ls16 | (hs16 << 16);
}

pattern patterns[] = {
  {42, 0, 0},
  {0xff, 0, 0},
  {2, 0, 0},
  {0, 0, 0},
};

int npatterns = sizeof(patterns)/sizeof(patterns[0]);
