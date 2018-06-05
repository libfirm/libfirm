#include "testcase.h"

uint32_t test_atom(uint8_t control, uint32_t input0, uint32_t input1)
{
  uint8_t c1, p1;
  int16_t temp;

  control = control & 3;

  c1 = -5;

  p1 = input0 >> 16;

  temp = (c1*p1);

  return temp;
}

pattern patterns[] = {
  {0, 0xd3f7c22d, 0x749cf9fb},
};

int npatterns = sizeof(patterns)/sizeof(patterns[0]);
