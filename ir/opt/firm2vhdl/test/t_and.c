#include "testcase.h"

uint32_t test_atom(uint8_t control, uint32_t input0, uint32_t input1)
{
  return input0 & input1;
}

pattern patterns[] = {
  {42, 1, 1},
  {42, 4, 5},
  {42, 0x55555555, 0xFFCC1177},
  {42, -1, -1},
  {42, 0xffffffff, 1},
};

int npatterns = sizeof(patterns)/sizeof(patterns[0]);
