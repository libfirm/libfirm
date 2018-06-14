#include "testcase.h"

uint32_t test_atom(uint8_t control, uint32_t input0, uint32_t input1)
{
  uint32_t i;
  int j = 0;
  do {
    i = j++ * input0;
  } while (j<5);

  return i;
}

pattern patterns[] = {
  {42, 1, 1},
  {42, 4, 5},
  {42, 4, 0},
  {42, -1, -1},
  {42, 0xffffffff, 1},
};

int npatterns = sizeof(patterns)/sizeof(patterns[0]);
