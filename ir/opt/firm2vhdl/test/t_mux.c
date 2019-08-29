#include "testcase.h"

uint32_t test_atom(uint8_t control, uint32_t input0, uint32_t input1)
{
  uint32_t r;
  if (control)
    r = input0;
  else
    r = input1;
  return r;
}

pattern patterns[] = {
  {42, 1, 2},
  {0, 1, 2}
};

int npatterns = sizeof(patterns)/sizeof(patterns[0]);
