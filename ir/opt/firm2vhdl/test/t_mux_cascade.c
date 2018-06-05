#include "testcase.h"

uint32_t test_atom(uint8_t control, uint32_t input0, uint32_t input1)
{
  uint32_t r;
  if (control == 0) {
    r = input0 + input1;
  } else if (control == 1) {
    r = input0 - input1;
  } else if (control == 2) {
    r = input0 | input1;
  } else {
    r = input0 ^ input1;
  }
  return r;
}

pattern patterns[] = {
  {0, -666, 42},
  {1, -666, 42},
  {2, -666, 42},
  {3, -666, 42},
  {4, -666, 42},
};

int npatterns = sizeof(patterns)/sizeof(patterns[0]);
