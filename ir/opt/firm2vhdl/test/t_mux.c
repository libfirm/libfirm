#include "testcase.h"

int32_t __attribute__((special_instruction)) test_atom(uint8_t control, int32_t input0, uint32_t input1)
{
//  uint32_t r;
  return input0 < 3 ? 42 : -input0;
/*  if (control)
    r = 1;
  else
    r = 0;
  return r;*/
}

pattern patterns[] = {
  {42, 1, 2},
  {0, 1, 2}
};

int npatterns = sizeof(patterns)/sizeof(patterns[0]);
