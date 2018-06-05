#include "testcase.h"

uint32_t test_atom(uint8_t control, uint32_t input0, uint32_t input1)
{
  if (control == 1) {
    return input0 << 1;
  } else if (control == 2){
    return input0 >> 1;
  } else {
    return ((int32_t)input0) >> 1;
  }
}

pattern patterns[] = {
  {1, 0x55, 0},
  {1, -0x55, 0},
  {2, 0x55, 0},
  {2, -0x55, 0},
  {3, 0x55, 0},
  {3, -0x55, 0},
};

int npatterns = sizeof(patterns)/sizeof(patterns[0]);
