#include "testcase.h"

uint32_t __attribute__((special_instruction)) testvhdl(uint8_t control, uint32_t input0, uint32_t input1)
{
  return input0 - 2;
}

pattern patterns[] = {
  {42, 1, 1},
  {42, 4, 5},
  {42, 4, 0},
  {42, -1, -1},
  {42, 0xffffffff, 1},
};

int npatterns = sizeof(patterns)/sizeof(patterns[0]);

int main() {
    return testvhdl(0, 3, 4);
}
