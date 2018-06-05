#include "testcase.h"

uint32_t test_atom(uint8_t control, uint32_t input0, uint32_t input1)
{
  uint32_t a, b, c;
  a = input0 & ~input1;
  b = input0 | input1;
  c = input0 ^ input1;
  return (~a ^ b ^ c);
}

pattern patterns[] = {
  {42, 1, 1},
  {42, 4, 5},
  {42, 0x55555555, 0xFFCC1177},
  {42, -1, -1},
  {42, 0xffffffff, 1},
  {42, 0x49202f07, 0xd500047b},
  {42, 0x5b491c58, 0xffbae83c},
  {42, 0xc50a210d, 0xf46527bb},
  {42, 0x6ab68480, 0x3cc089ea},
  {42, 0x0453d3ac, 0x20d4c141},
  {42, 0x6121cf0c, 0xa2589286},
  {42, 0x5b5a1b74, 0xff0afb29},
  {42, 0xdc8fe498, 0xf2058d60},
  {42, 0x1098fbb4, 0x9e17d33b},
  {42, 0x343ced5b, 0x8a423c05},
};

int npatterns = sizeof(patterns)/sizeof(patterns[0]);
