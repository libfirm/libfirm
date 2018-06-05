#include "testcase.h"

/* Popcount, but this allows the synthesis to use compressors instead of adders.
   (see http://www.altera.com/literature/manual/stx_cookbook.pdf)
*/

uint32_t test_atom(uint8_t control, uint32_t x, uint32_t input1)
{
  return
    ((x & (1<<0)) >>0)

    + ((x & (1<<2)) >>2)
    + ((x & (1<<3)) >>3)
    + ((x & (1<<4)) >>4)
    + ((x & (1<<5)) >>5)
    + ((x & (1<<6)) >>6)
    + ((x & (1<<7)) >>7)
    + ((x & (1<<8)) >>8)
    + ((x & (1<<9)) >>9)
    + ((x & (1<<10)) >>10)
    + ((x & (1<<11)) >>11)
    + ((x & (1<<12)) >>12)
    + ((x & (1<<13)) >>13)
    + ((x & (1<<14)) >>14)
    + ((x & (1<<15)) >>15)
    + ((x & (1<<16)) >>16)
    + ((x & (1<<17)) >>17)
    + ((x & (1<<18)) >>18)
    + ((x & (1<<19)) >>19)
    + ((x & (1<<20)) >>20)
    + ((x & (1<<21)) >>21)
    + ((x & (1<<22)) >>22)
    + ((x & (1<<23)) >>23)
    + ((x & (1<<24)) >>24)
    + ((x & (1<<25)) >>25)
    + ((x & (1<<26)) >>26)
    + ((x & (1<<27)) >>27)
    + ((x & (1<<28)) >>28)
    + ((x & (1<<29)) >>29)
    + ((x & (1<<30)) >>30)
    + ((x & (1<<31)) >>31)
    ;
}

pattern patterns[] = {
  {0, 0, 0},
  {0, 1, 0},
  {0, 3, 0},
  {0, 7, 0},
  {0, 0x55, 0},
  {0, 0xaa, 0},
  {0, 0x12345678, 0},
  {0, 0xffffffff, 0},
  {0, 0xdead9bfc, 0},
};

int npatterns = sizeof(patterns)/sizeof(patterns[0]);
