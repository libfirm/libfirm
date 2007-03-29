#include "dumpmem.h"

struct bf {
  unsigned x:13;
  unsigned y:17;
  unsigned z:3;
  unsigned w:9;
} mybf;


int main(void) {

  mybf.x = 4097;
  mybf.y = 65537;
  mybf.z = 5;
  mybf.w = 257;

  dumpMem(&mybf, sizeof mybf);

  return 0;
}
