//#include "dumpmem.h"

struct bf {
  int a;
  unsigned x:13;
  unsigned y:17;
  unsigned z:3;
  char c;
  double d;
  unsigned w:9;
};

struct bf mybf = { 0xffffffff, 4097, 65537, 5, 0xff, 4.5, 257 };

int main(void) {

  //dumpMem(&mybf, sizeof mybf);
  printf("sizeof mybf %d\n", sizeof mybf);

  return 0;
}
