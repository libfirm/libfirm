#include "dumpmem.h"

struct bf {
  int a;
  unsigned x:13;
  unsigned y:17;
  unsigned z:3;
  char c;
  double d;
  unsigned w:9;
};

#define offsetof(TYPE, MEMB) ((size_t) &((TYPE *)0)->MEMB)

int main(void) {
  struct bf mybf = { 0xffffffff, 4097, 65537, 5, 0xff, 4.5, 257 };

  dumpMem(&mybf, sizeof mybf);
  printf("sizeof mybf %d\n", sizeof mybf);
  printf("offset a = %d\n", offsetof(struct bf, a));
  printf("offset c = %d\n", offsetof(struct bf, c));
  printf("offset d = %d\n", offsetof(struct bf, d));

  return 0;
}
