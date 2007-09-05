/**
 * check all kinds of multiplication with constants
 */

#define test_C(C) \
int test_##C(int i) { \
  return i * C; \
}
#define test_N(C) \
int test_m##C(int i) { \
  return i * -C; \
}
#include "mulconst.h"
#undef test_C
#undef test_N

#define test_C(C) printf("%d\n", test_##C(x));
#define test_N(C) printf("%d\n", test_m##C(x));

int x = 1;

int main() {
#include "mulconst.h"
}
