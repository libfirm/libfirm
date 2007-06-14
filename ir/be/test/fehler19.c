/*
 * compiler with -f no-inline
 */
#include <stdio.h>

#ifdef __GNUC__
#define NO_INLINE __attribute__((noinline))
#else
#define NO_INLINE __declspec(noinline)
#endif

static NO_INLINE void func(float a, float b, float *c, float *d);

static void func(float a, float b, float *c, float *d) {
  *c = a;
  *d = b;
}

int main(int argc, char *argv[]) {
  float a, b;

  func(3.0f, 4.0f, &a, &b);

  printf("a = %f (should be 3.0)\n", a);
  printf("b = %f (should be 4.0)\n", b);
}
