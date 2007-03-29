#include <stdio.h>

/*
 * Multiplication test for mul register constraints
 */
int a = -2, b = 3, c = -4, d = 6, e = -6;
unsigned ua = 2, ub = 3;

int main()
{
  int A, B, C;
  unsigned D;

  A = b * c;
  B = A * d;
  C = A * e;
  D = ua * ub;

  printf("Result: %d %u\n", A+B+C, D);

  return 0;
}
