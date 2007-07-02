#include <stdio.h>

int test(int a)
{
  int b = 0xff;

  switch (a * b) {
    case 0:
      b *= a;
    case 1:
      b *= a;
      ++b;
      break;
    case 2:
      b /= a;
      break;
    case 3:
      b *= a*a;
      break;
    case 4:
      b = a % 3;
      break;
    case 5:
      b = 2;
      break;
    default:
      return 0;
  }
  return b;
}

int main()
{
  printf("SwitchTest\n");
  printf(" on %d %d\n", -1, test(-1));
  printf(" on %d %d\n", 0, test(0));
  printf(" on %d %d\n", 1, test(1));
  printf(" on %d %d\n", 2, test(2));
  printf(" on %d %d\n", 3, test(3));
  printf(" on %d %d\n", 4, test(4));
  printf(" on %d %d\n", 5, test(5));
  printf(" on %d %d\n", 6, test(6));

  return 0;
}
