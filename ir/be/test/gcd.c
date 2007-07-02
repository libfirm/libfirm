#include <stdio.h>

int gcd(int a, int b)
{
    int i = 0;

    while(a != b)
    {
        if(a > b) {
            a = a - b;
        }
        else {
            b = b - a;
        }
        ++i;
    }
    return a;
}

int main(int argc, char** argv)
{
  int a = 49, b = 35, i, g = 0;

  if(argc > 2) {
      a = atoi(argv[1]);
      b = atoi(argv[2]);
  }

  printf("gcd.c\n");

  for (i = 0; i < 100000; i++) {
    g = gcd(a, b);
  }

  printf("  GCD of %d and %d is %d\n", a, b, g);

  return 0;
}
