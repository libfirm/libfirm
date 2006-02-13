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

int main(void)
{
  int a = 49, b = 35;

  printf("gcd.c\n");
  printf("  GCD of %d and %d is %d\n", a, b, gcd(a,b));

  return 0;
}
