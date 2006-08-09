#include <stdio.h>
#include <limits.h>

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

void gcd_no_out(void) {
  int i, j;

  for (i = 1; i < 10000; i++) {
    for (j = 1; j < 10000; j++) {
	  gcd(i, j);
	}
  }
}

void gcd_out(void) {
  int i, j;

  for (i = 1; i < 1000; i++) {
    for (j = 1; j < 1000; j++) {
	  printf("gcd(%d, %d) = %d\n", i, j, gcd(i, j));
	}
  }
}

int main(int argc)
{
  printf("gcd.c\n");

  if (argc > 1) {
    gcd_no_out();
  }
  else {
    gcd_out();
  }

  return 0;
}
