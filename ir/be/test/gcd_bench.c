#include <stdio.h>
#include <limits.h>
#include <assert.h>

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

void gcd_no_out(int numruns) {
  int i, j;

  for (i = 1; i < numruns; i++) {
    for (j = 1; j < numruns; j++) {
	  gcd(i, j);
	}
  }
}

void gcd_out(int numruns) {
  int i, j;

  for (i = 1; i < numruns; i++) {
    for (j = 1; j < numruns; j++) {
	  printf("gcd(%d, %d) = %d\n", i, j, gcd(i, j));
	}
  }
}

int main(int argc, char **argv)
{
  printf("gcd.c\n");

  if (argc == 1) {
    gcd_out(10);
  }
  else {
	int numruns = 1000;
	numruns = atoi(argv[1]);
    gcd_out(numruns);
  }

  return 0;
}
