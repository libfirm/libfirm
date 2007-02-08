#include <stdio.h>

char C = 1;
short S = 2;
int I = 4;
long L = 4;
long long LL = 8;

float F = 4.0;
double D = 8.0;
long double LD = 10.0;

int a, b = 3;

int d = 4;

float fadd(float a, float b)
{
  return a + b;
}

float fadd_m(float a)
{
  return a + F;
}

int iadd(int a, int b)
{
  return a + b + (a*b) + (a / d);
}

int iadd_m(int a)
{
  return a + I;
}

int main (int argc, char *argv[]) {
  printf("Float Add   %f\n", fadd(F, 5));
  printf("Float Add+m %f\n", fadd_m(5));
  printf("Int   Add   %d\n", iadd(I, 5));
  printf("Int   Add+m %d\n", iadd_m(5));

  return 0;
}
