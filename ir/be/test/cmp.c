#include <stdio.h>

int cmp1(int a, int b)
{
  return a < b;
}

int cmp2(int a, int b)
{
  return a > b;
}

int cmp3(int a)
{
  return a < 3;
}

int cmp4(int a)
{
  return a > 3;
}

int cmp5(int a)
{
  return -3 < a;
}

int cmp6(int a)
{
  return -3 > a;
}

int cmp7(int a)
{
  return 3 < -a;
}

int cmp8(int a)
{
  return 3 > -a;
}

int cmp9(int a, int b)
{
  return -a < -b;
}

int cmp10(int a, int b)
{
  return a-b == 0;
}

int cmp11(int a)
{
  return a-3 != 5;
}

int cmp12(int a)
{
  return a+3 == 5;
}

int cmp13(int a)
{
  if (a < 0)
   return -a;
  else
   return a;
}

int cmp14(int a)
{
  return a == 0 ? a : -a;
}

int cmp15(int a)
{
  return a > 0;
}

int cmp16(int a)
{
  return a < 0 ? -1 : 0;
}

int cmp17(int a, int b)
{
  if (a < b)
    return a;
  return b;
}

int cmp18(int a, int b)
{
  if (a > b)
    return a;
  return b;
}

double dcmp1(double a, double b) {
  if (a < b)
    return a;
  else
    return b;
}

double dcmp2(double a, double b) {
  if (a > b)
    return a;
  else
    return b;
}

int A = 1;
int B = -1;
double Fa = 200.;
double Fb = 2;

int main()
{
  int a = A, b = B;
  double fa = Fa, fb = Fb;

  printf("cmp1(%d, %d) = %d\n", a, b, cmp1(a, b));
  printf("cmp2(%d, %d) = %d\n", a, b, cmp2(a, b));
  printf("cmp3(%d) = %d\n", a, cmp3(a));
  printf("cmp4(%d) = %d\n", a, cmp4(a));
  printf("cmp5(%d) = %d\n", a, cmp5(a));
  printf("cmp6(%d) = %d\n", a, cmp6(a));
  printf("cmp7(%d) = %d\n", a, cmp7(a));
  printf("cmp8(%d) = %d\n", a, cmp8(a));
  printf("cmp9(%d, %d) = %d\n", a, b, cmp9(a, b));
  printf("cmp10(%d, %d) = %d\n", a, b, cmp10(a, b));
  printf("cmp11(%d) = %d\n", a, cmp11(a));
  printf("cmp12(%d) = %d\n", a, cmp12(a));
  printf("cmp13(%d) = %d\n", a, cmp13(a));
  printf("cmp14(%d) = %d\n", a, cmp14(a));
  printf("cmp15(%d) = %d\n", a, cmp15(a));
  printf("cmp16(%d) = %d\n", a, cmp16(a));
  printf("cmp17(%d, %d) = %d\n", a, b, cmp18(a, b));
  printf("cmp18(%d, %d) = %d\n", a, b, cmp18(a, b));

  printf("dcmp1(%f, %f) = %f\n", -fa, -fb, dcmp1(-fa, -fb));
  printf("dcmp2(%f, %f) = %f\n", -fa, -fb, dcmp2(-fa, -fb));
  printf("dcmp1(%f, %f) = %f\n", fa, fb, dcmp1(fa, fb));
  printf("dcmp2(%f, %f) = %f\n", fa, fb, dcmp2(fa, fb));

	return 0;
}
