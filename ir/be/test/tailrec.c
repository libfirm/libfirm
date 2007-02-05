#include <stdio.h>

static unsigned _fak(unsigned a, unsigned b)
{
  if (a == 0)
    return b;

  return _fak(a-1, a*b);
}

static fak(unsigned a)
{
  return _fak(a, 1);
}


int main(int argc, char *argv[])
{
  int x = 4;

  printf("%d! = %d\n", x, fak(x));

  return 0;
}
