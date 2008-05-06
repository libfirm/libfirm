#include <stdio.h>

static unsigned fak(unsigned n)
{
  if (n == 0)
    return 1;

  return n * fak(n - 1);
}


int main(int argc, char *argv[])
{
  int x = 4;

  printf("%d! = %d\n", x, fak(x));

  return 0;
}
