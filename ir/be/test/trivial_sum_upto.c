#include <stdio.h>

/*
 * A simple loop
 * Shows backendges.
 */
int sum_upto(int n)
{
  int i, res = 0;

  for(i = 0; i < n; ++i)
    res += i;

  return res;
}

int main(int argc, char *argv[])
{
  printf("%d\n", sum_upto(42));
  return 0;
}
