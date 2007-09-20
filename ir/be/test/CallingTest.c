#include <stdio.h>

int int_func(void)
{
  return 42;
}

float float_func(void)
{
  return 13.5f;
}

double double_func(void)
{
  return 13.5;
}

int main(int argc, char *argv[])
{
  printf("calltest.c\n");

  printf("  Calling int    function: %d\n", int_func());
  printf("  Calling float  function: %f\n", float_func());
  printf("  Calling double function: %f\n", double_func());

  return 0;
}
