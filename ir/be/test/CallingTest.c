#include <stdio.h>

int int_func(void)
{
  return 42;
}

float float_func(void)
{
  return 13.5f;
}

int main(int argc, char *argv[])
{
  printf("calltest.c\n");

  printf("  Calling int   function: %d\n", int_func());
  printf("  Calling float function: %f\n", float_func());

  return 0;
}
