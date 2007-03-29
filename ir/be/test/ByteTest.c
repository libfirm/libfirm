#include <stdio.h>

int atoi(const char *s);

int main(int argc, char *argv[])
{
  unsigned char a, b, c;

  printf("ByteTest.c\n");

  a = atoi("200");
  b = atoi("56");

  c = a+b;
  printf(" 200 + 56 = %d (expected 0)\n", c);

  return 0;
}
