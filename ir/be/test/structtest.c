#include <stdio.h>

struct A {
  int x;
  int y;
};

int main(int argc, char *argv[])
{
  struct A a;

  printf("structtest.c\n");

  printf("sizeof struct(A) %ld\n", sizeof(struct A));
  printf("sizeof a         %ld\n", sizeof(a));

  a.x = 3;
  a.y = 4;

  printf("a.x = %d (should be 3)\n", a.x);
  printf("a.y = %d (should be 4)\n", a.y);

  return 0;
}
