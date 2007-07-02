#include <stdio.h>

/*
 * Proj out of DF node
 */
int do_div(int a, int b) {
  return a / b;
}


int main(int argc, char *argv[])
{
  printf("%d\n", do_div(1, 2));
  return 0;
}
