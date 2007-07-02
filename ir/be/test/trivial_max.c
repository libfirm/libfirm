#include <stdio.h>

/*
 * Control flow divertion in an if statement.
 */
int max(int a, int b) {
  return a > b ? a : b;
}

int main(int argc, char *argv[])
{
  printf("%d\n", max(1, 2));
  return 0;
}
