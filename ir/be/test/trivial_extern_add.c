/*
 * loads/stores from/to variables
 * Shows the sel node
 */
#include <stdio.h>

int a;
int b;
int c;

void add_a_and_b_to_c(void)
{
  c = a + b;
}

int main(int argc, char *argv[])
{
  a = 1;
  b = 42;
  add_a_and_b_to_c();
  printf("%d\n", c);
  return 0;
}
