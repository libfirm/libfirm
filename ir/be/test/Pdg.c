/*
  --
  -- beispiel fuer register allokation ueber program dependence graph
  --
*/

#include <stdio.h>

int main(int argc, char *argv[]) {
  int i, j, k;

  printf("Pdg.c\n");

  i = 1;
  k = 0;
  while (i < 10) {
    j = i + 1;
    if (j == 7)
      k = 0;
    else
      k = 1;
    i = i + 1;
  }

  (void) k;
  return 0;
}
