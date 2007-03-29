/*
 * File name:   test/Swap.c
 * Purpose:     test swap of to variables in a loop
 * Author:      Boris Boesler
 * Modified by: Michael Beck
 * Created:     XX.02.2003
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003 Universitaet Karlsruhe
 * Licence:
 */

#include <stdio.h>

static void nop2(int i, int j) {
  printf("i = %d\n", i);
  printf("j = %d\n", j);
}

int main (int argc, char *argv[]) {
  int i, j, t, k;

  i = 1; j = 10; k = 3;

  while(0 < k) {
    nop2(i, j);

    t = i;
    i = j;
    j = t;

    //nop2(i, j);
    --k;
  }

  return 0;
}
