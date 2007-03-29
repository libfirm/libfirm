/*
 * File name:   test/CondExpr.c
 * Purpose:     test conditional expressions
 * Author:      Boris Boesler
 * Modified by: Michael Beck
 * Created:     XX.02.2003
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003 Universitaet Karlsruhe
 * Licence:
 */

#include <stdio.h>

static int id(int i) {
  return(i);
}

static void nop(int i) {
  printf("  i = %d\n", i);
}

int main (int argc, char *argv[]) {
  int i, j, res;

  printf("CondExpr.c:\n");
  i = id(1);
  j = id(2);

  i = (i == j)? id(10) : id(0);
  nop(i);

  return 0;
}
