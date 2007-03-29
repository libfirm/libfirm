/*
 * File name:   test/BinaryOpTest.c
 * Purpose:     test binary operators
 * Author:      Boris Boesler
 * Modified by: Michael Beck
 * Created:     X.Y.2003
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003 Universitaet Karlsruhe
 * Licence:
 */

#include <stdio.h>

typedef int boolean;
#define true	1
#define false	0

static int id(int i) {
  return(i);
}

static boolean bid(boolean i) {
  return(i);
}

static void nop(int i) {
  printf("  %d\n", i);
}

static void test_and_and(int i, int j) {
  if((i + 5 == 1) && (j + 4 == 3))
    nop(1);
  else
    nop(2);
}

static void test_or_or(int i, int j) {
  if((i + 5 == 1) || (i + 4 == 3))
    nop(1);
  else
    nop(2);
}

int main(int argc, char *argv[]) {
  int i, j;
  int res;
  boolean b;

  printf("BinaryOpTest.c\n");

  b = bid(true);
  if(!b)
    i = id(1);
  else
    i = id(3);
  j = id(2);

  nop(i << j);
  nop(i >> j);
  nop(i & j);
  nop(i | j);
  nop(i ^ j);
  nop(~i);

  nop(i % j);
  nop(i / j);

  test_and_and(i,j);
  test_or_or(i,j);

  return 0;
}
