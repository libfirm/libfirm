/*
 * Project:     GCC-firm
 * File name:   test/Queens.c
 * Purpose:     solve the queens problem
 * Author:      Markus Armbruster (in sather-k)
 * Modified by: Michael Beck (for GCC-firm)
 * Created:     XX.11.2001
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001 Universitaet Karlsruhe
 * Licence:
 */
/*
  -- The notorious n-queens problem (C.F. Gauss, 1850)
  -- Copyright (C) 1996 Markus Armbruster
*/

#include <stdlib.h>
#include <stdio.h>


typedef int boolean;

#define true	1
#define false	0

static int *row;
// queen in column c is at row[c]

static int myabs(int i) {
  if(0 > i)
    i = -i;
  return(i);
}

boolean place_ok (int i) {
  // return whether queen in column i is
  // not in check from queens left of it
  int j = 0;
  boolean res;

  while (j < i) {
    if ((row[j] == row[i]) || ((myabs(row[i]-row[j])) == (i-j))) {
      res = false;
      return(res);
    }
    j = j+1;
  }
  res = true;
  return(res);
}

int solve (int n) {
  // return the number of solutions to the n-queens problem
  int c = 0;
  int res = 0;

  row = (void *)malloc(sizeof(*row) * n);
  row[0] = -1;
  while (c >= 0) {
    row[c] = row[c]+1;
    while ((row[c] < n) && (!place_ok(c))) {
      row[c] = row[c]+1;
    }
    if (row[c] < n) { // successfully placed at (c,row[c])
      if (c == n-1)
	res = res+1;
      else {
	c = c+1;
	row[c] = -1;
      }
    }
    else // dead end, track back
      c = c-1;
  }
  free(row);

  return(res);
}

static void usage (const char *progname) {
  printf("usage: %s\n", progname);
}


int main (int argc, char *argv[]) {
  int n;

  switch (argc) {
  case 1:
    n = 8;
    break;
  case 2:
    n = atoi(argv[1]);
    break;
  default:
    usage("queens");
    return 0;
  }
  printf("The %d-queens problem has %d solutions.\n", n, solve(n));

  return 0;
}
