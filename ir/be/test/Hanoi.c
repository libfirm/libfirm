/*
 * Project:     GCC-firm
 * File name:   test/Hanoi.c
 * Purpose:     Towers of hanoi
 * Author:      Arne Frick (in Sather-k)
 * Modified by: Michael Beck (for C)
 * Created:     XX.11.2001
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001 Universitaet Karlsruhe
 * Licence:
 */

#include <stdlib.h>
#include <stdio.h>

typedef int boolean;

#define true	1
#define false	0

static int stick[3];
static int moves;
static boolean verbose = false;

void init(int n) {
  //-- initializes an array of pegs
  stick[0] = n;
  stick[1] = 0;
  stick[2] = 0;
  moves = 0;
}

void hanoi(int n, int from, int to) {
  int spare = 3 - from - to;
  if(n > 0) {
    //-- moves the stack of pegs from stick[from] via stick[temp]
    //-- to stick[to]

    hanoi (n-1, from, spare);
    ++moves;
    stick[from] = stick[from] - 1;
    stick[to] = stick[to] + 1;
    if (verbose) {
      printf("move %d to %d\n", from, to);
    }
    hanoi (n-1, spare, to);
  }
}

//--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

int main(int argc, char *argv[]) {
  int i = 1;
  boolean n_specified = false;

  int n;

  /*
    if args.asize > i  and  args[i].equals ("-v") {
    verbose = true;
    i = i + 1;
    }
  */
  printf("Hanoi.c\n");

  if (argc <= 1) {
    /*
      if i /= args.asize - 1 {
      << " wrong # of arguments\n\n"
      "Usage: hanoi [-v] n\n"
      "   where n is the number of pegs\n"
      "   -v enables verbose printing of moves\n";
    */
      printf("Usage: hanoi n\nWhere n is the number of pegs.\nContinuing with default: n = 21 (-> 2097151)\n");
      n = 21;
  }
  else {
    n = atoi(argv[1]);
  }

  init (n);
  hanoi (n, 0, 2);
  //<< "Total #moves: " << p.moves << "\n";
  // hanoi(28) = 268435455
  printf(" number of moves : %d\n", moves);

  return 0;
}
