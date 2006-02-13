//
// $Id$
//

#include <stdio.h>

int f;

int poops(int i) {
  f++;
  return(i);
}

int main (int argc, char *argv[]) {
  int i;

  printf("Doit.c\n");

  i = 0;
  do {
    // i = i + 1;
  } while(i++ < 10);

  return 0;
}
