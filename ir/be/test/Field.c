/*
 * $Id$
 */

#include <stdio.h>

struct s {
  int a;
};

int m(struct s *pThis) {
  return(pThis->a);
}

int main(void) {

  printf("Field.c\n");

  return 0;
}
