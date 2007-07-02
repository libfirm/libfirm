#include <stdio.h>

//
// GCC-firm Project
//
// $Id$
//
// Testprogram to test GCC-firm : Local.c

 /*
public long id_0(long i, int j, long k, long l) {
  return(i + 1);
}
  */
int id_1(int i, int j) {
  int k;
  int g;
  k = i + 1;
  g = j - 1;
  return((k * g) / 2);
}

int main (int argc, char *argv[]) {
  printf("Local.c\n");
  printf(" id_1(3,5) = %d\n", id_1(3,5));
  return 0;
}
