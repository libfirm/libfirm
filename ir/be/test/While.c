//
// $Id$
//
// Testprogram to test gcc-firm : While loop

#include <stdio.h>

static int test (int i) {
  int j;
  j = 0;
  while(i > 0) {
    i = i - 1;
    if(i == 3)
      break;
  }
  printf(" 3 == %d\n", i);
  return(j);
}

static int gcd (int a, int b) {
  int i = 0;

  while((a != b) && (i >= 0)){
    if(a > b) {
      a = a - b;
    }
    else {
      b = b - a;
    }
    ++i;
  }
  return(a);
}

int main (int argc, char *argv[]) {
  printf("While.c\n");
  printf(" 5 == %d\n", gcd(20, 15));
  test(10);
  return 0;
}
