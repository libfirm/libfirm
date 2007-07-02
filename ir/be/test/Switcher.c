/*
 * File name:   test/Switcher.c
 * Purpose:     test the switch statement
 * Author:      Boris Boesler
 * Modified by: Michael Beck
 * Created:     XX.02.2003
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003 Universitaet Karlsruhe
 * Licence:
 */

#include <stdio.h>

static void print_int(int i) {
    printf(" %d\n", i);
}

// standard switch with 1 case
static void switch1(int i) {
  switch(i) {
  case 0:
    print_int(0);
    break;
  default:
    print_int(-1);
    //break;
  }
}

// standard switch with more than 1 case
static void switch2(int i) {
  switch(i) {
  case 0:
    print_int(0);
    break;
  case 2:
    print_int(2);
    break;
  default:
    print_int(-1);
    break;
  }
}

// standard switch with fall through
static void switch3(int i) {
  switch(i) {
  case 0:
    print_int(0);
    // fall through
  case 3:
    print_int(3);
    break;
  default:
    print_int(-1);
    break;
  }
}

// standard switch without default
static void switch4(int i) {
  switch(i) {
  case 0:
    print_int(0);
    break;
  case 4:
    print_int(4);
    break;
  }
}

// standard switch without case
static void switch5(int i) {
  switch(i) {
  default:
    print_int(-1);
    break;
  }
}

// standard switch with more than 1 case and controlflow change
static void switch6(int i) {
  switch(i) {
  case 0:
    print_int(0);
    break;
  case 2:
    if(i < 6)
      print_int(2);
    else
      print_int(-2);
    break;
  default:
    print_int(-1);
    break;
  }
}

// standard switch with more than 1 case label
static void switch7(int i) {
  switch(i) {
  case 0:
    print_int(0);
    break;
  case 2:
  case 3:
      print_int(5);
    break;
  default:
    print_int(-1);
    break;
  }
}


static void double_switch(int i) {

    switch(i) {
    case 16:
	printf(" is 16\n");
	switch(i % 4) {
	case 0:
	    printf(" multiple of 4\n");
	    break;
	default:
	    printf(" not multiple of 4\n");
	    break;
	}
	break;

    default:
	printf(" != 10 und != 16\n");
	break;
    }

}

int main (int argc, char *argv[]) {
  printf("Switcher.c\n");
  printf(" must print:\n 0\n 2\n 0\n 3\n -1\n 2\n 5\n is 16\n multiple of 4\n\n");
  switch1(0);
  switch2(2);
  switch3(0);
  switch4(5);
  switch5(0);
  switch6(2);
  switch7(3);

  double_switch(16);
  return 0;
}
