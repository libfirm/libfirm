/*
 * Project:     GCC-firm
 * File name:   test/Strings.c
 * Purpose:     test some String stuff
 * Author:      Boris Boesler
 * Modified by: Michael Beck
 * Created:     03.03.2003
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003 Universitaet Karlsruhe
 * Licence:
 */

#include <stdio.h>
#include <stdlib.h>

static void print_char(char c) {
  printf(" %c\n", c);
}

static void print_int(int i) {
  printf(" %d\n", i);
}

static void print_String(const char *s) {
  printf(" %s\n", s);
}

int main(int argc, char *argv[]) {
  int i;
  char *s = "test";

  printf("Strings.c\n");
  for(i = 1; i < argc; i++) {
    print_String(argv[i]);
  }
  print_String(s);
  print_char('\101');
  print_char('\x41');
  print_int(atoi("0"));
  print_int(atoi("1"));
  print_int(atoi("-4711"));
  print_int(atoi("0815"));
  print_int(atoi("+1001"));
  print_int(atoi("42"));
  print_int(atoi("000"));

  return 0;
}
