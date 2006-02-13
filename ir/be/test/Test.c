#include <stdio.h>

void processOutputs() {
  switch(1) {
  case 0:
      if( 1 ) {
	  printf("1\n");
      }
      else {
	  printf("0\n");
      }
  }
}

int main(int argc, char *argv[]) {
  printf("Test.c\n");

  processOutputs();

  printf(" Ok\n");

  return 0;
}
