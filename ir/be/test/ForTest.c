//
// GCC-firm Project
//
// $Id$
//
// Testprogram to test GCC-firm : For loop

#include <stdio.h>

typedef int boolean;

#define true	1
#define false	0

static int simpleloop (int a, int b) {
	int i, j;
	boolean loopfinal = true;

	for(i = 0; (i < 10) && loopfinal; i++) {
		if(5 == i)
			loopfinal = false;
		printf("%d ", i);
	}
	printf("\n");

	for(i = 0; i < a; ++i) {
		for(j = 0; j < b; ++j) {
			printf("%d,%d\n", i, j);
		}
	}
	return(i);
}

int main (int argc, char *argv[]) {
	printf("ForTest.c\n");

	simpleloop(3, 10);
	return 0;
}
