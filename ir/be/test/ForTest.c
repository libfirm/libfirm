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

static void piff(void) {
	int i, j, x, delta_x;

	delta_x = 2;
	j = 0;
	x = 0;
	for (i = 0, x = 0; i < 1000; i++, x += delta_x) {
		j += x;
	}
}

static int simpleloop (int a, int b) {
	int i, j, delta_x, x;
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
	int i, j;

	printf("ForTest.c\n");

	simpleloop(3, 10);
	return 0;
}
