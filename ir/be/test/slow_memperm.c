#include <stdio.h>

unsigned m = 7;

int main(void) {

	unsigned a;
	unsigned c = 0;

	for (a=0; a<=m; a++) {
		c = a;
	}

	printf("%d %d\n", a, c);

	return 0;
}
