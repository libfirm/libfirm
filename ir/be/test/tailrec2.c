#include <stdio.h>
#include <stdlib.h>

unsigned fak(unsigned n) {
	if (n == 0)
		return 1;
	return fak(n-1) * n;
}

int main(int argc, char *argv[]) {
	unsigned v = 7;

	if (argc > 1)
		v = atoi(argv[1]);
	printf("%u! = %u\n", v, fak(v));
}
