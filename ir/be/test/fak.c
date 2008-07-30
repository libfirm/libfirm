#include <stdio.h>
#include <stdlib.h>

int factorial(int x) {
	if (x == 0)
		return 1;

	return x * factorial(x-1);
}

int main(int argc, char **argv) {
	int val = atoi(argv[1]);
	printf("%d\n", factorial(val));
}
