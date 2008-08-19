#include <stdio.h>

int main(int argc, char **argv) {
	printf("Result: %d (should be 42)\n", rot(21,1));

	return 0;
}

unsigned rot(unsigned x, unsigned y)
{
	return x << y | x >> (32-y);
}
