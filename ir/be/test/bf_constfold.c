#include <stdio.h>

/* Demonstrates a bug where constant folding ignores width of bitfields */

struct __attribute__((packed)) A
{
	unsigned int i:1, l:1, j:3, k:11;
};
struct A sA;

int main()
{
	unsigned int mask;
	struct A x;

	sA.k = -1;
	mask = sA.k;
	x = sA;

	printf("Val1: %x (expected 7ff) val2: %x (expected 7ff)\n", mask, x.k);

	return 0;
}
