#include "dumpmem.h"

struct Bla {
	char  a : 2;
	short b : 15;
	int   c : 1;
};

struct Bla b = { 0, -1, 0 };

int main(void)
{
	dumpMem(&b, sizeof(struct Bla));
	printf("Sizeof %d\n", sizeof(struct Bla));
	printf("Vals: %d %d %d\n", b.a, b.b, b.c);
	return 0;
}
