#include <stdio.h>

void func(int *i)
{
	*i = 0;
}

int main (int argc, char *argv[]) {
	int i;

	printf("callref.c\n");

	func(&i);

	printf(" i = %d (should be 0)\n", i);

	return 0;
}
