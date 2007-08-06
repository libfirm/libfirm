#include <stdio.h>

static void f(int i)
{
	printf("%d (should be 42)\n", i);
}

void (*x)(int) = f;

int main(void)
{
	x(42);
	return 0;
}
