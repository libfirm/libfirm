#include <stdio.h>

static int test(int a, int i)
{
	a &= ~(1 << (i & 0x0000001F));
	return a;
}

int A = 15;
int I = 3;

int main()
{
	printf("test(%d, %d) = %d\n", A, I, test(A,I));

	return 0;
}
