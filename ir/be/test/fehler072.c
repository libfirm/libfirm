/*$ -fno-inline $*/
#include <stdio.h>

unsigned f(unsigned a, unsigned b)
{
	return (long long)a * b >> 16;
}


int main(void)
{
	printf("%X\n", f(0xFFFFFFFF, 1));
	return 0;
}
