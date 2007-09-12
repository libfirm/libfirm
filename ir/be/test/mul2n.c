/* should produce only ONE mul, no conv's */
#include <stdio.h>

int a = 0x80000000;

int main(void)
{
	long long x = (long long)a * a;
	printf("%lld\n", x);
	return 0;
}
