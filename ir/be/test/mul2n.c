/* should produce only ONE mul, no conv's */
#include <stdio.h>

int a = 0x80000000;

int main(void)
{
	unsigned long long x = 3 * (unsigned long long)a;
	printf("%lld\n", x);
	x = (unsigned long long)a * (unsigned long long)a;
	printf("%lld\n", x);
	return 0;
}
