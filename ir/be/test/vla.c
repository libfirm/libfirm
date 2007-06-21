/* stabs debug info has problems with VLAs */

#include <stdlib.h>

void f(int x)
{
	int a[x];
	for (int i = 0; i < x; ++i) {
		a[i] = rand();
	}
}


int main(void)
{
	srand(23);
	f(rand() & 31);
	return 0;
}
