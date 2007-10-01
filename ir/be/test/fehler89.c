#include <stdio.h>

unsigned long long x = 43;

int main(void) {
	printf("Res: %lld (should be 42)\n", x - 1);
	return 0;
}
