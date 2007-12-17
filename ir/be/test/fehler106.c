#include <stdio.h>

long long k = 0x100000000LL;

double f(void) {
	return k;
}

int main(void) {
	printf("Res: %f\n", f());
	return 0;
}
