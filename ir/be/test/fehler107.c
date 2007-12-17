#include <stdio.h>

double k = 4294967296.000000;

long long f(void) {
	return k;
}

int main(void) {
	printf("Res: %llx\n", f());
	return 0;
}
