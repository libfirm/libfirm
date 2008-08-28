#include <stdio.h>

long long test(long *i, int p) {
	long long v = *i + (p != 0);
	return v;
}

int main() {
	long x = 0xFFFFFFFF;
	printf("%llx\n", test(&x, 5));
	return 0;
}
