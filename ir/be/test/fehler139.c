#include <stdio.h>

long long test(long long *i, int *p) {
	long long v = *i + (*p != 0);
	return v;
}

int main() {
	long long x = 0xFFFFFFFF;
	int       i = 5;
	printf("%llx\n", test(&x, &i));
	return 0;
}
