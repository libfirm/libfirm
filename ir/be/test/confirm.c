#include <stdio.h>

int test(int a, int b) {
	int x = a * b;
	if (a == 0)
		return x;
	else
		return x - 1;
}

static int abs(int x) {
	if (x < 0)
		return -x;
	return x;
}

int test2(int a, int b) {
	if (a > 0) {
		return abs(a);
	}
	return b;
}

int test3(int a, int b) {
	if (a != 0) {
		b = b / -a;
	}
	return b;
}

int main(void) {
	printf("test() = %d\n", test(0, 1));
	printf("test() = %d\n", test2(1, 3));
	printf("test() = %d\n", test3(-3, 3));
	return 0;
}
