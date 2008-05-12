#include <stdio.h>

int bttest(int x, int n) {
	if (x & (1 << n))
		return 1;
	return 0;
}

int nbttest(int x, int n) {
	if (!(x & (1 << n)))
		return 1;
	return 0;
}

int bttest1(int x, int n) {
	if ((x & (1 << n)) == (1 << n))
		return 1;
	return 0;
}

int bttest2(int x, int n) {
	if ((x & (1 << n)) != (1 << n))
		return 1;
	return 0;
}

int main() {
	printf("%d\n", bttest(128, 7));
	printf("%d\n", nbttest(128, 7));
	printf("%d\n", bttest1(128, 7));
	printf("%d\n", bttest2(128, 7));
	return 0;
}
