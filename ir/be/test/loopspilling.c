#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int r = 0;
int a1 = 0,
	a2 = 0,
	a3 = 0,
	a4 = 0,
	a5 = 0,
	a6 = 0,
	a7 = 0,
	a8 = 0,
	a9 = 0,
	a10 = 0,
	a11 = 0;

int main() {
	int r1 = a1,
		r2 = a2,
		r3 = a3,
		r4 = a4,
		r5 = a5,
		r6 = a6,
		r7 = a7;
	int i, i2;

	for(i = 0; i < r; ++i) {
		int r8 = a8,
			r9 = a9,
			r10 = a10;
#if 1
		for(i2 = 0; i2 < r; ++i2) {
			printf("%d %d %d\n", i2, r2, r9);
		}
#endif

		printf("%d %d\n", i+1+r8+r9+r10, r2);
		i *= 2;
	}

	printf("%d", r1+r2+r3+r4+r5+r6+r7+i);
	return 0;
}
