/*$ -fno-inline $*/

#include <stdio.h>
#include <limits.h>

int f(unsigned x) {
	return x < 10;
}

int main(void) {
	printf("Res: %d (should be 0)\n", f(INT_MIN));
	return 0;
}
