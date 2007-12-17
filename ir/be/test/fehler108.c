#include <stdio.h>

int f(void) {
	return 42;
}

int (*f_ptr) (void) = &f;

int main(void) {
	(**printf)("Res: %d (should be 42)\n", (********f_ptr)());
	return 0;
}
