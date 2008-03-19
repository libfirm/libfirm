#include <setjmp.h>

int main(void) {
	jmp_buf buf;
	volatile int     k = 0;

	int val = setjmp(buf);
	printf("Val: %d K: %d\n", val, k);

	k = 1;
	if(val == 0)
		longjmp(buf, 20);

	return 0;
}
