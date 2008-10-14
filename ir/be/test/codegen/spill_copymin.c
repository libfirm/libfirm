#include <stdio.h>

static void __attribute__((noinline)) f(void)
{
	/* firm spiller will probably spill %ebp which will result in 2 additional
	 * copies; %ebx->%ebp before asm and %ebp->%ebx at the end */
	unsigned eax = 0;
	unsigned ebx;
	unsigned ecx;
	unsigned edx;

	asm("cpuid" : "+a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx));

	unsigned buf[] = { ebx, edx, ecx };
	printf("%.12s\n", (char const*)buf);
}

int main(void)
{
	f();
	return 0;
}
