#include <stdio.h>
#include <math.h>

int end = 4;
float k[] = { 2.8, 2.8, 2.8, 2.8, 2.8 };

void print_fpcw()
{
#ifdef __i386__
	int val = 0;
	__asm__ (
			"subl $4, %%esp\n"
			"fnstcw (%%esp)\n"
			"movzwl (%%esp), %0\n"
			"addl $4, %%esp\n" : "=r"(val));

	printf("%x\n", val);
#else
	printf("%d\n", (int) k[0]);
#endif
}

int main()
{
	int i;
	int res = 0;

	for(i = 0; i < end; ++i) {
		print_fpcw();
		res = (int) k[i];
	}
	print_fpcw();
	printf("%d\n", res);

	return 0;
}
