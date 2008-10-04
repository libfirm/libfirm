/*$ -fomit-frame-pointer $*/

#include <stdio.h>


unsigned __attribute__((noinline)) get_sp(void)
{
	unsigned esp;
	asm("mov %%esp, %0": "=r" (esp));
	return esp;
}


int main(void)
{
#ifndef __INTEL_COMPILER // ICC does not align the stack
	unsigned sp = get_sp();
	if (sp % 16 != 12) {
		printf("stack is unaligned after call: 0x%X\n", sp);
		return 1;
	}
#endif
	return 0;
}
