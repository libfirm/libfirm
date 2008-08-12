/*$ -fomit-frame-pointer -O3 $*/

int main(void)
{
	int in = 42;
	int out;
	__asm__(
			"xorl %%eax,%%eax\n"
			"xorl %%ebx,%%ebx\n"
			"xorl %%ecx,%%ecx\n"
			"xorl %%edx,%%edx\n"
			"xorl %%esi,%%esi\n"
			"xorl %%edi,%%edi\n"
			"movl %1,%0\n"
			"incl %0\n"
			: "=r" (out) : "r" (in)
			: "eax", "ebx", "ecx", "edx", "esi", "edi", "cc"
	);

	return out != 43;
}
