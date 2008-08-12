int main(void)
{
	int x = 19;
	asm("addl $23, %0" : "+r" (x));
	printf("%d\n", x);
	return x != 42;
}
