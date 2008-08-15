int main(void)
{
	int x = 1;
	asm("movl $0, %0" : "=m" (x));
	return x;
}
