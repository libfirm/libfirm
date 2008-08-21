
unsigned get(void)
{
	unsigned a;
	__asm__(" movl $17,%0 ": "=D"(a));
	return a;
}

int main(void) {
	printf("a: %u\n", get());
	return 0;
}
