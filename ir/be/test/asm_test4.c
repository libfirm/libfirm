unsigned long get_sp(void)
{
	unsigned long esp;
	__asm__("movl %%esp, %0" : "=mr" (esp));
	return esp;
}

int main(void) {
	printf("stack pointer available: %d\n", get_sp() > 42);
	return 0;
}
