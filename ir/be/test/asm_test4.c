
unsigned long get_sp(void)
{
	  __asm__(" movl %esp,%eax ");
}

int main(void) {
	printf("stack pointer available: %d\n", get_sp() > 42);
	return 0;
}
