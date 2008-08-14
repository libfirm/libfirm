int main(void)
{
	int x = 1;
	asm("mov $0, %0" : "=m" (x));
	return x;
}
