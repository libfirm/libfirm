unsigned long long f(void)
{
	unsigned long long res;
	asm(
		"mul %2\n\t"
		: "=A" (res)
		: "a" (0x10000), "r" (0x10000)
	);
	return res;
}

int main(void)
{
	printf("0x%llX\n", f());
	return 0;
}
