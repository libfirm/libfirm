char str[42];

int test(void)
{
	return str != 0;
}

int main(void)
{
	printf("%d (should be 1)\n", test());
	return 0;
}
