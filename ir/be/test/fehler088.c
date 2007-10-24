/*$ -fno-inline $*/

int test(unsigned int a)
{
	return a > 0;
}

int main(void)
{
	printf("0xFFFFFFFFU > 0 is %d (should be 1)\n", test(0xFFFFFFFFU));
	return 0;
}
