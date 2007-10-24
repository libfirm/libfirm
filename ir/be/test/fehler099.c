int c = 16;

unsigned long long test(int c)
{
	return 0xFFFFFFFFFFFFFFFFULL >> c;
}

int main(void)
{
	printf("0x%016llX (should be 0x0000FFFFFFFFFFFF)\n", test(c));
	return 0;
}
