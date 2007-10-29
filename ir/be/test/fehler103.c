unsigned long long test_shld(unsigned x)
{
	return ((unsigned long long)x << 32 | x) << x;
}

int main(void)
{
	return 0;
}
