/*$ -fno-inline -fno-if-conv $*/

long long k(long long a)
{
	return a < 0 ? -a : a;
}

int main(void)
{
	printf("%lld\n", k(0x80000000LL));
}
