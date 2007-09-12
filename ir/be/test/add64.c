long long k;
long long k2;

void f(long long a, long long b)
{
	long long c = (a & 0xffffffffLL) | 0x100000000LL;
	k  = c + b;
	k2 = a + b;
}

int main(int argc, char **argv) {
	f(0x100000000LL, 0x100000000LL);
	printf("Res: %llx %llx\n", k, k2);
	return 0;
}
