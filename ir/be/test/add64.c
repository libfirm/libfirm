
long long f(long long a, long long b)
{
	long long c = (a & 0xffffffff) | 0x100000000;
	return a + b + c;
}

int main(int argc, char **argv) {
	printf("Res: %llx\n", f(0x100000000, 0x100000000));
}
