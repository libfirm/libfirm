unsigned long fib_iter(int n) {
	unsigned long a = 1, b = 1, c = 0, i;

	if (n < 2)
		return 1;

	for (i = 1; i < n; i++) {
		c = a + b;
		a = b;
		b = c;
	}

	return c;
}

int main(int argc, char *argv[]) {
	int i, n = 10;

	if (argc > 1) {
		n = atoi(argv[1]);
	}

	for (i = 0; i <= n; i++)
		printf("fib(%d) = %lu\n", i, fib_iter(i));

	return 0;
}
