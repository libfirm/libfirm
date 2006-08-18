unsigned long fib_iter(int n) {
	unsigned long a = 1, b = 1, c, i;

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
	int n = 10;

	if (argc > 1) {
		n = atoi(argv[1]);
	}

	printf("fib(%d) = %lu\n", n, fib_iter(n));

	return 0;
}
