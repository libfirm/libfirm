int x = 42;
int y = 10;

int signed_div(int x, int y) {
	printf("%d (should be 4)\n", x / y);
	printf("%d (should be 2)\n", x % y);
	return (x / y) + (x % y);
}

unsigned int unsigned_div(unsigned int x, unsigned int y) {
	printf("%u (should be 4)\n", x / y);
	printf("%u (should be 2)\n", x % y);
	return (x / y) + (x % y);
}

double f_div(double x, double y) {
	printf("%f (should be 4.2)\n", x / y);
	return (x / y);
}

int main(void) {
	int x = signed_div(42, 10)
			+ unsigned_div(42, 10)
			+ f_div(42.0, 10.0);
	return 16-x;
}
