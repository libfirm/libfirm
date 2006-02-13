int blubber(int a, int b, int c, int d) {
	int *x = &a;
	int *y = &b;

	*x = c;
	*y = d;

	return a * b;
}

int main(void) {
	return blubber(2, 3, 22, 33);
}
