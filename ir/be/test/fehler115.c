int test(float x, double y) {
	return x + y;
}

int main() {
	printf("%d (expected 15)\n", test(7.753f, 8.222));
	return 0;
}
