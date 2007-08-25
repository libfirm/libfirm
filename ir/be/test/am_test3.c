int arr[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

int sum(int c) {
	int i, res = 0;

	for(i = 0; i < c; ++i) {
		res += i;
	}

	return res;
}

int main(void) {
	printf("Sum: %d\n", sum(10));
	return 0;
}
