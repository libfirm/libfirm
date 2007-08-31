/*$ -fno-inline $*/

char arr[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
               18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 };

int sum(int c, int offs) {
	int i, res = 0;

	for(i = 0; i < c; ++i) {
		res += arr[i * 2 + offs + 2];
	}

	return res;
}

int main(void) {
	printf("Sum: %d\n", sum(10, 0));
	return 0;
}
